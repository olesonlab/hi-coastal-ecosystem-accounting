# app/view/tabs/conditions/conditions_page.R

# Purpose
# - Page-level UI and server for the "Ecosystem Conditions" section.
# - Renders a leaflet choropleth map (value or change), focused trend chart, and ranked change bars.
# - Filters: Island, Category, Indicator, Mode, Year/YearA-YearB (via controlbar).

# Imports
box::use(
  shiny[
    NS, moduleServer, fluidRow, column, div, p, strong, icon, tagList, tags, h4, h5,
    req, reactiveVal, observeEvent, uiOutput, renderUI
  ],
  bs4Dash[box],
  htmltools[HTML],
  leaflet[
    leaflet, leafletOutput, renderLeaflet, addPolygons, addLegend, colorNumeric,
    addTiles, addProviderTiles, highlightOptions, providers, addLabelOnlyMarkers, labelOptions, fitBounds
  ],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_markers, add_lines, add_trace],
  dplyr[filter, left_join, select, arrange, mutate],
  sf[st_drop_geometry, st_point_on_surface, st_coordinates, st_bbox],
  scales[number],
  jsonlite[toJSON]
)

# =============================================================================
# UI
# =============================================================================

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    #-------------------------------------------------
    # Description
    #-------------------------------------------------
    fluidRow(
      column(
        width = 12,
        box(
          title = tagList(icon("heartbeat"), strong("Ecosystem Condition Overview")),
          status = "olive", solidHeader = FALSE, width = 12,
          collapsible = TRUE, maximizable = TRUE,
          tagList(
            p(
              "Ecosystem condition accounts track the biophysical health of marine and terrestrial
              ecosystems over time using the SEEA EA Ecosystem Condition Typology (ECT) framework.
              Marine indicators include water quality (kd490, SST), coral cover, fish functional
              groups, and disease prevalence. Terrestrial indicators include NDVI, rainfall, and
              temperature — all aggregated by moku."
            ),
            p(
              strong("How to use: "),
              "Select a Category, Indicator, and Year from the filter panel on the right.
              The ranked chart summarizes current values (or net change), and the trend chart
              shows how each moku's value changed across available years."
            ),
            p(
              class = "about-our-project-notes",
              tagList(
                icon("info-circle", class = "about-our-project-notes-icon"),
                " Marine biotic indicators (coral, fish, disease) report only moku with
                statistically significant change between 2013 and 2019 (p < 0.05, NOAA NCRMP)."
              )
            )
          )
        )
      )
    ),

    fluidRow(
      column(
        width = 6,
        uiOutput(ns("mode_panel"))
      ),
      column(
        width = 6,
        uiOutput(ns("trend_panel"))
      )
    )
  )
}

# =============================================================================
# Server
# =============================================================================

#' @export
server <- function(
  id,
  conditions_df = NULL,
  moku_sf       = NULL,
  filtered_df   = NULL,
  selected      = NULL,
  island_summary_ts = NULL,
  change_tbl = NULL,
  indicator_meta = NULL
) {
  moduleServer(id, function(input, output, session) {
    req(filtered_df, selected, conditions_df, moku_sf, island_summary_ts, change_tbl, indicator_meta)
    .AGENT_LOG_PATH <- "F:/projects/oleson_lab_projects/projects/current/hi-coastal-ecosystem-accounting/debug-2c6332.log"
    .agent_log <- function(hypothesisId, message, data = list(), runId = "pre-fix-1") {
      payload <- list(
        sessionId = "2c6332",
        runId = runId,
        hypothesisId = hypothesisId,
        location = "app/view/tabs/conditions/conditions_page.R",
        message = message,
        data = data,
        timestamp = as.numeric(Sys.time()) * 1000
      )
      cat(
        paste0(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"), "\n"),
        file = .AGENT_LOG_PATH,
        append = TRUE
      )
    }

    focus_name2 <- reactiveVal(NULL)

    output$mode_panel <- renderUI({
      sel <- selected()
      mode <- if (!is.null(sel$mode) && nzchar(sel$mode)) sel$mode else "value"
      has_island <- !is.null(sel$island) && nzchar(sel$island)
      panel_title <- if (identical(mode, "change")) {
        "Ranked Change by Moku (B − A)"
      } else {
        "Current Year Values by Moku"
      }
      panel_footer <- if (identical(mode, "change")) {
        "Ranked delta (Year B − Year A) by moku for the selected indicator."
      } else {
        "Ranked indicator values for the selected map year."
      }
      box(
        title = tagList(icon("chart-bar"), strong(panel_title)),
        status = if (identical(mode, "change")) "warning" else "info",
        solidHeader = FALSE, width = 12, height = "600px",
        maximizable = TRUE,
        footer = if (has_island) panel_footer else "Select an island and apply filters to show chart values.",
        plotlyOutput(session$ns("side_chart"), height = "520px")
      )
    })

    output$map_box <- renderUI({
      sel <- selected()
      mode <- if (!is.null(sel$mode) && nzchar(sel$mode)) sel$mode else "value"
      has_island <- !is.null(sel$island) && nzchar(sel$island)
      map_title <- if (identical(mode, "change")) {
        "Net Change Map by Moku (B − A)"
      } else {
        "Condition Indicator Map (by Moku)"
      }
      map_footer <- if (identical(mode, "change")) {
        "Choropleth: net change in indicator value from baseline year (A) to comparison year (B). Grey = no data."
      } else {
        "Choropleth: indicator value by moku for the selected year. Grey = no data."
      }
      box(
        title = tagList(icon("globe-americas"), strong(map_title)),
        status = "olive", solidHeader = FALSE, width = 12, height = "600px",
        maximizable = TRUE,
        footer = if (has_island) map_footer else "Select an island and apply filters to render the map.",
        if (has_island) {
          leafletOutput(session$ns("map"), height = "520px")
        } else {
          div(
            style = "height:520px; display:flex; align-items:center; justify-content:center; color:#666;",
            "Select an island in the filters to show the map."
          )
        }
      )
    })

    output$trend_panel <- renderUI({
      box(
        title = tagList(icon("chart-line"), strong("Indicator Trend by Moku")),
        status = "info", solidHeader = FALSE, width = 12, height = "600px",
        maximizable = TRUE,
        footer = "Trend chart: focus moku with island context (median and IQR across mokus).",
        plotlyOutput(session$ns("chart"), height = "520px")
      )
    })

    # ------------------------------------------------------------------
    # Map — Value or Change mode
    # ------------------------------------------------------------------
    output$map <- renderLeaflet({
      sel     <- selected()
      mode <- if (!is.null(sel$mode) && nzchar(sel$mode)) sel$mode else "value"
      req(!is.null(sel$island), nzchar(sel$island))
      dat_df <- filtered_df()
      # #region agent log
      .agent_log(
        "H3",
        "conditions map render entered",
        list(
          island = sel$island,
          category = sel$category,
          indicator = sel$indicator,
          mode = mode,
          year = sel$year,
          year_a = sel$year_a,
          year_b = sel$year_b,
          filtered_rows = nrow(dat_df)
        )
      )
      # #endregion

      # Determine realm from category (marine vs terrestrial)
      cat_realm <- if (grepl("Terrestrial", sel$category, ignore.case = TRUE)) "Terrestrial" else "Marine"

      # Get moku polygons filtered by realm/island
      moku_geom_base <- moku_sf |>
        filter(realm == cat_realm) |>
        filter(island == sel$island) |>
        dplyr::distinct(name2, .keep_all = TRUE) |>
        select(name2, moku, moku_olelo, island_olelo, island)

      moku_geom <- moku_geom_base
      if (!is.null(sel$moku) && nzchar(sel$moku)) {
        moku_geom <- moku_geom |>
          filter(name2 == sel$moku)
      }
      # If moku selection becomes stale/incompatible, fall back to island-wide view.
      if (nrow(moku_geom) == 0) {
        moku_geom <- moku_geom_base
      }
      # #region agent log
      .agent_log(
        "H1",
        "conditions map geometry filtered",
        list(
          cat_realm = cat_realm,
          moku_geom_base_rows = nrow(moku_geom_base),
          moku_geom_rows = nrow(moku_geom),
          selected_moku = sel$moku
        )
      )
      # #endregion
      req(nrow(moku_geom) > 0)

      if (identical(mode, "change")) {
        chg <- change_tbl()
        dat_sf <- moku_geom |>
          mutate(
            join_key = toupper(
              trimws(
                ifelse(!is.na(moku) & nzchar(moku), moku, name2)
              )
            )
          ) |>
          left_join(
            chg |>
              select(name2, moku, value_a, value_b, delta, pct_delta) |>
              mutate(
                join_key = toupper(
                  trimws(
                    ifelse(!is.na(moku) & nzchar(moku), moku, name2)
                  )
                )
              ) |>
              select(-name2, -moku),
            by = "join_key"
          ) |>
          select(-join_key)
        # #region agent log
        .agent_log(
          "H2",
          "conditions change map join complete",
          list(
            dat_sf_rows = nrow(dat_sf),
            na_delta_rows = sum(is.na(dat_sf$delta)),
            non_na_delta_rows = sum(!is.na(dat_sf$delta))
          )
        )
        # #endregion

        dat_sf <- dat_sf |>
          mutate(
            hover_label = paste0(
              "<b>", moku_olelo, "</b>",
              "<br/>A ", sel$year_a, ": ", scales::number(value_a, accuracy = 0.01),
              "<br/>B ", sel$year_b, ": ", scales::number(value_b, accuracy = 0.01),
              "<br/>\u0394 (B\u2212A): ", scales::number(delta, accuracy = 0.01),
              "<br/>%\u0394: ", scales::number(pct_delta, accuracy = 0.1), "%"
            )
          )
        dat_sf$hover_label <- lapply(dat_sf$hover_label, htmltools::HTML)

        max_abs <- max(abs(dat_sf$delta), na.rm = TRUE)
        if (!is.finite(max_abs) || max_abs == 0) max_abs <- 1

        pal <- colorNumeric(
          palette  = grDevices::colorRampPalette(c("#de2d26", "#f7f7f7", "#2ca25f"))(256),
          domain   = c(-max_abs, max_abs),
          na.color = "#cccccc"
        )

        bb <- sf::st_bbox(dat_sf)
        pts <- suppressWarnings(sf::st_point_on_surface(dat_sf))
        xy <- sf::st_coordinates(pts)
        map_obj <- leaflet(dat_sf) |>
          addProviderTiles(providers$CartoDB.Positron) |>
          addPolygons(
            fillColor   = ~pal(delta),
            fillOpacity = 0.75,
            color       = "#555",
            weight      = 1,
            layerId     = ~name2,
            label       = ~hover_label,
            labelOptions = leaflet::labelOptions(textsize = "13px", direction = "auto"),
            highlightOptions = highlightOptions(weight = 3, fillOpacity = 0.9, bringToFront = TRUE)
          ) |>
          addLabelOnlyMarkers(
            lng = xy[, 1],
            lat = xy[, 2],
            label = lapply(dat_sf$moku_olelo, function(x) htmltools::HTML(paste0("<b>", x, "</b>"))),
            labelOptions = labelOptions(
              noHide = TRUE,
              direction = "center",
              textOnly = TRUE,
              style = list(
                "color" = "rgba(255,255,255,0.95)",
                "font-size" = "12px",
                "font-weight" = "700",
                "text-shadow" = "0 0 1px rgba(0,0,0,0.98), 0 0 3px rgba(0,0,0,0.85)"
              )
            )
          ) |>
          fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
      } else {
        dat_year <- dat_df |> filter(year == sel$year)
        dat_sf <- moku_geom |>
          mutate(
            join_key = toupper(
              trimws(
                ifelse(!is.na(moku) & nzchar(moku), moku, name2)
              )
            )
          ) |>
          left_join(
            dat_year |>
              select(name2, moku, value, indicator) |>
              mutate(
                join_key = toupper(
                  trimws(
                    ifelse(!is.na(moku) & nzchar(moku), moku, name2)
                  )
                )
              ) |>
              select(-name2, -moku),
            by = "join_key"
          ) |>
          select(-join_key)
        # #region agent log
        .agent_log(
          "H2",
          "conditions value map join complete",
          list(
            dat_year_rows = nrow(dat_year),
            dat_sf_rows = nrow(dat_sf),
            na_value_rows = sum(is.na(dat_sf$value)),
            non_na_value_rows = sum(!is.na(dat_sf$value))
          )
        )
        # #endregion

        dat_sf <- dat_sf |>
          mutate(
            hover_label = paste0(
              "<b>", moku_olelo, "</b>",
              ifelse(!is.na(value), paste0("<br/>", scales::number(value, accuracy = 0.001)), "<br/>no data")
            )
          )
        dat_sf$hover_label <- lapply(dat_sf$hover_label, htmltools::HTML)

        has_value_domain <- any(!is.na(dat_sf$value))
        pal <- colorNumeric(
          palette  = "YlOrBr",
          domain   = if (has_value_domain) dat_sf$value else c(0, 1),
          na.color = "#cccccc"
        )

        bb <- sf::st_bbox(dat_sf)
        pts <- suppressWarnings(sf::st_point_on_surface(dat_sf))
        xy <- sf::st_coordinates(pts)
        map_obj <- leaflet(dat_sf) |>
          addProviderTiles(providers$CartoDB.Positron) |>
          addPolygons(
            fillColor   = ~pal(value),
            fillOpacity = 0.75,
            color       = "#555",
            weight      = 1,
            layerId     = ~name2,
            label       = ~hover_label,
            labelOptions = leaflet::labelOptions(textsize = "13px", direction = "auto"),
            highlightOptions = highlightOptions(weight = 3, fillOpacity = 0.9, bringToFront = TRUE)
          ) |>
          addLabelOnlyMarkers(
            lng = xy[, 1],
            lat = xy[, 2],
            label = lapply(dat_sf$moku_olelo, function(x) htmltools::HTML(paste0("<b>", x, "</b>"))),
            labelOptions = labelOptions(
              noHide = TRUE,
              direction = "center",
              textOnly = TRUE,
              style = list(
                "color" = "rgba(255,255,255,0.95)",
                "font-size" = "12px",
                "font-weight" = "700",
                "text-shadow" = "0 0 1px rgba(0,0,0,0.98), 0 0 3px rgba(0,0,0,0.85)"
              )
            )
          )
        if (has_value_domain) {
          map_obj <- map_obj |>
            addLegend(
              pal      = pal,
              values   = ~value,
              title    = sel$indicator,
              position = "bottomright",
              na.label = "No data"
            )
        }
        map_obj |>
          fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
      }
    })

    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      req(click$id)
      focus_name2(click$id)
    })

    # ------------------------------------------------------------------
    # Trend chart — focus moku + island context
    # ------------------------------------------------------------------
    output$chart <- renderPlotly({
      sel <- selected()
      if (is.null(sel$island) || !nzchar(sel$island)) {
        return(plotly::plotly_empty(type = "scatter", mode = "lines") |>
                 layout(annotations = list(list(
                   text = "Select an island and apply filters to show trend.",
                   showarrow = FALSE
                 ))))
      }
      dat <- filtered_df() |>
        arrange(name2, year)

      if (nrow(dat) == 0) {
        return(plotly::plotly_empty(type = "scatter", mode = "lines") |>
                 layout(annotations = list(list(
                   text = "No data available for current filter selection.",
                   showarrow = FALSE
                 ))))
      }

      # Use moku_olelo for labels (join from moku_sf) with explicit column name to avoid suffix collisions
      moku_labels <- st_drop_geometry(moku_sf) |>
        dplyr::distinct(name2, moku_olelo) |>
        dplyr::select(name2, moku_olelo_lut = moku_olelo)

      has_moku_olelo <- "moku_olelo" %in% names(dat)
      dat <- dat |>
        mutate(
          label_base = if (has_moku_olelo) {
            dplyr::if_else(!is.na(moku_olelo), moku_olelo, name2)
          } else {
            name2
          }
        ) |>
        left_join(moku_labels, by = "name2") |>
        mutate(label = dplyr::if_else(!is.na(moku_olelo_lut), moku_olelo_lut, label_base))

      focus <- focus_name2()
      if (is.null(focus) || !focus %in% dat$name2) {
        focus <- dat$name2[[1]]
      }

      d_focus <- dat |> filter(name2 == focus) |> arrange(year)
      if (nrow(d_focus) == 0) {
        return(plotly::plotly_empty(type = "scatter", mode = "lines") |>
                 layout(annotations = list(list(
                   text = "Select a moku with available data on the map.",
                   showarrow = FALSE
                 ))))
      }
      focus_label <- d_focus$label[[1]]

      ctx <- island_summary_ts()
      if (is.null(ctx) || nrow(ctx) == 0) {
        return(plotly::plotly_empty(type = "scatter", mode = "lines") |>
                 layout(annotations = list(list(
                   text = "No trend context available for this selection.",
                   showarrow = FALSE
                 ))))
      }

      p <- plot_ly() |>
        add_trace(
          data = ctx,
          x = ~year,
          y = ~q75,
          type = "scatter",
          mode = "lines",
          line = list(width = 0),
          showlegend = FALSE,
          hoverinfo = "skip"
        ) |>
        add_trace(
          data = ctx,
          x = ~year,
          y = ~q25,
          type = "scatter",
          mode = "lines",
          fill = "tonexty",
          fillcolor = "rgba(31,119,180,0.15)",
          line = list(width = 0),
          name = "Island IQR",
          hovertemplate = "Year: %{x}<br>IQR: %{y:.3f}<extra></extra>"
        ) |>
        add_lines(
          data = ctx,
          x = ~year,
          y = ~median,
          name = "Island median",
          line = list(color = "rgba(31,119,180,0.6)", width = 2),
          hovertemplate = "Year: %{x}<br>Median: %{y:.3f}<extra></extra>"
        ) |>
        add_lines(
          data = d_focus,
          x = ~year, y = ~value,
          name = paste0("Focus moku: ", focus_label),
          line = list(color = "rgba(44,62,80,1)", width = 3),
          hovertemplate = paste0(focus_label, "<br>Year: %{x}<br>Value: %{y:.3f}<extra></extra>")
        ) |>
        add_markers(
          data = d_focus,
          x = ~year, y = ~value,
          showlegend = FALSE,
          marker = list(size = 7, color = "rgba(44,62,80,1)")
        )

      has_ci_cols <- all(c("ci_lower", "ci_upper") %in% names(d_focus))
      if (has_ci_cols && !all(is.na(d_focus$ci_lower)) && !all(is.na(d_focus$ci_upper))) {
        p <- p |>
          add_trace(
            data = d_focus,
            x = ~year,
            y = ~ci_upper,
            type = "scatter",
            mode = "lines",
            line = list(width = 0),
            showlegend = FALSE,
            hoverinfo = "skip"
          ) |>
          add_trace(
            data = d_focus,
            x = ~year,
            y = ~ci_lower,
            type = "scatter",
            mode = "lines",
            fill = "tonexty",
            fillcolor = "rgba(44,62,80,0.10)",
            line = list(width = 0),
            name = "Focus 95% CI",
            hoverinfo = "skip",
            showlegend = TRUE
          )
      }

      p |>
        layout(
          xaxis  = list(title = "Year"),
          yaxis  = list(title = sel$indicator),
          legend = list(orientation = "v", x = 1.02),
          margin = list(r = 160)
        )
    })

    output$change_chart <- renderPlotly({
      sel <- selected()
      req(sel$mode)
      if (!identical(sel$mode, "change")) {
        return(plotly::plotly_empty(type = "scatter", mode = "lines") |>
                 layout(annotations = list(list(text = "Switch Mode to Change to view ranked deltas.", showarrow = FALSE))))
      }

      dat <- change_tbl() |>
        arrange(dplyr::desc(delta)) |>
        mutate(delta_dir = ifelse(is.na(delta), "No data", ifelse(delta >= 0, "Gain", "Loss")))
      if (nrow(dat) == 0) {
        return(plotly::plotly_empty(type = "scatter", mode = "lines") |>
                 layout(annotations = list(list(text = "No change data available for current filters.", showarrow = FALSE))))
      }

      plot_ly(
        dat,
        x = ~moku_olelo,
        y = ~delta,
        type = "bar",
        color = ~delta_dir,
        colors = c(Gain = "#2ca25f", Loss = "#de2d26", `No data` = "#9e9e9e"),
        hovertext = ~paste0(
          moku_olelo,
          "<br>A ", sel$year_a, ": ", scales::number(value_a, accuracy = 0.01),
          "<br>B ", sel$year_b, ": ", scales::number(value_b, accuracy = 0.01),
          "<br>\u0394 (B\u2212A): ", scales::number(delta, accuracy = 0.01),
          "<br>%\u0394: ", scales::number(pct_delta, accuracy = 0.1), "%"
        ),
        hoverinfo = "text"
      ) |>
        layout(
          xaxis = list(title = "Moku", tickangle = -45),
          yaxis = list(title = paste0("\u0394 (", sel$year_b, " \u2212 ", sel$year_a, ")"), zeroline = TRUE, zerolinecolor = "rgba(0,0,0,0.35)"),
          legend = list(title = list(text = "Direction")),
          margin = list(b = 120)
        )
    })

    output$side_chart <- renderPlotly({
      sel <- selected()
      mode <- if (!is.null(sel$mode) && nzchar(sel$mode)) sel$mode else "value"
      if (is.null(sel$island) || !nzchar(sel$island)) {
        return(plotly::plotly_empty(type = "scatter", mode = "lines") |>
                 layout(annotations = list(list(
                   text = "Select an island and apply filters to show chart.",
                   showarrow = FALSE
                 ))))
      }

      if (identical(mode, "change")) {
        dat_chg <- change_tbl() |>
          arrange(dplyr::desc(delta)) |>
          mutate(delta_dir = ifelse(is.na(delta), "No data", ifelse(delta >= 0, "Gain", "Loss")))
        if (nrow(dat_chg) == 0) {
          return(plotly::plotly_empty(type = "scatter", mode = "lines") |>
                   layout(annotations = list(list(
                     text = "No change data available for current filters.",
                     showarrow = FALSE
                   ))))
        }
        return(
          plot_ly(
            dat_chg,
            x = ~moku_olelo,
            y = ~delta,
            type = "bar",
            color = ~delta_dir,
            colors = c(Gain = "#2ca25f", Loss = "#de2d26", `No data` = "#9e9e9e"),
            hovertext = ~paste0(
              moku_olelo,
              "<br>A ", sel$year_a, ": ", scales::number(value_a, accuracy = 0.01),
              "<br>B ", sel$year_b, ": ", scales::number(value_b, accuracy = 0.01),
              "<br>\u0394 (B\u2212A): ", scales::number(delta, accuracy = 0.01),
              "<br>%\u0394: ", scales::number(pct_delta, accuracy = 0.1), "%"
            ),
            hoverinfo = "text"
          ) |>
            layout(
              xaxis = list(title = "Moku", tickangle = -45),
              yaxis = list(title = paste0("\u0394 (", sel$year_b, " \u2212 ", sel$year_a, ")"), zeroline = TRUE, zerolinecolor = "rgba(0,0,0,0.35)"),
              legend = list(title = list(text = "Direction")),
              margin = list(b = 120)
            )
        )
      }

      dat <- filtered_df() |>
        filter(year == sel$year) |>
        arrange(dplyr::desc(value))
      if (nrow(dat) == 0) {
        return(plotly::plotly_empty(type = "scatter", mode = "lines") |>
                 layout(annotations = list(list(
                   text = "No value data available for the selected year.",
                   showarrow = FALSE
                 ))))
      }

      plot_ly(
        dat,
        x = ~moku_olelo,
        y = ~value,
        type = "bar",
        marker = list(color = "#3c8dbc"),
        hovertext = ~paste0(
          moku_olelo,
          "<br>Year: ", year,
          "<br>Value: ", scales::number(value, accuracy = 0.001)
        ),
        hoverinfo = "text"
      ) |>
        layout(
          xaxis = list(title = "Moku", tickangle = -45),
          yaxis = list(title = sel$indicator),
          margin = list(b = 120)
        )
    })
  })
}
