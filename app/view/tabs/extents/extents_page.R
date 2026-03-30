# app/view/tabs/extents/extents_page.R

# Purpose
# - Page-level UI and server for the "Extents" account.
# - Renders a delta-change leaflet map, plotly change chart, and gt account table.
# - Filters: Island, Realm, Ecosystem Type, Year A/B (via controlbar).

# Imports
box::use(
  shiny[
    NS, moduleServer, fluidRow, column, div, p, strong, icon, tagList, tags, h4,
    renderUI, uiOutput, req
  ],
  bs4Dash[box],
  htmltools[HTML],
  leaflet[
    leaflet, leafletOutput, renderLeaflet, addPolygons, addLegend, colorNumeric,
    addTiles, addProviderTiles, highlightOptions, providers, addLabelOnlyMarkers, labelOptions
  ],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_bars],
  dplyr[filter, arrange, desc, mutate],
  gt[gt, as_raw_html, fmt_number, cols_label, tab_options],
  scales[col_numeric, number]
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
          title = tagList(icon("layer-group"), strong("Ecosystem Extent Overview")),
          status = "olive", solidHeader = FALSE, width = 12,
          collapsible = TRUE, maximizable = TRUE,
          tagList(
            p(
              "The ecosystem extent accounts for Hawai\u02bbi track the spatial coverage (km\u00b2)
              of pre-defined marine and terrestrial ecosystem types aggregated by moku \u2014 traditional
              Hawaiian land-sea districts. Accounts follow the UN SEEA Ecosystem Accounting (SEEA EA) framework. See ",
              tags$a(
                href = "https://seea.un.org",
                target = "_blank",
                "https://seea.un.org"
              ),
              " for more information."
            ),
            p(
              strong("How to use: "),
              "Select an Island, Ecosystem Type, and two years (A and B) from the filter panel on the right.
              The map compares area (km\u00b2) by moku for year A vs year B. The chart and table summarize net change
              (B \u2212 A) across mokus."
            )
          )
        )
      )
    ),

    #-------------------------------------------------
    # Map
    #-------------------------------------------------
    fluidRow(
      column(
        width = 7,
        box(
          title = tagList(icon("globe-americas"), strong("Net Change Map (B − A)")),
          status = "olive", solidHeader = FALSE, width = 12, height = "600px",
          maximizable = TRUE,
          footer = "Choropleth: net change in extent area (km\u00b2) from baseline year (A) to comparison year (B).",
          leafletOutput(ns("map"), height = "560px")
        )
      ),
      column(
        width = 5,
        box(
          title = tagList(icon("chart-bar"), strong("Net Change by Moku (B − A)")),
          status = "info", solidHeader = FALSE, width = 12, height = "600px",
          maximizable = TRUE,
          footer = "Ranked bar chart: net change in area (km\u00b2) from baseline year (A) to comparison year (B).",
          plotlyOutput(ns("chart"), height = "560px")
        )
      )
    ),

    #-------------------------------------------------
    # Table
    #-------------------------------------------------
    fluidRow(
      column(
        width = 12,
        box(
          title = tagList(icon("table"), strong("Extent Account (A, B, and Change)")),
          status = "success", solidHeader = FALSE, width = 12,
          maximizable = TRUE,
          footer = "Account table by moku for the selected ecosystem type: area in A, area in B, and net change.",
          uiOutput(ns("table"))
        )
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
  extents_sf      = NULL,
  extents_df      = NULL,
  filtered_map_sf_a = NULL,
  filtered_map_sf_b = NULL,
  filtered_df     = NULL,
  selected        = NULL,
  change_tbl      = NULL,
  moku_names_lut  = NULL
) {
  moduleServer(id, function(input, output, session) {
    req(filtered_map_sf_a, filtered_map_sf_b, selected, change_tbl, moku_names_lut)

    # ------------------------------------------------------------------
    # Map (delta B - A)
    # ------------------------------------------------------------------
    output$map <- renderLeaflet({
      dat_a <- filtered_map_sf_a()
      dat_b <- filtered_map_sf_b()
      req(nrow(dat_a) > 0, nrow(dat_b) > 0)

      sel <- selected()
      chg <- change_tbl()

      # Join change metrics to geometry (use baseline geometry as the base layer)
      dat_sf <- dat_a |>
        dplyr::select(name2, moku_olelo, island_olelo) |>
        dplyr::left_join(
          chg |>
            dplyr::select(name2, area_a, area_b, delta, pct_delta),
          by = "name2"
        )

      # Use authoritative ʻōlelo moku names from lookup for labels
      dat_sf <- dat_sf |>
        dplyr::left_join(
          moku_names_lut |>
            dplyr::select(name2, moku_olelo_lut = moku_olelo),
          by = "name2"
        ) |>
        dplyr::mutate(
          moku_olelo_label = dplyr::if_else(
            !is.na(moku_olelo_lut) & nzchar(moku_olelo_lut),
            moku_olelo_lut,
            moku_olelo
          ),
          hover_label = paste0(
            "<b>",
            moku_olelo_label,
            "</b>",
            "<br/>A ",
            sel$year_a,
            ": ",
            scales::number(area_a, accuracy = 0.01),
            " km\u00b2",
            "<br/>B ",
            sel$year_b,
            ": ",
            scales::number(area_b, accuracy = 0.01),
            " km\u00b2",
            "<br/>\u0394 (B\u2212A): ",
            scales::number(delta, accuracy = 0.01),
            " km\u00b2",
            "<br/>%\u0394: ",
            scales::number(pct_delta, accuracy = 0.1),
            "%"
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

      map <- leaflet(dat_sf) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        addPolygons(
          fillColor   = ~pal(delta),
          fillOpacity = 0.75,
          color       = "#555555",
          weight      = 1,
          label       = ~hover_label,
          labelOptions = leaflet::labelOptions(
            textsize = "13px",
            direction = "auto"
          ),
          highlightOptions = highlightOptions(
            weight      = 3,
            color       = "#333",
            fillOpacity = 0.9,
            bringToFront = TRUE
          )
        )

      # Add bold ʻōlelo moku labels at polygon interior points
      pts <- sf::st_point_on_surface(dat_sf)
      xy <- sf::st_coordinates(pts)
      map |>
        addLabelOnlyMarkers(
          lng = xy[, 1],
          lat = xy[, 2],
          label = lapply(dat_sf$moku_olelo_label, function(x) htmltools::HTML(paste0("<b>", x, "</b>"))),
          labelOptions = labelOptions(
            noHide = TRUE,
            direction = "center",
            textOnly = TRUE,
            style = list(
              "color" = "rgba(255,255,255,0.95)",
              "font-size" = "12px",
              "font-weight" = "700",
              "text-shadow" = paste(
                "0 0 1px rgba(0,0,0,0.98),",
                "0 0 2px rgba(0,0,0,0.92),",
                "0 0 3px rgba(0,0,0,0.85),",
                "0 0 4px rgba(0,0,0,0.70)"
              )
            )
          )
        )
    })

    # ------------------------------------------------------------------
    # Chart — ranked delta by moku
    # ------------------------------------------------------------------
    output$chart <- renderPlotly({
      sel <- selected()
      dat <- change_tbl() |>
        arrange(desc(delta))
      req(nrow(dat) > 0)

      dat <- dat |>
        mutate(
          delta_dir = ifelse(is.na(delta), "No data", ifelse(delta >= 0, "Gain", "Loss"))
        )

      dat <- dat |>
        mutate(
          hover_text = paste0(
            moku_olelo,
            "<br>A ", sel$year_a, ": ", scales::number(area_a, accuracy = 0.01), " km\u00b2",
            "<br>B ", sel$year_b, ": ", scales::number(area_b, accuracy = 0.01), " km\u00b2",
            "<br>\u0394 (B\u2212A): ", scales::number(delta, accuracy = 0.01), " km\u00b2",
            "<br>%\u0394: ", scales::number(pct_delta, accuracy = 0.1), "%"
          )
        )

      p <- plot_ly(
        dat,
        x    = ~moku_olelo,
        y    = ~delta,
        color = ~delta_dir,
        colors = c(Gain = "#2ca25f", Loss = "#de2d26", `No data` = "#9e9e9e"),
        type = "bar",
        hovertext = ~hover_text,
        hoverinfo = "text"
      ) |>
        layout(
          barmode = "group",
          xaxis   = list(title = "Moku", tickangle = -45),
          yaxis   = list(
            title = paste0("Net change (", sel$year_b, " − ", sel$year_a, ") km\u00b2"),
            zeroline = TRUE,
            zerolinecolor = "rgba(0,0,0,0.35)"
          ),
          legend  = list(title = list(text = "Direction")),
          margin  = list(b = 100)
        )
      p
    })

    # ------------------------------------------------------------------
    # Table — A/B/change by moku for selected ecosystem type
    # ------------------------------------------------------------------
    output$table <- renderUI({
      sel <- selected()
      dat <- change_tbl()
      req(nrow(dat) > 0)

      dat_tbl <- dat |>
        arrange(desc(delta), moku_olelo) |>
        mutate(
          change = dplyr::case_when(
            is.na(delta) ~ "\u2014",
            delta > 0 ~ paste0("\u25B2 +", scales::number(delta, accuracy = 0.01)),
            delta < 0 ~ paste0("\u25BC ", scales::number(delta, accuracy = 0.01)),
            TRUE ~ paste0("\u2014 ", scales::number(delta, accuracy = 0.01))
          )
        ) |>
        dplyr::select(
          moku_olelo,
          island_olelo,
          realm,
          ecosystem_type,
          area_a,
          area_b,
          change,
          pct_delta
        )

      tbl <- gt(dat_tbl) |>
        cols_label(
          moku_olelo = "Moku",
          island_olelo = "Island",
          realm = "Realm",
          ecosystem_type = "Ecosystem Type",
          area_a = paste0("Area (km\u00b2) A ", sel$year_a),
          area_b = paste0("Area (km\u00b2) B ", sel$year_b),
          change = "Net change (km²)",
          pct_delta = "% change"
        ) |>
        fmt_number(columns = c(area_a, area_b), decimals = 2) |>
        fmt_number(columns = pct_delta, decimals = 1) |>
        tab_options(
          table.font.size = "12px",
          data_row.padding = "4px",
          table.width = gt::pct(100)
        )

      HTML(as_raw_html(tbl))
    })
  })
}
