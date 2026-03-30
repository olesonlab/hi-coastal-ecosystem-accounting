# app/main.R

# Purpose
# - App entrypoint and wiring diagram.
# - Owns parent/global reactive state (navigation, filter state, data).
# - Wires layout slots (sidebar/body/controlbar) and routes what is shown based on nav scope.

# Rules
# - Keep main.R focused on orchestration.
# - Parent owns all state; feature and controls modules stay stateless.
# - Use shiny:: for reactive primitives (reactiveValues, observeEvent, req, renderUI, etc.).
# - Use box::use() for imports; do not use library().

# Imports
box::use(
  shiny[
    NS,
    moduleServer,
    uiOutput,
    reactive,
    reactiveValues,
    observeEvent,
    renderUI,
    req,
    tagList,
    div,
    updateSelectInput
  ],
  dplyr[filter, distinct, arrange],
  stats[setNames],
  jsonlite[toJSON]
)

# Modules
box::use(
  app/view/layout/nav_model[HOME, CONDITIONS, EXTENTS],
  app/view/layout/dashboard_shell,
  app/view/layout/nav,
  app/view/controls/controls_global,
  app/view/controls/controls_extents,
  app/view/controls/controls_conditions,
  app/view/tabs/extents/extents_page,
  app/view/tabs/conditions/conditions_page,
  app/logic/routing_spec,
  app/logic/data_loader,
  app/logic/extents_summaries,
  app/logic/conditions_summaries,
  app/logic/conditions_metadata
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  dashboard_shell$ui(
    id = ns("shell"),
    sidebar = nav$ui(ns("nav")),
    body = uiOutput(ns("body")),
    controlbar = uiOutput(ns("controlbar"))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    ########################
    # Data Loading
    ########################

    # Extents data
    extents_sf     <- data_loader$load_extents_sf()
    extents_df     <- data_loader$load_extents_df()
    filter_choices <- data_loader$get_filter_choices(extents_df)
    moku_names_lut <- data_loader$load_moku_names_lut()

    # Conditions data
    conditions_df  <- data_loader$load_conditions_df()
    moku_sf        <- data_loader$load_moku_sf()
    conditions_filter_choices <- data_loader$get_conditions_filter_choices(conditions_df)

    conditions_island_choices <- {
      island_lookup <- conditions_df |>
        distinct(island, island_olelo) |>
        filter(!is.na(island), nzchar(island)) |>
        arrange(island_olelo)
      setNames(island_lookup$island, island_lookup$island_olelo)
    }
    conditions_moku_choices <- stats::setNames(character(0), character(0))

    ########################
    # State
    ########################

    default_cond_category <- if ("Marine Abiotic" %in% conditions_filter_choices$categories) "Marine Abiotic" else conditions_filter_choices$categories[[1]]
    default_cond_indicator <- {
      candidates <- c("SST", "kd490")
      inds <- data_loader$get_indicators_for_category(conditions_df, default_cond_category)
      hit <- candidates[candidates %in% inds]
      if (length(hit) > 0) hit[[1]] else inds[[1]]
    }

    state <- reactiveValues(
      nav_scope = HOME,

      # Extents applied filter values
      applied_island          = filter_choices$islands[[1]],
      applied_moku            = "",
      applied_realm           = "All",
      applied_ecosystem_type  = filter_choices$ecosystem_types[[1]],
      applied_year_a          = min(filter_choices$years),
      applied_year_b          = max(filter_choices$years),

      # Conditions applied filter values
      applied_cond_island    = "",
      applied_cond_moku      = "",
      applied_cond_category  = default_cond_category,
      applied_cond_indicator = default_cond_indicator,
      applied_cond_mode      = "value",
      applied_cond_year      = max(conditions_filter_choices$years)
    )

    ########################
    # Navigation
    ########################

    selected_scope <- nav$server("nav")

    observeEvent(selected_scope(), {
      req(selected_scope())
      state$nav_scope <- selected_scope()
    })

    ########################
    # Extents Controls Server
    ########################

    extents_inputs <- controls_extents$server("controls_extents")

    observeEvent(extents_inputs$island(), {
      island_val <- extents_inputs$island()
      moku_choices <- data_loader$get_mokus_for_island(extents_df, island_val)
      updateSelectInput(
        session,
        inputId = "controls_extents-moku",
        choices = c("All mokus" = "", moku_choices),
        selected = ""
      )
    }, ignoreInit = FALSE)

    # Cascade: Update ecosystem type choices when realm changes
    observeEvent(extents_inputs$realm(), {
      realm_val <- extents_inputs$realm()
      new_types <- if (is.null(realm_val) || realm_val == "All") {
        filter_choices$ecosystem_types
      } else {
        data_loader$get_ecosystem_types_for_realm(extents_df, realm_val)
      }
      updateSelectInput(
        session,
        inputId  = "controls_extents-ecosystem_type",
        choices  = new_types,
        selected = new_types[[1]]
      )
    })

    # Apply extents filters when Apply button is clicked
    observeEvent(extents_inputs$apply(), {
      year_a <- as.integer(extents_inputs$year_a())
      year_b <- as.integer(extents_inputs$year_b())

      if (!is.na(year_a) && !is.na(year_b) && year_a == year_b) {
        year_a <- min(filter_choices$years)
        year_b <- max(filter_choices$years)
      } else if (!is.na(year_a) && !is.na(year_b) && year_a > year_b) {
        tmp <- year_a
        year_a <- year_b
        year_b <- tmp
      }

      state$applied_island         <- extents_inputs$island()
      state$applied_moku           <- extents_inputs$moku()
      state$applied_realm          <- extents_inputs$realm()
      state$applied_ecosystem_type <- extents_inputs$ecosystem_type()
      state$applied_year_a         <- year_a
      state$applied_year_b         <- year_b
    }, ignoreInit = TRUE)

    observeEvent(extents_inputs$reset(), {
      default_type <- filter_choices$ecosystem_types[[1]]
      default_island <- filter_choices$islands[[1]]
      default_moku_choices <- data_loader$get_mokus_for_island(extents_df, default_island)
      updateSelectInput(session, inputId = "controls_extents-island", choices = filter_choices$islands, selected = default_island)
      updateSelectInput(session, inputId = "controls_extents-moku", choices = c("All mokus" = "", default_moku_choices), selected = "")
      updateSelectInput(session, inputId = "controls_extents-realm", choices = c("All", filter_choices$realms), selected = "All")
      updateSelectInput(session, inputId = "controls_extents-ecosystem_type", choices = filter_choices$ecosystem_types, selected = default_type)
      updateSelectInput(session, inputId = "controls_extents-year_a", choices = filter_choices$years, selected = min(filter_choices$years))
      updateSelectInput(session, inputId = "controls_extents-year_b", choices = filter_choices$years, selected = max(filter_choices$years))

      state$applied_island <- default_island
      state$applied_moku <- ""
      state$applied_realm <- "All"
      state$applied_ecosystem_type <- default_type
      state$applied_year_a <- min(filter_choices$years)
      state$applied_year_b <- max(filter_choices$years)
    }, ignoreInit = TRUE)

    ########################
    # Conditions Controls Server
    ########################

    conditions_inputs <- controls_conditions$server("controls_conditions")

    # Cascade: Update indicator choices when category changes
    observeEvent(conditions_inputs$category(), {
      cat_val   <- conditions_inputs$category()
      new_inds  <- data_loader$get_indicators_for_category(conditions_df, cat_val)
      updateSelectInput(
        session,
        inputId  = "controls_conditions-indicator",
        choices  = new_inds,
        selected = new_inds[[1]]
      )
    })

    observeEvent(list(conditions_inputs$island(), conditions_inputs$category()), {
      island_val <- conditions_inputs$island()
      cat_val <- conditions_inputs$category()
      realm_val <- if (!is.null(cat_val) && grepl("Terrestrial", cat_val, ignore.case = TRUE)) "Terrestrial" else "Marine"
      if (is.null(island_val) || length(island_val) == 0 || !nzchar(island_val)) {
        updateSelectInput(
          session,
          inputId = "controls_conditions-moku",
          choices = c("All mokus" = ""),
          selected = ""
        )
        return()
      }
      moku_lookup <- tryCatch({
        conditions_df |>
          filter(realm == realm_val) |>
          filter(island == island_val) |>
          distinct(name2, moku_olelo) |>
          arrange(moku_olelo)
      }, error = function(e) {
        return(conditions_df[0, c("name2", "moku_olelo")])
      })
      moku_choices <- setNames(moku_lookup$name2, moku_lookup$moku_olelo)
      updateSelectInput(
        session,
        inputId = "controls_conditions-moku",
        choices = c("All mokus" = "", moku_choices),
        selected = ""
      )
    }, ignoreInit = FALSE)

    # Apply conditions filters when Apply button is clicked
    observeEvent(conditions_inputs$apply(), {
      year_a <- as.integer(conditions_inputs$year_a())
      year_b <- as.integer(conditions_inputs$year_b())
      if (!is.na(year_a) && !is.na(year_b) && year_a > year_b) {
        tmp <- year_a
        year_a <- year_b
        year_b <- tmp
      }

      island_val <- conditions_inputs$island()
      moku_val <- conditions_inputs$moku()

      state$applied_cond_island    <- island_val
      state$applied_cond_moku      <- if (!is.null(island_val) && nzchar(island_val)) moku_val else ""
      state$applied_cond_category  <- conditions_inputs$category()
      state$applied_cond_indicator <- conditions_inputs$indicator()
      state$applied_cond_mode      <- conditions_inputs$mode()
      state$applied_cond_year      <- conditions_inputs$year()
      state$applied_cond_year_a    <- year_a
      state$applied_cond_year_b    <- year_b
    }, ignoreInit = TRUE)

    observeEvent(conditions_inputs$reset(), {
      default_indicator_choices <- data_loader$get_indicators_for_category(conditions_df, default_cond_category)
      default_indicator <- if (length(default_indicator_choices) > 0) default_indicator_choices[[1]] else ""
      default_year <- max(conditions_filter_choices$years)
      default_year_a <- min(conditions_filter_choices$years)
      default_year_b <- max(conditions_filter_choices$years)

      updateSelectInput(session, inputId = "controls_conditions-mode", choices = c("Value" = "value", "Change (B - A)" = "change"), selected = "value")
      updateSelectInput(session, inputId = "controls_conditions-island", choices = c("Select island..." = "", conditions_island_choices), selected = "")
      updateSelectInput(session, inputId = "controls_conditions-moku", choices = c("All mokus" = ""), selected = "")
      updateSelectInput(session, inputId = "controls_conditions-category", choices = conditions_filter_choices$categories, selected = default_cond_category)
      updateSelectInput(session, inputId = "controls_conditions-indicator", choices = default_indicator_choices, selected = default_indicator)
      updateSelectInput(session, inputId = "controls_conditions-year", choices = conditions_filter_choices$years, selected = default_year)
      updateSelectInput(session, inputId = "controls_conditions-year_a", choices = conditions_filter_choices$years, selected = default_year_a)
      updateSelectInput(session, inputId = "controls_conditions-year_b", choices = conditions_filter_choices$years, selected = default_year_b)

      state$applied_cond_island <- ""
      state$applied_cond_moku <- ""
      state$applied_cond_category <- default_cond_category
      state$applied_cond_indicator <- default_indicator
      state$applied_cond_mode <- "value"
      state$applied_cond_year <- default_year
      state$applied_cond_year_a <- default_year_a
      state$applied_cond_year_b <- default_year_b
    }, ignoreInit = TRUE)

    ########################
    # Extents Filtered Data Reactives
    ########################

    # For map: filter by ecosystem_type + year (show all mokus for selected island)
    filtered_extents_map_sf_a <- reactive({
      dat <- extents_sf
      dat <- dat |> filter(island == state$applied_island)
      dat <- dat |> filter(year == as.integer(state$applied_year_a))

      realm_val <- state$applied_realm
      if (!is.null(realm_val) && realm_val != "All") {
        dat <- dat |> filter(realm == realm_val)
      }
      dat <- dat |> filter(ecosystem_type == state$applied_ecosystem_type)
      if (!is.null(state$applied_moku) && nzchar(state$applied_moku)) {
        dat <- dat |> filter(moku == state$applied_moku)
      }
      dat
    })

    filtered_extents_map_sf_b <- reactive({
      dat <- extents_sf
      dat <- dat |> filter(island == state$applied_island)
      dat <- dat |> filter(year == as.integer(state$applied_year_b))

      realm_val <- state$applied_realm
      if (!is.null(realm_val) && realm_val != "All") {
        dat <- dat |> filter(realm == realm_val)
      }
      dat <- dat |> filter(ecosystem_type == state$applied_ecosystem_type)
      if (!is.null(state$applied_moku) && nzchar(state$applied_moku)) {
        dat <- dat |> filter(moku == state$applied_moku)
      }
      dat
    })

    # For chart/table: filter by island (keep all years and ecosystem types)
    filtered_extents_df <- reactive({
      dat <- extents_df
      dat <- dat |> filter(island == state$applied_island)

      realm_val <- state$applied_realm
      if (!is.null(realm_val) && realm_val != "All") {
        dat <- dat |> filter(realm == realm_val)
      }
      if (!is.null(state$applied_moku) && nzchar(state$applied_moku)) {
        dat <- dat |> filter(moku == state$applied_moku)
      }
      dat
    })

    # Selected filter reactives for the extents page server
    extents_selected <- reactive(list(
      island         = state$applied_island,
      moku           = state$applied_moku,
      realm          = state$applied_realm,
      ecosystem_type = state$applied_ecosystem_type,
      year_a         = as.integer(state$applied_year_a),
      year_b         = as.integer(state$applied_year_b)
    ))

    extents_change_tbl <- reactive({
      sel <- extents_selected()
      extents_summaries$extents_change_table(
        extents_df = filtered_extents_df(),
        island = sel$island,
        realm = sel$realm,
        ecosystem_type = sel$ecosystem_type,
        year_a = sel$year_a,
        year_b = sel$year_b
      )
    })

    ########################
    # Conditions Filtered Data Reactives
    ########################

    filtered_conditions_df <- reactive({
      dat <- conditions_df |>
        filter(
          indicator == state$applied_cond_indicator,
          island == state$applied_cond_island,
          !is.na(value)
        )
      if (!is.null(state$applied_cond_moku) && nzchar(state$applied_cond_moku)) {
        dat <- dat |>
          filter(name2 == state$applied_cond_moku)
      }
      dat
    })

    conditions_selected <- reactive(list(
      indicator = state$applied_cond_indicator,
      year      = as.integer(state$applied_cond_year),
      category  = state$applied_cond_category,
      island    = state$applied_cond_island,
      moku      = state$applied_cond_moku,
      mode      = state$applied_cond_mode,
      year_a    = as.integer(state$applied_cond_year_a),
      year_b    = as.integer(state$applied_cond_year_b)
    ))

    conditions_island_summary_ts <- reactive({
      sel <- conditions_selected()
      conditions_summaries$conditions_island_summary_ts(
        conditions_df = conditions_df,
        indicator = sel$indicator,
        island = sel$island
      )
    })

    conditions_change_tbl <- reactive({
      sel <- conditions_selected()
      conditions_summaries$conditions_change_table(
        conditions_df = conditions_df,
        indicator = sel$indicator,
        island = sel$island,
        year_a = sel$year_a,
        year_b = sel$year_b
      )
    })

    conditions_indicator_meta <- reactive({
      sel <- conditions_selected()
      conditions_metadata$get_indicator_meta(sel$indicator)
    })

    ########################
    # Body Rendering
    ########################

    output$body <- renderUI({
      pages <- routing_spec$page_modules_by_scope()
      page  <- pages[[state$nav_scope]]

      if (is.null(page)) {
        div("Unknown scope: ", state$nav_scope)
      } else {
        page$ui(ns(paste0("page_", state$nav_scope)))
      }
    })

    # Wire extents page server
    observeEvent(state$nav_scope, {
      if (state$nav_scope == EXTENTS) {
        extents_page$server(
          paste0("page_", EXTENTS),
          extents_sf       = extents_sf,
          extents_df       = extents_df,
          filtered_map_sf_a  = filtered_extents_map_sf_a,
          filtered_map_sf_b  = filtered_extents_map_sf_b,
          filtered_df      = filtered_extents_df,
          selected         = extents_selected,
          change_tbl       = extents_change_tbl,
          moku_names_lut   = moku_names_lut
        )
      }
    })

    # Wire conditions page server
    observeEvent(state$nav_scope, {
      if (state$nav_scope == CONDITIONS) {
        conditions_page$server(
          paste0("page_", CONDITIONS),
          conditions_df = conditions_df,
          moku_sf       = moku_sf,
          filtered_df   = filtered_conditions_df,
          selected      = conditions_selected,
          island_summary_ts = conditions_island_summary_ts,
          change_tbl        = conditions_change_tbl,
          indicator_meta    = conditions_indicator_meta
        )
      }
    })

    ########################
    # Controlbar Rendering
    ########################

    output$controlbar <- renderUI({
      scope_controls <- routing_spec$control_modules_by_scope()

      # Build scope-specific controls UI
      scope_ui <- switch(
        state$nav_scope,

        extents = tagList(
          controls_extents$ui(
            ns("controls_extents"),
            island_choices         = filter_choices$islands,
            moku_choices           = data_loader$get_mokus_for_island(extents_df, state$applied_island),
            realm_choices          = filter_choices$realms,
            ecosystem_type_choices = filter_choices$ecosystem_types,
            year_choices           = filter_choices$years
          )
        ),

        conditions = tagList(
          controls_conditions$ui(
            ns("controls_conditions"),
            island_choices    = conditions_island_choices,
            moku_choices      = conditions_moku_choices,
            category_choices  = conditions_filter_choices$categories,
            indicator_choices = conditions_filter_choices$indicators_by_category[[1]],
            year_choices      = conditions_filter_choices$years
          )
        ),

        # Default: no controls for home, fisheries, recreation
        NULL
      )

      scope_ui
    })

  })
}
