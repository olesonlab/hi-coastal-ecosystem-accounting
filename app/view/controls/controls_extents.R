# app/view/controls/controls_extents.R

# Purpose
# - Defines the scope-specific controlbar UI for the "Extents" section.
# - Provides Realm, Ecosystem Type, and comparison period filters plus an Apply button.

# Imports
box::use(
  shiny[
    NS, tagList, div, p, selectInput, actionButton, moduleServer, reactive
  ],
  bslib[accordion, accordion_panel]
)

#' @export
ui <- function(
  id,
  island_choices = NULL,
  moku_choices = NULL,
  realm_choices = NULL,
  ecosystem_type_choices = NULL,
  year_choices = NULL
) {
  ns <- NS(id)

  years <- if (!is.null(year_choices)) sort(unique(as.integer(year_choices))) else c(2013L, 2016L, 2019L)
  year_a_default <- years[[1]]
  year_b_default <- years[[length(years)]]

  tagList(
    div(
      class = "filters-panel",
      accordion(
        multiple = TRUE,
        open = c("location", "ecosystem"),
        accordion_panel(
          title = "Location",
          value = "location",
          class = "filters-disclosure-body",
          selectInput(
            inputId  = ns("island"),
            label    = "Island",
            choices  = island_choices,
            selected = if (!is.null(island_choices)) island_choices[[1]] else NULL
          ),
          selectInput(
            inputId  = ns("moku"),
            label    = "Moku",
            choices  = c("All mokus" = "", moku_choices),
            selected = ""
          )
        ),
        accordion_panel(
          title = "Ecosystem",
          value = "ecosystem",
          class = "filters-disclosure-body",
          selectInput(
            inputId  = ns("realm"),
            label    = "Realm",
            choices  = if (!is.null(realm_choices)) c("All", realm_choices) else c("All", "Marine", "Terrestrial"),
            selected = "All"
          ),
          selectInput(
            inputId  = ns("ecosystem_type"),
            label    = "Ecosystem Type",
            choices  = if (!is.null(ecosystem_type_choices)) ecosystem_type_choices else NULL,
            selected = if (!is.null(ecosystem_type_choices)) ecosystem_type_choices[[1]] else NULL
          )
        ),
        accordion_panel(
          title = "Time Comparison",
          value = "time_comparison",
          class = "filters-disclosure-body",
          p("Compare baseline (A) and comparison year (B).", class = "filters-help"),
          selectInput(
            inputId  = ns("year_a"),
            label    = "Baseline Year (A)",
            choices  = years,
            selected = year_a_default
          ),
          selectInput(
            inputId  = ns("year_b"),
            label    = "Comparison Year (B)",
            choices  = years,
            selected = year_b_default
          )
        )
      ),
      div(
        class = "filters-actions",
        actionButton(
          inputId = ns("apply"),
          label   = "Apply Filters",
          class   = "btn-apply-filters"
        ),
        actionButton(
          inputId = ns("reset"),
          label   = "Reset",
          class   = "btn-reset-filters"
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    list(
      island         = reactive(input$island),
      moku           = reactive(input$moku),
      realm          = reactive(input$realm),
      ecosystem_type = reactive(input$ecosystem_type),
      year_a         = reactive(input$year_a),
      year_b         = reactive(input$year_b),
      apply          = reactive(input$apply),
      reset          = reactive(input$reset)
    )
  })
}
