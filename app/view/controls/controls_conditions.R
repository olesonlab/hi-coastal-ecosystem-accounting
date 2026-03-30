# app/view/controls/controls_conditions.R

# Purpose
# - Defines the scope-specific controlbar UI for the "Conditions" section.
# - Provides Island, Category, Indicator, and Value/Change mode filters.

# Imports
box::use(
  shiny[
    NS, tagList, div, p, selectInput, actionButton, moduleServer, reactive, conditionalPanel
  ],
  bslib[accordion, accordion_panel]
)

#' @export
ui <- function(
  id,
  island_choices = NULL,
  category_choices = NULL,
  indicator_choices = NULL,
  moku_choices = NULL,
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
        open = c("mode", "location", "indicator"),
        accordion_panel(
          title = "Mode",
          value = "mode",
          class = "filters-disclosure-body",
          selectInput(
            inputId  = ns("mode"),
            label    = "Analysis Mode",
            choices  = c("Value" = "value", "Change (B - A)" = "change"),
            selected = "value"
          )
        ),
        accordion_panel(
          title = "Location",
          value = "location",
          class = "filters-disclosure-body",
          p("Select an island to enable the moku selector.", class = "filters-help"),
          selectInput(
            inputId  = ns("island"),
            label    = "Island",
            choices  = c("Select island..." = "", island_choices),
            selected = ""
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] !== ''", ns("island")),
            selectInput(
              inputId = ns("moku"),
              label = "Moku",
              choices = c("All mokus" = "", moku_choices),
              selected = ""
            )
          )
        ),
        accordion_panel(
          title = "Indicator",
          value = "indicator",
          class = "filters-disclosure-body",
          selectInput(
            inputId  = ns("category"),
            label    = "Category",
            choices  = if (!is.null(category_choices)) category_choices else c("Marine Abiotic", "Marine Biotic", "Terrestrial"),
            selected = if (!is.null(category_choices)) category_choices[[1]] else "Marine Abiotic"
          ),
          selectInput(
            inputId  = ns("indicator"),
            label    = "Indicator",
            choices  = if (!is.null(indicator_choices)) indicator_choices else NULL,
            selected = if (!is.null(indicator_choices)) indicator_choices[[1]] else NULL
          )
        ),
        accordion_panel(
          title = "Time",
          value = "time",
          class = "filters-disclosure-body",
          conditionalPanel(
            condition = sprintf("input['%s'] === 'value'", ns("mode")),
            p("Value mode displays one map year.", class = "filters-help"),
            selectInput(
              inputId  = ns("year"),
              label    = "Year (Map)",
              choices  = if (!is.null(year_choices)) year_choices else c(2013, 2019),
              selected = if (!is.null(year_choices)) max(year_choices) else 2019
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] === 'change'", ns("mode")),
            p("Change mode compares baseline (A) against comparison year (B).", class = "filters-help"),
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
      island    = reactive(input$island),
      moku      = reactive(input$moku),
      category  = reactive(input$category),
      indicator = reactive(input$indicator),
      mode      = reactive(input$mode),
      year      = reactive(input$year),
      year_a    = reactive(input$year_a),
      year_b    = reactive(input$year_b),
      apply     = reactive(input$apply),
      reset     = reactive(input$reset)
    )
  })
}
