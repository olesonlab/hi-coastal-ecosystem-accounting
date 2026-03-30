# app/view/controls/controls_global.R

# Purpose
# - Defines the global controlbar UI that applies across all dashboard sections (scopes).
# - Provides shared filters: Island and Year.

# Imports
box::use(
  shiny[NS, tagList, div, selectInput, actionButton, moduleServer, reactive],
  bslib[accordion, accordion_panel]
)

#' @export
ui <- function(id, island_choices = NULL, moku_choices = NULL, year_choices = NULL, show_year = TRUE) {
  ns <- NS(id)

  tagList(
    div(
      class = "filters-panel",
      accordion(
        multiple = TRUE,
        open = "location",
        accordion_panel(
          title = "Location",
          value = "location",
          class = "filters-disclosure-body",
          selectInput(
            inputId = ns("island"),
            label   = "Island",
            choices = island_choices,
            selected = if (!is.null(island_choices)) island_choices[[1]] else NULL
          )
        ),
      if (isTRUE(show_year)) {
        accordion_panel(
            title = "Time",
            value = "time",
            class = "filters-disclosure-body",
            selectInput(
              inputId  = ns("year"),
              label    = "Year",
              choices  = if (!is.null(year_choices)) year_choices else c(2013, 2016, 2019),
              selected = if (!is.null(year_choices)) max(year_choices) else 2019
            )
          
        )
      },
      ),
      div(
        class = "filters-actions",
        actionButton(
          inputId = ns("reset"),
          label   = "Reset Global Filters",
          class   = "btn-reset-filters"
        )
      )
    )
  )
}

#' @export
server <- function(id, extents_df = NULL) {
  moduleServer(id, function(input, output, session) {
    list(
      island = reactive(input$island),
      moku   = reactive(NULL),   # moku cascading handled in main.R
      year   = reactive(input$year),
      reset  = reactive(input$reset)
    )
  })
}
