# app/logic/routing_spec.R
#
# Purpose
# - Centralizes the page + control mappings by scope for testability.
# - Keeps main.R focused on orchestration while letting testthat verify coverage.
#
# Rules
# - Data-only: return vectors/lists of module paths or identifiers.
# - No Shiny side effects, no data loading.

box::use(
  app/view/layout/nav_model[
    HOME,
    EXTENTS,
    CONDITIONS
  ],
  app/view/tabs/home/home_page,
  app/view/tabs/extents/extents_page,
  app/view/tabs/conditions/conditions_page,
  app/view/controls/controls_extents,
  app/view/controls/controls_conditions
)

#' @export
page_modules_by_scope <- function() {
  list(
    home = home_page,
    extents = extents_page,
    conditions = conditions_page
  )
}

#' @export
control_modules_by_scope <- function() {
  list(
    home = NULL,
    extents = controls_extents,
    conditions = controls_conditions
  )
}
