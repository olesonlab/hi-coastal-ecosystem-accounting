# app/logic/data_loader.R

# Purpose
# - Loads processed data from the targets pipeline for use in the Shiny app.
# - Provides functions to read spatial (sf) and tabular data.
# - Data is loaded once at app startup and cached in main.R reactiveValues.

# Rules
# - Read-only: do not modify data files.
# - No Shiny reactivity here; this is pure R functions.
# - Returns data frames/sf objects for main.R to store in state.

# Imports
box::use(
  sf[st_read],
  readr[read_csv],
  dplyr[filter, pull, distinct, arrange, select],
  here[here]
)

# =============================================================================
# Shared lookups
# =============================================================================

#' Load moku names lookup (name2 -> ʻōlelo labels)
#' @return tibble with name2, island_olelo, moku_olelo, moku, island
#' @export
load_moku_names_lut <- function() {
  read_csv(
    here("data/02_interim/moku_names_lut.csv"),
    show_col_types = FALSE
  )
}

# =============================================================================
# Extents
# =============================================================================

#' Load extents data as sf object (with geometry)
#' @return sf object with extents data
#' @export
load_extents_sf <- function() {
  st_read(
    here("data/03_processed/extents/mokus_extents.gpkg"),
    quiet = TRUE
  )
}

#' Load extents data as tibble (without geometry)
#' @return tibble with extents data
#' @export
load_extents_df <- function() {
  read_csv(
    here("data/03_processed/extents/mokus_extents.csv"),
    show_col_types = FALSE
  )
}

#' Get unique filter values from extents data
#' Returns named vectors where names are display text (olelo) and values are filter keys
#' @param extents_df Extents data frame
#' @return List of named vectors for each filter dimension
#' @export
get_filter_choices <- function(extents_df) {
  # Islands: value = island, display = island_olelo
  island_lookup <- extents_df |>
    distinct(island, island_olelo) |>
    arrange(island_olelo)
  islands <- stats::setNames(island_lookup$island, island_lookup$island_olelo)

  # Mokus: value = moku, display = moku_olelo
  moku_lookup <- extents_df |>
    distinct(moku, moku_olelo) |>
    arrange(moku_olelo)
  mokus <- stats::setNames(moku_lookup$moku, moku_lookup$moku_olelo)

  # Years (no display transformation needed)
  years <- sort(unique(extents_df$year))

  # Ecosystem types (no display transformation needed)
  ecosystem_types <- sort(unique(extents_df$ecosystem_type))

  # Realms (no display transformation needed)
  realms <- sort(unique(extents_df$realm))

  list(
    islands = islands,
    mokus = mokus,
    years = years,
    ecosystem_types = ecosystem_types,
    realms = realms
  )
}

#' Get mokus for a given island (returns named vector)
#' @param extents_df Extents data frame
#' @param island Selected island (island column value, not olelo)
#' @return Named vector: names = moku_olelo (display), values = moku (filter key)
#' @export
get_mokus_for_island <- function(extents_df, island) {
  if (is.null(island) || length(island) == 0 || island == "") {
    moku_lookup <- extents_df |>
      distinct(moku, moku_olelo) |>
      arrange(moku_olelo)
  } else {
    moku_lookup <- extents_df |>
      filter(island == !!island) |>
      distinct(moku, moku_olelo) |>
      arrange(moku_olelo)
  }

  stats::setNames(moku_lookup$moku, moku_lookup$moku_olelo)
}

#' Get ecosystem types for a given realm
#' @param extents_df Extents data frame
#' @param realm Selected realm ("Marine" or "Terrestrial")
#' @return Character vector of ecosystem types for the realm
#' @export
get_ecosystem_types_for_realm <- function(extents_df, realm) {
  if (is.null(realm) || length(realm) == 0 || realm == "") {
    return(sort(unique(extents_df$ecosystem_type)))
  }

  extents_df |>
    filter(realm == !!realm) |>
    pull(ecosystem_type) |>
    unique() |>
    sort()
}

# =============================================================================
# Conditions
# =============================================================================

#' Load conditions data as sf object (with geometry, from GeoPackage)
#' @return sf object with conditions data and moku geometries
#' @export
load_conditions_sf <- function() {
  st_read(
    here("data/03_processed/conditions/mokus_conditions.gpkg"),
    quiet = TRUE
  )
}

#' Load conditions data as tibble (without geometry)
#' @return tibble with conditions data
#' @export
load_conditions_df <- function() {
  read_csv(
    here("data/03_processed/conditions/mokus_conditions.csv"),
    show_col_types = FALSE
  )
}

#' Load moku geometries (no conditions data — just polygons for map base)
#' @return sf object with moku polygons
#' @export
load_moku_sf <- function() {
  st_read(
    here("data/02_interim/mokus_combined.gpkg"),
    quiet = TRUE
  )
}

#' Get unique filter values from conditions data
#' @param conditions_df Conditions data frame
#' @return List with categories, indicators (nested by category), years, islands, mokus
#' @export
get_conditions_filter_choices <- function(conditions_df) {
  categories <- sort(unique(conditions_df$category))

  # Indicators nested by category
  indicators_by_category <- lapply(
    stats::setNames(categories, categories),
    function(cat) {
      conditions_df |>
        filter(category == cat) |>
        pull(indicator) |>
        unique() |>
        sort()
    }
  )

  years   <- sort(unique(conditions_df$year))
  islands <- sort(unique(conditions_df$island[!is.na(conditions_df$island)]))

  list(
    categories = categories,
    indicators_by_category = indicators_by_category,
    years = years,
    islands = islands
  )
}

#' Get indicators for a given condition category
#' @param conditions_df Conditions data frame
#' @param category Selected category
#' @return Character vector of indicators
#' @export
get_indicators_for_category <- function(conditions_df, category) {
  if (is.null(category) || category == "") {
    return(sort(unique(conditions_df$indicator)))
  }
  conditions_df |>
    filter(category == !!category) |>
    pull(indicator) |>
    unique() |>
    sort()
}
