# app/logic/extents_summaries.R
#
# Purpose
# - Pure helpers to compute extent A/B comparisons and deltas by moku.
# - Keeps wrangling out of Shiny modules.
#
# Rules
# - No Shiny, no file I/O.

box::use(
  dplyr[filter, mutate, transmute, arrange, left_join, select, distinct],
  tidyr[pivot_wider]
)

#' Build an A/B comparison table by moku for a single ecosystem type.
#'
#' @param extents_df Extents table (no geometry).
#' @param island Island key (e.g., "kauai").
#' @param realm Realm ("All", "Marine", "Terrestrial").
#' @param ecosystem_type Ecosystem type string.
#' @param year_a Baseline year.
#' @param year_b Comparison year.
#' @return Tibble with moku labels and area_a/area_b/delta/pct_delta.
#' @export
extents_change_table <- function(extents_df, island, realm, ecosystem_type, year_a, year_b) {
  stopifnot(!is.null(extents_df))
  stopifnot(!is.null(island), nzchar(island))
  stopifnot(!is.null(ecosystem_type), nzchar(ecosystem_type))
  stopifnot(!is.null(year_a), !is.null(year_b))

  dat <- extents_df |>
    filter(
      island == !!island,
      ecosystem_type == !!ecosystem_type,
      year %in% c(as.integer(year_a), as.integer(year_b))
    )

  if (!is.null(realm) && realm != "All") {
    dat <- dat |> filter(realm == !!realm)
  }

  wide <- dat |>
    transmute(
      name2,
      moku,
      moku_olelo,
      island,
      island_olelo,
      realm,
      ecosystem_type,
      year = as.integer(year),
      area_km2
    ) |>
    pivot_wider(
      names_from = year,
      values_from = area_km2
    )

  # Ensure columns exist even if a year has no rows
  ya <- as.character(as.integer(year_a))
  yb <- as.character(as.integer(year_b))
  if (!ya %in% names(wide)) wide[[ya]] <- NA_real_
  if (!yb %in% names(wide)) wide[[yb]] <- NA_real_

  wide |>
    mutate(
      area_a = .data[[ya]],
      area_b = .data[[yb]],
      delta = area_b - area_a,
      pct_delta = dplyr::if_else(
        is.na(area_a) | area_a == 0,
        NA_real_,
        100 * delta / area_a
      )
    ) |>
    select(
      name2,
      moku,
      moku_olelo,
      island,
      island_olelo,
      realm,
      ecosystem_type,
      area_a,
      area_b,
      delta,
      pct_delta
    ) |>
    arrange(dplyr::desc(delta))
}

