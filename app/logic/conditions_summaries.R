# app/logic/conditions_summaries.R
#
# Purpose
# - Pure helpers for condition indicator summaries used by the Shiny app.
# - Supports focused moku time series, island context summaries, and A/B change tables.
#
# Rules
# - No Shiny, no file I/O.

box::use(
  dplyr[filter, mutate, transmute, arrange, group_by, summarise, left_join, select, distinct],
  tidyr[pivot_wider]
)

#' Build an A/B change table by moku for one indicator.
#'
#' @param conditions_df Long conditions table.
#' @param indicator Indicator key.
#' @param island Optional island key to filter (NULL for all).
#' @param year_a Baseline year.
#' @param year_b Comparison year.
#' @return Tibble with value_a/value_b/delta (+ CI columns when present).
#' @export
conditions_change_table <- function(conditions_df, indicator, island = NULL, year_a, year_b) {
  stopifnot(!is.null(conditions_df))
  stopifnot(!is.null(indicator), nzchar(indicator))
  stopifnot(!is.null(year_a), !is.null(year_b))

  dat <- conditions_df |>
    filter(
      indicator == !!indicator,
      year %in% c(as.integer(year_a), as.integer(year_b))
    )
  if (!is.null(island) && nzchar(island) && island != "All") {
    dat <- dat |> filter(island == !!island)
  }

  wide <- dat |>
    transmute(
      name2,
      moku,
      moku_olelo,
      island,
      island_olelo,
      realm,
      category,
      indicator,
      year = as.integer(year),
      value,
      ci_lower,
      ci_upper
    ) |>
    pivot_wider(
      names_from = year,
      values_from = c(value, ci_lower, ci_upper),
      names_sep = "__"
    )

  ya <- as.character(as.integer(year_a))
  yb <- as.character(as.integer(year_b))

  value_a_col <- paste0("value__", ya)
  value_b_col <- paste0("value__", yb)
  if (!value_a_col %in% names(wide)) wide[[value_a_col]] <- NA_real_
  if (!value_b_col %in% names(wide)) wide[[value_b_col]] <- NA_real_

  wide |>
    mutate(
      value_a = .data[[value_a_col]],
      value_b = .data[[value_b_col]],
      delta = value_b - value_a,
      pct_delta = dplyr::if_else(
        is.na(value_a) | value_a == 0,
        NA_real_,
        100 * delta / value_a
      )
    ) |>
    select(
      name2,
      moku,
      moku_olelo,
      island,
      island_olelo,
      realm,
      category,
      indicator,
      value_a,
      value_b,
      delta,
      pct_delta,
      dplyr::starts_with("ci_lower__"),
      dplyr::starts_with("ci_upper__")
    ) |>
    arrange(dplyr::desc(delta))
}

#' Island context summary time series for one indicator.
#'
#' Returns per-year median and IQR across mokus.
#'
#' @param conditions_df Long conditions table.
#' @param indicator Indicator key.
#' @param island Optional island key to filter (NULL for all).
#' @return Tibble: year, median, q25, q75, n_mokus
#' @export
conditions_island_summary_ts <- function(conditions_df, indicator, island = NULL) {
  stopifnot(!is.null(conditions_df))
  stopifnot(!is.null(indicator), nzchar(indicator))

  dat <- conditions_df |>
    filter(indicator == !!indicator, !is.na(value))
  if (!is.null(island) && nzchar(island) && island != "All") {
    dat <- dat |> filter(island == !!island)
  }

  dat |>
    group_by(year) |>
    summarise(
      n_mokus = dplyr::n_distinct(name2),
      median = stats::median(value, na.rm = TRUE),
      q25 = stats::quantile(value, 0.25, na.rm = TRUE, names = FALSE),
      q75 = stats::quantile(value, 0.75, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    ) |>
    arrange(year)
}

