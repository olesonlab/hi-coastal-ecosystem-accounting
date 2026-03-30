# app/logic/conditions_metadata.R
#
# Purpose
# - Central place to store indicator display names, units, and interpretation notes.
# - Keeps UI modules lightweight and consistent across indicators.
#
# Rules
# - No Shiny, no file I/O.

box::use(
  dplyr[filter, distinct]
)

.DEFAULT_META <- function(indicator) {
  list(
    title = indicator,
    units = NULL,
    higher_is = NULL,
    description = NULL,
    notes = NULL
  )
}

#' Get metadata for a condition indicator
#' @param indicator Indicator key (e.g., "SST", "kd490")
#' @return Named list: title, units, higher_is, description, notes
#' @export
get_indicator_meta <- function(indicator) {
  if (is.null(indicator) || !nzchar(indicator)) return(.DEFAULT_META(indicator))

  meta <- list(
    SST = list(
      title = "Sea surface temperature (SST)",
      units = "\u00b0C",
      higher_is = "warmer",
      description = "Mean sea surface temperature by moku.",
      notes = "Where available, 95% confidence intervals are shown."
    ),
    kd490 = list(
      title = "kd490 (water clarity proxy)",
      units = "unitless",
      higher_is = "less_clear",
      description = "Diffuse attenuation coefficient at 490nm; higher values generally indicate lower water clarity.",
      notes = "Where available, 95% confidence intervals are shown."
    ),
    PAR = list(
      title = "Photosynthetically active radiation (PAR)",
      units = "W/m\u00b2",
      higher_is = "more_light",
      description = "Mean photosynthetically active radiation by moku.",
      notes = NULL
    )
  )

  if (!indicator %in% names(meta)) return(.DEFAULT_META(indicator))
  meta[[indicator]]
}

