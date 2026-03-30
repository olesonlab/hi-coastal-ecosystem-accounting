# Shared ggplot theme and palettes for extent and condition account figures.
# Aligns with fisheries exchange-value figures (`.theme_ev()` in prep_uses_fisheries.R).
# Prefixed 00_ so tar_source("R/") loads this before prep_*.R scripts.
#
# Many-moku rule: Prefer color encodings for year, island, or metric — not one color
# per moku when there are many levels. For moku on the x-axis, use neutral fills or
# facet by island when color is needed.

PAL_YEAR_3 <- c(
  `2013` = "#E69F00",
  `2016` = "#0072B2",
  `2019` = "#009E73"
)

PAL_YEAR_2 <- c(
  `2013` = "#E69F00",
  `2019` = "#009E73"
)

# Baseline year in grey + account years (rainfall panels)
PAL_YEAR_RAINFALL <- c(
  `1990` = "#999999",
  `2013` = "#E69F00",
  `2019` = "#009E73"
)

# Island colors keyed by lowercase ASCII island slug (matches join keys in pipeline)
account_island_palette_lower <- function() {
  c(
    kauai = "#CC79A7", oahu = "#009E73", molokai = "#56B4E9",
    lanai = "#F0E442", maui = "#0072B2", kahoolawe = "#BABABA",
    hawaii = "#D55E00", niihau = "#BABABA"
  )
}

.theme_account <- function() {
  ggplot2::theme_bw(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = 14),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 11, color = "grey40"),
      plot.caption = ggplot2::element_text(size = 9, color = "grey35", hjust = 0),
      axis.title = ggplot2::element_text(face = "bold", size = 11),
      axis.text = ggplot2::element_text(size = 10),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold", size = 11),
      legend.text = ggplot2::element_text(size = 10),
      legend.key.size = ggplot2::unit(0.9, "lines"),
      panel.grid.major = ggplot2::element_line(color = "grey92"),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 8, r = 12, b = 8, l = 8)
    )
}

.save_account_fig <- function(plot, path, width, height, dpi = 300L) {
  fs::dir_create(fs::path_dir(path))
  ggplot2::ggsave(
    path, plot = plot, width = width, height = height,
    dpi = dpi, bg = "white", limitsize = FALSE
  )
  path
}

# Geographic order of island slugs (NW → SE) for consistent facet/legend ordering
ISLAND_ORDER <- c("niihau", "kauai", "oahu", "molokai", "lanai", "kahoolawe", "maui", "hawaii")

# Display labels for island slugs — uses U+02BB (ʻokina) consistent with fisheries figures
ISLAND_LABELS <- c(
  niihau    = "Ni\u02bbihau",
  kauai     = "Kaua\u02bbi",
  oahu      = "O\u02bbahu",
  molokai   = "Moloka\u02bbi",
  lanai     = "L\u0101na\u02bbi",
  kahoolawe = "Kaho\u02bbolawe",
  maui      = "Maui",
  hawaii    = "Hawai\u02bbi"
)

#' Build name2 → display-name lookup from mokus_combined sf object.
#' Returns a tibble with name2, moku_olelo, island (slug), island_label columns.
#' Use island (slug) for grouping/coloring and island_label for display.
moku_name_lookup <- function(mokus_combined_sf) {
  mokus_combined_sf |>
    sf::st_drop_geometry() |>
    dplyr::distinct(name2, moku_olelo, island) |>
    dplyr::filter(!is.na(name2)) |>
    dplyr::mutate(island_label = dplyr::recode(island, !!!ISLAND_LABELS, .default = island))
}
