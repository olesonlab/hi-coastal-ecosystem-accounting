# Conditions Data Preprocessing
# Processing functions for ecosystem condition indicators
#
# Data are organized by:
#   category = "Marine Abiotic" | "Marine Biotic" | "Terrestrial"
#   realm    = "Marine" | "Terrestrial"
#
# Output schema (long format):
#   name2 | year | indicator | value | ci_lower | ci_upper | category | realm

# =============================================================================
# Marine Abiotic
# =============================================================================

#' Load marine abiotic condition indicators
#'
#' Reads kd490 and PAR/SST CSVs, standardizes to long format.
#'
#' @param paths Named list with paths_in entries for abiotic CSVs
#' @return Tibble with cols: name2, island, year, indicator, value, ci_lower, ci_upper, category, realm
load_conditions_marine_abiotic <- function(paths) {
  kd490 <- .load_kd490(paths$conditions_abiotic_kd490)
  par_sst <- .load_par_sst(paths$conditions_abiotic_par_sst)
  dplyr::bind_rows(kd490, par_sst)
}

.load_kd490 <- function(path) {
  df <- readr::read_csv(path, show_col_types = FALSE, name_repair = "unique") |>
    janitor::clean_names()

  df |>
    dplyr::select(
      name2 = name2,
      island,
      year = date,
      value = mean_kd490_esa_oc_cci_8day_span,
      ci_lower = q05_kd490_esa_oc_cci_8day_span,
      ci_upper = q95_kd490_esa_oc_cci_8day_span
    ) |>
    dplyr::mutate(
      year = as.integer(year),
      name2 = dplyr::if_else(name2 == "KALAWA", "HALAWA", name2),
      indicator = "kd490",
      category = "Marine Abiotic",
      realm = "Marine"
    )
}

.load_par_sst <- function(path) {
  df <- readr::read_csv(path, show_col_types = FALSE, name_repair = "unique") |>
    janitor::clean_names()

  sst <- df |>
    dplyr::select(
      name2 = name2,
      island,
      year = date,
      ci_lower = q05_sea_surface_temperature_crw_daily_span,
      ci_upper = q95_sea_surface_temperature_crw_daily_span
    ) |>
    dplyr::mutate(
      value = (ci_lower + ci_upper) / 2,
      year = as.integer(year),
      name2 = dplyr::if_else(name2 == "KALAWA", "HALAWA", name2),
      indicator = "sst",
      category = "Marine Abiotic",
      realm = "Marine"
    )

  par <- df |>
    dplyr::select(
      name2 = name2,
      island,
      year = date,
      value = mean_par_nasa_viirs_8day_span
    ) |>
    dplyr::mutate(
      year = as.integer(year),
      name2 = dplyr::if_else(name2 == "KALAWA", "HALAWA", name2),
      indicator = "par",
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      category = "Marine Abiotic",
      realm = "Marine"
    )

  dplyr::bind_rows(sst, par)
}

# =============================================================================
# Marine Biotic
# =============================================================================

#' Load marine biotic condition indicators
#'
#' Reads all biotic CSVs (coral, fish functional groups, disease).
#' These have only 2013 and 2019 data (no 2016) and no CI values.
#'
#' @param paths Named list with paths_in entries for biotic CSVs
#' @return Tibble with cols: name2, year, indicator, value, ci_lower, ci_upper, category, realm
load_conditions_marine_biotic <- function(paths) {
  biotic_files <- list(
    list(path = paths$conditions_biotic_piscivore,   indicator = "piscivore_biomass"),
    list(path = paths$conditions_biotic_planktivore, indicator = "planktivore_biomass"),
    list(path = paths$conditions_biotic_primary,     indicator = "primary_consumer_biomass"),
    list(path = paths$conditions_biotic_coral_cover, indicator = "coral_cover"),
    list(path = paths$conditions_biotic_coral_div,   indicator = "coral_diversity"),
    list(path = paths$conditions_biotic_adult_den,   indicator = "adult_coral_density"),
    list(path = paths$conditions_biotic_juv_den,     indicator = "juvenile_coral_density"),
    list(path = paths$conditions_biotic_disease,     indicator = "disease_prevalence")
  )

  purrr::map_dfr(biotic_files, function(item) {
    .load_biotic_file(item$path, item$indicator)
  })
}

.load_biotic_file <- function(path, indicator_name) {
  df <- readr::read_csv(path, show_col_types = FALSE, name_repair = "unique") |>
    janitor::clean_names() |>
    dplyr::filter(!is.na(moku))

  # Wide columns: mean_2013, mean_2019
  df |>
    dplyr::select(name2 = moku, mean_2013, mean_2019) |>
    tidyr::pivot_longer(
      cols = c(mean_2013, mean_2019),
      names_to = "year",
      values_to = "value"
    ) |>
    dplyr::mutate(
      year = as.integer(gsub("mean_", "", year)),
      indicator = indicator_name,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      island = NA_character_,
      category = "Marine Biotic",
      realm = "Marine"
    ) |>
    dplyr::filter(!is.na(value))
}

# =============================================================================
# Terrestrial
# =============================================================================

#' Load terrestrial condition indicators
#'
#' Reads NDVI, rainfall, temperature, and burnt area CSVs.
#'
#' @param paths Named list with paths_in entries for terrestrial CSVs
#' @return Tibble with cols: name2, year, indicator, value, ci_lower, ci_upper, category, realm
load_conditions_terrestrial <- function(paths) {
  ndvi     <- .load_terr_ndvi(paths$conditions_terr_ndvi)
  rainfall <- .load_terr_rainfall(paths$conditions_terr_rainfall)
  temp     <- .load_terr_temp(paths$conditions_terr_temp)
  burnt    <- .load_terr_burnt(paths$conditions_terr_burnt)
  dplyr::bind_rows(ndvi, rainfall, temp, burnt)
}

.load_terr_ndvi <- function(path) {
  df <- readr::read_csv(path, show_col_types = FALSE, name_repair = "unique") |>
    janitor::clean_names()

  # Cols: moku, mean_ndvi13, mean_ndvi19
  df |>
    dplyr::select(name2 = moku, mean_ndvi13, mean_ndvi19) |>
    tidyr::pivot_longer(
      cols = c(mean_ndvi13, mean_ndvi19),
      names_to = "year_raw",
      values_to = "value"
    ) |>
    dplyr::mutate(
      year = dplyr::case_when(
        year_raw == "mean_ndvi13" ~ 2013L,
        year_raw == "mean_ndvi19" ~ 2019L
      ),
      indicator = "ndvi",
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      island = NA_character_,
      category = "Terrestrial",
      realm = "Terrestrial"
    ) |>
    dplyr::select(-year_raw)
}

.load_terr_rainfall <- function(path) {
  df <- readr::read_csv(path, show_col_types = FALSE, name_repair = "unique") |>
    janitor::clean_names()

  # Cols: name2, year, rain_avg, rain_max — file has 2013, 2019, and 1990 (reference baseline)
  df |>
    dplyr::filter(year %in% c(2013, 2019)) |>
    dplyr::select(name2 = name2, year, value = rain_avg) |>
    dplyr::mutate(
      year = as.integer(year),
      indicator = "mean_rainfall",
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      island = NA_character_,
      category = "Terrestrial",
      realm = "Terrestrial"
    )
}

.load_terr_temp <- function(path) {
  df <- readr::read_csv(path, show_col_types = FALSE, name_repair = "unique") |>
    janitor::clean_names()

  # Cols: moku, max_temp13, mean_temp13, max_temp19, mean_temp19
  df |>
    dplyr::select(name2 = moku, mean_temp13, mean_temp19) |>
    tidyr::pivot_longer(
      cols = c(mean_temp13, mean_temp19),
      names_to = "year_raw",
      values_to = "value"
    ) |>
    dplyr::mutate(
      year = dplyr::case_when(
        year_raw == "mean_temp13" ~ 2013L,
        year_raw == "mean_temp19" ~ 2019L
      ),
      indicator = "mean_temperature",
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      island = NA_character_,
      category = "Terrestrial",
      realm = "Terrestrial"
    ) |>
    dplyr::select(-year_raw)
}

.load_terr_burnt <- function(path) {
  df <- readr::read_csv(path, show_col_types = FALSE, name_repair = "unique") |>
    janitor::clean_names()

  # Cols: moku, area_burnt, year — "Reference" year = baseline
  df |>
    dplyr::filter(year != "Reference") |>
    dplyr::select(name2 = moku, year, value = area_burnt) |>
    dplyr::mutate(
      year = as.integer(year),
      indicator = "burnt_area",
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      island = NA_character_,
      category = "Terrestrial",
      realm = "Terrestrial"
    )
}

# =============================================================================
# Combine & Join
# =============================================================================

#' Combine all condition indicator datasets
#'
#' @param marine_abiotic Output of load_conditions_marine_abiotic()
#' @param marine_biotic Output of load_conditions_marine_biotic()
#' @param terrestrial Output of load_conditions_terrestrial()
#' @return Tibble with standardized long-format conditions data
combine_conditions <- function(marine_abiotic, marine_biotic, terrestrial) {
  dplyr::bind_rows(marine_abiotic, marine_biotic, terrestrial) |>
    dplyr::select(name2, island, year, indicator, value, ci_lower, ci_upper, category, realm)
}

#' Join moku geometries with condition data
#'
#' Left-joins conditions to the moku geometry layer on name2 + realm.
#' Mokus with no condition data are retained (value = NA).
#'
#' @param mokus_sf Combined moku sf object (from combine_mokus())
#' @param conditions Conditions tibble (from combine_conditions())
#' @return sf object with condition data and geometry
join_mokus_conditions <- function(mokus_sf, conditions) {
  # Normalise name2 in conditions so HALAWA/KALAWA are consistent
  conditions_clean <- conditions |>
    dplyr::mutate(name2 = dplyr::if_else(name2 == "KALAWA", "HALAWA", name2))

  # Drop geometry for the join, then re-attach
  mokus_df <- sf::st_drop_geometry(mokus_sf) |>
    dplyr::select(name2, moku, moku_olelo, island, island_olelo, realm)

  joined <- dplyr::left_join(
    conditions_clean |> dplyr::select(-island),  # use island from mokus LUT
    mokus_df,
    by = c("name2", "realm")
  ) |>
    dplyr::filter(!is.na(moku))  # drop rows that couldn't be matched

  # Re-attach geometry via a join with the sf object
  mokus_geom <- mokus_sf |>
    dplyr::select(name2, realm) |>
    dplyr::distinct(name2, realm, .keep_all = TRUE)

  result <- dplyr::left_join(joined, mokus_geom, by = c("name2", "realm")) |>
    sf::st_as_sf()

  result
}

# =============================================================================
# Figure Generation
# =============================================================================

#' Generate abiotic/disease dashboard PNGs for pipeline_report.qmd (not on public website).
#'
#' @param marine_abiotic Output of load_conditions_marine_abiotic()
#' @param marine_biotic Output of load_conditions_marine_biotic()
#' @param outdir Directory to write PNG files (created if absent)
#' @return Character vector of output file paths
generate_condition_figs <- function(marine_abiotic, marine_biotic, mokus_combined = NULL, outdir = "outputs/figs/conditions/") {
  fs::dir_create(outdir)
  nms_cf <- if (!is.null(mokus_combined)) moku_name_lookup(mokus_combined) else NULL
  lbl_moku_cf <- if (!is.null(nms_cf)) tibble::deframe(dplyr::distinct(nms_cf, name2, moku_olelo)) else ggplot2::waiver()

  # ---- 1. kd490 -------------------------------------------------------
  kd490_df <- marine_abiotic |>
    dplyr::filter(indicator == "kd490") |>
    dplyr::arrange(name2)

  p_kd490 <- ggplot2::ggplot(
    kd490_df,
    ggplot2::aes(
      x = name2, y = value,
      group = as.factor(year),
      color = as.factor(year)
    )
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
      position = ggplot2::position_dodge(width = 0.8), width = 0.2
    ) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.8), size = 2.2) +
    ggplot2::scale_color_manual(name = "Year", values = PAL_YEAR_3) +
    ggplot2::scale_x_discrete(labels = lbl_moku_cf) +
    ggplot2::labs(
      title = "Light attenuation (kd490)",
      subtitle = "Marine moku, 95% uncertainty intervals",
      x = "Moku", y = "kd490 (greater = lower clarity)",
      caption = "Source: NOAA synthesis"
    ) +
    .theme_account() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1, size = 9)) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 1))

  kd490_path <- file.path(outdir, "kd490_by_moku.png")
  .save_account_fig(p_kd490, kd490_path, width = 12, height = 5.5, dpi = 300L)

  # ---- 2. SST ---------------------------------------------------------
  sst_df <- marine_abiotic |>
    dplyr::filter(indicator == "sst") |>
    dplyr::arrange(name2)

  p_sst <- ggplot2::ggplot(
    sst_df,
    ggplot2::aes(
      x = name2, y = value,
      group = as.factor(year),
      color = as.factor(year)
    )
  ) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.8), size = 2.2) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
      position = ggplot2::position_dodge(width = 0.8), width = 0.2
    ) +
    ggplot2::scale_color_manual(name = "Year", values = PAL_YEAR_3) +
    ggplot2::scale_x_discrete(labels = lbl_moku_cf) +
    ggplot2::labs(
      title = "Mean sea surface temperature",
      subtitle = "Marine moku, 95% uncertainty intervals (\u00b0C)",
      x = "Moku", y = "\u00b0C",
      caption = "Source: NOAA synthesis"
    ) +
    .theme_account() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1, size = 9)) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 1))

  sst_path <- file.path(outdir, "sst_by_moku.png")
  .save_account_fig(p_sst, sst_path, width = 12, height = 5.5, dpi = 300L)

  # ---- 3. Disease prevalence ------------------------------------------
  disease_df <- marine_biotic |>
    dplyr::filter(indicator == "disease_prevalence") |>
    dplyr::arrange(dplyr::desc(dplyr::if_else(year == 2013, value, NA_real_)))

  moku_order <- disease_df |>
    dplyr::filter(year == 2013) |>
    dplyr::arrange(dplyr::desc(value)) |>
    dplyr::pull(name2)

  disease_df <- disease_df |>
    dplyr::mutate(name2 = factor(name2, levels = moku_order))

  p_disease <- ggplot2::ggplot(
    disease_df,
    ggplot2::aes(x = name2, y = value, fill = as.factor(year))
  ) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.72) +
    ggplot2::scale_fill_manual(name = "Year", values = PAL_YEAR_2) +
    ggplot2::scale_x_discrete(labels = lbl_moku_cf) +
    ggplot2::labs(
      title = "Coral disease prevalence",
      subtitle = "By moku, 2013 and 2019",
      x = "Moku", y = "Prevalence",
      caption = "Source: NOAA National Coral Reef Monitoring Program"
    ) +
    .theme_account() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1, size = 9)) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))

  disease_path <- file.path(outdir, "disease_prev_by_moku.png")
  .save_account_fig(p_disease, disease_path, width = 10, height = 5.5, dpi = 300L)

  c(kd490_path, sst_path, disease_path)
}

# Ocean Tipping Points (OTP) rasters: zonal summaries by marine moku
#
# Source GeoTIFFs live under data/01_raw/conditions/source/otp/

.otp_raster_catalog <- function() {
  c(
    osds_nitrogen       = "hi_otp_all_osds_nitrogen.tif",
    osds_phosphorus     = "hi_otp_all_osds_phosphorus.tif",
    osds_effluent       = "hi_otp_all_osds_effluent.tif",
    nearshore_sediment  = "hi_otp_all_nearshore_sediment.tif",
    coastal_mod         = "hi_otp_all_coastal_mod.tif",
    wave_avg            = "hi_otp_all_wave_avg.tif"
  )
}

.otp_figure_labels <- function() {
  c(
    osds_nitrogen       = "OSDS nitrogen",
    osds_phosphorus     = "OSDS phosphorus",
    osds_effluent       = "OSDS effluent",
    nearshore_sediment  = "Nearshore sediment",
    coastal_mod         = "Coastal modification",
    wave_avg            = "Wave (mean)"
  )
}

compute_otp_zonal_by_moku <- function(mokus_marine_sf, otp_dir) {
  stopifnot(inherits(mokus_marine_sf, "sf"))
  catalog <- .otp_raster_catalog()
  lut <- mokus_marine_sf |>
    sf::st_drop_geometry() |>
    dplyr::distinct(name2, island)

  out <- tibble::tibble(name2 = mokus_marine_sf$name2)

  for (i in seq_along(catalog)) {
    col <- names(catalog)[i]
    fn <- unname(catalog[i])
    path <- file.path(otp_dir, fn)
    if (!fs::file_exists(path)) {
      stop("Missing OTP raster: ", path, call. = FALSE)
    }
    r <- terra::rast(path)
    v <- terra::vect(mokus_marine_sf)
    v <- terra::project(v, terra::crs(r))
    ex <- terra::extract(r, v, fun = mean, na.rm = TRUE)
    idx <- match(seq_len(nrow(mokus_marine_sf)), ex[[1]])
    val <- if (ncol(ex) >= 2) ex[idx, 2] else NA_real_
    out[[col]] <- as.numeric(val)
  }

  out |>
    dplyr::left_join(lut, by = "name2")
}

export_otp_moku_zonal_stats <- function(mokus_marine_sf, otp_dir, out_csv) {
  df <- compute_otp_zonal_by_moku(mokus_marine_sf, otp_dir)
  fs::dir_create(fs::path_dir(out_csv))
  meta <- tibble::tibble(
    aggregation = "mean_of_raster_cells_intersecting_moku_polygon",
    na_rm = TRUE,
    otp_dir = otp_dir
  )
  readr::write_csv(meta, file.path(fs::path_dir(out_csv), "otp_moku_zonal_mean.meta.csv"))
  readr::write_csv(df, out_csv)
  out_csv
}

generate_fig16_otp_faceted_maps <- function(mokus_marine_sf, otp_moku_tbl, out_png) {
  fs::dir_create(fs::path_dir(out_png))
  catalog <- .otp_raster_catalog()
  labels  <- .otp_figure_labels()
  pal     <- account_island_palette_lower()

  # Get moku display names and island from the sf object
  nms_marine <- mokus_marine_sf |>
    sf::st_drop_geometry() |>
    dplyr::distinct(name2, moku_olelo, island)
  lbl_moku <- tibble::deframe(dplyr::distinct(nms_marine, name2, moku_olelo))

  # Order mokus by island (NW→SE) then name2
  moku_ord <- nms_marine |>
    dplyr::mutate(island = factor(island, levels = ISLAND_ORDER)) |>
    dplyr::arrange(island, name2) |>
    dplyr::pull(name2)

  d <- otp_moku_tbl |>
    dplyr::select(name2, dplyr::all_of(names(catalog))) |>
    dplyr::left_join(dplyr::select(nms_marine, name2, island), by = "name2") |>
    dplyr::mutate(
      island = factor(island, levels = ISLAND_ORDER),
      name2  = factor(name2, levels = moku_ord)
    ) |>
    tidyr::pivot_longer(
      cols     = dplyr::all_of(names(catalog)),
      names_to = "variable",
      values_to = "value"
    ) |>
    dplyr::mutate(
      variable  = factor(variable, levels = names(catalog)),
      panel_lbl = labels[as.character(variable)]
    )

  make_otp_panel <- function(var_name) {
    dd  <- dplyr::filter(d, variable == var_name)
    lbl <- unique(dd$panel_lbl)
    ggplot2::ggplot(dd, ggplot2::aes(x = name2, y = value, fill = island)) +
      ggplot2::geom_col(na.rm = TRUE) +
      ggplot2::scale_fill_manual(
        name = "Island", values = pal, labels = ISLAND_LABELS, na.value = "grey70"
      ) +
      ggplot2::scale_x_discrete(labels = lbl_moku) +
      ggplot2::labs(title = lbl, x = NULL, y = NULL) +
      .theme_account() +
      ggplot2::theme(
        axis.text.x    = ggplot2::element_text(angle = 55, hjust = 1, size = 7),
        legend.position = "none"
      )
  }

  # Top row (indices 1-3): suppress duplicate moku x-axis; bottom row (4-6): show labels
  all_names <- names(catalog)
  top_row  <- all_names[1:3]
  bot_row  <- all_names[4:6]
  plots_top <- purrr::map(top_row, function(vn) {
    make_otp_panel(vn) + ggplot2::theme(
      axis.text.x  = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )
  })
  plots_bot <- purrr::map(bot_row, make_otp_panel)
  plots <- c(plots_top, plots_bot)

  leg <- cowplot::get_legend(
    make_otp_panel(names(catalog)[1]) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, title.position = "top"))
  )

  title_grob <- cowplot::ggdraw() +
    cowplot::draw_label(
      "Ocean Tipping Points indicators by marine moku",
      fontface = "bold", size = 13, x = 0.5, hjust = 0.5
    )

  grid <- cowplot::plot_grid(plotlist = plots, ncol = 3, align = "hv")
  out  <- cowplot::plot_grid(title_grob, grid, leg, ncol = 1, rel_heights = c(0.04, 1, 0.06))
  .save_account_fig(out, out_png, width = 18, height = 14, dpi = 300L)
  out_png
}

# Pew-style condition figures (Figs 9–12, 14–16)

.load_biotic_means <- function(path) {
  readr::read_csv(path, show_col_types = FALSE, name_repair = "unique") |>
    janitor::clean_names() |>
    dplyr::filter(!is.na(moku)) |>
    dplyr::transmute(
      name2 = moku,
      y2013 = mean_2013,
      y2019 = mean_2019
    )
}

.rainfall_vs_baseline <- function(path) {
  readr::read_csv(path, show_col_types = FALSE, name_repair = "unique") |>
    janitor::clean_names() |>
    dplyr::mutate(year = as.integer(year)) |>
    dplyr::filter(year %in% c(1990L, 2013L, 2019L)) |>
    tidyr::pivot_wider(id_cols = name2, names_from = year, values_from = rain_avg, names_prefix = "y") |>
    dplyr::mutate(
      chg2013 = y2013 - y1990,
      chg2019 = y2019 - y1990
    )
}

generate_conditions_report_figs <- function(
    paths_in,
    mokus_combined,
    mokus_marine,
    marine_abiotic,
    marine_biotic,
    otp_moku_csv,
    outdir = "outputs/figs/conditions"
) {
  fs::dir_create(outdir)
  pal <- account_island_palette_lower()
  nms <- moku_name_lookup(mokus_combined)
  lbl_moku <- tibble::deframe(dplyr::distinct(nms, name2, moku_olelo))

  tree_r <- .rainfall_vs_baseline(paths_in$conditions_terr_rainfall)
  wetl_r <- .rainfall_vs_baseline(paths_in$conditions_wetl_rainfall)

  isl <- nms |>
    dplyr::distinct(name2, island) |>
    dplyr::mutate(island_l = island)

  f9a <- tree_r |>
    dplyr::left_join(isl, by = "name2") |>
    tidyr::pivot_longer(cols = c(chg2013, chg2019), names_to = "period", values_to = "delta") |>
    dplyr::mutate(
      period = dplyr::recode(period, chg2013 = "2013 vs 1990", chg2019 = "2019 vs 1990"),
      island_l = factor(island_l, levels = names(pal))
    )

  p9a <- ggplot2::ggplot(f9a, ggplot2::aes(x = stats::reorder(name2, delta), y = delta, fill = island_l)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.75) +
    ggplot2::facet_wrap(~period, ncol = 2) +
    ggplot2::scale_fill_manual(name = "Island", values = pal, na.value = "grey70", na.translate = FALSE) +
    ggplot2::scale_x_discrete(labels = lbl_moku) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Tree cover extent",
      subtitle = "Rainfall change vs. 1990 mean (mm)",
      x = NULL, y = "\u0394 Mean monthly rainfall (mm)"
    ) +
    .theme_account() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )

  f9b <- wetl_r |>
    dplyr::left_join(isl, by = "name2") |>
    tidyr::pivot_longer(cols = c(chg2013, chg2019), names_to = "period", values_to = "delta") |>
    dplyr::mutate(
      period = dplyr::recode(period, chg2013 = "2013 vs 1990", chg2019 = "2019 vs 1990"),
      island_l = factor(island_l, levels = names(pal))
    )

  p9b <- ggplot2::ggplot(f9b, ggplot2::aes(x = stats::reorder(name2, delta), y = delta, fill = island_l)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.75) +
    ggplot2::facet_wrap(~period, ncol = 2) +
    ggplot2::scale_fill_manual(name = "Island", values = pal, na.value = "grey70", na.translate = FALSE) +
    ggplot2::scale_x_discrete(labels = lbl_moku) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Freshwater wetland extent",
      subtitle = "Rainfall change vs. 1990 mean (mm)",
      x = NULL, y = NULL
    ) +
    .theme_account() +
    ggplot2::theme(legend.position = "none")

  leg9 <- cowplot::get_legend(
    p9b + ggplot2::theme(legend.position = "bottom") +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, title.position = "top"))
  )
  fig9_mid <- cowplot::plot_grid(p9a, p9b, ncol = 1, align = "v", rel_heights = c(1, 1))
  fig9 <- cowplot::plot_grid(fig9_mid, leg9, ncol = 1, rel_heights = c(1, 0.08))
  f9p <- file.path(outdir, "fig09_rainfall_delta_baseline.png")
  .save_account_fig(fig9, f9p, width = 15, height = 12, dpi = 300L)

  tree_w <- readr::read_csv(paths_in$conditions_terr_rainfall, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::filter(year %in% c(1990, 2013, 2019)) |>
    dplyr::mutate(et = "Tree cover", year = as.integer(year))

  wetl_w <- readr::read_csv(paths_in$conditions_wetl_rainfall, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::filter(year %in% c(1990, 2013, 2019)) |>
    dplyr::mutate(et = "Freshwater wetland", year = as.integer(year))

  f10 <- dplyr::bind_rows(
    tree_w |> dplyr::select(name2, year, rain_max, rain_avg, et),
    wetl_w |> dplyr::select(name2, year, rain_max, rain_avg, et)
  ) |>
    dplyr::left_join(isl, by = "name2") |>
    dplyr::mutate(island_l = factor(island_l, levels = names(pal)))

  ref_ord <- f10 |>
    dplyr::filter(year == 1990, et == "Tree cover") |>
    dplyr::arrange(dplyr::desc(rain_avg)) |>
    dplyr::pull(name2)

  f10 <- f10 |> dplyr::mutate(name2 = factor(name2, levels = ref_ord))

  p10 <- ggplot2::ggplot(f10, ggplot2::aes(x = name2, y = rain_max, fill = factor(year))) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.85), width = 0.8) +
    ggplot2::facet_grid(et ~ ., scales = "free_y") +
    ggplot2::scale_fill_manual(name = "Year", values = PAL_YEAR_RAINFALL) +
    ggplot2::scale_x_discrete(labels = lbl_moku) +
    ggplot2::labs(
      title = "Maximum monthly rainfall",
      subtitle = "By extent type and moku",
      x = NULL, y = "mm"
    ) +
    .theme_account() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 55, hjust = 1, size = 7)
    )

  p10b <- ggplot2::ggplot(f10, ggplot2::aes(x = name2, y = rain_avg, fill = factor(year))) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.85), width = 0.8) +
    ggplot2::facet_grid(et ~ ., scales = "free_y") +
    ggplot2::scale_fill_manual(name = "Year", values = PAL_YEAR_RAINFALL) +
    ggplot2::scale_x_discrete(labels = lbl_moku) +
    ggplot2::labs(
      title = "Mean monthly rainfall",
      subtitle = "By extent type and moku",
      x = NULL, y = "mm"
    ) +
    .theme_account() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )

  leg10 <- cowplot::get_legend(
    p10 + ggplot2::theme(legend.position = "bottom") +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, title.position = "top"))
  )
  fig10 <- cowplot::plot_grid(
    cowplot::plot_grid(p10, p10b, ncol = 2, align = "hv"),
    leg10,
    ncol = 1, rel_heights = c(1, 0.05)
  )
  f10p <- file.path(outdir, "fig10_rainfall_2x2.png")
  .save_account_fig(fig10, f10p, width = 20, height = 10, dpi = 300L)

  cor <- .load_biotic_means(paths_in$conditions_biotic_coral_cover)
  ad <- .load_biotic_means(paths_in$conditions_biotic_adult_den)
  div <- .load_biotic_means(paths_in$conditions_biotic_coral_div)
  dis <- .load_biotic_means(paths_in$conditions_biotic_disease)

  .panel_topn <- function(df, title, y_lab = NULL, n = 12L) {
    df2 <- df |>
      dplyr::mutate(mx = pmax(y2013, y2019, na.rm = TRUE)) |>
      dplyr::slice_max(order_by = mx, n = n, with_ties = FALSE)
    ord <- df2$name2
    df2 <- df2 |>
      tidyr::pivot_longer(cols = c(y2013, y2019), names_to = "yr", values_to = "val") |>
      dplyr::mutate(
        yr = dplyr::recode(yr, y2013 = "2013", y2019 = "2019"),
        name2 = factor(name2, levels = ord)
      )

    ggplot2::ggplot(df2, ggplot2::aes(x = name2, y = val, fill = yr)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75), width = 0.65) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(name = "Year", values = PAL_YEAR_2) +
      ggplot2::scale_x_discrete(labels = lbl_moku) +
      ggplot2::labs(title = title, x = NULL, y = y_lab) +
      .theme_account() +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))
  }

  p11a <- .panel_topn(cor, "Coral cover", y_lab = "%")
  p11b <- .panel_topn(ad, "Adult coral density", y_lab = "colonies / m\u00b2")
  p11c <- .panel_topn(div, "Coral diversity (COV)", y_lab = "COV (Dq1)")
  p11d <- .panel_topn(dis, "Disease prevalence", y_lab = "proportion")
  leg11 <- cowplot::get_legend(
    p11a + ggplot2::theme(legend.position = "bottom") +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, title.position = "top"))
  )
  .no_y <- ggplot2::theme(
    legend.position = "none",
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  )
  fig11 <- cowplot::plot_grid(
    cowplot::plot_grid(
      p11a + ggplot2::theme(legend.position = "none"),
      p11b + .no_y,
      p11c + ggplot2::theme(legend.position = "none"),
      p11d + .no_y,
      nrow = 2, align = "hv"
    ),
    leg11,
    ncol = 1, rel_heights = c(1, 0.06)
  )
  f11p <- file.path(outdir, "fig11_coral_2x2.png")
  .save_account_fig(fig11, f11p, width = 12, height = 10, dpi = 300L)

  pr <- .load_biotic_means(paths_in$conditions_biotic_primary)
  pl <- .load_biotic_means(paths_in$conditions_biotic_planktivore)
  pis <- .load_biotic_means(paths_in$conditions_biotic_piscivore)
  p12a <- .panel_topn(pr, "Primary consumer biomass", y_lab = "g / m\u00b2")
  p12b <- .panel_topn(pl, "Planktivore biomass", y_lab = "g / m\u00b2")
  p12c <- .panel_topn(pis, "Piscivore biomass", y_lab = "g / m\u00b2")
  leg12 <- cowplot::get_legend(
    p12a + ggplot2::theme(legend.position = "bottom") +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, title.position = "top"))
  )
  fig12 <- cowplot::plot_grid(
    cowplot::plot_grid(
      p12a + .no_y,
      p12b + .no_y,
      p12c + ggplot2::theme(legend.position = "none"),
      ncol = 1, align = "v"
    ),
    leg12,
    ncol = 1, rel_heights = c(1, 0.05)
  )
  f12p <- file.path(outdir, "fig12_fish_biomass_panels.png")
  .save_account_fig(fig12, f12p, width = 8, height = 14, dpi = 300L)

  sst_df <- marine_abiotic |>
    dplyr::filter(indicator == "sst") |>
    dplyr::mutate(
      island = tolower(gsub("[^a-z]", "", island)),
      island = factor(island, levels = ISLAND_ORDER)
    ) |>
    dplyr::arrange(island, name2)

  kd_df <- marine_abiotic |>
    dplyr::filter(indicator == "kd490") |>
    dplyr::mutate(
      island = tolower(gsub("[^a-z]", "", island)),
      island = factor(island, levels = ISLAND_ORDER)
    ) |>
    dplyr::arrange(island, name2)

  # Order all marine mokus by island (NW→SE) then name2
  moku_ord_14 <- sst_df |>
    dplyr::distinct(island, name2) |>
    dplyr::arrange(island, name2) |>
    dplyr::pull(name2)
  sst_df <- sst_df |> dplyr::mutate(name2 = factor(name2, levels = moku_ord_14))
  kd_df  <- kd_df  |> dplyr::mutate(name2 = factor(name2, levels = moku_ord_14))

  p14a <- sst_df |>
    ggplot2::ggplot(ggplot2::aes(x = name2, y = value, color = factor(year))) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.8), size = 2.2) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
      position = ggplot2::position_dodge(width = 0.8),
      width = 0.2
    ) +
    ggplot2::scale_color_manual(name = "Year", values = PAL_YEAR_3) +
    ggplot2::scale_x_discrete(labels = lbl_moku) +
    ggplot2::labs(
      title = "Mean sea surface temperature",
      subtitle = "All marine moku, 95% uncertainty (\u00b0C)",
      x = NULL, y = "\u00b0C",
      caption = "Source: NOAA synthesis"
    ) +
    .theme_account() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 55, hjust = 1, size = 7)) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 1))

  p14b <- kd_df |>
    ggplot2::ggplot(ggplot2::aes(x = name2, y = value, color = factor(year))) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.8), size = 2.2) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ci_lower, ymax = ci_upper),
      position = ggplot2::position_dodge(width = 0.8),
      width = 0.2
    ) +
    ggplot2::scale_color_manual(name = "Year", values = PAL_YEAR_3) +
    ggplot2::scale_x_discrete(labels = lbl_moku) +
    ggplot2::labs(
      title = "kd490 (light attenuation)",
      subtitle = "All marine moku, 95% uncertainty",
      x = NULL, y = "kd490"
    ) +
    .theme_account() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 55, hjust = 1, size = 7)) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 1))

  leg14 <- cowplot::get_legend(
    p14a + ggplot2::theme(legend.position = "bottom") +
      ggplot2::guides(color = ggplot2::guide_legend(nrow = 1, title.position = "top"))
  )
  fig14 <- cowplot::plot_grid(
    cowplot::plot_grid(
      p14a + ggplot2::theme(legend.position = "none"),
      p14b + ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      ),
      ncol = 2, align = "hv"
    ),
    leg14,
    ncol = 1, rel_heights = c(1, 0.05)
  )
  f14p <- file.path(outdir, "fig14_sst_kd490_mhi_composite.png")
  .save_account_fig(fig14, f14p, width = 20, height = 8, dpi = 300L)

  par_df <- readr::read_csv(paths_in$conditions_abiotic_par_sst, show_col_types = FALSE) |>
    janitor::clean_names() |>
    dplyr::filter(date == 2019) |>
    dplyr::left_join(
      dplyr::distinct(nms, name2, island) |> dplyr::rename(island_l = island),
      by = "name2"
    ) |>
    dplyr::mutate(island_l = factor(island_l, levels = names(pal))) |>
    dplyr::filter(!is.na(island_l)) |>
    dplyr::group_by(island_l) |>
    dplyr::arrange(dplyr::desc(par_chge), .by_group = TRUE) |>
    dplyr::ungroup() |>
    dplyr::mutate(name2 = factor(name2, levels = unique(name2)))

  p15 <- ggplot2::ggplot(par_df, ggplot2::aes(x = name2, y = par_chge, fill = island_l)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(name = "Island", values = pal, labels = ISLAND_LABELS, na.value = "grey70", na.translate = FALSE) +
    ggplot2::scale_x_discrete(labels = lbl_moku) +
    ggplot2::facet_wrap(~island_l, scales = "free_x", labeller = ggplot2::as_labeller(ISLAND_LABELS)) +
    ggplot2::labs(
      title = "Photosynthetically active radiation (PAR)",
      subtitle = "2019 vs. mean of 2013 and 2016",
      x = NULL, y = expression(W~m^-2),
      caption = "Source: NASA VIIRS / NOAA synthesis"
    ) +
    .theme_account() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 55, hjust = 1, size = 8)) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))

  f15p <- file.path(outdir, "fig15_par_increase.png")
  .save_account_fig(p15, f15p, width = 14, height = 10, dpi = 300L)

  otp_tbl <- readr::read_csv(otp_moku_csv, show_col_types = FALSE)
  f16p <- file.path(outdir, "fig16_otp_faceted_maps.png")
  generate_fig16_otp_faceted_maps(mokus_marine, otp_tbl, f16p)

  c(f9p, f10p, f11p, f12p, f14p, f15p, f16p)
}

# =============================================================================
# All condition figures (legacy + report): single entry for targets
# =============================================================================

generate_all_condition_figs <- function(
    paths_in,
    mokus_combined,
    mokus_marine,
    marine_abiotic,
    marine_biotic,
    otp_moku_csv,
    outdir = "outputs/figs/conditions/"
) {
  od <- sub("/$", "", outdir)
  c(
    generate_condition_figs(marine_abiotic, marine_biotic, mokus_combined, outdir = outdir),
    generate_conditions_report_figs(
      paths_in = paths_in,
      mokus_combined = mokus_combined,
      mokus_marine = mokus_marine,
      marine_abiotic = marine_abiotic,
      marine_biotic = marine_biotic,
      otp_moku_csv = otp_moku_csv,
      outdir = od
    )
  )
}