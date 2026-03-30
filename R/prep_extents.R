# Extents Data Preprocessing
# Processing functions for ecosystem type extent/area data

# -----------------------------------------------------------------------------
# Load & Clean
# -----------------------------------------------------------------------------

#' Load and clean marine ecosystem type areas
#' 
#' Marine data is static across years in source, so we cross with analysis years.
#' 
#' @param path Path to CSV file
#' @param years Vector of years to cross with data
#' @return Tibble with name2, ecosystem_type, area_km2, year, realm
load_et_areas_marine <- function(path, years) {
  read_csv_clean(path) |>
    # Drop any unnamed index columns (e.g., ...1 from row numbers)
    dplyr::select(-dplyr::starts_with("...")) |>
    dplyr::select(
      name2 = moku,
      ecosystem_type = value,
      area_km2 = area_km_2
    ) |>
    tidyr::crossing(year = years) |>
    dplyr::mutate(
      realm = "Marine",
      year = as.integer(year)
    )
}

#' Load and clean terrestrial ecosystem type areas
#' 
#' Terrestrial data is in wide format with year column, requires pivoting.
#' Ecosystem type names need standardization.
#' 
#' @param path Path to CSV file
#' @return Tibble with name2, ecosystem_type, area_km2, year, realm
load_et_areas_terrestrial <- function(path) {
  read_csv_clean(path) |>
    tidyr::pivot_longer(
      cols = -c(name2, year),
      names_to = "ecosystem_type",
      values_to = "area_km2"
    ) |>
    dplyr::mutate(
      # Standardize ecosystem type names
      ecosystem_type = stringr::str_to_title(gsub("_", " ", ecosystem_type)),
      ecosystem_type = dplyr::case_when(
        ecosystem_type == "Grass Shrub" ~ "Grass/Shrub",
        ecosystem_type == "Beaches Dunes" ~ "Beaches/Dunes",
        TRUE ~ ecosystem_type
      ),
      realm = "Terrestrial"
    ) |>
    # Aggregate any duplicates (shouldn't exist, but defensive)
    dplyr::group_by(name2, year, ecosystem_type, realm) |>
    dplyr::summarise(
      area_km2 = mean(area_km2, na.rm = TRUE),
      .groups = "drop"
    )
}

# -----------------------------------------------------------------------------
# Combine & Join
# -----------------------------------------------------------------------------

#' Combine marine and terrestrial extent areas
#' @param et_marine Marine extent areas tibble
#' @param et_terrestrial Terrestrial extent areas tibble
#' @return Combined tibble
combine_et_areas <- function(et_marine, et_terrestrial) {
  dplyr::bind_rows(et_marine, et_terrestrial)
}

#' Join moku geometries with extent area data
#' 
#' Creates the final spatial dataset for the Extents scope.
#' 
#' @param mokus_sf Combined moku sf object
#' @param et_areas Combined extent areas tibble
#' @return sf object with extent areas attached
join_mokus_extents <- function(mokus_sf, et_areas) {
  dplyr::left_join(
    mokus_sf,
    et_areas,
    by = c("name2", "realm")
  )
}

# -----------------------------------------------------------------------------
# Figures (extent account — Figs 4, 5, 6, 8)
# Uses palettes and theme from 00_theme_account_figs.R (fisheries-consistent).
# -----------------------------------------------------------------------------

generate_extent_figs <- function(
    mokus_combined,
    et_areas_terrestrial,
    mokus_marine,
    himarc_tif,
    outdir = "outputs/figs/extents"
) {
  fs::dir_create(outdir)
  nms <- moku_name_lookup(mokus_combined)
  c(
    .extent_fig4_moku_map(mokus_combined, nms, file.path(outdir, "moku_eaa_fig4.png")),
    .extent_fig5_cropland_grass(et_areas_terrestrial, nms, file.path(outdir, "fig05_cropland_grass_shrub.png")),
    .extent_fig6_tree_change(mokus_combined, et_areas_terrestrial, nms, file.path(outdir, "fig06_tree_cover_change_map_bars.png")),
    .extent_fig8_benthic(mokus_marine, himarc_tif, nms, file.path(outdir, "fig08_kona_koolaupoko_benthic_maps.png"))
  )
}

# HIMARC benthic habitat class levels (canonical order from VAT)
.himarc_class_levels <- function() {
  c("Soft Bottom", "Other Hard Bottom", "Rock/Boulder", "Pavement",
    "Coral Dominated Hard Bottom")
}

# ColorBrewer Set2 palette keyed by class label
.himarc_class_pal <- function() {
  c(
    "Soft Bottom"                  = "#66C2A5",
    "Other Hard Bottom"            = "#FC8D62",
    "Rock/Boulder"                 = "#8DA0CB",
    "Pavement"                     = "#E78AC3",
    "Coral Dominated Hard Bottom"  = "#A6D854"
  )
}

.extent_fig4_moku_map <- function(mokus_combined, nms, out_png) {
  # Single MHI map with all terrestrial moku boundaries, colored by county
  county_map <- c(
    niihau    = "Kaua\u02bbiCounty",  kauai  = "Kaua\u02bbiCounty",
    oahu      = "HonoluluCounty",
    molokai   = "MauiCounty", lanai  = "MauiCounty",
    kahoolawe = "MauiCounty", maui   = "MauiCounty",
    hawaii    = "Hawai\u02bbiCounty"
  )
  county_labels <- c(
    "Kaua\u02bbiCounty"   = "Kaua\u02bbi County",
    "HonoluluCounty"      = "Honolulu County",
    "MauiCounty"          = "Maui County",
    "Hawai\u02bbiCounty"  = "Hawai\u02bbi County"
  )
  county_pal <- c(
    "Kaua\u02bbiCounty"   = "#CC79A7",
    "HonoluluCounty"      = "#009E73",
    "MauiCounty"          = "#0072B2",
    "Hawai\u02bbiCounty"  = "#D55E00"
  )

  m <- mokus_combined |>
    dplyr::filter(realm == "Terrestrial") |>
    sf::st_transform(4326) |>
    dplyr::mutate(county = county_map[island])

  p <- ggplot2::ggplot(m) +
    ggplot2::geom_sf(ggplot2::aes(fill = county), color = "grey45", linewidth = 0.2) +
    ggplot2::scale_fill_manual(
      name   = "County",
      values = county_pal,
      labels = county_labels,
      na.value = "grey90"
    ) +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.2, text_cex = 0.8) +
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true",
      pad_x = grid::unit(0.08, "in"), pad_y = grid::unit(0.08, "in"),
      style = ggspatial::north_arrow_minimal(text_size = 9)
    ) +
    ggplot2::labs(
      title    = "Moku ecosystem accounting areas (EAAs)",
      subtitle = "Main Hawaiian Islands \u2014 terrestrial boundaries. Marine units extend to 30 m depth."
    ) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", hjust = 0.5, size = 14),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10, color = "grey40"),
      legend.position = "bottom",
      legend.title    = ggplot2::element_text(face = "bold", size = 11)
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))

  .save_account_fig(p, out_png, width = 13, height = 7, dpi = 300L)
  out_png
}

.extent_fig5_cropland_grass <- function(et_areas_terrestrial, nms, out_png) {
  pal <- account_island_palette_lower()
  lbl_moku <- dplyr::distinct(nms, name2, moku_olelo) |> tibble::deframe()

  d <- et_areas_terrestrial |>
    dplyr::filter(
      ecosystem_type %in% c("Cropland", "Grass/Shrub"),
      year %in% c(2013L, 2019L)
    ) |>
    dplyr::left_join(nms, by = "name2") |>
    dplyr::mutate(
      year  = factor(year),
      island = factor(island, levels = ISLAND_ORDER)
    ) |>
    # sort name2 within each island by max area across years for bar ordering
    dplyr::group_by(island, ecosystem_type, name2) |>
    dplyr::mutate(max_area = max(area_km2, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::arrange(island, ecosystem_type, max_area) |>
    dplyr::mutate(name2 = factor(name2, levels = unique(name2)))

  make_et_panel <- function(et_label) {
    ggplot2::ggplot(
      dplyr::filter(d, ecosystem_type == et_label),
      ggplot2::aes(x = name2, y = area_km2, fill = year)
    ) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75), width = 0.65) +
      ggplot2::scale_fill_manual(name = "Year", values = PAL_YEAR_2) +
      ggplot2::scale_x_discrete(labels = lbl_moku) +
      ggplot2::labs(
        title = et_label,
        x = NULL, y = expression(km^2)
      ) +
      .theme_account() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 55, hjust = 1, size = 8)) +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))
  }

  p1 <- make_et_panel("Cropland")
  p2 <- make_et_panel("Grass/Shrub")

  leg <- cowplot::get_legend(p1 + ggplot2::theme(legend.position = "bottom"))
  p1  <- p1 + ggplot2::theme(legend.position = "none")
  p2  <- p2 + ggplot2::theme(legend.position = "none")

  grid <- cowplot::plot_grid(p1, p2, ncol = 1, align = "v")
  out  <- cowplot::plot_grid(grid, leg, ncol = 1, rel_heights = c(1, 0.06))
  .save_account_fig(out, out_png, width = 15, height = 14, dpi = 300L)
  out_png
}

.extent_fig6_tree_change <- function(mokus_combined, et_areas_terrestrial, nms, out_png) {
  pal <- account_island_palette_lower()
  lbl_moku <- dplyr::distinct(nms, name2, moku_olelo) |> tibble::deframe()

  d <- et_areas_terrestrial |>
    dplyr::filter(ecosystem_type == "Tree Cover", year %in% c(2013L, 2019L)) |>
    tidyr::pivot_wider(id_cols = name2, names_from = year, values_from = area_km2, names_prefix = "y") |>
    dplyr::mutate(delta = y2019 - y2013) |>
    dplyr::left_join(nms, by = "name2")

  m <- mokus_combined |>
    dplyr::filter(realm == "Terrestrial") |>
    dplyr::left_join(dplyr::select(d, name2, delta), by = "name2") |>
    sf::st_transform(4326)

  p_map <- ggplot2::ggplot(m) +
    ggplot2::geom_sf(ggplot2::aes(fill = delta), color = "grey25", linewidth = 0.12) +
    ggplot2::scale_fill_gradient2(
      name     = expression(Delta~tree~cover~(km^2)),
      low      = "#2166ac", mid = "#f7f7f7", high = "#b2182b",
      midpoint = 0, na.value = "grey90"
    ) +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.22, text_cex = 0.8) +
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true",
      pad_x = grid::unit(0.08, "in"), pad_y = grid::unit(0.08, "in"),
      style = ggspatial::north_arrow_minimal(text_size = 9)
    ) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::labs(title = "Tree cover change", subtitle = "2013\u20132019 (km\u00b2)") +
    ggplot2::theme_void(base_size = 13) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", hjust = 0.5, size = 14),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 11, color = "grey40"),
      legend.position = "bottom",
      legend.title    = ggplot2::element_text(face = "bold", size = 10)
    ) +
    ggplot2::guides(fill = ggplot2::guide_colorbar(
      barwidth = grid::unit(3, "in"), title.position = "top"
    ))

  b <- d |>
    dplyr::arrange(dplyr::desc(delta)) |>
    dplyr::mutate(
      name2 = factor(name2, levels = unique(name2)),
      island = factor(island, levels = ISLAND_ORDER)
    )

  p_bar <- ggplot2::ggplot(b, ggplot2::aes(x = name2, y = delta, fill = island)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(
      name   = "Island",
      values = pal,
      labels = ISLAND_LABELS,
      na.value = "grey70"
    ) +
    ggplot2::scale_x_discrete(labels = lbl_moku) +
    ggplot2::labs(
      x = NULL, y = expression(Delta~tree~cover~(km^2)),
      title = "Change by moku", subtitle = "2013\u20132019"
    ) +
    .theme_account() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 55, hjust = 1, size = 9)) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2))

  comb <- cowplot::plot_grid(p_map, p_bar, nrow = 1, rel_widths = c(1.1, 1), align = "h", axis = "tb")
  .save_account_fig(comb, out_png, width = 14, height = 7, dpi = 300L)
  out_png
}

.extent_fig8_benthic <- function(mokus_marine, himarc_tif, nms, out_png) {
  if (!fs::file_exists(himarc_tif)) {
    stop("Missing HIMARC raster: ", himarc_tif, call. = FALSE)
  }
  r <- terra::rast(himarc_tif)

  # HIMARC moku name2 codes and their proper display names
  target_name2  <- c("KONA OAH", "KOOLAUPOKO")
  lbl_moku    <- dplyr::distinct(nms, name2, moku_olelo) |> tibble::deframe()
  class_lvls  <- .himarc_class_levels()
  class_pal   <- .himarc_class_pal()

  plots <- vector("list", length(target_name2))
  for (i in seq_along(target_name2)) {
    nm <- target_name2[i]
    mi <- mokus_marine |>
      dplyr::filter(name2 == nm) |>
      sf::st_transform(sf::st_crs(r))
    rv  <- terra::crop(r, terra::vect(mi), snap = "near")
    rv  <- terra::mask(rv, terra::vect(mi))
    df  <- as.data.frame(rv, xy = TRUE)
    # The terra VAT gives text labels directly; rename to "class" and set level order
    val_col <- setdiff(names(df), c("x", "y"))
    if (length(val_col) >= 1) names(df)[names(df) == val_col[1]] <- "class"
    df      <- stats::na.omit(df)
    df$class <- factor(as.character(df$class), levels = class_lvls)

    panel_title <- if (!is.na(lbl_moku[nm])) lbl_moku[nm] else nm

    plots[[i]] <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, fill = class)) +
      ggplot2::geom_raster() +
      ggplot2::coord_fixed() +
      ggplot2::scale_fill_manual(
        name     = "Benthic class",
        values   = class_pal,
        drop     = FALSE,
        na.value = "transparent"
      ) +
      ggplot2::labs(title = panel_title, x = NULL, y = NULL) +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::theme(
        axis.text        = ggplot2::element_blank(),
        axis.ticks       = ggplot2::element_blank(),
        panel.grid       = ggplot2::element_blank(),
        panel.border     = ggplot2::element_blank(),
        plot.title       = ggplot2::element_text(face = "bold", size = 12, hjust = 0.5),
        legend.position  = "none"
      )
  }

  # Build shared legend from consistent scale
  legend_plot <- plots[[1]] +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, title.position = "top"))
  shared_leg <- cowplot::get_legend(legend_plot)

  title_row <- cowplot::ggdraw() +
    cowplot::draw_label(
      "Nearshore benthic habitat (HIMARC)",
      fontface = "bold", size = 14, x = 0.5, hjust = 0.5
    )
  maps_row <- cowplot::plot_grid(plotlist = plots, nrow = 1, align = "h")

  out <- cowplot::plot_grid(
    title_row, maps_row, shared_leg,
    ncol = 1, rel_heights = c(0.07, 1, 0.18)
  )
  .save_account_fig(out, out_png, width = 12, height = 6.5, dpi = 300L)
  out_png
}

# =============================================================================
# SPATIAL UNITS MAP
# =============================================================================

#' Generate extents & conditions spatial reporting units map
#'
#' Side-by-side map of terrestrial moku (left) and marine moku (right),
#' both colored by county, with a globe inset and north arrow.
#' Modelled on generate_fisheries_spatial_maps().
#'
#' @param mokus_combined Combined moku sf object (from pipeline)
#' @param outdir Output directory
#' @return Character vector of output file paths
generate_extents_spatial_map <- function(mokus_combined, outdir = "outputs/figs/extents") {
  fs::dir_create(outdir, recurse = TRUE)

  mokus <- mokus_combined |> sf::st_transform(4326)
  terr  <- dplyr::filter(mokus, realm == "Terrestrial")
  mar   <- dplyr::filter(mokus, realm == "Marine")

  terr_dissolved <- rmapshaper::ms_dissolve(terr, field = "island")

  bbox_all    <- sf::st_bbox(sf::st_union(terr, mar))
  xlim_shared <- c(bbox_all["xmin"] - 0.15, bbox_all["xmax"] + 0.15)
  ylim_shared <- c(bbox_all["ymin"] - 0.25, bbox_all["ymax"] + 0.25)

  moku_fill <- "#7BAFD4"   # neutral blue for all moku

  map_terr <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = terr, fill = moku_fill, color = "black", linewidth = 0.35) +
    ggspatial::annotation_scale(
      location   = "bl",
      width_hint = 0.15,
      pad_x      = grid::unit(1.0, "cm"),
      pad_y      = grid::unit(2.0, "cm"),
      text_cex   = 2.5,
      height     = grid::unit(0.6, "cm")
    ) +
    ggplot2::ggtitle("Terrestrial Moku") +
    ggplot2::labs(subtitle = "Mountain to coast") +
    ggplot2::coord_sf(xlim = xlim_shared, ylim = ylim_shared, expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(size = 28, face = "bold", hjust = 0.5),
      plot.subtitle   = ggplot2::element_text(size = 20, hjust = 0.5, color = "grey40"),
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )

  map_mar <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = terr_dissolved, fill = "grey80", color = "black", linewidth = 0.4) +
    ggplot2::geom_sf(data = mar, fill = moku_fill, color = "black", linewidth = 0.35) +
    ggplot2::annotate(
      "text",
      x = Inf, y = -Inf,
      label = "Source: HIMARC",
      hjust = 1.05, vjust = -0.8,
      size = 6, color = "grey40", fontface = "italic"
    ) +
    ggplot2::ggtitle("Marine Moku") +
    ggplot2::labs(subtitle = "Coast to 30 m depth") +
    ggplot2::coord_sf(xlim = xlim_shared, ylim = ylim_shared, expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(size = 28, face = "bold", hjust = 0.5),
      plot.subtitle   = ggplot2::element_text(size = 20, hjust = 0.5, color = "grey40"),
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )

  # North arrow grob (same as fisheries map)
  north_arrow_grob <- grid::grobTree(
    grid::polygonGrob(
      x  = grid::unit(c(0.5, 0.34, 0.66), "npc"),
      y  = grid::unit(c(0.85, 0.65, 0.65), "npc"),
      gp = grid::gpar(fill = "black", col = "black")
    ),
    grid::textGrob(
      "N",
      x  = grid::unit(0.5, "npc"),
      y  = grid::unit(0.5, "npc"),
      gp = grid::gpar(fontsize = 28, fontface = "bold", col = "black")
    )
  )

  # Globe inset (same as fisheries map)
  hawaii_lon <- -157.5
  hawaii_lat <-  20.5
  ortho_crs  <- sprintf("+proj=ortho +lat_0=%s +lon_0=%s", hawaii_lat, hawaii_lon)

  world      <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  hemisphere <- sf::st_buffer(
    sf::st_sfc(sf::st_point(c(hawaii_lon, hawaii_lat)), crs = 4326),
    dist = 9000000
  )
  world_clipped <- suppressWarnings(
    sf::st_intersection(sf::st_make_valid(world), hemisphere)
  ) |> sf::st_transform(ortho_crs)

  angles       <- seq(0, 2 * pi, length.out = 361)
  r            <- 6350000
  coords       <- cbind(r * cos(angles), r * sin(angles))
  coords       <- rbind(coords, coords[1, ])
  ocean_circle <- sf::st_sfc(sf::st_polygon(list(coords)), crs = ortho_crs)

  hawaii_point <- sf::st_sfc(sf::st_point(c(hawaii_lon, hawaii_lat)), crs = 4326) |>
    sf::st_transform(ortho_crs)
  hawaii_pt_df <- as.data.frame(sf::st_coordinates(hawaii_point))

  globe_inset <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = ocean_circle, fill = "#a8d8ea", color = "black", linewidth = 0.4) +
    ggplot2::geom_sf(data = world_clipped, fill = "grey75", color = "white", linewidth = 0.15) +
    ggplot2::geom_point(
      data = hawaii_pt_df,
      ggplot2::aes(x = X, y = Y),
      color = "#FF0000", shape = 17, size = 4
    ) +
    ggplot2::theme_void() +
    ggplot2::coord_sf(clip = "off")

  # Title grob
  title_grob <- cowplot::ggdraw() +
    cowplot::draw_label(
      "Spatial Reporting Units: Extent and Condition Accounts",
      fontface = "bold", size = 40, hjust = 0.5, x = 0.5, y = 0.70
    ) +
    cowplot::draw_label(
      "Main Hawaiian Islands",
      size = 30, hjust = 0.5, x = 0.5, y = 0.15, color = "grey40"
    )

  # Assemble (no legend row)
  maps_row <- cowplot::plot_grid(map_terr, map_mar, ncol = 2, align = "hv")

  combined_maps <- cowplot::plot_grid(
    title_grob, maps_row,
    ncol = 1, rel_heights = c(0.12, 1)
  )

  final_figure <- cowplot::ggdraw(combined_maps) +
    cowplot::draw_plot(
      globe_inset,
      x = 0.86, y = 0.62,
      width = 0.12, height = 0.28
    ) +
    cowplot::draw_grob(
      north_arrow_grob,
      x = 0.02, y = 0.82,
      width = 0.04, height = 0.15
    ) +
    ggplot2::theme(plot.margin = ggplot2::margin(40, 50, 40, 50))

  # Export (report PNG + web SVG + preview PNG)
  filename_base <- "extents_spatial_units"
  report_path   <- file.path(outdir, paste0(filename_base, "_report.png"))
  svg_path      <- file.path(outdir, paste0(filename_base, "_web.svg"))
  png_path      <- file.path(outdir, paste0(filename_base, "_preview.png"))

  ggplot2::ggsave(report_path, plot = final_figure, width = 24, height = 14, dpi = 300, bg = "white")
  ggplot2::ggsave(svg_path,    plot = final_figure, width = 24, height = 14, bg = "white")
  ggplot2::ggsave(png_path,    plot = final_figure, width = 24, height = 14, dpi = 150, bg = "white")

  c(report_path, svg_path, png_path)
}
