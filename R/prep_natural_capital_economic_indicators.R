# Natural capital and economic indicators (Activity 5 context)
# BEA CAGDP8: real GDP quantity indexes by Hawaiʻi county — private vs government

read_bea_cagdp8_county_xlsx <- function(
    path,
    year_min = 2001L,
    year_max = 2018L
) {
  if (!file.exists(path)) {
    stop("BEA Excel not found: ", path, call. = FALSE)
  }

  raw <- readxl::read_xlsx(path, sheet = "Table", col_names = FALSE)
  hdr_row <- which(
    !is.na(raw[[1]]) & trimws(as.character(raw[[1]])) == "GeoFIPS"
  )
  if (!length(hdr_row)) {
    stop("Could not find GeoFIPS header row in BEA Table sheet.", call. = FALSE)
  }

  wide <- readxl::read_xlsx(
    path,
    sheet = "Table",
    skip = hdr_row[[1]] - 1L
  ) |>
    janitor::clean_names()

  nm <- names(wide)
  # janitor maps GeoFIPS -> geo_fips (or geofips depending on version)
  id_candidates <- list(
    c("geo_fips", "geo_name", "line_code", "description"),
    c("geofips", "geoname", "linecode", "description")
  )
  id_idx <- NULL
  for (cols in id_candidates) {
    m <- match(cols, nm)
    if (!any(is.na(m))) {
      id_idx <- m
      break
    }
  }
  if (is.null(id_idx)) {
    stop(
      "Unexpected columns in BEA sheet; expected GeoFIPS, GeoName, LineCode, Description. Got: ",
      paste(nm, collapse = ", "),
      call. = FALSE
    )
  }

  year_names <- nm[-id_idx]
  long <- wide |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(year_names),
      names_to = "year_col",
      values_to = "value"
    )

  long <- long |>
    dplyr::mutate(
      year = suppressWarnings(as.integer(gsub("^x", "", .data$year_col))),
      year = dplyr::if_else(
        is.na(.data$year) | .data$year < 1900L | .data$year > 2100L,
        suppressWarnings(as.integer(gsub("^x", "", gsub("_.*", "", .data$year_col)))),
        .data$year
      )
    )

  # Standard id column names for dplyr steps below
  cn <- names(wide)[id_idx]
  long <- long |>
    dplyr::rename(
      geofips = dplyr::all_of(cn[[1]]),
      geoname = dplyr::all_of(cn[[2]]),
      linecode = dplyr::all_of(cn[[3]]),
      description = dplyr::all_of(cn[[4]])
    )

  out <- long |>
    dplyr::filter(
      !is.na(.data$geofips),
      !is.na(.data$linecode),
      !grepl(
        "^Legend|^Last updated",
        as.character(.data$description),
        ignore.case = TRUE
      )
    ) |>
    dplyr::transmute(
      geofips = as.character(.data$geofips),
      county = as.character(.data$geoname),
      linecode = as.integer(.data$linecode),
      description = as.character(.data$description),
      year = as.integer(.data$year),
      value = as.numeric(.data$value)
    ) |>
    dplyr::filter(
      .data$linecode %in% c(2L, 83L),
      .data$year >= .env$year_min,
      .data$year <= .env$year_max,
      !is.na(.data$value)
    ) |>
    dplyr::mutate(
      sector = dplyr::case_when(
        .data$linecode == 2L ~ "Private industries",
        .data$linecode == 83L ~ "Government and government enterprises",
        TRUE ~ NA_character_
      ),
      measure = "Chain-type quantity index for real GDP (2017 = 100), BEA CAGDP8"
    ) |>
    dplyr::select(
      "year", "geofips", "county", "linecode", "sector",
      "value", "measure", "description"
    ) |>
    dplyr::arrange(.data$county, .data$sector, .data$year)

  n_exp <- 4L * 2L * (year_max - year_min + 1L)
  if (nrow(out) != n_exp) {
    stop(
      "Expected ", n_exp, " rows (4 counties × 2 sectors × years); got ", nrow(out),
      ". Check LineCode 2/83 and year range.",
      call. = FALSE
    )
  }

  counties <- unique(out$county)
  if (length(counties) != 4L) {
    stop(
      "Expected 4 counties, got ", length(counties), ": ",
      paste(counties, collapse = "; "),
      call. = FALSE
    )
  }

  out
}

export_bea_hi_county_gdp_processed <- function(df, path) {
  fs::dir_create(fs::path_dir(path))
  readr::write_csv(df, path)
  invisible(path)
}

plot_bea_hi_county_gdp_private_government_lines <- function(df, path) {
  df <- df |>
    dplyr::mutate(
      county_lbl = sub("\\s*\\*$", "", .data$county),
      county_lbl = dplyr::recode(
        .data$county_lbl,
        `Hawaii, HI` = "Hawaii County",
        `Honolulu, HI` = "Honolulu County",
        `Kauai, HI` = "Kauai County",
        `Maui + Kalawao, HI` = "Maui + Kalawao County",
        .default = .data$county_lbl
      ),
      sector_lbl = .data$sector
    )

  pal_county <- c(
    `Hawaii County` = "#0072B2",
    `Honolulu County` = "#D55E00",
    `Kauai County` = "#009E73",
    `Maui + Kalawao County` = "#CC79A7"
  )

  # Keep current output dimensions used by this BEA figure target.
  fig_w <- 10
  fig_h <- 7

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = .data$year,
      y = .data$value,
      color = .data$county_lbl,
      linetype = .data$sector_lbl
    )
  ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::scale_color_manual(values = pal_county, name = "County") +
    ggplot2::scale_linetype_manual(
      name = "Sector",
      breaks = c("Private industries", "Government and government enterprises"),
      values = c(
        `Private industries` = "solid",
        `Government and government enterprises` = "longdash"
      ),
      labels = c("Private industries", "Government")
    ) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::labs(
      title = "Real GDP by County: Private Industries and Government",
      subtitle = paste0(
        "Chain-type quantity indexes (2017 = 100), Hawaii counties, 2001-2018. "
      ),
      x = "Year",
      y = "Quantity index (2017 = 100)",
      caption = "Source: U.S. Bureau of Economic Analysis (BEA), table CAGDP8."
    ) +
    .theme_account() +
    ggplot2::theme(
      axis.title.x = element_text(
        face = "bold",
        size = 11,
        margin = margin(t = 8, b = 0)
      ),
      plot.margin = margin(t = 8, r = 12, b = 5, l = 8),
      legend.position = c(0.5, -0.1),
      legend.justification = "top",
      legend.box = "horizontal",
      legend.box.just = "center",
      legend.box.spacing = unit(1.5, "cm"),
      legend.key.height = unit(0.28, "cm"),
      legend.key.width = unit(0.5, "cm"),
      legend.key.spacing.x = unit(0.1, "cm"),
      legend.text = element_text(size = 7.5),
      legend.title = element_text(size = 8, face = "bold"),
      plot.caption = element_text(
        size = 8.5,
        color = "grey35",
        hjust = 1,
        margin = margin(t = 60, b = 2)
      ),
      plot.caption.position = "plot"
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        nrow = 1,
        byrow = TRUE,
        order = 1,
        title.position = "top",
        label.position = "bottom",
        keywidth = grid::unit(0.6, "cm")
      ),
      linetype = ggplot2::guide_legend(
        nrow = 1,
        byrow = TRUE,
        order = 2,
        title.position = "top",
        label.position = "bottom",
        keywidth = grid::unit(1, "cm"),
        override.aes = list(
          color = "grey15",
          linewidth = 0.55
        )
      )
    )

  .save_account_fig(p, path, width = fig_w, height = fig_h, dpi = 300L)
  invisible(path)
}

generate_natural_capital_economic_indicator_figs <- function(
    raw_xlsx,
    out_csv,
    out_png
) {
  df <- read_bea_cagdp8_county_xlsx(raw_xlsx)
  export_bea_hi_county_gdp_processed(df, out_csv)
  plot_bea_hi_county_gdp_private_government_lines(df, out_png)
  c(out_csv, out_png)
}
