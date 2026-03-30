# Fisheries Exchange Value — Pipeline Functions
#
# Refactored from:
#   scripts/fisheries_exchange_values/run_ev_workflow.R  (EV calculation)
#   scripts/fisheries_exchange_values/gen_ev_figs.R      (figure generation)
#
# Pipeline flow:
#   load_raw_fisheries()  →  compute_comm_ev()    →  export_fisheries_ev()
#                         →  compute_noncomm_ev() ↗
#                         →  generate_fisheries_ev_figs()


# =============================================================================
# DATA LOADING
# =============================================================================

#' Load all raw fisheries inputs
#'
#' Reads commercial landings, non-commercial landings, prices, costs, wages,
#' CPI, SBF allocations, and DAR area codes from data/01_raw/.
#'
#' @param paths_in Named list from _targets.R containing paths to raw files
#' @return Named list: comm_long, noncomm_clean, prices_raw, costs_other,
#'   wages, cpi, sbf_allocations, dar_areas
load_raw_fisheries <- function(paths_in) {
  # ---- CPI ----------------------------------------------------------------
  cpi <- readxl::read_excel(paths_in$fisheries_trace, sheet = "CPI Fred") |>
    dplyr::rename(year = 1, cpi.index = 2) |>
    dplyr::mutate(year = as.integer(year), cpi.index = as.numeric(cpi.index))

  # ---- SBF allocations ----------------------------------------------------
  sbf_allocations <- readxl::read_excel(
    paths_in$fisheries_trace, sheet = "SBF allocations"
  ) |>
    dplyr::mutate(
      year             = as.integer(year),
      area             = as.numeric(area),
      sbf.ratio.REEFET = as.numeric(sbf.ratio.REEFET)
    )

  # ---- DAR area codes -----------------------------------------------------
  dar_areas <- readxl::read_excel(
    paths_in$fisheries_trace, sheet = "DAR area codes"
  ) |>
    dplyr::mutate(
      area   = as.numeric(area),
      county = tolower(as.character(county))
    )

  # ---- Commercial landings (multi-sheet loop) -----------------------------
  comm_long <- NULL
  for (sn in readxl::excel_sheets(paths_in$fisheries_comm_landings)) {
    grp <- dplyr::case_when(
      sn == "Bottomfish-Deep 7"                     ~ "deep7",
      sn %in% c("Bottomfish-Jacks",
                "Bottomfish-Other Bottomfish",
                "Bottomfish-Seamount Groundfish")    ~ "sbf",
      stringr::str_starts(sn, "Inshore")            ~ "reef",
      stringr::str_starts(sn, "Pelagic")            ~ "pelagic",
      TRUE                                          ~ NA_character_
    )
    if (is.na(grp)) next

    d <- readxl::read_excel(paths_in$fisheries_comm_landings, sheet = sn)
    if (!"Sum of pounds_caught" %in% names(d)) next

    piece <- d |>
      dplyr::transmute(
        area          = as.numeric(area),
        day_fished    = as.Date(day_fished),
        lbs           = readr::parse_number(as.character(`Sum of pounds_caught`)),
        species_group = grp
      ) |>
      dplyr::filter(!is.na(area), !is.na(day_fished)) |>
      dplyr::mutate(year = as.integer(format(day_fished, "%Y")))

    comm_long <- dplyr::bind_rows(comm_long, piece)
  }

  # ---- Non-commercial landings -------------------------------------------
  noncomm_clean <- readxl::read_excel(
    paths_in$fisheries_noncomm, sheet = "Lbs by island"
  ) |>
    dplyr::mutate(
      year      = as.integer(year),
      island    = stringr::str_to_lower(as.character(island)),
      lbs.total = as.numeric(lbs.total)
    )

  # ---- Prices -------------------------------------------------------------
  prices_raw <- readxl::read_excel(
    paths_in$fisheries_prices, sheet = "DAR species grouping"
  ) |>
    dplyr::rename(
      year          = Year,
      price.nominal = `Landing prices (nominal)`,
      price.2021    = `Landing prices 2021 (inflation-adj.)`
    ) |>
    dplyr::mutate(
      species_group = dplyr::case_when(
        category == "Bottomfish - Deep 7"                        ~ "deep7",
        category %in% c("Bottomfish - Jacks",
                        "Bottomfish - Other Bottomfish",
                        "Bottomfish - Seamount groundfish")      ~ "sbf",
        stringr::str_starts(category, "Inshore")                 ~ "reef",
        stringr::str_starts(category, "Pelagic")                 ~ "pelagic",
        TRUE                                                     ~ NA_character_
      ),
      price.nominal = as.numeric(price.nominal),
      price.2021    = as.numeric(price.2021)
    )

  # ---- Other costs (non-labor per-lb) ------------------------------------
  costs_other <- utils::read.csv(paths_in$fisheries_costs, stringsAsFactors = FALSE)

  # ---- Labor wages (BLS county) ------------------------------------------
  labor_bls <- utils::read.csv(paths_in$fisheries_wages, stringsAsFactors = FALSE)

  if (!("county_label" %in% names(labor_bls)) && "county" %in% names(labor_bls)) {
    labor_bls <- dplyr::rename(labor_bls, county_label = county)
  }
  if (!("mean_hourly_wage_USD" %in% names(labor_bls))) {
    alt <- intersect(c("mean_hourly_wage", "mean_hourly_wage_usd"), names(labor_bls))[1]
    labor_bls <- dplyr::rename(labor_bls, mean_hourly_wage_USD = dplyr::all_of(alt))
  }

  wages <- labor_bls |>
    dplyr::transmute(
      county   = tolower(as.character(county_label)),
      wage.2_3 = (2 / 3) * as.numeric(mean_hourly_wage_USD)
    ) |>
    dplyr::filter(county %in% c("kauai", "oahu", "maui", "hawaii"))

  list(
    comm_long        = comm_long,
    noncomm_clean    = noncomm_clean,
    prices_raw       = prices_raw,
    costs_other      = costs_other,
    wages            = wages,
    cpi              = cpi,
    sbf_allocations  = sbf_allocations,
    dar_areas        = dar_areas
  )
}


# =============================================================================
# EV CALCULATION
# =============================================================================

CPI_BASE_YEAR <- 2021L

.cpi_factor <- function(cpi_tbl, y) {
  cpi_base <- cpi_tbl$cpi.index[cpi_tbl$year == CPI_BASE_YEAR][1]
  hit      <- cpi_tbl$cpi.index[cpi_tbl$year == y]
  if (length(hit) != 1L || is.na(hit) || is.na(cpi_base)) NA_real_ else cpi_base / hit
}

#' Compute commercial fisheries exchange value
#'
#' @param raw Output of load_raw_fisheries()
#' @return Wide tibble with per-species EV columns (dot-separated names)
compute_comm_ev <- function(raw) {
  cpi             <- raw$cpi
  prices_raw      <- raw$prices_raw
  costs_other_raw <- raw$costs_other
  wages           <- raw$wages
  comm_long       <- raw$comm_long
  dar_areas       <- raw$dar_areas
  sbf_allocations <- raw$sbf_allocations

  # ---- Mean weighted price (MWP) by species group -------------------------
  prices_use <- prices_raw |>
    dplyr::mutate(price.use = price.2021)

  landings_for_mwp <- comm_long |>
    dplyr::group_by(year, species_group) |>
    dplyr::summarise(landings = sum(lbs, na.rm = TRUE), .groups = "drop")

  price_by_year_group <- prices_use |>
    dplyr::filter(!is.na(species_group), !is.na(year), !is.na(price.use), price.use > 0) |>
    dplyr::group_by(year, species_group) |>
    dplyr::summarise(avg.price = mean(price.use, na.rm = TRUE), .groups = "drop")

  mwp_df <- price_by_year_group |>
    dplyr::inner_join(landings_for_mwp, by = c("year", "species_group")) |>
    dplyr::filter(landings > 0) |>
    dplyr::group_by(species_group) |>
    dplyr::summarise(mwp = sum(avg.price * landings) / sum(landings), .groups = "drop")

  mwp <- c(
    deep7   = mwp_df$mwp[mwp_df$species_group == "deep7"][1],
    sbf     = mwp_df$mwp[mwp_df$species_group == "sbf"][1],
    reef    = mwp_df$mwp[mwp_df$species_group == "reef"][1],
    pelagic = mwp_df$mwp[mwp_df$species_group == "pelagic"][1]
  )

  # ---- Other costs (non-labor per-lb) ------------------------------------
  cost_col <- if ("costs_other_usd_per_lb" %in% names(costs_other_raw)) {
    "costs_other_usd_per_lb"
  } else if ("nonlabor_usd_per_lb_nominal" %in% names(costs_other_raw)) {
    "nonlabor_usd_per_lb_nominal"
  } else {
    "usd_per_lb"
  }

  costs_other_vec <- costs_other_raw |>
    dplyr::mutate(
      species_group = tolower(as.character(species_group)),
      v             = as.numeric(.data[[cost_col]])
    ) |>
    dplyr::filter(species_group %in% c("deep7", "sbf", "reef", "pelagic")) |>
    dplyr::group_by(species_group) |>
    dplyr::summarise(v = mean(v, na.rm = TRUE), .groups = "drop")

  cost_other <- c(
    deep7   = costs_other_vec$v[costs_other_vec$species_group == "deep7"][1],
    sbf     = costs_other_vec$v[costs_other_vec$species_group == "sbf"][1],
    reef    = costs_other_vec$v[costs_other_vec$species_group == "reef"][1],
    pelagic = costs_other_vec$v[costs_other_vec$species_group == "pelagic"][1]
  )

  # ---- CPUE table (county × species) -------------------------------------
  cpue <- tibble::tribble(
    ~county,   ~cpue_deep7, ~cpue_sbf, ~cpue_reef, ~cpue_pelagic,
    "kauai",   13.2,        19.5,      31.0,        6.0,
    "oahu",    11.8,        17.5,      27.5,        5.6,
    "maui",    10.5,        13.5,      23.5,        6.2,
    "hawaii",  13.8,        18.5,      29.0,        6.0
  )

  # ---- Labor costs per-lb (county × species, pi-weighted) ----------------
  pi_county <- comm_long |>
    dplyr::left_join(dar_areas |> dplyr::select(area, county), by = "area") |>
    dplyr::group_by(county, species_group) |>
    dplyr::summarise(lbs = sum(lbs, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = species_group, values_from = lbs, values_fill = 0)

  for (g in c("deep7", "sbf", "reef", "pelagic")) {
    if (!g %in% names(pi_county)) pi_county[[g]] <- 0
  }

  pi_county <- pi_county |>
    dplyr::mutate(
      lbs_tot    = deep7 + sbf + reef + pelagic,
      pi_deep7   = deep7   / lbs_tot,
      pi_sbf     = sbf     / lbs_tot,
      pi_reef    = reef    / lbs_tot,
      pi_pelagic = pelagic / lbs_tot
    )

  pi_long <- pi_county |>
    tidyr::pivot_longer(
      dplyr::starts_with("pi_"), names_to = "tmp", values_to = "pi"
    ) |>
    dplyr::mutate(species_group = stringr::str_remove(tmp, "^pi_")) |>
    dplyr::select(county, species_group, pi)

  labor_comm_long <- wages |>
    dplyr::left_join(
      cpue |>
        tidyr::pivot_longer(-county, names_to = "tmp", values_to = "cpue") |>
        dplyr::mutate(species_group = stringr::str_remove(tmp, "^cpue_")),
      by = "county"
    ) |>
    dplyr::left_join(pi_long, by = c("county", "species_group")) |>
    dplyr::mutate(labor.perlb = (wage.2_3 / cpue) * pi)

  labor_wide <- labor_comm_long |>
    tidyr::pivot_wider(
      id_cols      = county,
      names_from   = species_group,
      values_from  = labor.perlb,
      names_prefix = "labor."
    )

  # ---- Build wide landings table ------------------------------------------
  master_comm_keys <- comm_long |>
    dplyr::distinct(year, area = as.numeric(area)) |>
    dplyr::filter(!is.na(year), !is.na(area))

  comm_wide <- comm_long |>
    dplyr::group_by(year, area, species_group) |>
    dplyr::summarise(landings = sum(lbs, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = species_group, values_from = landings, values_fill = 0)

  for (g in c("deep7", "sbf", "reef", "pelagic")) {
    if (!g %in% names(comm_wide)) comm_wide[[g]] <- 0
  }

  comm_wide <- master_comm_keys |>
    dplyr::left_join(comm_wide, by = c("year", "area")) |>
    dplyr::mutate(
      deep7   = tidyr::replace_na(deep7,   0),
      sbf     = tidyr::replace_na(sbf,     0),
      reef    = tidyr::replace_na(reef,    0),
      pelagic = tidyr::replace_na(pelagic, 0)
    ) |>
    dplyr::rename(
      landings.deep7   = deep7,
      landings.sbf     = sbf,
      landings.reef    = reef,
      landings.pelagic = pelagic
    )

  # ---- Compute EV --------------------------------------------------------
  comm_ev <- comm_wide |>
    dplyr::left_join(dar_areas, by = "area") |>
    dplyr::left_join(labor_wide, by = "county") |>
    dplyr::mutate(
      cpi.f            = vapply(year, .cpi_factor, numeric(1), cpi_tbl = cpi),
      mwp.deep7        = mwp[["deep7"]],
      mwp.sbf          = mwp[["sbf"]],
      mwp.reef         = mwp[["reef"]],
      mwp.pelagic      = mwp[["pelagic"]],
      mwp.deep7.adj    = mwp.deep7,   # prices already in 2021 USD
      mwp.sbf.adj      = mwp.sbf,
      mwp.reef.adj     = mwp.reef,
      mwp.pelagic.adj  = mwp.pelagic,
      rev.deep7        = landings.deep7   * mwp.deep7.adj,
      rev.sbf          = landings.sbf     * mwp.sbf.adj,
      rev.reef         = landings.reef    * mwp.reef.adj,
      rev.pelagic      = landings.pelagic * mwp.pelagic.adj,
      cost.all.deep7   = (cost_other[["deep7"]]   + labor.deep7)   * cpi.f,
      cost.all.sbf     = (cost_other[["sbf"]]     + labor.sbf)     * cpi.f,
      cost.all.reef    = (cost_other[["reef"]]    + labor.reef)    * cpi.f,
      cost.all.pelagic = (cost_other[["pelagic"]] + labor.pelagic) * cpi.f,
      ev.deep7         = rev.deep7   - landings.deep7   * cost.all.deep7,
      ev.sbf           = rev.sbf     - landings.sbf     * cost.all.sbf,
      ev.reef          = rev.reef    - landings.reef    * cost.all.reef,
      ev.pelagic       = rev.pelagic - landings.pelagic * cost.all.pelagic
    ) |>
    dplyr::left_join(
      sbf_allocations |> dplyr::select(year, area, sbf.ratio.REEFET),
      by = c("year", "area")
    ) |>
    dplyr::mutate(
      ev.sbf.REEFET   = dplyr::if_else(
        landings.sbf > 0 & !is.na(ev.sbf) & !is.na(sbf.ratio.REEFET),
        ev.sbf * sbf.ratio.REEFET,
        NA_real_
      ),
      ev.sbf.NONREEFET = dplyr::if_else(
        !is.na(ev.sbf) & !is.na(ev.sbf.REEFET),
        ev.sbf - ev.sbf.REEFET,
        NA_real_
      ),
      ev.REEFET   = dplyr::coalesce(ev.sbf.REEFET, 0) + ev.reef,
      ev.NONREEFET= ev.deep7 + dplyr::coalesce(ev.sbf.NONREEFET, 0) + ev.pelagic,
      ev.TOTAL    = ev.deep7 + ev.sbf + ev.reef + ev.pelagic
    ) |>
    dplyr::select(-cpi.f)

  # ---- Enforce reference column order (38 columns) -------------------------
  comm_ev <- comm_ev |>
    dplyr::select(
      year, area,
      landings.deep7, landings.pelagic, landings.reef, landings.sbf,
      county,
      labor.deep7, labor.sbf, labor.reef, labor.pelagic,
      mwp.deep7, mwp.sbf, mwp.reef, mwp.pelagic,
      mwp.deep7.adj, mwp.sbf.adj, mwp.reef.adj, mwp.pelagic.adj,
      rev.deep7, rev.sbf, rev.reef, rev.pelagic,
      cost.all.deep7, cost.all.sbf, cost.all.reef, cost.all.pelagic,
      ev.deep7, ev.sbf, ev.reef, ev.pelagic,
      sbf.ratio.REEFET,
      ev.sbf.REEFET, ev.sbf.NONREEFET, ev.REEFET, ev.NONREEFET, ev.TOTAL
    )

  comm_ev
}


#' Compute non-commercial fisheries exchange value
#'
#' @param raw Output of load_raw_fisheries()
#' @return Tibble with island × year EV columns (dot-separated names)
compute_noncomm_ev <- function(raw) {
  cpi             <- raw$cpi
  noncomm_clean   <- raw$noncomm_clean
  wages           <- raw$wages
  costs_other_raw <- raw$costs_other
  prices_raw      <- raw$prices_raw
  comm_long       <- raw$comm_long
  dar_areas       <- raw$dar_areas

  # ---- MWP for reef (same as commercial, reuse logic) --------------------
  landings_for_mwp <- comm_long |>
    dplyr::group_by(year, species_group) |>
    dplyr::summarise(landings = sum(lbs, na.rm = TRUE), .groups = "drop")

  prices_use <- prices_raw |>
    dplyr::mutate(price.use = price.2021)

  price_by_year_group <- prices_use |>
    dplyr::filter(!is.na(species_group), !is.na(year), !is.na(price.use), price.use > 0) |>
    dplyr::group_by(year, species_group) |>
    dplyr::summarise(avg.price = mean(price.use, na.rm = TRUE), .groups = "drop")

  mwp_df <- price_by_year_group |>
    dplyr::inner_join(landings_for_mwp, by = c("year", "species_group")) |>
    dplyr::filter(landings > 0) |>
    dplyr::group_by(species_group) |>
    dplyr::summarise(mwp = sum(avg.price * landings) / sum(landings), .groups = "drop")

  mwp_reef <- mwp_df$mwp[mwp_df$species_group == "reef"][1]

  # ---- Other costs for reef ----------------------------------------------
  cost_col <- if ("costs_other_usd_per_lb" %in% names(costs_other_raw)) {
    "costs_other_usd_per_lb"
  } else if ("nonlabor_usd_per_lb_nominal" %in% names(costs_other_raw)) {
    "nonlabor_usd_per_lb_nominal"
  } else {
    "usd_per_lb"
  }

  cost_other_reef <- costs_other_raw |>
    dplyr::mutate(
      species_group = tolower(as.character(species_group)),
      v             = as.numeric(.data[[cost_col]])
    ) |>
    dplyr::filter(species_group == "reef") |>
    dplyr::summarise(v = mean(v, na.rm = TRUE)) |>
    dplyr::pull(v)

  # ---- CPUE reef by county (for labor cost per lb) -----------------------
  cpue_reef <- tibble::tribble(
    ~county,  ~cpue_reef,
    "kauai",  31.0,
    "oahu",   27.5,
    "maui",   23.5,
    "hawaii", 29.0
  )

  island_to_county <- tibble::tribble(
    ~island,   ~county,
    "kauai",   "kauai",
    "oahu",    "oahu",
    "maui",    "maui",
    "lanai",   "maui",
    "molokai", "maui",
    "hawaii",  "hawaii"
  )

  labor_noncomm <- island_to_county |>
    dplyr::left_join(wages, by = "county") |>
    dplyr::left_join(cpue_reef, by = "county") |>
    dplyr::transmute(island, labor.perlb = wage.2_3 / cpue_reef)

  # ---- Compute non-commercial EV -----------------------------------------
  master_noncomm_keys <- noncomm_clean |> dplyr::distinct(year, island)

  noncomm_ev <- master_noncomm_keys |>
    dplyr::left_join(
      noncomm_clean |> dplyr::select(year, island, lbs.total),
      by = c("year", "island")
    ) |>
    dplyr::left_join(labor_noncomm, by = "island") |>
    dplyr::mutate(
      cpi.f               = vapply(year, .cpi_factor, numeric(1), cpi_tbl = cpi),
      reef.price          = mwp_reef,
      reef.price.adj      = reef.price,   # prices already in 2021 USD
      rev.total           = lbs.total * reef.price.adj,
      costs.other.perlb.adj = cost_other_reef * cpi.f,
      costs.labor.perlb.adj = labor.perlb    * cpi.f,
      ev.total            = rev.total - lbs.total * (costs.other.perlb.adj + costs.labor.perlb.adj),
      ev.total.REEF       = ev.total,
      ev.total.OO         = 0
    ) |>
    dplyr::select(
      year, island, lbs.total, reef.price.adj, rev.total,
      costs.other.perlb.adj, costs.labor.perlb.adj,
      ev.total, ev.total.REEF, ev.total.OO
    )

  noncomm_ev
}


# =============================================================================
# EXPORT
# =============================================================================

#' Export EV tables as CSVs
#'
#' @param comm_ev Output of compute_comm_ev()
#' @param noncomm_ev Output of compute_noncomm_ev()
#' @param paths_out Named list with fisheries_comm_csv and fisheries_noncomm_csv
#' @return Character vector of written file paths (for format = "file")
export_fisheries_ev <- function(comm_ev, noncomm_ev, paths_out) {
  fs::dir_create(dirname(paths_out$fisheries_comm_csv), recurse = TRUE)
  readr::write_csv(comm_ev,    paths_out$fisheries_comm_csv)
  readr::write_csv(noncomm_ev, paths_out$fisheries_noncomm_csv)
  c(paths_out$fisheries_comm_csv, paths_out$fisheries_noncomm_csv)
}


# =============================================================================
# FIGURE GENERATION
# =============================================================================

# ---- Internal helpers (ported from gen_ev_figs.R) -------------------------

.county_lab <- function(x) {
  dplyr::recode(x,
    hawaii = "Hawai\u02bbi", kauai = "Kaua\u02bbi", maui = "Maui", oahu = "O\u02bbahu", all = "All"
  )
}

.island_lab <- function(x) {
  dplyr::recode(x,
    hawaii  = "Hawai\u02bbi",
    kauai   = "Kaua\u02bbi",
    lanai   = "L\u0101na\u02bbi",
    maui    = "Maui",
    molokai = "Moloka\u02bbi",
    oahu    = "O\u02bbahu",
    all     = "All"
  )
}

.spcs_lab <- function(x) {
  dplyr::case_when(
    x == "deep7"   ~ "Deep 7 Bottomfish",
    x == "sbf"     ~ "Shallow Bottomfish",
    x == "reef"    ~ "Reef-Associated",
    x == "pelagic" ~ "Pelagic",
    x == "all"     ~ "All",
    TRUE           ~ x
  )
}

.theme_ev <- function() {
  ggplot2::theme_bw(base_size = 13) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold",  hjust = 0.5, size = 15),
      plot.subtitle    = ggplot2::element_text(hjust = 0.5,    size = 12,   color = "grey40"),
      axis.title       = ggplot2::element_text(face = "bold",  size = 12),
      axis.text        = ggplot2::element_text(size = 11),
      legend.position  = "bottom",
      legend.title     = ggplot2::element_text(face = "bold",  size = 11),
      legend.text      = ggplot2::element_text(size = 10),
      legend.key.size  = ggplot2::unit(0.9, "lines"),
      panel.grid.major = ggplot2::element_line(color = "grey92"),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin      = ggplot2::margin(t = 10, r = 15, b = 10, l = 10)
    )
}

.save_plot <- function(plot, outdir, filename, width = 8, height = 5, dpi = 300) {
  path <- file.path(outdir, filename)
  ggplot2::ggsave(path, plot = plot, width = width, height = height,
                  dpi = dpi, limitsize = FALSE)
  invisible(path)
}

# ---- Reshape helpers ------------------------------------------------------

.as_year_int <- function(x) {
  suppressWarnings(as.integer(as.numeric(x)))
}

#' Long commercial table from published master workbook (same as gen_ev_figs.R).
#' Uses per-lb cost columns from Excel; avoids K-derivation from recomputed comm_ev.
.build_comm_long_master <- function(comm) {
  comm |>
    dplyr::select(
      year, area, county,
      landings.deep7, rev.deep7, costs.other.perlb.deep7, costs.labor.perlb.deep7,
      costs.all.perlb.deep7.adj, ev.deep7,
      landings.sbf, rev.sbf, costs.other.perlb.sbf, costs.labor.perlb.sbf,
      costs.all.perlb.sbf.adj, ev.sbf,
      ev.sbf.NONREEFET, ev.sbf.REEFET,
      landings.reef, rev.reef, costs.other.perlb.reef, costs.labor.perlb.reef,
      costs.all.perlb.reef.adj, ev.reef,
      landings.pelagic, rev.pelagic, costs.other.perlb.pelagic, costs.labor.perlb.pelagic,
      costs.all.perlb.pelagic.adj, ev.pelagic,
      ev.REEFET, ev.NONREEFET, ev.TOTAL
    ) |>
    dplyr::rename(
      ev.sbf.OO = ev.sbf.NONREEFET,
      ev.sbf.REEF = ev.sbf.REEFET,
      ev.REEF = ev.REEFET,
      ev.OO = ev.NONREEFET
    ) |>
    dplyr::mutate(
      costs.other.deep7 = costs.other.perlb.deep7 * landings.deep7,
      costs.labor.deep7 = costs.labor.perlb.deep7 * landings.deep7,
      costs.all.deep7 = costs.all.perlb.deep7.adj * landings.deep7,
      costs.other.sbf = costs.other.perlb.sbf * landings.sbf,
      costs.labor.sbf = costs.labor.perlb.sbf * landings.sbf,
      costs.all.sbf = costs.all.perlb.sbf.adj * landings.sbf,
      costs.other.reef = costs.other.perlb.reef * landings.reef,
      costs.labor.reef = costs.labor.perlb.reef * landings.reef,
      costs.all.reef = costs.all.perlb.reef.adj * landings.reef,
      costs.other.pelagic = costs.other.perlb.pelagic * landings.pelagic,
      costs.labor.pelagic = costs.labor.perlb.pelagic * landings.pelagic,
      costs.all.pelagic = costs.all.perlb.pelagic.adj * landings.pelagic,
      ev.deep7.OO = ev.deep7, ev.deep7.REEF = 0,
      ev.reef.OO = 0, ev.reef.REEF = ev.reef,
      ev.pelagic.OO = ev.pelagic, ev.pelagic.REEF = 0
    ) |>
    dplyr::select(
      -costs.other.perlb.deep7, -costs.labor.perlb.deep7, -costs.all.perlb.deep7.adj,
      -costs.other.perlb.sbf, -costs.labor.perlb.sbf, -costs.all.perlb.sbf.adj,
      -costs.other.perlb.reef, -costs.labor.perlb.reef, -costs.all.perlb.reef.adj,
      -costs.other.perlb.pelagic, -costs.labor.perlb.pelagic, -costs.all.perlb.pelagic.adj
    ) |>
    tidyr::pivot_longer(cols = -c(year, area, county), names_to = "var", values_to = "value") |>
    dplyr::mutate(
      var = stringr::str_replace(var, "^ev\\.sbf\\.(OO|REEF)$", "ev.sbf.\\1"),
      var = stringr::str_replace(var, "^ev\\.(REEF|OO|TOTAL)$", "ev.all.\\1"),
      var = stringr::str_replace(var, "^ev\\.deep7\\.(OO|REEF)$", "ev.deep7.\\1"),
      var = stringr::str_replace(var, "^ev\\.reef\\.OO$", "ev.reef.OO"),
      var = stringr::str_replace(var, "^ev\\.pelagic\\.OO$", "ev.pelagic.OO"),
      parts = stringr::str_match(
        var,
        "^(landings|rev|costs\\.other|costs\\.labor|costs\\.all|ev)\\.([a-z0-9]+)\\.?([A-Z]{2,4})?$"
      )
    ) |>
    dplyr::transmute(
      year, area, county,
      var_type = parts[, 2],
      spcs.grp = parts[, 3],
      eco.type = parts[, 4],
      value
    ) |>
    dplyr::mutate(
      spcs.grp = dplyr::recode(
        spcs.grp,
        deep7 = "deep7",
        sbf = "sbf",
        reef = "reef",
        pelagic = "pelagic",
        all = "all"
      ),
      eco.type = dplyr::if_else(is.na(eco.type), "all", eco.type)
    ) |>
    tidyr::pivot_wider(names_from = var_type, values_from = value) |>
    dplyr::select(year, area, county, spcs.grp, eco.type,
                  landings, rev, costs.other, costs.labor, costs.all, ev)
}

.build_noncomm_long_master <- function(noncomm) {
  nc <- noncomm |>
    dplyr::select(
      year, island, lbs.total, reef.price.adj, rev.total,
      costs.other.perlb.adj, costs.labor.perlb.adj,
      ev.total, ev.total.REEF, ev.total.OO
    ) |>
    dplyr::mutate(
      costs.other = costs.other.perlb.adj * lbs.total,
      costs.labor = costs.labor.perlb.adj * lbs.total
    ) |>
    dplyr::rename(landings = lbs.total, price = reef.price.adj) |>
    dplyr::select(-costs.other.perlb.adj, -costs.labor.perlb.adj)

  ncl <- nc |>
    dplyr::select(year, island, landings, price, rev.total, ev.total, ev.total.REEF, ev.total.OO) |>
    tidyr::pivot_longer(
      cols = c(ev.total, ev.total.REEF, ev.total.OO),
      names_to = "eco.type",
      values_to = "ev"
    ) |>
    dplyr::mutate(
      eco.type = dplyr::case_when(
        eco.type == "ev.total" ~ "ALL",
        eco.type == "ev.total.REEF" ~ "REEF",
        eco.type == "ev.total.OO" ~ "OO"
      ),
      spcs.grp = "reef"
    ) |>
    dplyr::select(year, island, spcs.grp, landings, price, rev.total, eco.type, ev)

  statewide <- ncl |>
    dplyr::group_by(year, eco.type) |>
    dplyr::summarise(
      landings = sum(landings, na.rm = TRUE),
      price = NA_real_,
      rev.total = sum(rev.total, na.rm = TRUE),
      ev = sum(ev, na.rm = TRUE),
      spcs.grp = "reef",
      .groups = "drop"
    ) |>
    dplyr::mutate(island = "all") |>
    dplyr::select(year, island, spcs.grp, landings, price, rev.total, eco.type, ev)

  dplyr::bind_rows(ncl, statewide)
}

.build_comm_long <- function(comm) {
  # ---- Derive cost_other per lb (K) for each species -----------------------
  # cost.all.* = (K + labor.*) * cpi.f
  # For the same year, two counties A and B with different labor costs give:
  #   K = (cost.all_A * labor_B - cost.all_B * labor_A) / (cost.all_B - cost.all_A)
  # K is constant per species (cost_other scalar from CSV), so we average over years.
  county_yr <- comm |>
    dplyr::filter(!is.na(county)) |>
    dplyr::select(year, county,
                  cost.all.deep7, cost.all.sbf, cost.all.reef, cost.all.pelagic,
                  labor.deep7, labor.sbf, labor.reef, labor.pelagic) |>
    dplyr::distinct()

  .derive_K <- function(cost_col, labor_col, data) {
    grp <- data |>
      dplyr::filter(!is.na(.data[[labor_col]]), !is.na(.data[[cost_col]]),
                    .data[[labor_col]] > 0, .data[[cost_col]] > 0) |>
      dplyr::group_by(year) |>
      dplyr::filter(dplyr::n_distinct(county) >= 2) |>
      dplyr::arrange(county) |>
      dplyr::slice(1:2) |>
      dplyr::ungroup()
    if (nrow(grp) < 2) return(0)
    K_vals <- grp |>
      dplyr::group_by(year) |>
      dplyr::summarise(
        K = (.data[[cost_col]][1] * .data[[labor_col]][2] -
               .data[[cost_col]][2] * .data[[labor_col]][1]) /
          (.data[[cost_col]][2] - .data[[cost_col]][1]),
        .groups = "drop"
      ) |>
      dplyr::filter(is.finite(K))
    mean(K_vals$K, na.rm = TRUE)
  }

  K <- c(
    deep7   = .derive_K("cost.all.deep7",   "labor.deep7",   county_yr),
    sbf     = .derive_K("cost.all.sbf",     "labor.sbf",     county_yr),
    reef    = .derive_K("cost.all.reef",    "labor.reef",    county_yr),
    pelagic = .derive_K("cost.all.pelagic", "labor.pelagic", county_yr)
  )

  comm |>
    dplyr::select(
      year, area, county,
      landings.deep7, rev.deep7, labor.deep7, cost.all.deep7, ev.deep7,
      landings.sbf, rev.sbf, labor.sbf, cost.all.sbf, ev.sbf,
      ev.sbf.NONREEFET, ev.sbf.REEFET,
      landings.reef, rev.reef, labor.reef, cost.all.reef, ev.reef,
      landings.pelagic, rev.pelagic, labor.pelagic, cost.all.pelagic, ev.pelagic,
      ev.REEFET, ev.NONREEFET, ev.TOTAL
    ) |>
    dplyr::rename(
      ev.sbf.OO  = ev.sbf.NONREEFET,
      ev.sbf.REEF= ev.sbf.REEFET,
      ev.REEF    = ev.REEFET,
      ev.OO      = ev.NONREEFET
    ) |>
    dplyr::mutate(
      # Total costs (2021 USD) = cost per lb (adj) × landings
      costs.all.deep7   = cost.all.deep7   * landings.deep7,
      costs.all.sbf     = cost.all.sbf     * landings.sbf,
      costs.all.reef    = cost.all.reef    * landings.reef,
      costs.all.pelagic = cost.all.pelagic * landings.pelagic,
      # Labor/other split using K-derived proportions (exact; K derived from county pairs)
      costs.labor.deep7   = costs.all.deep7   * labor.deep7   / (labor.deep7   + K[["deep7"]]),
      costs.other.deep7   = costs.all.deep7   - costs.labor.deep7,
      costs.labor.sbf     = costs.all.sbf     * labor.sbf     / (labor.sbf     + K[["sbf"]]),
      costs.other.sbf     = costs.all.sbf     - costs.labor.sbf,
      costs.labor.reef    = costs.all.reef    * labor.reef    / (labor.reef    + K[["reef"]]),
      costs.other.reef    = costs.all.reef    - costs.labor.reef,
      costs.labor.pelagic = costs.all.pelagic * labor.pelagic / (labor.pelagic + K[["pelagic"]]),
      costs.other.pelagic = costs.all.pelagic - costs.labor.pelagic,
      ev.deep7.OO    = ev.deep7,   ev.deep7.REEF  = 0,
      ev.reef.OO     = 0,          ev.reef.REEF   = ev.reef,
      ev.pelagic.OO  = ev.pelagic, ev.pelagic.REEF = 0
    ) |>
    dplyr::select(
      -cost.all.deep7, -cost.all.sbf, -cost.all.reef, -cost.all.pelagic,
      -labor.deep7, -labor.sbf, -labor.reef, -labor.pelagic
    ) |>
    tidyr::pivot_longer(cols = -c(year, area, county), names_to = "var", values_to = "value") |>
    dplyr::mutate(
      var   = stringr::str_replace(var, "^ev\\.sbf\\.(OO|REEF)$",    "ev.sbf.\\1"),
      var   = stringr::str_replace(var, "^ev\\.(REEF|OO|TOTAL)$",    "ev.all.\\1"),
      var   = stringr::str_replace(var, "^ev\\.deep7\\.(OO|REEF)$",  "ev.deep7.\\1"),
      var   = stringr::str_replace(var, "^ev\\.reef\\.OO$",          "ev.reef.OO"),
      var   = stringr::str_replace(var, "^ev\\.pelagic\\.OO$",       "ev.pelagic.OO"),
      parts = stringr::str_match(
        var,
        "^(landings|rev|costs\\.other|costs\\.labor|costs\\.all|ev)\\.([a-z0-9]+)\\.?([A-Z]{2,4})?$"
      )
    ) |>
    dplyr::transmute(
      year, area, county,
      var_type = parts[, 2],
      spcs.grp = parts[, 3],
      eco.type = parts[, 4],
      value
    ) |>
    dplyr::mutate(eco.type = dplyr::if_else(is.na(eco.type), "all", eco.type)) |>
    tidyr::pivot_wider(names_from = var_type, values_from = value) |>
    dplyr::select(year, area, county, spcs.grp, eco.type,
                  landings, rev, costs.other, costs.labor, costs.all, ev)
}

.build_noncomm_long <- function(noncomm) {
  nc <- noncomm |>
    dplyr::mutate(
      costs.other = costs.other.perlb.adj * lbs.total,
      costs.labor = costs.labor.perlb.adj * lbs.total
    ) |>
    dplyr::rename(landings = lbs.total, price = reef.price.adj) |>
    dplyr::select(-costs.other.perlb.adj, -costs.labor.perlb.adj)

  ncl <- nc |>
    dplyr::select(year, island, landings, price, rev.total, ev.total, ev.total.REEF, ev.total.OO) |>
    tidyr::pivot_longer(
      cols      = c(ev.total, ev.total.REEF, ev.total.OO),
      names_to  = "eco.type",
      values_to = "ev"
    ) |>
    dplyr::mutate(
      eco.type = dplyr::case_when(
        eco.type == "ev.total"      ~ "ALL",
        eco.type == "ev.total.REEF" ~ "REEF",
        eco.type == "ev.total.OO"   ~ "OO"
      ),
      spcs.grp = "reef"
    ) |>
    dplyr::select(year, island, spcs.grp, landings, price, rev.total, eco.type, ev)

  statewide <- ncl |>
    dplyr::group_by(year, eco.type) |>
    dplyr::summarise(
      landings  = sum(landings,  na.rm = TRUE),
      price     = NA_real_,
      rev.total = sum(rev.total, na.rm = TRUE),
      ev        = sum(ev,        na.rm = TRUE),
      spcs.grp  = "reef",
      .groups   = "drop"
    ) |>
    dplyr::mutate(island = "all") |>
    dplyr::select(year, island, spcs.grp, landings, price, rev.total, eco.type, ev)

  dplyr::bind_rows(ncl, statewide)
}


#' Generate all 11 fisheries exchange value figures
#'
#' Ports the complete plotting workflow from scripts/fisheries_exchange_values/gen_ev_figs.R.
#'
#' @param comm_ev    Output of compute_comm_ev()
#' @param noncomm_ev Output of compute_noncomm_ev()
#' @param outdir     Directory to write PNGs into
#' @param master_xlsx Optional path to `ev_master_finale.xlsx`. When present and
#'   readable, figures use **Comm EV** / **Non-comm EV** sheets (same as
#'   `gen_ev_figs.R`) so plots match the published workbook; CSV exports from
#'   [compute_comm_ev()] are unchanged.
#' @return Character vector of written PNG paths (excludes _reference/_archive/trash)
generate_fisheries_ev_figs <- function(comm_ev, noncomm_ev, outdir, master_xlsx = NULL) {
  fs::dir_create(outdir, recurse = TRUE)

  master_path_ok <- !is.null(master_xlsx) && nzchar(master_xlsx) && file.exists(master_xlsx)
  sheets_master <- if (master_path_ok) readxl::excel_sheets(master_xlsx) else character()
  use_master <- master_path_ok &&
    all(c("Comm EV", "Non-comm EV") %in% sheets_master)

  if (use_master) {
    comm_raw <- readxl::read_excel(master_xlsx, sheet = "Comm EV") |>
      dplyr::mutate(
        county = tolower(as.character(county)),
        year = .as_year_int(year)
      )
    noncomm_raw <- readxl::read_excel(master_xlsx, sheet = "Non-comm EV") |>
      dplyr::mutate(
        island = tolower(as.character(island)),
        year = .as_year_int(year)
      )
    comm_long <- .build_comm_long_master(comm_raw)
    noncomm_long <- .build_noncomm_long_master(noncomm_raw)
  } else {
    if (master_path_ok) {
      warning(
        "master_xlsx has no sheets 'Comm EV' and 'Non-comm EV'; ",
        "figures use computed comm_ev / noncomm_ev (K-derived cost split). ",
        "Add those sheets to match gen_ev_figs.R / the published workbook."
      )
    }
    comm_long <- .build_comm_long(comm_ev)
    noncomm_long <- .build_noncomm_long(noncomm_ev)
  }

  # ---- Okabe-Ito colorblind-safe palette ----------------------------------
  # Okabe & Ito (2008): https://jfly.uni-koeln.de/color/
  PAL <- list(
    comm    = "#E69F00",   # Orange  — all commercial series
    noncomm = "#0072B2",   # Blue    — all non-commercial series
    species = c(
      "Deep 7 Bottomfish"  = "#D55E00",   # Vermillion
      "Shallow Bottomfish" = "#E69F00",   # Orange
      "Reef-Associated"    = "#009E73",   # Bluish Green
      "Pelagic"            = "#0072B2",   # Blue
      "All"                = "#999999"    # Grey
    ),
    county  = c(
      "Hawai\u02bbi" = "#D55E00",   # Vermillion
      "Kaua\u02bbi"  = "#CC79A7",   # Reddish Purple
      "Maui"         = "#0072B2",   # Blue
      "O\u02bbahu"   = "#009E73",   # Bluish Green
      "All"          = "#999999"    # Grey
    ),
    island  = c(
      "Hawai\u02bbi"    = "#D55E00",   # Vermillion
      "Kaua\u02bbi"     = "#CC79A7",   # Reddish Purple
      "L\u0101na\u02bbi" = "#F0E442",  # Yellow
      "Maui"            = "#0072B2",   # Blue
      "Moloka\u02bbi"   = "#56B4E9",   # Sky Blue
      "O\u02bbahu"      = "#009E73",   # Bluish Green
      "All"             = "#999999"    # Grey
    ),
    costs2  = c(Costs = "#0072B2", EV = "#E69F00"),
    components = c(
      "Revenue"        = "#999999",
      "Labor costs"    = "#D55E00",
      "Other costs"    = "#E69F00",
      "Total costs"    = "#0072B2",
      "Exchange value" = "#009E73"
    )
  )

  # ---- Uniform figure dimensions ------------------------------------------
  FIG_W <- 10
  FIG_H <- 6

  # noncomm_costs_src: raw noncomm data with per-lb cost columns used by p10/p11.
  # When master sheets are present, use noncomm_raw (authoritative); otherwise fall
  # back to pipeline-computed noncomm_ev which has the same column structure.
  noncomm_costs_src <- if (use_master) noncomm_raw else noncomm_ev

  yr_comm <- range(comm_long$year,    na.rm = TRUE)
  yr_nc   <- range(noncomm_long$year, na.rm = TRUE)

  # ---- 1. Commercial EV by county ----------------------------------------
  comm_county <- comm_long |>
    dplyr::filter(eco.type == "all") |>
    dplyr::group_by(year, county) |>
    dplyr::summarise(ev = sum(ev, na.rm = TRUE), .groups = "drop") |>
    dplyr::bind_rows(
      comm_long |>
        dplyr::filter(eco.type == "all") |>
        dplyr::group_by(year) |>
        dplyr::summarise(ev = sum(ev, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(county = "all")
    ) |>
    dplyr::mutate(county = .county_lab(county))

  p1 <- ggplot2::ggplot(comm_county, ggplot2::aes(year, ev, color = county)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_color_manual(values = PAL$county) +
    ggplot2::labs(
      title    = "Commercial Fisheries Exchange Value by County",
      subtitle = paste0("Statewide total, all counties, ", yr_comm[1], "\u2013", yr_comm[2], " (2021 USD)"),
      x = "Year", y = "Exchange Value ($)", color = "County"
    ) +
    .theme_ev() +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 1))
  .save_plot(p1, outdir, "comm_county.png", width = FIG_W, height = FIG_H)

  # ---- 2. Commercial EV by species group ---------------------------------
  comm_spcs <- comm_long |>
    dplyr::filter(eco.type == "all",
                  spcs.grp %in% c("deep7", "sbf", "reef", "pelagic", "all")) |>
    dplyr::group_by(year, spcs.grp) |>
    dplyr::summarise(ev = sum(ev, na.rm = TRUE), .groups = "drop") |>
    dplyr::bind_rows(
      comm_long |>
        dplyr::filter(eco.type == "all",
                      spcs.grp %in% c("deep7", "sbf", "reef", "pelagic", "all")) |>
        dplyr::group_by(year) |>
        dplyr::summarise(ev = sum(ev, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(spcs.grp = "all")
    ) |>
    dplyr::mutate(spcs.grp = .spcs_lab(spcs.grp))

  p2 <- ggplot2::ggplot(comm_spcs, ggplot2::aes(year, ev, color = spcs.grp)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_color_manual(values = PAL$species) +
    ggplot2::labs(
      title    = "Commercial Exchange Value by Species Group",
      subtitle = paste0("Statewide total, all species, ", yr_comm[1], "\u2013", yr_comm[2], " (2021 USD)"),
      x = "Year", y = "Exchange Value ($)", color = "Species Group"
    ) +
    .theme_ev() +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 1))
  .save_plot(p2, outdir, "comm_spcs.png", width = FIG_W, height = FIG_H)

  # ---- 3. Non-commercial EV by island ------------------------------------
  noncomm_by_island <- noncomm_long |>
    dplyr::filter(eco.type == "ALL") |>
    dplyr::group_by(year, island) |>
    dplyr::summarise(ev = sum(ev, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(island_lab = .island_lab(island))

  p3 <- ggplot2::ggplot(noncomm_by_island, ggplot2::aes(year, ev, color = island_lab)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_color_manual(values = PAL$island) +
    ggplot2::labs(
      title    = "Non-Commercial Fisheries Exchange Value by Island",
      subtitle = paste0("Statewide total by island, reef herbivores, ", yr_nc[1], "\u2013", yr_nc[2], " (2021 USD)"),
      x = "Year", y = "Exchange Value ($)", color = "Island"
    ) +
    .theme_ev() +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 1))
  .save_plot(p3, outdir, "noncomm_island.png", width = FIG_W, height = FIG_H)

  # ---- 4. Commercial vs non-commercial (totals) --------------------------
  noncomm_ev_total <- noncomm_long |>
    dplyr::filter(eco.type == "ALL") |>
    dplyr::group_by(year) |>
    dplyr::summarise(ev.noncomm = sum(ev, na.rm = TRUE), .groups = "drop")

  ev_both <- comm_long |>
    dplyr::filter(eco.type == "all") |>
    dplyr::group_by(year) |>
    dplyr::summarise(ev.comm = sum(ev, na.rm = TRUE), .groups = "drop") |>
    dplyr::left_join(noncomm_ev_total, by = "year") |>
    tidyr::pivot_longer(c(ev.comm, ev.noncomm), names_to = "comm", values_to = "ev") |>
    dplyr::mutate(comm = dplyr::if_else(comm == "ev.comm", "comm", "noncomm")) |>
    dplyr::filter(is.finite(ev))

  yr_both <- c(min(ev_both$year, na.rm = TRUE),
               max(c(ev_both$year, noncomm_costs_src$year), na.rm = TRUE))

  p4 <- ggplot2::ggplot(ev_both, ggplot2::aes(year, ev, color = comm)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::scale_color_manual(
      values = c(comm = PAL$comm, noncomm = PAL$noncomm),
      labels = c(comm = "Commercial", noncomm = "Non-commercial")
    ) +
    ggplot2::labs(
      title    = "Total Exchange Value: Commercial vs. Non-Commercial",
      subtitle = paste0("Statewide total, all species, ", min(yr_both), "\u2013", max(yr_both), " (2021 USD)"),
      x = "Year", y = "Exchange Value ($)", color = "Fishery Sector"
    ) +
    .theme_ev() +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 10, r = 15, b = 10, l = 10)) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 1))
  .save_plot(p4, outdir, "ev_both_total.png", width = FIG_W, height = FIG_H)

  # ---- 5. Commercial vs non-commercial, reef only ------------------------
  ev_both_reef <- comm_long |>
    dplyr::filter(eco.type == "all", spcs.grp == "reef") |>
    dplyr::group_by(year) |>
    dplyr::summarise(ev.comm = sum(ev, na.rm = TRUE), .groups = "drop") |>
    dplyr::left_join(noncomm_ev_total, by = "year") |>
    tidyr::pivot_longer(c(ev.comm, ev.noncomm), names_to = "comm", values_to = "ev") |>
    dplyr::mutate(comm = dplyr::if_else(comm == "ev.comm", "comm", "noncomm")) |>
    dplyr::filter(is.finite(ev))

  p5 <- ggplot2::ggplot(ev_both_reef, ggplot2::aes(year, ev, color = comm)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::scale_color_manual(
      values = c(comm = PAL$comm, noncomm = PAL$noncomm),
      labels = c(comm = "Commercial", noncomm = "Non-commercial")
    ) +
    ggplot2::labs(
      title    = "Reef-Associated Exchange Value: Commercial vs. Non-Commercial",
      subtitle = paste0("Statewide total, reef-associated species, ", min(yr_both), "\u2013", max(yr_both), " (2021 USD)"),
      x = "Year", y = "Exchange Value ($)", color = "Fishery Sector"
    ) +
    .theme_ev() +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 10, r = 15, b = 10, l = 10)) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 1))
  .save_plot(p5, outdir, "ev_both_reef.png", width = FIG_W, height = FIG_H)

  # ---- 6. Commercial stacked components ----------------------------------
  # Aggregate costs from comm_long (already has correct CPI-adjusted split)
  comm_effects <- comm_long |>
    dplyr::filter(eco.type == "all", spcs.grp %in% c("deep7", "sbf", "reef", "pelagic")) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      revenue     = sum(rev,          na.rm = TRUE),
      costs.other = sum(costs.other,  na.rm = TRUE),
      costs.labor = sum(costs.labor,  na.rm = TRUE),
      costs.all   = sum(costs.all,    na.rm = TRUE),
      ev          = sum(ev,           na.rm = TRUE),
      .groups = "drop"
    )

  comm_effects_long <- comm_effects |>
    tidyr::pivot_longer(c(revenue, costs.other, costs.labor, costs.all, ev),
                        names_to = "variable", values_to = "value") |>
    dplyr::mutate(variable = dplyr::recode(variable,
      revenue     = "Revenue",       costs.labor = "Labor costs",
      costs.other = "Other costs",   costs.all   = "Total costs",
      ev          = "Exchange value"
    ))

  p6 <- ggplot2::ggplot(comm_effects_long,
                        ggplot2::aes(year, value, fill = variable)) +
    ggplot2::geom_col(width = 0.9) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_fill_manual(values = PAL$components) +
    ggplot2::labs(
      title    = "Commercial Exchange Value: Revenue and Cost Decomposition",
      subtitle = paste0("Statewide total, all species, ", yr_comm[1], "\u2013", yr_comm[2], " (2021 USD)"),
      x = "Year", y = "Value ($)", fill = "Component"
    ) +
    .theme_ev() +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))
  .save_plot(p6, outdir, "comm_effects_stacked.png", width = FIG_W, height = FIG_H)

  # ---- 7. Commercial EV by element & species (comm_elements_by_spcs) ----
  meas <- c("rev", "costs.labor", "costs.other", "costs.total", "ev")

  comm_elements <- comm_long |>
    dplyr::filter(eco.type == "all") |>
    dplyr::mutate(costs.total = costs.other + costs.labor) |>
    dplyr::select(year, county, spcs.grp, rev, costs.labor, costs.other, costs.total, ev) |>
    tidyr::drop_na()

  county_tot <- comm_elements |>
    dplyr::group_by(year, spcs.grp) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(meas), ~ sum(.x, na.rm = TRUE)),
                     .groups = "drop") |>
    dplyr::mutate(county = "all") |>
    dplyr::relocate(year, county, spcs.grp)

  spcs_tot <- comm_elements |>
    dplyr::group_by(year, county) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(meas), ~ sum(.x, na.rm = TRUE)),
                     .groups = "drop") |>
    dplyr::mutate(spcs.grp = "all") |>
    dplyr::relocate(year, county, spcs.grp)

  grand_tot <- comm_elements |>
    dplyr::group_by(year) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(meas), ~ sum(.x, na.rm = TRUE)),
                     .groups = "drop") |>
    dplyr::mutate(county = "all", spcs.grp = "all") |>
    dplyr::relocate(year, county, spcs.grp)

  comm_elements_all <- dplyr::bind_rows(
    comm_elements, county_tot, spcs_tot, grand_tot
  ) |>
    dplyr::group_by(year) |>
    tidyr::complete(
      county   = union(unique(comm_elements$county), "all"),
      spcs.grp = union(unique(comm_elements$spcs.grp), "all")
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(dplyr::across(dplyr::all_of(meas), ~ tidyr::replace_na(.x, 0))) |>
    dplyr::arrange(year, county, spcs.grp)

  p7 <- ggplot2::ggplot(
    comm_elements_all |> dplyr::mutate(spcs.grp = .spcs_lab(spcs.grp)),
    ggplot2::aes(year, ev, color = spcs.grp)
  ) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_color_manual(values = PAL$species) +
    ggplot2::labs(
      title    = "Commercial Exchange Value by Species Group",
      subtitle = paste0("Statewide total, ", yr_comm[1], "\u2013", yr_comm[2], " (2021 USD)"),
      x = "Year", y = "Exchange Value ($)", color = "Species Group"
    ) +
    .theme_ev() +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 1))
  .save_plot(p7, outdir, "comm_elements_by_spcs.png", width = FIG_W, height = FIG_H)

  # ---- 8. Commercial cost share of revenue -------------------------------
  plotdf_comm_econ <- comm_elements_all |>
    dplyr::filter(county == "all", spcs.grp == "all") |>
    dplyr::transmute(
      year,
      rev,
      costs      = costs.total,
      remainder  = pmax(rev - costs.total, 0),
      cost_share = dplyr::if_else(rev > 0, costs.total / rev, NA_real_)
    ) |>
    tidyr::pivot_longer(c(costs, remainder), names_to = "segment", values_to = "value") |>
    dplyr::mutate(
      segment = factor(segment, levels = c("remainder", "costs")),
      segment = dplyr::recode(segment, costs = "Costs", remainder = "EV")
    )

  p8 <- ggplot2::ggplot(plotdf_comm_econ, ggplot2::aes(year, value, fill = segment)) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::geom_text(
      data = subset(plotdf_comm_econ, segment == "Costs"),
      ggplot2::aes(label = scales::percent(cost_share, accuracy = 1), y = value / 2),
      size = 3.5, color = "white", fontface = "bold"
    ) +
    ggplot2::scale_fill_manual(values = PAL$costs2) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(
      title    = "Commercial Fisheries: Cost Share of Gross Revenue",
      subtitle = paste0("Statewide total, all species, ", yr_comm[1], "\u2013", yr_comm[2], " (2021 USD)"),
      x = "Year", y = "Dollars ($)", fill = "Revenue Component"
    ) +
    .theme_ev() +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))
  .save_plot(p8, outdir, "comm_cost_share_revenue.png", width = FIG_W, height = FIG_H)

  # ---- 9. Commercial revenue vs cost lines --------------------------------
  plotdf_comm_rc <- comm_elements_all |>
    dplyr::filter(county == "all", spcs.grp == "all") |>
    dplyr::select(year, rev, costs.labor, costs.other, ev) |>
    tidyr::pivot_longer(-year, names_to = "variable", values_to = "value") |>
    dplyr::mutate(variable = dplyr::recode(variable,
      rev         = "Revenue",      costs.labor = "Labor costs",
      costs.other = "Other costs",  ev          = "Exchange value"
    ))

  p9 <- ggplot2::ggplot(plotdf_comm_rc, ggplot2::aes(year, value, color = variable)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_color_manual(values = PAL$components) +
    ggplot2::labs(
      title    = "Commercial Fisheries: Revenue and Costs Over Time",
      subtitle = paste0("Statewide total, all species, ", yr_comm[1], "\u2013", yr_comm[2], " (2021 USD)"),
      x = "Year", y = "Dollars ($)", color = "Component"
    ) +
    .theme_ev() +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 1))
  .save_plot(p9, outdir, "comm_revenue_costs_lines.png", width = FIG_W, height = FIG_H)

  # ---- 10. Non-commercial cost share --------------------------------------
  noncomm_elements <- noncomm_costs_src |>
    dplyr::mutate(
      costs.other = lbs.total * costs.other.perlb.adj,
      costs.labor = lbs.total * costs.labor.perlb.adj,
      costs.total = costs.other + costs.labor
    ) |>
    dplyr::select(-costs.other.perlb.adj, -costs.labor.perlb.adj, -lbs.total)

  meas_nc <- c("rev.total", "ev.total", "costs.other", "costs.labor", "costs.total")

  noncomm_with_all <- noncomm_elements |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(meas_nc), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::mutate(island = "all") |>
    dplyr::relocate(year, island) |>
    dplyr::bind_rows(noncomm_elements) |>
    dplyr::arrange(year, island)

  plotdf_nc_econ <- noncomm_with_all |>
    dplyr::filter(island == "all") |>
    dplyr::transmute(
      year,
      rev       = rev.total,
      costs     = costs.total,
      remainder = pmax(rev.total - costs.total, 0),
      cost_share = dplyr::if_else(rev.total > 0, costs.total / rev.total, NA_real_)
    ) |>
    tidyr::pivot_longer(c(costs, remainder), names_to = "segment", values_to = "value") |>
    dplyr::mutate(segment = dplyr::recode(segment, costs = "Costs", remainder = "EV"))

  p10 <- ggplot2::ggplot(plotdf_nc_econ, ggplot2::aes(year, value, fill = segment)) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::geom_text(
      data = subset(plotdf_nc_econ, segment == "Costs"),
      ggplot2::aes(label = scales::percent(cost_share, accuracy = 1), y = value / 2),
      size = 3.5, color = "white", fontface = "bold"
    ) +
    ggplot2::scale_fill_manual(values = PAL$costs2) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(
      title    = "Non-Commercial Fisheries: Cost Share of Gross Revenue",
      subtitle = paste0("Statewide total, reef herbivores, ", yr_nc[1], "\u2013", yr_nc[2], " (2021 USD)"),
      x = "Year", y = "Dollars ($)", fill = "Revenue Component"
    ) +
    .theme_ev() +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))
  .save_plot(p10, outdir, "noncomm_cost_share_revenue.png", width = FIG_W, height = FIG_H)

  # ---- 11. Non-commercial revenue vs cost lines ---------------------------
  plotdf_nc_lines <- noncomm_with_all |>
    dplyr::filter(island == "all") |>
    dplyr::select(year, rev.total, costs.other, costs.labor) |>
    tidyr::pivot_longer(-year, names_to = "variable", values_to = "value") |>
    dplyr::mutate(variable = dplyr::recode(variable,
      rev.total   = "Revenue", costs.other = "Other costs", costs.labor = "Labor costs"
    ))

  p11 <- ggplot2::ggplot(plotdf_nc_lines, ggplot2::aes(year, value, color = variable)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_color_manual(
      values = PAL$components[c("Revenue", "Other costs", "Labor costs")]
    ) +
    ggplot2::labs(
      title    = "Non-Commercial Fisheries: Revenue and Costs Over Time",
      subtitle = paste0("Statewide total, reef herbivores, ", yr_nc[1], "\u2013", yr_nc[2], " (2021 USD)"),
      x = "Year", y = "Dollars ($)", color = "Component"
    ) +
    .theme_ev() +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 1))
  .save_plot(p11, outdir, "noncomm_revenue_costs_lines.png", width = FIG_W, height = FIG_H)

  # ---- Return paths (exclude _reference, _archive, trash) ----------------
  all_pngs <- list.files(outdir, pattern = "\\.png$", full.names = TRUE)
  grep("_reference|_archive|trash", all_pngs, value = TRUE, invert = TRUE)
}


# =============================================================================
# FIGURE EXPORT HELPER
# Exports a plot in three formats: PNG (report/Google Docs), SVG (web), PNG (preview).
# File names include a suffix clarifying intended use:
#   {name}_report.png  — high-res raster (300 DPI), for Google Docs / Word / reports
#   {name}_web.svg     — vector, for the Quarto website (crisp at any zoom)
#   {name}_preview.png — lower-res raster, quick preview / fallback
# Returns a character vector of all three paths (for targets format = "file").
# =============================================================================

export_figure <- function(plot, filename_base, outdir, width = 24, height = 14) {
  fs::dir_create(outdir, recurse = TRUE)

  report_path <- file.path(outdir, paste0(filename_base, "_report.png"))
  svg_path    <- file.path(outdir, paste0(filename_base, "_web.svg"))
  png_path    <- file.path(outdir, paste0(filename_base, "_preview.png"))

  ggplot2::ggsave(report_path, plot = plot, width = width, height = height,
                  dpi = 300, bg = "white")
  ggplot2::ggsave(svg_path, plot = plot, width = width, height = height,
                  bg = "white")
  ggplot2::ggsave(png_path, plot = plot, width = width, height = height,
                  dpi = 150, bg = "white")

  c(report_path, svg_path, png_path)
}

# =============================================================================
# SPATIAL UNITS MAP
# =============================================================================

#' Generate fisheries spatial units map
#'
#' Produces a side-by-side map of commercial (DAR catch areas) and
#' non-commercial (island/county) spatial reporting units, with a globe
#' inset showing Hawaiʻi's location and a north arrow.
#'
#' @param fish_catch_areas_fp Path to DAR fish catch areas shapefile
#' @param mokus_fp Path to combined mokus GeoPackage
#' @param outdir Directory to write PNG into
#' @return Character path to written PNG (for format = "file")
generate_fisheries_spatial_maps <- function(fish_catch_areas_fp, mokus_fp, outdir) {
  fs::dir_create(outdir, recurse = TRUE)

  # ── Area IDs ────────────────────────────────────────────────────────────────
  valid_area_ids <- c(
    100:108, 120:128,
    300:314, 320:328, 331:333,
    400:409, 420:429,
    500:506, 508, 520:528
  )

  # ── Load data ────────────────────────────────────────────────────────────────
  fish_catch_areas <- sf::st_read(fish_catch_areas_fp, quiet = TRUE) |>
    sf::st_transform(4326) |>
    janitor::clean_names() |>
    dplyr::select(area_id, geometry) |>
    dplyr::filter(area_id %in% valid_area_ids) |>
    sf::st_make_valid()

  mokus <- sf::st_read(mokus_fp, quiet = TRUE) |>
    sf::st_transform(4326) |>
    janitor::clean_names()

  # ── Dissolve to islands ──────────────────────────────────────────────────────
  .dissolve_islands <- function(mokus_sf) {
    mokus_sf |>
      dplyr::mutate(
        island       = stringr::str_to_title(island),
        island_olelo = stringr::str_replace_all(island_olelo, "\u2018|\u2019|'|'", "\u02BB")
      ) |>
      rmapshaper::ms_dissolve(field = "island", copy_fields = c("island", "island_olelo"))
  }

  terrestrial_islands <- mokus |> dplyr::filter(realm == "Terrestrial") |> .dissolve_islands()
  marine_islands      <- mokus |> dplyr::filter(realm == "Marine")      |> .dissolve_islands()

  # ── Shared extent ────────────────────────────────────────────────────────────
  bbox_all    <- sf::st_bbox(sf::st_union(terrestrial_islands, fish_catch_areas))
  xlim_shared <- c(bbox_all["xmin"] - 0.2, bbox_all["xmax"] + 0.2)
  ylim_shared <- c(bbox_all["ymin"] - 0.3, bbox_all["ymax"] + 0.3)

  # ── Colors ───────────────────────────────────────────────────────────────────
  okabe_blue  <- "#0072B2"
  bright_red  <- "#FF0000"
  island_grey <- "grey80"

  # ── Commercial map ───────────────────────────────────────────────────────────
  map_comm <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = terrestrial_islands, fill = island_grey, color = "black") +
    ggplot2::geom_sf(data = fish_catch_areas, fill = okabe_blue, color = "black", linewidth = 0.2, alpha = 0.6) +
    ggplot2::ggtitle("Commercial Fisheries") +
    ggplot2::labs(subtitle = "DAR catch area boundaries (83 areas)") +
    ggplot2::coord_sf(xlim = xlim_shared, ylim = ylim_shared, expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(size = 28, face = "bold", hjust = 0.5),
      plot.subtitle   = ggplot2::element_text(size = 20, hjust = 0.5, color = "grey40"),
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )

  # ── Non-commercial map ───────────────────────────────────────────────────────
  map_noncomm <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = marine_islands, fill = okabe_blue, color = "black", alpha = 0.6) +
    ggplot2::geom_sf(data = terrestrial_islands, fill = island_grey, color = "black") +
    ggspatial::annotation_scale(
      location   = "br",
      width_hint = 0.15,
      pad_x      = grid::unit(1.0, "cm"),
      pad_y      = grid::unit(0.15, "cm"),
      text_cex   = 2.5,
      height     = grid::unit(0.6, "cm")
    ) +
    ggplot2::ggtitle("Non-Commercial Fisheries") +
    ggplot2::labs(subtitle = "Island-level zones \u00b7 Kaua\u02bbi, Honolulu, Maui, Hawai\u02bbi counties") +
    ggplot2::coord_sf(xlim = xlim_shared, ylim = ylim_shared, expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(size = 28, face = "bold", hjust = 0.5),
      plot.subtitle   = ggplot2::element_text(size = 20, hjust = 0.5, color = "grey40"),
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )

  # ── North arrow grob ─────────────────────────────────────────────────────────
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

  # ── Globe inset ──────────────────────────────────────────────────────────────
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
      color = bright_red, shape = 17, size = 4
    ) +
    ggplot2::theme_void() +
    ggplot2::coord_sf(clip = "off")

  # ── Overall title (same cowplot pattern as EV change map) ────────────────────
  title_grob <- cowplot::ggdraw() +
    cowplot::draw_label(
      "Spatial Reporting Units: Commercial and Non-Commercial Fisheries",
      fontface = "bold", size = 40, hjust = 0.5, x = 0.5, y = 0.70
    ) +
    cowplot::draw_label(
      "Main Hawaiian Islands",
      size = 30, hjust = 0.5, x = 0.5, y = 0.15, color = "grey40"
    )

  # ── Combine maps ─────────────────────────────────────────────────────────────
  maps_row <- cowplot::plot_grid(
    map_comm, map_noncomm,
    ncol = 2, align = "hv"
  )

  combined_maps <- cowplot::plot_grid(
    title_grob, maps_row,
    ncol = 1, rel_heights = c(0.12, 1)
  )

  # ── Final composition ────────────────────────────────────────────────────────
  final_figure <- cowplot::ggdraw(combined_maps) +
    cowplot::draw_plot(
      globe_inset,
      x = 0.01, y = 0.02,
      width = 0.12, height = 0.28
    ) +
    cowplot::draw_grob(
      north_arrow_grob,
      x = 0.02, y = 0.76,
      width = 0.04, height = 0.15
    ) +
    ggplot2::theme(plot.margin = ggplot2::margin(40, 50, 40, 50))

  # ── Export (PDF report + SVG web + PNG preview) ──────────────────────────────
  export_figure(final_figure, "fisheries_spatial_units", outdir, width = 24, height = 14)
}

# =============================================================================
# generate_ev_change_maps — Figure 23
# Side-by-side choropleth of gross commercial EV by DAR catch area, 2014 vs 2021
# =============================================================================

#' @param comm_ev    Data frame from `fisheries_comm_ev` target
#' @param fish_catch_areas_fp  Path to DAR catch areas shapefile
#' @param mokus_fp   Path to combined mokus GeoPackage (for island outlines)
#' @param outdir     Directory to write PNG into
#' @return Character path to written PNG (for format = "file")
generate_ev_change_maps <- function(master_xlsx, fish_catch_areas_fp, mokus_fp, outdir) {
  fs::dir_create(outdir, recurse = TRUE)

  valid_area_ids <- c(
    100:108, 120:128,
    300:314, 320:328, 331:333,
    400:409, 420:429,
    500:506, 508, 520:528
  )

  # ── Load spatial data ────────────────────────────────────────────────────────
  fish_catch_areas <- sf::st_read(fish_catch_areas_fp, quiet = TRUE) |>
    sf::st_transform(4326) |>
    janitor::clean_names() |>
    dplyr::select(area_id, geometry) |>
    dplyr::filter(area_id %in% valid_area_ids) |>
    sf::st_make_valid()

  mokus <- sf::st_read(mokus_fp, quiet = TRUE) |>
    sf::st_transform(4326) |>
    janitor::clean_names()

  .dissolve_islands_ev <- function(mokus_sf) {
    mokus_sf |>
      dplyr::mutate(island = stringr::str_to_title(island)) |>
      rmapshaper::ms_dissolve(field = "island", copy_fields = "island")
  }

  terrestrial_islands <- mokus |> dplyr::filter(realm == "Terrestrial") |> .dissolve_islands_ev()

  # ── Load authoritative EV from master sheet ──────────────────────────────────
  comm_master <- readxl::read_excel(master_xlsx, sheet = "Comm EV") |>
    dplyr::mutate(year = as.integer(year), area = as.integer(area))

  # ── Summarise EV by area for target years ───────────────────────────────────
  ev_by_area <- comm_master |>
    dplyr::filter(year %in% c(2014L, 2021L)) |>
    dplyr::group_by(year, area) |>
    dplyr::summarise(ev_total = sum(ev.TOTAL, na.rm = TRUE), .groups = "drop") |>
    dplyr::rename(area_id = area)

  ev_2014 <- ev_by_area |> dplyr::filter(year == 2014L)
  ev_2021 <- ev_by_area |> dplyr::filter(year == 2021L)

  areas_2014 <- fish_catch_areas |>
    dplyr::left_join(ev_2014, by = "area_id") |>
    dplyr::mutate(ev_total = tidyr::replace_na(ev_total, 0))

  areas_2021 <- fish_catch_areas |>
    dplyr::left_join(ev_2021, by = "area_id") |>
    dplyr::mutate(ev_total = tidyr::replace_na(ev_total, 0))

  # ── Shared fill scale ───────────────────────────────────────────────────────
  ev_max   <- max(c(areas_2014$ev_total, areas_2021$ev_total), na.rm = TRUE)
  fill_lim <- c(0, ev_max)

  # Okabe-Ito blue gradient (light → #0072B2), consistent with spatial units map.
  # Areas with ev_total < 0 (costs > revenue) fall outside limits → grey85.
  fill_scale <- ggplot2::scale_fill_gradient(
    low      = "#deebf7",
    high     = "#0072B2",
    limits   = fill_lim,
    labels   = scales::label_dollar(scale = 1e-6, suffix = "M", accuracy = 0.1),
    name     = "Exchange Value\n(2021 USD)",
    na.value = "grey85",
    guide    = ggplot2::guide_colorbar(
      title.theme = ggplot2::element_text(face = "bold", size = 26),
      barwidth    = grid::unit(18, "cm"),
      barheight   = grid::unit(1.2, "cm")
    )
  )

  # ── Shared extent ────────────────────────────────────────────────────────────
  bbox_all    <- sf::st_bbox(sf::st_union(terrestrial_islands, fish_catch_areas))
  xlim_shared <- c(bbox_all["xmin"] - 0.2, bbox_all["xmax"] + 0.2)
  ylim_shared <- c(bbox_all["ymin"] - 0.3, bbox_all["ymax"] + 0.3)

  island_grey <- "grey80"

  # ── Map builder ──────────────────────────────────────────────────────────────
  .make_ev_map <- function(areas_sf, yr, show_scalebar = TRUE) {
    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = terrestrial_islands, fill = island_grey, color = "black", linewidth = 0.3) +
      ggplot2::geom_sf(data = areas_sf, ggplot2::aes(fill = ev_total), color = "black", linewidth = 0.15) +
      fill_scale +
      ggplot2::ggtitle(as.character(yr)) +
      ggplot2::coord_sf(xlim = xlim_shared, ylim = ylim_shared, expand = FALSE) +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.title       = ggplot2::element_text(size = 28, face = "bold", hjust = 0.5),
        plot.background  = ggplot2::element_rect(fill = "white", color = NA),
        legend.position  = "bottom",
        legend.direction = "horizontal",
        legend.title     = ggplot2::element_text(face = "bold", size = 26),
        legend.text      = ggplot2::element_text(size = 22)
      )
    if (show_scalebar) {
      p <- p + ggspatial::annotation_scale(
        location   = "bl",
        width_hint = 0.15,
        pad_x      = grid::unit(1.0, "cm"),
        pad_y      = grid::unit(0.6, "cm"),
        text_cex   = 2.5,
        height     = grid::unit(0.6, "cm")
      )
    }
    p
  }

  map_2014 <- .make_ev_map(areas_2014, 2014, show_scalebar = TRUE)
  map_2021 <- .make_ev_map(areas_2021, 2021, show_scalebar = FALSE)

  # ── North arrow grob ─────────────────────────────────────────────────────────
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

  # ── Globe inset ──────────────────────────────────────────────────────────────
  hawaii_lon <- -157.5
  hawaii_lat <-  20.5
  ortho_crs  <- sprintf("+proj=ortho +lat_0=%s +lon_0=%s", hawaii_lat, hawaii_lon)
  bright_red <- "#FF0000"

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
      color = bright_red, shape = 17, size = 4
    ) +
    ggplot2::theme_void() +
    ggplot2::coord_sf(clip = "off")

  # ── Combine with shared legend at bottom center ──────────────────────────────
  shared_legend <- cowplot::get_legend(map_2021)

  map_2014_noleg <- map_2014 + ggplot2::theme(legend.position = "none")
  map_2021_noleg <- map_2021 + ggplot2::theme(legend.position = "none")

  maps_row <- cowplot::plot_grid(map_2014_noleg, map_2021_noleg, ncol = 2, align = "hv")

  # ── Title ────────────────────────────────────────────────────────────────────
  title_grob <- cowplot::ggdraw() +
    cowplot::draw_label(
      "Change in Spatial Distribution of Gross Commercial Fisheries Exchange Value, MHI",
      fontface = "bold", size = 40, hjust = 0.5, x = 0.5, y = 0.70
    ) +
    cowplot::draw_label(
      "DAR catch areas, 2014 and 2021 (2021 USD)",
      size = 30, hjust = 0.5, x = 0.5, y = 0.15, color = "grey40"
    )

  combined_maps <- cowplot::plot_grid(
    title_grob, maps_row, shared_legend,
    ncol = 1, rel_heights = c(0.12, 1, 0.12)
  )

  # ── Final composition ────────────────────────────────────────────────────────
  final_figure <- cowplot::ggdraw(combined_maps) +
    cowplot::draw_plot(
      globe_inset,
      x = 0.89, y = 0.68,
      width = 0.12, height = 0.30
    ) +
    cowplot::draw_grob(
      north_arrow_grob,
      x = 0.02, y = 0.76,
      width = 0.04, height = 0.15
    ) +
    ggplot2::theme(plot.margin = ggplot2::margin(40, 50, 40, 50))

  # ── Export ───────────────────────────────────────────────────────────────────
  # ── Export (PDF report + SVG web + PNG preview) ──────────────────────────────
  export_figure(final_figure, "comm_ev_change_map", outdir, width = 24, height = 14)
}

# =============================================================================
# REBUILD EV MASTER FINALE FROM TRACE RAW
# Replicates Ela's standalone script rebuild_ev_master_finale_from_trace_raw.R.
# Reads trace_raw.xlsx (which contains all pre-processed inputs in a single
# workbook) and writes the authoritative ev_master_finale_rebuilt.xlsx used
# by generate_fisheries_ev_figs() and generate_ev_change_maps().
# =============================================================================

#' Rebuild ev_master_finale workbook from trace_raw.xlsx
#'
#' @param trace_path Path to trace_raw.xlsx (input).
#' @param out_path   Path to write ev_master_finale_rebuilt.xlsx (output).
#' @return `out_path` invisibly (for use with `format = "file"` in targets).
rebuild_ev_master_finale <- function(trace_path, out_path) {
  base_year <- 2021L

  read_sheet <- function(sheet) readxl::read_xlsx(trace_path, sheet = sheet)

  comm_landings    <- read_sheet("Comm Landings")
  noncomm_landings <- read_sheet("Non-comm Landings")
  wmp_raw          <- read_sheet("MWP")
  labor_raw        <- read_sheet("Labor costs per lb")
  costs_other_raw  <- read_sheet("Non-labor costs per lb")
  sbf_alloc        <- read_sheet("SBF allocations")
  dar_area_codes   <- read_sheet("DAR area codes")
  cpi_tbl          <- read_sheet("CPI Fred")

  # ---- normalize inputs -------------------------------------------------------

  cpi_tbl <- cpi_tbl %>%
    dplyr::rename(year = 1, CPI = 2) %>%
    dplyr::mutate(year = as.integer(year), CPI = as.numeric(CPI))
  cpi_lookup <- setNames(cpi_tbl$CPI, cpi_tbl$year)
  cpi_base <- as.numeric(cpi_lookup[as.character(base_year)])

  wmp_calc <- wmp_raw %>%
    dplyr::rename(species_group = 1, mwp = 2) %>%
    dplyr::mutate(
      species_group = tolower(trimws(as.character(species_group))),
      mwp = as.numeric(mwp)
    )
  mwp_map <- setNames(wmp_calc$mwp, wmp_calc$species_group)

  costs_other_calc <- costs_other_raw %>%
    dplyr::rename(species_group = 1, costs.other.per.lb = 2) %>%
    dplyr::mutate(
      species_group = tolower(trimws(as.character(species_group))),
      costs.other.per.lb = as.numeric(costs.other.per.lb)
    )
  costs_other_map <- setNames(costs_other_calc$costs.other.per.lb, costs_other_calc$species_group)

  labor_calc <- labor_raw %>%
    dplyr::rename(species_group = 1) %>%
    dplyr::mutate(species_group = tolower(trimws(as.character(species_group))))

  island_cols <- setdiff(names(labor_calc), "species_group")
  names(labor_calc)[names(labor_calc) %in% island_cols] <- tolower(trimws(island_cols))

  make_labor_map <- function(group) {
    row_df <- labor_calc %>%
      dplyr::filter(species_group == group) %>%
      dplyr::select(-species_group)
    vals <- as.numeric(as.matrix(row_df[1, , drop = FALSE]))
    setNames(vals, names(row_df))
  }

  labor_maps <- list(
    deep7   = make_labor_map("deep7"),
    sbf     = make_labor_map("sbf"),
    reef    = make_labor_map("reef"),
    pelagic = make_labor_map("pelagic")
  )

  comm_landings <- comm_landings %>%
    dplyr::mutate(
      year            = as.integer(year),
      area            = as.numeric(area),
      county          = tolower(trimws(as.character(county))),
      landings.deep7  = as.numeric(landings.deep7),
      landings.sbf    = as.numeric(landings.sbf),
      landings.reef   = as.numeric(landings.reef),
      landings.pelagic = as.numeric(landings.pelagic)
    )

  noncomm_landings <- noncomm_landings %>%
    dplyr::mutate(
      year          = as.integer(year),
      island        = tolower(trimws(as.character(island))),
      island.weight = as.numeric(island.weight),
      lbs.total     = as.numeric(lbs.total),
      kg.total      = as.numeric(kg.total),
      reef.price    = as.numeric(reef.price)
    )

  sbf_alloc <- sbf_alloc %>%
    dplyr::mutate(
      year = as.integer(year),
      area = as.numeric(area)
    )

  dar_area_codes <- dar_area_codes %>%
    dplyr::mutate(area = as.numeric(area), county = as.character(county))

  # ---- commercial EV ----------------------------------------------------------

  cpi_factor_fn <- function(year_vec) as.numeric(cpi_base / cpi_lookup[as.character(year_vec)])

  comm_ev <- comm_landings %>%
    dplyr::mutate(
      cpi_factor = cpi_factor_fn(year),

      mwp.deep7   = mwp_map[["deep7"]],
      mwp.sbf     = mwp_map[["sbf"]],
      mwp.reef    = mwp_map[["reef"]],
      mwp.pelagic = mwp_map[["pelagic"]],

      mwp.deep7.adj   = mwp.deep7   * cpi_factor,
      mwp.sbf.adj     = mwp.sbf     * cpi_factor,
      mwp.reef.adj    = mwp.reef    * cpi_factor,
      mwp.pelagic.adj = mwp.pelagic * cpi_factor,

      costs.other.perlb.deep7   = costs_other_map[["deep7"]],
      costs.other.perlb.sbf     = costs_other_map[["sbf"]],
      costs.other.perlb.reef    = costs_other_map[["reef"]],
      costs.other.perlb.pelagic = costs_other_map[["pelagic"]],

      county_lc = county,
      costs.labor.perlb.deep7   = round(unname(labor_maps$deep7[county_lc]),   2),
      costs.labor.perlb.sbf     = round(unname(labor_maps$sbf[county_lc]),     2),
      costs.labor.perlb.reef    = round(unname(labor_maps$reef[county_lc]),    2),
      costs.labor.perlb.pelagic = round(unname(labor_maps$pelagic[county_lc]), 2),

      costs.all.perlb.deep7.adj   = (costs.other.perlb.deep7   + costs.labor.perlb.deep7)   * cpi_factor,
      costs.all.perlb.sbf.adj     = (costs.other.perlb.sbf     + costs.labor.perlb.sbf)     * cpi_factor,
      costs.all.perlb.reef.adj    = (costs.other.perlb.reef    + costs.labor.perlb.reef)    * cpi_factor,
      costs.all.perlb.pelagic.adj = (costs.other.perlb.pelagic + costs.labor.perlb.pelagic) * cpi_factor,

      rev.deep7   = dplyr::if_else(is.na(landings.deep7),   NA_real_, landings.deep7   * mwp.deep7.adj),
      rev.sbf     = dplyr::if_else(is.na(landings.sbf),     NA_real_, landings.sbf     * mwp.sbf.adj),
      rev.reef    = dplyr::if_else(is.na(landings.reef),    NA_real_, landings.reef    * mwp.reef.adj),
      rev.pelagic = dplyr::if_else(is.na(landings.pelagic), NA_real_, landings.pelagic * mwp.pelagic.adj),

      ev.deep7   = dplyr::if_else(is.na(landings.deep7),   NA_real_, rev.deep7   - landings.deep7   * costs.all.perlb.deep7.adj),
      ev.sbf     = dplyr::if_else(is.na(landings.sbf),     NA_real_, rev.sbf     - landings.sbf     * costs.all.perlb.sbf.adj),
      ev.reef    = dplyr::if_else(is.na(landings.reef),    NA_real_, rev.reef    - landings.reef    * costs.all.perlb.reef.adj),
      ev.pelagic = dplyr::if_else(is.na(landings.pelagic), NA_real_, rev.pelagic - landings.pelagic * costs.all.perlb.pelagic.adj)
    ) %>%
    dplyr::left_join(sbf_alloc, by = c("year", "area")) %>%
    dplyr::mutate(
      ev.sbf.REEFET    = dplyr::if_else(is.na(ev.sbf), NA_real_, ev.sbf * sbf.ratio.REEFET),
      ev.sbf.NONREEFET = dplyr::if_else(is.na(ev.sbf), NA_real_, ev.sbf - ev.sbf.REEFET),
      ev.REEFET    = rowSums(cbind(ev.sbf.REEFET, ev.reef),                    na.rm = TRUE),
      ev.NONREEFET = rowSums(cbind(ev.deep7, ev.sbf.NONREEFET, ev.pelagic),    na.rm = TRUE),
      ev.TOTAL     = rowSums(cbind(ev.deep7, ev.sbf, ev.reef, ev.pelagic),     na.rm = TRUE)
    )

  comm_ev_out <- comm_ev %>%
    dplyr::transmute(
      year, area, county,
      landings.deep7, mwp.deep7, mwp.deep7.adj, rev.deep7,
      costs.other.perlb.deep7, costs.labor.perlb.deep7, costs.all.perlb.deep7.adj, ev.deep7,
      landings.sbf, mwp.sbf, mwp.sbf.adj, rev.sbf,
      costs.other.perlb.sbf, costs.labor.perlb.sbf, costs.all.perlb.sbf.adj, ev.sbf,
      ev.sbf.REEFET, ev.sbf.NONREEFET,
      landings.reef, mwp.reef, mwp.reef.adj, rev.reef,
      costs.other.perlb.reef, costs.labor.perlb.reef, costs.all.perlb.reef.adj, ev.reef,
      landings.pelagic, mwp.pelagic, mwp.pelagic.adj, rev.pelagic,
      costs.other.perlb.pelagic, costs.labor.perlb.pelagic, costs.all.perlb.pelagic.adj, ev.pelagic,
      ev.REEFET, ev.NONREEFET, ev.TOTAL
    )

  # ---- non-commercial EV ------------------------------------------------------

  noncomm_ev <- noncomm_landings %>%
    dplyr::mutate(
      cpi_factor          = cpi_factor_fn(year),
      reef.price.adj      = reef.price * cpi_factor,
      rev.total           = lbs.total * reef.price.adj,
      cost.other.per.lb   = costs_other_map[["reef"]],
      costs.other.perlb.adj = cost.other.per.lb * cpi_factor,
      island_for_labor    = dplyr::if_else(island == "lanai", "maui", island),
      costs.labor.perlb   = round(unname(labor_maps$reef[island_for_labor]), 2),
      costs.labor.perlb.adj = costs.labor.perlb * cpi_factor,
      ev.total            = rev.total - lbs.total * (costs.other.perlb.adj + costs.labor.perlb.adj),
      ev.total.REEF       = ev.total,
      ev.total.OO         = ev.total - ev.total.REEF
    )

  noncomm_ev_out <- noncomm_ev %>%
    dplyr::transmute(
      year, island, island.weight, lbs.total, kg.total,
      reef.price, reef.price.adj, rev.total,
      cost.other.per.lb, costs.other.perlb.adj,
      costs.labor.perlb, costs.labor.perlb.adj,
      ev.total, ev.total.REEF, ev.total.OO
    )

  # ---- write workbook ---------------------------------------------------------

  fs::dir_create(dirname(out_path))
  writexl::write_xlsx(
    list("Comm EV" = comm_ev_out, "Non-comm EV" = noncomm_ev_out),
    out_path
  )

  invisible(out_path)
}
