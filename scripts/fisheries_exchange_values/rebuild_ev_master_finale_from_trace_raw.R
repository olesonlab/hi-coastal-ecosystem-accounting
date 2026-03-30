#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(here)
  library(readxl)
  library(dplyr)
  library(writexl)
})

trace_path <- here("data/01_raw/fisheries_exchange_values/trace_raw.xlsx") # Replace WD if needed
out_path <- here("data/01_raw/fisheries_exchange_values/test_ev_master_finale_rebuilt.xlsx") # Replace WD if needed

base_year <- 2021L

read_sheet <- function(sheet) {
  read_xlsx(trace_path, sheet = sheet)
}

comm_landings <- read_sheet("Comm Landings")
noncomm_landings <- read_sheet("Non-comm Landings")
wmp_raw <- read_sheet("MWP")
labor_raw <- read_sheet("Labor costs per lb")
costs_other_raw <- read_sheet("Non-labor costs per lb")
sbf_alloc <- read_sheet("SBF allocations")
dar_area_codes <- read_sheet("DAR area codes")
cpi_tbl <- read_sheet("CPI Fred")

# ---- normalize inputs -------------------------------------------------------

# CPI
cpi_tbl <- cpi_tbl %>%
  rename(year = 1, CPI = 2) %>%
  mutate(year = as.integer(year), CPI = as.numeric(CPI))
cpi_lookup <- setNames(cpi_tbl$CPI, cpi_tbl$year)
cpi_base <- as.numeric(cpi_lookup[as.character(base_year)])

# WMP (calculation version)
wmp_calc <- wmp_raw %>%
  rename(species_group = 1, mwp = 2) %>%
  mutate(
    species_group = tolower(trimws(as.character(species_group))),
    mwp = as.numeric(mwp)
  )
mwp_map <- setNames(wmp_calc$mwp, wmp_calc$species_group)

# Non-labor costs other ($/lb)
costs_other_calc <- costs_other_raw %>%
  rename(species_group = 1, costs.other.per.lb = 2) %>%
  mutate(
    species_group = tolower(trimws(as.character(species_group))),
    costs.other.per.lb = as.numeric(costs.other.per.lb)
  )
costs_other_map <- setNames(costs_other_calc$costs.other.per.lb, costs_other_calc$species_group)

# Labor costs per lb table (species_group rows; island columns)
labor_calc <- labor_raw %>%
  rename(species_group = 1) %>%
  mutate(species_group = tolower(trimws(as.character(species_group))))

island_cols <- setdiff(names(labor_calc), "species_group")
names(labor_calc)[names(labor_calc) %in% island_cols] <- tolower(trimws(island_cols))

make_labor_map <- function(group) {
  row_df <- labor_calc %>%
    filter(species_group == group) %>%
    select(-species_group)

  vals <- as.numeric(as.matrix(row_df[1, , drop = FALSE]))
  nms <- names(row_df)
  setNames(vals, nms)
}

labor_maps <- list(
  deep7 = make_labor_map("deep7"),
  sbf = make_labor_map("sbf"),
  reef = make_labor_map("reef"),
  pelagic = make_labor_map("pelagic")
)

# Comm landings
comm_landings <- comm_landings %>%
  mutate(
    year = as.integer(year),
    area = as.numeric(area),
    county = tolower(trimws(as.character(county))),
    landings.deep7 = as.numeric(landings.deep7),
    landings.sbf = as.numeric(landings.sbf),
    landings.reef = as.numeric(landings.reef),
    landings.pelagic = as.numeric(landings.pelagic)
  )

# Non-comm landings
noncomm_landings <- noncomm_landings %>%
  mutate(
    year = as.integer(year),
    island = tolower(trimws(as.character(island))),
    island.weight = as.numeric(island.weight),
    lbs.total = as.numeric(lbs.total),
    kg.total = as.numeric(kg.total),
    reef.price = as.numeric(reef.price)
  )

# SBF allocations
sbf_alloc <- sbf_alloc %>%
  mutate(
    year = as.integer(year),
    area = as.numeric(area)
  ) %>%
  rename(
    sbf.ratio.REEFET = `sbf.ratio.REEFET`,
    sbf.ratio.NONREEFET = `sbf.ratio.NONREEFET`
  )

# DAR area codes (keep as-is, but normalize types)
dar_area_codes <- dar_area_codes %>%
  mutate(area = as.numeric(area), county = as.character(county))

# ---- computations ------------------------------------------------------------

comm_cpi_factor <- function(year_vec) {
  as.numeric(cpi_base / cpi_lookup[as.character(year_vec)])
}

comm_ev <- comm_landings %>%
  mutate(
    cpi_factor = comm_cpi_factor(year),

    # MWPs (pooled constants from WMP)
    mwp.deep7 = mwp_map[["deep7"]],
    mwp.sbf = mwp_map[["sbf"]],
    mwp.reef = mwp_map[["reef"]],
    mwp.pelagic = mwp_map[["pelagic"]],

    # Inflation-adjusted MWPs
    mwp.deep7.adj = mwp.deep7 * cpi_factor,
    mwp.sbf.adj = mwp.sbf * cpi_factor,
    mwp.reef.adj = mwp.reef * cpi_factor,
    mwp.pelagic.adj = mwp.pelagic * cpi_factor,

    # Other costs ($/lb)
    costs.other.perlb.deep7 = costs_other_map[["deep7"]],
    costs.other.perlb.sbf = costs_other_map[["sbf"]],
    costs.other.perlb.reef = costs_other_map[["reef"]],
    costs.other.perlb.pelagic = costs_other_map[["pelagic"]],

    # Labor costs ($/lb) by county/island (rounded to match finale workbook)
    county_lc = county,
    costs.labor.perlb.deep7 = unname(labor_maps$deep7[county_lc]) %>% round(2),
    costs.labor.perlb.sbf = unname(labor_maps$sbf[county_lc]) %>% round(2),
    costs.labor.perlb.reef = unname(labor_maps$reef[county_lc]) %>% round(2),
    costs.labor.perlb.pelagic = unname(labor_maps$pelagic[county_lc]) %>% round(2),

    # Inflation-adjusted total cost per lb
    costs.all.perlb.deep7.adj = (costs.other.perlb.deep7 + costs.labor.perlb.deep7) * cpi_factor,
    costs.all.perlb.sbf.adj = (costs.other.perlb.sbf + costs.labor.perlb.sbf) * cpi_factor,
    costs.all.perlb.reef.adj = (costs.other.perlb.reef + costs.labor.perlb.reef) * cpi_factor,
    costs.all.perlb.pelagic.adj = (costs.other.perlb.pelagic + costs.labor.perlb.pelagic) * cpi_factor,

    # Revenues (blank in Excel when landings are blank)
    rev.deep7 = if_else(is.na(landings.deep7), NA_real_, landings.deep7 * mwp.deep7.adj),
    rev.sbf = if_else(is.na(landings.sbf), NA_real_, landings.sbf * mwp.sbf.adj),
    rev.reef = if_else(is.na(landings.reef), NA_real_, landings.reef * mwp.reef.adj),
    rev.pelagic = if_else(is.na(landings.pelagic), NA_real_, landings.pelagic * mwp.pelagic.adj),

    # EV components (blank when corresponding landings are blank)
    ev.deep7 = if_else(is.na(landings.deep7), NA_real_, rev.deep7 - landings.deep7 * costs.all.perlb.deep7.adj),
    ev.sbf = if_else(is.na(landings.sbf), NA_real_, rev.sbf - landings.sbf * costs.all.perlb.sbf.adj),
    ev.reef = if_else(is.na(landings.reef), NA_real_, rev.reef - landings.reef * costs.all.perlb.reef.adj),
    ev.pelagic = if_else(is.na(landings.pelagic), NA_real_, rev.pelagic - landings.pelagic * costs.all.perlb.pelagic.adj)
  ) %>%
  left_join(sbf_alloc, by = c("year", "area")) %>%
  mutate(
    ev.sbf.REEFET = if_else(is.na(ev.sbf), NA_real_, ev.sbf * sbf.ratio.REEFET),
    ev.sbf.NONREEFET = if_else(is.na(ev.sbf), NA_real_, ev.sbf - ev.sbf.REEFET),

    # Excel SUM() semantics: blanks treated as 0 in the sum
    ev.REEFET = rowSums(cbind(ev.sbf.REEFET, ev.reef), na.rm = TRUE),
    ev.NONREEFET = rowSums(cbind(ev.deep7, ev.sbf.NONREEFET, ev.pelagic), na.rm = TRUE),
    ev.TOTAL = rowSums(cbind(ev.deep7, ev.sbf, ev.reef, ev.pelagic), na.rm = TRUE)
  )

# Ensure exact column set and names to match ev.master.finale.xlsx
comm_ev_out <- comm_ev %>%
  transmute(
    year = year,
    area = area,
    county = county,
    landings.deep7 = landings.deep7,
    mwp.deep7 = mwp.deep7,
    mwp.deep7.adj = mwp.deep7.adj,
    rev.deep7 = rev.deep7,
    costs.other.perlb.deep7 = costs.other.perlb.deep7,
    costs.labor.perlb.deep7 = costs.labor.perlb.deep7,
    costs.all.perlb.deep7.adj = costs.all.perlb.deep7.adj,
    ev.deep7 = ev.deep7,
    landings.sbf = landings.sbf,
    mwp.sbf = mwp.sbf,
    mwp.sbf.adj = mwp.sbf.adj,
    rev.sbf = rev.sbf,
    costs.other.perlb.sbf = costs.other.perlb.sbf,
    costs.labor.perlb.sbf = costs.labor.perlb.sbf,
    costs.all.perlb.sbf.adj = costs.all.perlb.sbf.adj,
    ev.sbf = ev.sbf,
    ev.sbf.REEFET = ev.sbf.REEFET,
    ev.sbf.NONREEFET = ev.sbf.NONREEFET,
    landings.reef = landings.reef,
    mwp.reef = mwp.reef,
    mwp.reef.adj = mwp.reef.adj,
    rev.reef = rev.reef,
    costs.other.perlb.reef = costs.other.perlb.reef,
    costs.labor.perlb.reef = costs.labor.perlb.reef,
    costs.all.perlb.reef.adj = costs.all.perlb.reef.adj,
    ev.reef = ev.reef,
    landings.pelagic = landings.pelagic,
    mwp.pelagic = mwp.pelagic,
    mwp.pelagic.adj = mwp.pelagic.adj,
    rev.pelagic = rev.pelagic,
    costs.other.perlb.pelagic = costs.other.perlb.pelagic,
    costs.labor.perlb.pelagic = costs.labor.perlb.pelagic,
    costs.all.perlb.pelagic.adj = costs.all.perlb.pelagic.adj,
    ev.pelagic = ev.pelagic,
    ev.REEFET = ev.REEFET,
    ev.NONREEFET = ev.NONREEFET,
    ev.TOTAL = ev.TOTAL
  )

noncomm_cpi_factor <- function(year_vec) {
  as.numeric(cpi_base / cpi_lookup[as.character(year_vec)])
}

noncomm_ev <- noncomm_landings %>%
  mutate(
    cpi_factor = noncomm_cpi_factor(year),

    # Non-commercial reef price (nominal) and inflation-adjusted
    reef.price.adj = reef.price * cpi_factor,
    rev.total = lbs.total * reef.price.adj,

    # Reef other costs ($/lb) and adjusted
    cost.other.per.lb = costs_other_map[["reef"]],
    costs.other.perlb.adj = cost.other.per.lb * cpi_factor,

    # Reef labor costs per lb by island (lanai uses maui value in the finale workbook)
    island_for_labor = if_else(island == "lanai", "maui", island),
    costs.labor.perlb = unname(labor_maps$reef[island_for_labor]) %>% round(2),
    costs.labor.perlb.adj = costs.labor.perlb * cpi_factor,

    ev.total = rev.total - lbs.total * (costs.other.perlb.adj + costs.labor.perlb.adj),
    ev.total.REEF = ev.total,
    ev.total.OO = ev.total - ev.total.REEF
  )

noncomm_ev_out <- noncomm_ev %>%
  transmute(
    year = year,
    island = island,
    island.weight = island.weight,
    lbs.total = lbs.total,
    kg.total = kg.total,
    reef.price = reef.price,
    reef.price.adj = reef.price.adj,
    rev.total = rev.total,
    cost.other.per.lb = cost.other.per.lb,
    costs.other.perlb.adj = costs.other.perlb.adj,
    costs.labor.perlb = costs.labor.perlb,
    costs.labor.perlb.adj = costs.labor.perlb.adj,
    ev.total = ev.total,
    ev.total.REEF = ev.total.REEF,
    ev.total.OO = ev.total.OO
  )

# ---- write workbook (match reference exactly: only 2 sheets) ----------------

write_xlsx(
  list(
    "Comm EV" = comm_ev_out,
    "Non-comm EV" = noncomm_ev_out
  ),
  out_path
)

cat("Wrote:", out_path, "\n")