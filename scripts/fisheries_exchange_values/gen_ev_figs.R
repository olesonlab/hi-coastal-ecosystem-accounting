#!/usr/bin/env Rscript
# Figures for Hawaiʻi commercial + non-commercial exchange value (EV).
# Data source: ../data/ev_master_finale.xlsx — Comm EV (incl. ev.TOTAL) and Non-comm EV (incl. ev.total).
# (Not the reproduced CSVs; totals match the published master workbook.)

suppressPackageStartupMessages({
  library(here)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(scales)
})

# ---- paths ----------------------------------------------------------------------------
PATH_EV_MASTER <- here("data/01_raw/fisheries_exchange_values/ev_master_finale_rebuilt.xlsx")
FIG_DIR <- here("outputs/figs/fisheries_exchange_values/_reference/test")

stopifnot(file.exists(PATH_EV_MASTER))

as_year_int <- function(x) {
  suppressWarnings(as.integer(as.numeric(x)))
}

# ---- load published master (same layout as Excel Comm EV / Non-comm EV sheets) ---------
comm_raw <- read_excel(PATH_EV_MASTER, sheet = "Comm EV") %>%
  mutate(
    county = tolower(as.character(county)),
    year = as_year_int(year)
  )

noncomm_raw <- read_excel(PATH_EV_MASTER, sheet = "Non-comm EV") %>%
  mutate(
    island = tolower(as.character(island)),
    year = as_year_int(year)
  )

# ---- wide commercial → long (landings, rev, costs, ev by species × eco type) ---------
build_comm_long <- function(comm) {
  comm %>%
    select(
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
    ) %>%
    rename(
      ev.sbf.OO = ev.sbf.NONREEFET,
      ev.sbf.REEF = ev.sbf.REEFET,
      ev.REEF = ev.REEFET,
      ev.OO = ev.NONREEFET
    ) %>%
    mutate(
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
      ev.deep7.OO = ev.deep7,
      ev.deep7.REEF = 0,
      ev.reef.OO = 0,
      ev.reef.REEF = ev.reef,
      ev.pelagic.OO = ev.pelagic,
      ev.pelagic.REEF = 0
    ) %>%
    select(
      -costs.other.perlb.deep7, -costs.labor.perlb.deep7, -costs.all.perlb.deep7.adj,
      -costs.other.perlb.sbf, -costs.labor.perlb.sbf, -costs.all.perlb.sbf.adj,
      -costs.other.perlb.reef, -costs.labor.perlb.reef, -costs.all.perlb.reef.adj,
      -costs.other.perlb.pelagic, -costs.labor.perlb.pelagic, -costs.all.perlb.pelagic.adj
    ) %>%
    pivot_longer(cols = -c(year, area, county), names_to = "var", values_to = "value") %>%
    mutate(
      var = str_replace(var, "^ev\\.sbf\\.(OO|REEF)$", "ev.sbf.\\1"),
      var = str_replace(var, "^ev\\.(REEF|OO|TOTAL)$", "ev.all.\\1"),
      var = str_replace(var, "^ev\\.deep7\\.(OO|REEF)$", "ev.deep7.\\1"),
      var = str_replace(var, "^ev\\.reef\\.OO$", "ev.reef.OO"),
      var = str_replace(var, "^ev\\.pelagic\\.OO$", "ev.pelagic.OO"),
      parts = str_match(
        var,
        "^(landings|rev|costs\\.other|costs\\.labor|costs\\.all|ev)\\.([a-z0-9]+)\\.?([A-Z]{2,4})?$"
      )
    ) %>%
    transmute(
      year,
      area,
      county,
      var_type = parts[, 2],
      spcs.grp = parts[, 3],
      eco.type = parts[, 4],
      value
    ) %>%
    mutate(
      spcs.grp = recode(
        spcs.grp,
        deep7 = "deep7",
        sbf = "sbf",
        reef = "reef",
        pelagic = "pelagic",
        all = "all"
      ),
      eco.type = if_else(is.na(eco.type), "all", eco.type)
    ) %>%
    pivot_wider(names_from = var_type, values_from = value) %>%
    select(year, area, county, spcs.grp, eco.type, landings, rev, costs.other, costs.labor, costs.all, ev)
}

build_noncomm_long <- function(noncomm) {
  nc <- noncomm %>%
    select(
      year, island, lbs.total, reef.price.adj, rev.total,
      costs.other.perlb.adj, costs.labor.perlb.adj,
      ev.total, ev.total.REEF, ev.total.OO
    ) %>%
    mutate(
      costs.other = costs.other.perlb.adj * lbs.total,
      costs.labor = costs.labor.perlb.adj * lbs.total
    ) %>%
    rename(landings = lbs.total, price = reef.price.adj) %>%
    select(-costs.other.perlb.adj, -costs.labor.perlb.adj)

  ncl <- nc %>%
    select(year, island, landings, price, rev.total, ev.total, ev.total.REEF, ev.total.OO) %>%
    pivot_longer(
      cols = c(ev.total, ev.total.REEF, ev.total.OO),
      names_to = "eco.type",
      values_to = "ev"
    ) %>%
    mutate(
      eco.type = case_when(
        eco.type == "ev.total" ~ "ALL",
        eco.type == "ev.total.REEF" ~ "REEF",
        eco.type == "ev.total.OO" ~ "OO"
      )
    ) %>%
    mutate(spcs.grp = "reef") %>%
    select(year, island, spcs.grp, landings, price, rev.total, eco.type, ev)

  statewide <- ncl %>%
    group_by(year, eco.type) %>%
    summarise(
      landings = sum(landings, na.rm = TRUE),
      price = NA_real_,
      rev.total = sum(rev.total, na.rm = TRUE),
      ev = sum(ev, na.rm = TRUE),
      spcs.grp = "reef",
      .groups = "drop"
    ) %>%
    mutate(island = "all") %>%
    select(year, island, spcs.grp, landings, price, rev.total, eco.type, ev)

  bind_rows(ncl, statewide)
}

comm_long <- build_comm_long(comm_raw)
noncomm_long <- build_noncomm_long(noncomm_raw)

# ---- label helpers -------------------------------------------------------------------
county_lab <- function(x) {
  recode(
    x,
    hawaii = "Hawaii",
    kauai = "Kauai",
    maui = "Maui",
    oahu = "Oahu",
    all = "All"
  )
}

spcs_lab <- function(x) {
  case_when(
    x == "deep7" ~ "Deep 7 Bottomfish",
    x == "sbf" ~ "Shallow Bottomfish",
    x == "reef" ~ "Reef-Associated",
    x == "pelagic" ~ "Pelagic",
    x == "all" ~ "All",
    TRUE ~ x
  )
}

theme_ev <- function() {
  theme_bw(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
      plot.subtitle = element_text(hjust = 0.5, size = 14),
      axis.title = element_text(face = "bold"),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank()
    )
}

save_plot <- function(plot, filename, width = 8, height = 5, dpi = 300) {
  path <- file.path(FIG_DIR, filename)
  ggsave(path, plot = plot, width = width, height = height, dpi = dpi, limitsize = FALSE)
  message("Wrote ", path)
}

# ---- 1. Commercial EV by county -----------------------------------------------------
yr_comm <- range(comm_long$year, na.rm = TRUE)
comm_county <- comm_long %>%
  filter(eco.type == "all") %>%
  group_by(year, county) %>%
  summarise(ev = sum(ev, na.rm = TRUE), .groups = "drop") %>%
  bind_rows(
    comm_long %>%
      filter(eco.type == "all") %>%
      group_by(year) %>%
      summarise(ev = sum(ev, na.rm = TRUE), .groups = "drop") %>%
      mutate(county = "all")
  ) %>%
  mutate(county = county_lab(county))

p_comm_county <- ggplot(comm_county, aes(year, ev, color = county)) +
  geom_point(size = 0.8) +
  geom_line(linewidth = 0.7) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(
    values = c(
      Hawaii = "firebrick4", Kauai = "slateblue3", Maui = "deeppink3",
      Oahu = "goldenrod3", All = "grey40"
    )
  ) +
  labs(
    title = "Commercial exchange value by county",
    subtitle = paste0(yr_comm[1], "–", yr_comm[2]),
    x = "Year",
    y = "Exchange value ($)",
    color = "County"
  ) +
  theme_ev() +
  theme(legend.position = "right", legend.title = element_text(face = "bold"))

save_plot(p_comm_county, "comm_county.png")

# ---- 2. Commercial EV by species group ------------------------------------------------
comm_spcs <- comm_long %>%
  filter(eco.type == "all", spcs.grp %in% c("deep7", "sbf", "reef", "pelagic", "all")) %>%
  group_by(year, spcs.grp) %>%
  summarise(ev = sum(ev, na.rm = TRUE), .groups = "drop") %>%
  bind_rows(
    comm_long %>%
      filter(eco.type == "all", spcs.grp %in% c("deep7", "sbf", "reef", "pelagic", "all")) %>%
      group_by(year) %>%
      summarise(ev = sum(ev, na.rm = TRUE), .groups = "drop") %>%
      mutate(spcs.grp = "all")
  ) %>%
  mutate(spcs.grp = spcs_lab(spcs.grp))

p_comm_spcs <- ggplot(comm_spcs, aes(year, ev, color = spcs.grp)) +
  geom_point(size = 0.8) +
  geom_line(linewidth = 0.7) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(
    values = c(
      "Deep 7 Bottomfish" = "indianred3",
      "Shallow Bottomfish" = "peru",
      "Reef-Associated" = "aquamarine4",
      "Pelagic" = "steelblue4",
      "All" = "grey40"
    )
  ) +
  labs(
    title = "Commercial exchange value by species group",
    subtitle = paste0(yr_comm[1], "–", yr_comm[2]),
    x = "Year",
    y = "Exchange value ($)",
    color = "Group"
  ) +
  theme_ev() +
  theme(legend.position = "right", legend.title = element_text(face = "bold"))

save_plot(p_comm_spcs, "comm_spcs.png")

# ---- 3. Non-commercial EV by island -------------------------------------------------
yr_nc <- range(noncomm_long$year, na.rm = TRUE)
noncomm_county <- noncomm_long %>%
  group_by(year, island) %>%
  summarise(ev = sum(ev, na.rm = TRUE), .groups = "drop") %>%
  mutate(county = county_lab(island)) %>%
  select(-island)

p_noncomm_county <- ggplot(noncomm_county, aes(year, ev, color = county)) +
  geom_point(size = 0.8) +
  geom_line(linewidth = 0.7) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(
    values = c(
      Hawaii = "firebrick4", Kauai = "slateblue3", Maui = "deeppink3",
      Oahu = "goldenrod3", All = "grey40"
    )
  ) +
  labs(
    title = "Non-commercial exchange value by area",
    subtitle = paste0("Reef herbivores, ", yr_nc[1], "–", yr_nc[2]),
    x = "Year",
    y = "Exchange value ($)",
    color = "Area"
  ) +
  theme_ev() +
  theme(legend.position = "right", legend.title = element_text(face = "bold"))

save_plot(p_noncomm_county, "noncomm_county.png")

# ---- 4. Commercial vs non-commercial (totals) ---------------------------------------
noncomm_ev_total <- noncomm_long %>%
  filter(eco.type == "ALL") %>%
  group_by(year) %>%
  summarise(ev.noncomm = sum(ev, na.rm = TRUE), .groups = "drop")

ev_both <- comm_long %>%
  filter(eco.type == "all") %>%
  group_by(year) %>%
  summarise(ev.comm = sum(ev, na.rm = TRUE), .groups = "drop") %>%
  left_join(noncomm_ev_total, by = "year") %>%
  pivot_longer(c(ev.comm, ev.noncomm), names_to = "comm", values_to = "ev") %>%
  mutate(comm = if_else(comm == "ev.comm", "comm", "noncomm")) %>%
  filter(is.finite(ev))

x_off <- 0.5
y_rng <- max(ev_both$ev, na.rm = TRUE) - min(ev_both$ev, na.rm = TRUE)
y_off <- 0.01 * y_rng

ev_labels <- ev_both %>%
  group_by(comm) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  mutate(
    label = if_else(comm == "comm", "Commercial", "Non-commercial"),
    year = year + x_off,
    ev = ev + y_off
  )

yr_both <- c(min(ev_both$year, na.rm = TRUE), max(c(ev_both$year, noncomm_raw$year), na.rm = TRUE))

p_ev_both <- ggplot(ev_both, aes(year, ev, color = comm)) +
  geom_point(size = 0.8) +
  geom_line(linewidth = 0.7) +
  geom_text(
    data = ev_labels,
    aes(label = label),
    hjust = 0,
    size = 4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
  scale_color_manual(values = c(comm = "goldenrod3", noncomm = "steelblue4")) +
  labs(
    title = "Total EV: commercial vs. non-commercial",
    subtitle = paste0(min(yr_both), "–", max(yr_both)),
    x = "Year",
    y = "Exchange value ($)"
  ) +
  theme_ev() +
  theme(legend.position = "none")

save_plot(p_ev_both, "ev_both_total.png")

# ---- 5. Commercial vs non-commercial, reef only --------------------------------------
ev_both_reef <- comm_long %>%
  filter(eco.type == "all", spcs.grp == "reef") %>%
  group_by(year) %>%
  summarise(ev.comm = sum(ev, na.rm = TRUE), .groups = "drop") %>%
  left_join(noncomm_ev_total, by = "year") %>%
  pivot_longer(c(ev.comm, ev.noncomm), names_to = "comm", values_to = "ev") %>%
  mutate(comm = if_else(comm == "ev.comm", "comm", "noncomm")) %>%
  filter(is.finite(ev))

y_off_r <- 0.01 * (max(ev_both_reef$ev, na.rm = TRUE) - min(ev_both_reef$ev, na.rm = TRUE))
ev_labels_reef <- ev_both_reef %>%
  group_by(comm) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  mutate(
    label = if_else(comm == "comm", "Commercial", "Non-commercial"),
    year = year + x_off,
    ev = ev + y_off_r
  )

p_ev_both_reef <- ggplot(ev_both_reef, aes(year, ev, color = comm)) +
  geom_point(shape = 17, size = 2) +
  geom_line(linewidth = 1) +
  geom_text(
    data = ev_labels_reef,
    aes(label = label),
    hjust = 0,
    size = 4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_color_manual(values = c(comm = "goldenrod3", noncomm = "steelblue4")) +
  labs(
    title = "Total EV: commercial vs. non-commercial (reef-associated)",
    subtitle = paste0(min(yr_both), "–", max(yr_both)),
    x = "Year",
    y = "Exchange value ($)"
  ) +
  theme_ev() +
  theme(legend.position = "none", plot.margin = margin(t = 5, r = 40, b = 5, l = 5))

save_plot(p_ev_both_reef, "ev_both_reef.png")

# ---- 6. Commercial stacked components (revenue, costs, EV) ---------------------------
comm_wide <- comm_raw %>%
  mutate(
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
    costs.all.pelagic = costs.all.perlb.pelagic.adj * landings.pelagic
  )

comm_effects <- comm_wide %>%
  group_by(year) %>%
  summarise(
    revenue = sum(rev.deep7 + rev.sbf + rev.reef + rev.pelagic, na.rm = TRUE),
    costs.other = sum(
      costs.other.deep7 + costs.other.sbf + costs.other.reef + costs.other.pelagic,
      na.rm = TRUE
    ),
    costs.labor = sum(
      costs.labor.deep7 + costs.labor.sbf + costs.labor.reef + costs.labor.pelagic,
      na.rm = TRUE
    ),
    costs.all = sum(
      costs.all.deep7 + costs.all.sbf + costs.all.reef + costs.all.pelagic,
      na.rm = TRUE
    ),
    ev = sum(ev.deep7 + ev.sbf + ev.reef + ev.pelagic, na.rm = TRUE),
    .groups = "drop"
  )

comm_effects_long <- comm_effects %>%
  pivot_longer(
    c(revenue, costs.other, costs.labor, costs.all, ev),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(
    variable = recode(
      variable,
      revenue = "Revenue",
      costs.labor = "Labor costs",
      costs.other = "Other costs",
      costs.all = "Total costs",
      ev = "Exchange value"
    )
  )

p_comm_effects <- ggplot(comm_effects_long, aes(year, value, fill = variable)) +
  geom_col() +
  scale_y_continuous(labels = comma) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Commercial EV components (stacked)",
    subtitle = paste0(yr_comm[1], "–", yr_comm[2]),
    x = "Year",
    y = "Value ($)",
    fill = NULL
  ) +
  theme_ev() +
  theme(legend.position = "right")

save_plot(p_comm_effects, "comm_effects_stacked.png", width = 10, height = 5)

# ---- 7. Commercial elements → cost share of revenue (statewide all groups) ----------
options(scipen = 999)

comm_elements <- comm_long %>%
  filter(eco.type == "all") %>%
  mutate(costs.total = costs.other + costs.labor) %>%
  select(year, county, spcs.grp, rev, costs.labor, costs.other, costs.total, ev) %>%
  drop_na()

meas <- c("rev", "costs.labor", "costs.other", "costs.total", "ev")

county_tot <- comm_elements %>%
  group_by(year, spcs.grp) %>%
  summarise(across(all_of(meas), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(county = "all") %>%
  relocate(year, county, spcs.grp)

spcs_tot <- comm_elements %>%
  group_by(year, county) %>%
  summarise(across(all_of(meas), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(spcs.grp = "all") %>%
  relocate(year, county, spcs.grp)

grand_tot <- comm_elements %>%
  group_by(year) %>%
  summarise(across(all_of(meas), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(county = "all", spcs.grp = "all") %>%
  relocate(year, county, spcs.grp)

comm_elements_all <- bind_rows(comm_elements, county_tot, spcs_tot, grand_tot) %>%
  group_by(year) %>%
  complete(
    county = union(unique(comm_elements$county), "all"),
    spcs.grp = union(unique(comm_elements$spcs.grp), "all")
  ) %>%
  ungroup() %>%
  mutate(across(all_of(meas), ~ replace_na(.x, 0))) %>%
  arrange(year, county, spcs.grp)

comm_elements_labeled <- comm_elements_all %>%
  mutate(spcs.grp = spcs_lab(spcs.grp))

p_comm_elements <- ggplot(
  comm_elements_labeled,
  aes(year, ev, color = spcs.grp)
) +
  geom_point(size = 0.8) +
  geom_line(linewidth = 0.7) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(
    values = c(
      "Deep 7 Bottomfish" = "indianred3",
      "Shallow Bottomfish" = "peru",
      "Reef-Associated" = "aquamarine4",
      "Pelagic" = "steelblue4",
      "All" = "grey40"
    )
  ) +
  labs(
    title = "Commercial exchange value by species group",
    subtitle = paste0(yr_comm[1], "–", yr_comm[2]),
    x = "Year",
    y = "Exchange value ($)",
    color = "Group"
  ) +
  theme_ev() +
  theme(legend.position = "right", legend.title = element_text(face = "bold"))

save_plot(p_comm_elements, "comm_elements_by_spcs.png", width = 8, height = 5)

plotdf_comm_econ <- comm_elements_all %>%
  filter(county == "all", spcs.grp == "all") %>%
  transmute(
    year,
    rev,
    costs = costs.total,
    remainder = pmax(rev - costs.total, 0),
    cost_share = if_else(rev > 0, costs.total / rev, NA_real_)
  ) %>%
  pivot_longer(c(costs, remainder), names_to = "segment", values_to = "value") %>%
  mutate(
    segment = factor(segment, levels = c("remainder", "costs")),
    segment = recode(segment, costs = "Costs", remainder = "EV")
  )

p_comm_econ <- ggplot(plotdf_comm_econ, aes(year, value, fill = segment)) +
  geom_col() +
  geom_text(
    data = subset(plotdf_comm_econ, segment == "Costs"),
    aes(label = percent(cost_share, accuracy = 1), y = value / 2),
    size = 3.5,
    color = "white"
  ) +
  scale_fill_manual(values = c(Costs = "steelblue4", EV = "goldenrod3")) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Commercial cost share of revenue",
    subtitle = "All counties, all species groups",
    x = "Year",
    y = "Dollars",
    fill = NULL
  ) +
  theme_ev() +
  theme(legend.position = "bottom")

save_plot(p_comm_econ, "comm_cost_share_revenue.png", width = 10, height = 5)

# ---- 8. Commercial revenue vs cost lines ----------------------------------------------
plotdf_comm_rc <- comm_elements_all %>%
  filter(county == "all", spcs.grp == "all") %>%
  select(year, rev, costs.labor, costs.other, ev) %>%
  pivot_longer(-year, names_to = "variable", values_to = "value") %>%
  mutate(
    variable = recode(
      variable,
      rev = "Revenue",
      costs.labor = "Labor costs",
      costs.other = "Other costs",
      ev = "Exchange value"
    )
  )

p_comm_rev_costs <- ggplot(plotdf_comm_rc, aes(year, value, color = variable)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Commercial: revenue and costs",
    x = "Year",
    y = "Dollars",
    color = NULL
  ) +
  theme_ev() +
  theme(legend.position = "bottom")

save_plot(p_comm_rev_costs, "comm_revenue_costs_lines.png", width = 10, height = 5)

# ---- 9. Non-commercial cost share and lines -------------------------------------------
noncomm_elements <- noncomm_raw %>%
  select(year, island, lbs.total, rev.total, costs.other.perlb.adj, costs.labor.perlb.adj, ev.total) %>%
  mutate(
    costs.other = lbs.total * costs.other.perlb.adj,
    costs.labor = lbs.total * costs.labor.perlb.adj,
    costs.total = costs.other + costs.labor
  ) %>%
  select(-costs.other.perlb.adj, -costs.labor.perlb.adj, -lbs.total)

meas_nc <- c("rev.total", "ev.total", "costs.other", "costs.labor", "costs.total")

noncomm_with_all <- noncomm_elements %>%
  group_by(year) %>%
  summarise(across(all_of(meas_nc), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(island = "all") %>%
  relocate(year, island) %>%
  bind_rows(noncomm_elements) %>%
  arrange(year, island)

plotdf_nc_econ <- noncomm_with_all %>%
  filter(island == "all") %>%
  transmute(
    year,
    rev = rev.total,
    costs = costs.total,
    remainder = pmax(rev.total - costs.total, 0),
    cost_share = if_else(rev.total > 0, costs.total / rev.total, NA_real_)
  ) %>%
  pivot_longer(c(costs, remainder), names_to = "segment", values_to = "value") %>%
  mutate(segment = recode(segment, costs = "Costs", remainder = "EV"))

p_noncomm_econ <- ggplot(plotdf_nc_econ, aes(year, value, fill = segment)) +
  geom_col() +
  geom_text(
    data = subset(plotdf_nc_econ, segment == "Costs"),
    aes(label = percent(cost_share, accuracy = 1), y = value / 2),
    size = 3.5,
    color = "white"
  ) +
  scale_fill_manual(values = c(Costs = "steelblue4", EV = "goldenrod3")) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Non-commercial cost share of revenue",
    subtitle = paste0("Statewide total, ", yr_nc[1], "–", yr_nc[2]),
    x = "Year",
    y = "Dollars",
    fill = NULL
  ) +
  theme_ev() +
  theme(legend.position = "bottom")

save_plot(p_noncomm_econ, "noncomm_cost_share_revenue.png", width = 10, height = 5)

plotdf_nc_lines <- noncomm_with_all %>%
  filter(island == "all") %>%
  select(year, rev.total, costs.other, costs.labor) %>%
  pivot_longer(-year, names_to = "variable", values_to = "value") %>%
  mutate(
    variable = recode(
      variable,
      rev.total = "Revenue",
      costs.other = "Other costs",
      costs.labor = "Labor costs"
    )
  )

p_noncomm_lines <- ggplot(plotdf_nc_lines, aes(year, value, color = variable)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Non-commercial: revenue and costs",
    x = "Year",
    y = "Dollars",
    color = NULL
  ) +
  theme_ev() +
  theme(legend.position = "bottom")

save_plot(p_noncomm_lines, "noncomm_revenue_costs_lines.png", width = 10, height = 5)

message("Done. Figures saved in ", FIG_DIR)
