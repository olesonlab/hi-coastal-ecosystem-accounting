# Hawaiʻi Coastal Ecosystem Accounting — Data Preprocessing Pipeline
# 
# Run with: targets::tar_make()
# Visualize: targets::tar_visnetwork()
# 
# Pipeline structure:
#   1. Moku layer (shared unit of analysis)
#   2. Extents data → joins to mokus → exports GeoPackage + CSV
#   3. Conditions data → joins to mokus → exports GeoPackage + CSV + figures
#   4. Fisheries data → loads pre-processed CSVs + tracks existing figures
#   5. Pipeline report → renders Quarto HTML via tarchetypes::tar_quarto()
#   Recreation: out of scope (handled by separate researcher workflow)

library(targets)
library(tarchetypes)

# Source all helper functions from R/
tar_source("R/")

# -----------------------------------------------------------------------------
# Target Options
# -----------------------------------------------------------------------------

tar_option_set(
  packages = c(
    "fs", "here", "dplyr", "tidyr", "readr", "readxl",
    "janitor", "stringr", "stringi", "glue", "sf",
    "purrr", "ggplot2", "quarto", "fs",
    "cowplot", "terra", "tibble", "ggspatial", "rmapshaper", "rnaturalearth", "grid", "scales",
    "svglite", "writexl"
  ),
  format = "rds",
  error = "continue"
)

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

config <- list(
  crs_target = 2784L,
  crs_web = 4326L,
  years = c(2013L, 2016L, 2019L)
)

# Input paths
paths_in <- list(
  # Moku shapefiles
  mokus_marine = "data/01_raw/spatial_units/mokus/mokus_marine_hi_himarc/mokus_marine_hi_himarc.shp",
  mokus_terrestrial = "data/01_raw/spatial_units/mokus/mokus_terrestrial_hi_himarc/mokus_terrestrial_hi_himarc.shp",

  # Extents CSVs
  extents_marine = "data/01_raw/extents/prepared/et_areas_per_moku_marine_hi.csv",
  extents_terrestrial = "data/01_raw/extents/prepared/et_areas_per_moku_terrestrial_hi.csv",

  # Conditions CSVs (marine abiotic)
  conditions_abiotic_kd490   = "data/01_raw/conditions/prepared/abiotic/kd490_131619.csv",
  conditions_abiotic_par_sst = "data/01_raw/conditions/prepared/abiotic/PAR_SST_131619.csv",

  # Conditions CSVs (marine biotic)
  conditions_biotic_piscivore   = "data/01_raw/conditions/prepared/biotic/PISCIVORE_20132019.csv",
  conditions_biotic_planktivore = "data/01_raw/conditions/prepared/biotic/PLANKTIVORE_20132019.csv",
  conditions_biotic_primary     = "data/01_raw/conditions/prepared/biotic/PRIMARY_20132019.csv",
  conditions_biotic_coral_cover = "data/01_raw/conditions/prepared/biotic/CORAL_20132019.csv",
  conditions_biotic_coral_div   = "data/01_raw/conditions/prepared/biotic/COV_Dq1_20132019.csv",
  conditions_biotic_adult_den   = "data/01_raw/conditions/prepared/biotic/AdColDen_20132019.csv",
  conditions_biotic_juv_den     = "data/01_raw/conditions/prepared/biotic/JuvColDen_20132019.csv",
  conditions_biotic_disease     = "data/01_raw/conditions/prepared/biotic/TotDZ_prev_20132019.csv",

  # Conditions CSVs (terrestrial)
  conditions_terr_ndvi     = "data/01_raw/conditions/prepared/NDVI/treecover_ndvi_1319.csv",
  conditions_terr_rainfall = "data/01_raw/conditions/prepared/Rainfall/tree_rainfall_2013_19.csv",
  conditions_terr_temp     = "data/01_raw/conditions/prepared/Temp/treecover_1319_temp.csv",
  conditions_terr_burnt    = "data/01_raw/conditions/prepared/Burnt Area/tree_cover_burnt.csv",
  conditions_wetl_rainfall = "data/01_raw/conditions/prepared/wetl_rainfall_2013_19.csv",
  otp_dir = "data/01_raw/conditions/source/otp",
  himarc_benthic = "data/01_raw/extents/source/Nearshore Marine (Benthic)/Benthic_Habitat_5Classes_2022_10.tif",

  # Fisheries spatial units
  fish_catch_areas = "data/01_raw/spatial_units/dar_fish_catch_areas/dar_fish_catch_areas_2008.shp",

  # Fisheries raw inputs
  fisheries_comm_landings = "data/01_raw/fisheries_exchange_values/commercial_landings_V2.xlsx",
  fisheries_noncomm       = "data/01_raw/fisheries_exchange_values/noncomm_EV_2005_22.xlsx",
  fisheries_prices        = "data/01_raw/fisheries_exchange_values/pricesMasterOGYEARS.xlsx",
  fisheries_costs         = "data/01_raw/fisheries_exchange_values/costs_other_perlb.csv",
  fisheries_wages         = "data/01_raw/fisheries_exchange_values/county_wages.csv",
  fisheries_trace         = "data/01_raw/fisheries_exchange_values/trace_raw.xlsx"
)

# Output paths
paths_out <- list(
  # Interim outputs (shared resources)
  moku_names_csv = "data/02_interim/moku_names_lut.csv",
  mokus_gpkg = "data/02_interim/mokus_combined.gpkg",
  
  # Processed outputs (dashboard-ready, by scope)
  extents_gpkg    = "data/03_processed/extents/mokus_extents.gpkg",
  extents_csv     = "data/03_processed/extents/mokus_extents.csv",
  conditions_gpkg       = "data/03_processed/conditions/mokus_conditions.gpkg",
  conditions_csv        = "data/03_processed/conditions/mokus_conditions.csv",

  # Fisheries processed outputs (stable names, no date prefix)
  fisheries_master      = "data/02_interim/fisheries_exchange_values/ev_master_finale_rebuilt.xlsx",
  fisheries_comm_csv    = "data/03_processed/fisheries_exchange_values/comm_ev.csv",
  fisheries_noncomm_csv = "data/03_processed/fisheries_exchange_values/noncomm_ev.csv",
  otp_moku_csv = "data/01_raw/conditions/prepared/otp_moku_zonal_mean.csv"
)

# =============================================================================
# Pipeline Definition
# =============================================================================

list(
  # ---------------------------------------------------------------------------
  # MOKU LAYER (shared across all scopes)
  # ---------------------------------------------------------------------------
  
  # Load raw shapefiles
  tar_target(
    mokus_marine_raw,
    read_sf_clean(paths_in$mokus_marine)
  ),
  
  tar_target(
    mokus_terrestrial_raw,
    read_sf_clean(paths_in$mokus_terrestrial)
  ),
  
  # Build name lookup table (from marine data which has ʻōlelo names)
  tar_target(
    moku_names_lut,
    build_moku_names_lut(mokus_marine_raw)
  ),
  
  # Export name lookup for reference
  tar_target(
    export_moku_names,
    export_csv(moku_names_lut, paths_out$moku_names_csv),
    format = "file"
  ),
  
  # Clean and transform mokus
  tar_target(
    mokus_marine,
    clean_mokus(mokus_marine_raw, moku_names_lut, "Marine", config$crs_web)
  ),
  
  tar_target(
    mokus_terrestrial,
    clean_mokus(mokus_terrestrial_raw, moku_names_lut, "Terrestrial", config$crs_web)
  ),
  
  # Combine into single layer
  tar_target(
    mokus_combined,
    combine_mokus(mokus_marine, mokus_terrestrial)
  ),
  
  # Export combined moku layer
  tar_target(
    export_mokus,
    export_gpkg(mokus_combined, paths_out$mokus_gpkg),
    format = "file"
  ),
  
  # ---------------------------------------------------------------------------
  # EXTENTS SCOPE
  # ---------------------------------------------------------------------------
  
  # Load extent area data
  tar_target(
    et_areas_marine,
    load_et_areas_marine(paths_in$extents_marine, config$years)
  ),
  
  tar_target(
    et_areas_terrestrial,
    load_et_areas_terrestrial(paths_in$extents_terrestrial)
  ),
  
  # Combine marine and terrestrial
  tar_target(
    et_areas_combined,
    combine_et_areas(et_areas_marine, et_areas_terrestrial)
  ),
  
  # Join to moku geometries
  tar_target(
    mokus_extents,
    join_mokus_extents(mokus_combined, et_areas_combined)
  ),
  
  # Export extents (GeoPackage for spatial, CSV for tabular)
  tar_target(
    export_extents_gpkg,
    export_gpkg(mokus_extents, paths_out$extents_gpkg),
    format = "file"
  ),
  
  tar_target(
    export_extents_csv,
    export_sf_as_csv(mokus_extents, paths_out$extents_csv),
    format = "file"
  ),

  # ---------------------------------------------------------------------------
  # CONDITIONS SCOPE
  # ---------------------------------------------------------------------------

  tar_target(
    conditions_marine_abiotic,
    load_conditions_marine_abiotic(paths_in)
  ),

  tar_target(
    conditions_marine_biotic,
    load_conditions_marine_biotic(paths_in)
  ),

  tar_target(
    conditions_terrestrial,
    load_conditions_terrestrial(paths_in)
  ),

  tar_target(
    conditions_combined,
    combine_conditions(conditions_marine_abiotic, conditions_marine_biotic, conditions_terrestrial)
  ),

  tar_target(
    mokus_conditions,
    join_mokus_conditions(mokus_combined, conditions_combined)
  ),

  tar_target(
    export_conditions_gpkg,
    export_gpkg(mokus_conditions, paths_out$conditions_gpkg),
    format = "file"
  ),

  tar_target(
    export_conditions_csv,
    export_sf_as_csv(mokus_conditions, paths_out$conditions_csv),
    format = "file"
  ),

  tar_target(
    export_otp_moku_zonal,
    export_otp_moku_zonal_stats(
      mokus_marine,
      paths_in$otp_dir,
      paths_out$otp_moku_csv
    ),
    format = "file"
  ),

  tar_target(
    extents_spatial_map,
    generate_extents_spatial_map(
      mokus_combined,
      outdir = "outputs/figs/extents"
    ),
    format = "file"
  ),

  tar_target(
    extents_figs,
    generate_extent_figs(
      mokus_combined,
      et_areas_terrestrial,
      mokus_marine,
      paths_in$himarc_benthic,
      outdir = "outputs/figs/extents"
    ),
    format = "file"
  ),

  # Condition figures: generate_all_condition_figs() in R/prep_conditions.R
  tar_target(
    conditions_figs,
    {
      otp_fp <- export_otp_moku_zonal
      generate_all_condition_figs(
        paths_in = paths_in,
        mokus_combined = mokus_combined,
        mokus_marine = mokus_marine,
        marine_abiotic = conditions_marine_abiotic,
        marine_biotic = conditions_marine_biotic,
        otp_moku_csv = otp_fp,
        outdir = "outputs/figs/conditions/"
      )
    },
    format = "file"
  ),

  # ---------------------------------------------------------------------------
  # FISHERIES SCOPE
  # Reproduces Ela Ural's workflow from scripts/fisheries_exchange_values/.
  # Reference outputs (for validation) are in:
  #   data/03_processed/fisheries_exchange_values/_reference/
  #   outputs/figs/fisheries_exchange_values/_reference/
  # ---------------------------------------------------------------------------

  # Load all raw inputs (commercial landings, prices, costs, wages, CPI, etc.)
  tar_target(
    fisheries_raw,
    load_raw_fisheries(paths_in),
    format = "rds"
  ),

  # Compute commercial exchange values (refactored run_ev_workflow.R)
  tar_target(
    fisheries_comm_ev,
    compute_comm_ev(fisheries_raw)
  ),

  # Compute non-commercial exchange values (refactored run_ev_workflow.R)
  tar_target(
    fisheries_noncomm_ev,
    compute_noncomm_ev(fisheries_raw)
  ),

  # Export EV tables as stable-named CSVs
  tar_target(
    export_fisheries_csv,
    export_fisheries_ev(fisheries_comm_ev, fisheries_noncomm_ev, paths_out),
    format = "file"
  ),

  # Rebuild ev_master_finale.xlsx from trace_raw.xlsx (Ela's authoritative workbook)
  tar_target(
    fisheries_ev_master,
    rebuild_ev_master_finale(
      trace_path = paths_in$fisheries_trace,
      out_path   = paths_out$fisheries_master
    ),
    format = "file"
  ),

  # Generate all 11 figures (refactored gen_ev_figs.R)
  tar_target(
    fisheries_figs,
    generate_fisheries_ev_figs(
      fisheries_comm_ev,
      fisheries_noncomm_ev,
      outdir = "outputs/figs/fisheries_exchange_values",
      master_xlsx = fisheries_ev_master
    ),
    format = "file"
  ),

  # Generate spatial units map (DAR catch areas vs island/county zones)
  tar_target(
    fisheries_spatial_maps,
    {
      export_mokus  # ensure mokus GeoPackage exists before reading it
      generate_fisheries_spatial_maps(
        fish_catch_areas_fp = paths_in$fish_catch_areas,
        mokus_fp            = paths_out$mokus_gpkg,
        outdir              = "outputs/figs/fisheries_exchange_values"
      )
    },
    format = "file"
  ),

  # Generate EV change map (Figure 23: 2014 vs 2021 by DAR catch area)
  # Uses authoritative "Comm EV" sheet from ev_master_finale.xlsx
  tar_target(
    fisheries_ev_change_maps,
    {
      export_mokus  # ensure mokus GeoPackage exists before reading it
      generate_ev_change_maps(
        master_xlsx         = fisheries_ev_master,
        fish_catch_areas_fp = paths_in$fish_catch_areas,
        mokus_fp            = paths_out$mokus_gpkg,
        outdir              = "outputs/figs/fisheries_exchange_values"
      )
    },
    format = "file"
  ),

  # ---------------------------------------------------------------------------
  # QUARTO WEBSITE
  # Renders website/ after fisheries figures and CSVs are ready.
  # QMD source files are tracked so edits automatically invalidate this target.
  # ---------------------------------------------------------------------------

  tar_target(qmd_fisheries,    "website/fisheries.qmd",          format = "file"),
  tar_target(qmd_index,        "website/index.qmd",              format = "file"),
  tar_target(qmd_extents_cond, "website/extents_conditions.qmd", format = "file"),

  tar_target(
    website,
    {
      list(
        qmd_fisheries,
        qmd_index,
        qmd_extents_cond,
        fisheries_figs,
        fisheries_spatial_maps,
        fisheries_ev_change_maps,
        export_fisheries_csv,
        extents_spatial_map,
        extents_figs,
        conditions_figs,
        export_otp_moku_zonal
      )
      quarto::quarto_render(input = "website")
      list.files("website/_site", pattern = "\\.html$", full.names = TRUE)
    },
    format = "file"
  ),

  # ---------------------------------------------------------------------------
  # PIPELINE REPORT
  # Depends on all data exports and figures being complete.
  # Uses quarto::quarto_render() instead of tarchetypes::tar_quarto()
  # because tarchetypes::tar_quarto() can fail to find quarto.exe on Windows.
  # ---------------------------------------------------------------------------

  tar_target(
    pipeline_report,
    {
      # Declare dependencies so targets rebuilds when data changes
      list(
        export_extents_csv, export_extents_gpkg,
        export_conditions_csv, export_conditions_gpkg,
        export_otp_moku_zonal,
        extents_figs, conditions_figs, fisheries_figs, export_fisheries_csv
      )
      quarto::quarto_render(input = "notebooks/pipeline_report.qmd")
      "notebooks/pipeline_report.html"
    },
    format = "file"
  )
)