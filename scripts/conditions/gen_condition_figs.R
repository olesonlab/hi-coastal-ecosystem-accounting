# Generate Ecosystem Condition Figures (standalone runner)
#
# This script is a standalone runner for the condition figure generation logic
# implemented in R/prep_conditions.R.
#
# In the targets pipeline, figures are generated automatically via:
#   tar_target(conditions_figs, generate_condition_figs(...))
#
# Run this script manually to regenerate figures outside the pipeline:
#   Rscript scripts/conditions/gen_condition_figs.R

library(tidyverse)
library(here)

# Source the pipeline helper functions
source(here("R/prep_conditions.R"))

# Define input paths (mirrors paths_in in _targets.R)
paths_in <- list(
  conditions_abiotic_kd490    = here("data/01_raw/conditions/prepared/abiotic/kd490_131619.csv"),
  conditions_abiotic_par_sst  = here("data/01_raw/conditions/prepared/abiotic/PAR_SST_131619.csv"),
  conditions_biotic_piscivore  = here("data/01_raw/conditions/prepared/biotic/PISCIVORE_20132019.csv"),
  conditions_biotic_planktivore = here("data/01_raw/conditions/prepared/biotic/PLANKTIVORE_20132019.csv"),
  conditions_biotic_primary    = here("data/01_raw/conditions/prepared/biotic/PRIMARY_20132019.csv"),
  conditions_biotic_coral_cover = here("data/01_raw/conditions/prepared/biotic/CORAL_20132019.csv"),
  conditions_biotic_coral_div  = here("data/01_raw/conditions/prepared/biotic/COV_Dq1_20132019.csv"),
  conditions_biotic_adult_den  = here("data/01_raw/conditions/prepared/biotic/AdColDen_20132019.csv"),
  conditions_biotic_juv_den    = here("data/01_raw/conditions/prepared/biotic/JuvColDen_20132019.csv"),
  conditions_biotic_disease    = here("data/01_raw/conditions/prepared/biotic/TotDZ_prev_20132019.csv")
)

outdir <- here("outputs/figs/conditions/")

# Load data
marine_abiotic <- load_conditions_marine_abiotic(paths_in)
marine_biotic  <- load_conditions_marine_biotic(paths_in)

# Generate and save figures
output_paths <- generate_condition_figs(marine_abiotic, marine_biotic, outdir = outdir)

message("Generated ", length(output_paths), " condition figures:")
message(paste(" -", output_paths, collapse = "\n"))

