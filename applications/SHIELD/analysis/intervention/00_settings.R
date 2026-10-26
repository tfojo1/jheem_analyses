# ****************************************************************************************************
# 00 SETTINGS ----
# ****************************************************************************************************
# Shared by every numbered script in this folder. Each one sources this file first, so the
# calibration, interventions and years are set in ONE place.
#
# To switch to a new calibration: edit CALIB.NAME and N.SIM below, then re-run from step 01.
#
# RUN ORDER
#   Run from the jheem_analyses folder: every source() path is relative to it.
#
#   00_settings.R                      sourced by the scripts below; never run on its own
#   01_generate_raw_results_arrays.R   simsets -> raw arrays (total, sex; optionally race, age).
#                                      Takes hours: run it from Terminal with nohup (see its header)
#   02_generate_custom_outcomes.R      raw arrays -> custom outcomes: cases averted, rates,
#                                      proportions (total_calc / sex_calc arrays)
#   02b_compare_results.R              optional: compare the new arrays with an older run
#   03_manuscript_outputs.R            every manuscript table and figure
#   04_table_efficiency.R              doxycycline efficiency tables (by MSA and pooled)
#   05_croi26.R                        optional: CROI abstract figure and table
#
# HELPERS (sourced by the scripts above; never run on their own)
#   intervention_helper_functions.R   table builders, plot_* figures, colour palette
#   int_simset_helper_functions.R     loading and plotting the raw intervention simsets
#
# OPTIONAL QC
#   qc_intervention_plots.R   trajectories read straight from the simsets. Does not need the arrays.
#
# Old and exploratory scripts are in archive/. Nothing above sources them.
#
# WHERE THINGS ARE WRITTEN
#   <root>/shield/outputs/<CALIB.NAME>/            the four result arrays
#   <root>/shield/outputs/<CALIB.NAME>/extracts/   step 01's one-file-per-simset extracts
#   <root>/shield/outputs/<CALIB.NAME>/figures/    png
#   <root>/shield/outputs/<CALIB.NAME>/tables/     csv
# ****************************************************************************************************

source('../jheem_analyses/commoncode/locations_of_interest.R')
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/analysis/shield_output_paths.R')


## Calibration ----
CALIB.NAME <- "calib.10.20.stage3"
N.SIM      <- 800


## Locations ----
LOCATIONS <- SHIELD.TEN.MSAS          # names = city, values = location code
MSAS      <- names(SHIELD.TEN.MSAS)   # city names, in the one order every figure uses


## Interventions ----
# 1. INTERVENTION.CODES: every simset that step 01 puts into the arrays
# 2. COVERAGE.LEVELS: the levels the tables and figures show. Must be a subset of
#    INTERVENTION.CODES -- the arrays stop at 50%, so asking for 60-100% gives NA.
NOINT              <- "noint"
INTERVENTION.CODES <- c(NOINT, paste0("doxy.cov.", seq(5, 50, 5)))
COVERAGE.LEVELS    <- paste0("doxy.cov.", seq(10, 50, 10))


## Years ----
YEARS     <- 2000:2040   # years pulled into the raw arrays
BASE.YEAR <- "2022"      # baseline for the *_vs_baseline outcomes, and start of the cumulative sums
EVAL.YEAR <- "2030"      # policy evaluation horizon
