# ****************************************************************************************************
# 00 SETTINGS ----
# ****************************************************************************************************
# Shared by every numbered script in this folder. Each one sources this file first, so the
# calibration, interventions and years are set in ONE place.
#
# To switch to a new calibration: edit CALIB.NAME and N.SIM below, then re-run from step 01a.
#
# RUN ORDER
#   Run from the jheem_analyses folder: every source() path is relative to it.
#
#   00_settings.R                       sourced by the scripts below; never run on its own
#   01a_generate_raw_results_arrays.R   simsets -> raw arrays (total, sex; optionally race, age).
#                                       Takes hours: run it from Terminal with nohup (see its header)
#   01b_generate_custom_outcomes.R      raw arrays -> custom outcomes: cases averted, rates,
#                                       proportions (total_calc / sex_calc arrays)
#   02_pretty_calibration_plots.R       calibration-fit figures for the manuscript. Has its own
#                                       settings and reads the calibration simsets, not the arrays
#   03_manuscript_main.R                Table 1, Figure 3 (heatmap), Figure 4 (box plots) and
#                                       Table 2 (the values in Figure 4)
#   04_women_to_msm_impact_ratio.R      women-to-MSM relative impact ratio and its associations
#                                       with baseline characteristics. Reads the raw arrays only
#
# HELPERS (sourced by the scripts above; never run on their own)
#   intervention_helper_functions.R   table builders, plot_* figures, colour palette
#   int_simset_helper_functions.R     loading and plotting the raw intervention simsets
#   pretty_plots_helper_functions.R   shield.plot() and its data layer, used by 02
#   pretty_plots_style.R              years, colours, theme, labels and sizes for 02's figures
#
# Old and exploratory scripts are in archive/, including the older full manuscript script, the
# efficiency tables, the array comparison and the QC trajectory plots. Nothing above sources them.
# The CROI 2026 abstract analysis is frozen in ../croi26/, separate from this workflow.
#
# WHERE THINGS ARE WRITTEN
#   <root>/shield/outputs/<CALIB.NAME>/            the four result arrays
#   <root>/shield/outputs/<CALIB.NAME>/extracts/   step 01a's one-file-per-simset extracts
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
# 1. INTERVENTION.CODES: every simset that step 01a puts into the arrays
# 2. COVERAGE.LEVELS: the levels the tables and figures show. Must be a subset of
#    INTERVENTION.CODES -- the arrays stop at 50%, so asking for 60-100% gives NA.
NOINT              <- "noint"
INTERVENTION.CODES <- c(NOINT, paste0("doxy.cov.", seq(5, 50, 5)))
COVERAGE.LEVELS    <- paste0("doxy.cov.", seq(5, 50, 5))


## Years ----
YEARS     <- 2000:2040   # years pulled into the raw arrays
YEAR0     <- "2022"      # reference year for the *_vs_year0 outcomes, and start of the cumulative sums
EVAL.YEAR <- "2030"      # policy evaluation horizon
