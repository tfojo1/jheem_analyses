# Sensitivity analysis (v3): shared settings and outcome definitions
#
# Sourced at the top of every step, so all steps use the SAME outcome definitions.
# Pipeline (v3): step1 sort sims -> step2 top/bottom 20% ranking -> step3 PRCC -> step4 tornado plots
# Run from the jheem_analyses folder (same convention as analysis/intervention).
# Like the original scripts, this assumes ROOT.DIR, SHIELD.TEN.MSAS,
# load.calib.simsets() and extract.calib.simsets() are already available.
#
# v3 (2026-11-17): three outcomes, all = % reduction in 2030 incidence with
# doxy-PEP at 20% coverage vs. no doxy-PEP, within the same simulation:
#   total  : total population
#   msm    : MSM
#   female : women
# Positive values = fewer cases with doxy-PEP. Backup of v2: 00_sensitivity_settings_backup.R

library(tidyverse)

# SETTINGS ----
CALIB.CODE   <- "calib.10.20.stage4"
locations    <- setNames(SHIELD.TEN.MSAS, SHIELD.TEN.MSAS)
BASE.PATH    <- paste0(ROOT.DIR, "/shield/outputs/", CALIB.CODE, "/")    # inputs (results arrays)
V2.PATH      <- paste0(BASE.PATH, "sensitivity_v2/")                     # v2 outputs (param_matrices.Rdata is reused if present)
OUT.PATH     <- paste0(BASE.PATH, "sensitivity_v3_3out/")                # v3 outputs (v2 outputs are not overwritten)
# OUT.PATH     <- paste0(BASE.PATH, "sensitivity_v2/")                     # v2 outputs (originals are not overwritten)
dir.create(paste0(OUT.PATH, "figures/tornado"), recursive = TRUE, showWarnings = FALSE)

YEAR         <- "2030"
INTERVENTION <- "doxy.cov.20"    # compared with no intervention ("noint") only

# PRCC: p-values are adjusted for the number of parameters tested (per MSA and
# outcome) and saved in the tables only; they do NOT filter or flag parameters.
P.ADJUST.METHOD   <- "BH"

# Plot settings
TOP_BOTTOM_FRAC <- 0.2    # share of sims in the "top" and "bottom" groups
TOP_N_PARAMS    <- 10     # parameters shown per outcome panel
FIG_WIDTH       <- 18     # inches (three panels side by side)
FIG_HEIGHT      <- 1.5 + TOP_N_PARAMS * 0.6

# OUTCOMES ----
OUTCOMES <- c(total = "Total population", msm = "MSM", female = "Women")

# Returns one value per simulation (original sim order) for one MSA.
get_outcome <- function(outcome, city, total_calc_results, sex_calc_results) {
    if (outcome == "total") {
        -1 * total_calc_results[YEAR, , INTERVENTION, city, "pct_change_incidence_vs_noint"]
    } else {
        -1 * sex_calc_results[YEAR, outcome, , INTERVENTION, city, "pct_change_incidence_vs_noint"]
    }
}

# Writes a CSV whose first rows describe the columns, then the results
write_csv_with_notes <- function(df, file, notes) {
    writeLines(paste0("# ", notes), file)
    suppressWarnings(write.table(df, file, sep = ",", row.names = FALSE,
                                 append = TRUE, col.names = TRUE))
}

# V2 SETTINGS AND OUTCOMES (kept for reference) ----
# # Outcome 2 is a ratio of % changes (women / MSM). When the MSM % change is
# # close to 0 the ratio is unstable, so those sims are set to NA and dropped.
# # Units are percentage points (pct_change_incidence_vs_noint = -30 means a 30% reduction).
# MIN_MSM_CHANGE <- 1
# P.VALUE.THRESHOLD <- 0.05
# MIN_N_PARAMS    <- 5      # minimum number of parameters shown per plot
# MEDIAN_GAP      <- c("1" = 5, "2" = 0.05)   # min |median top - median bottom| to show a parameter
#
# # Outcome 1: % reduction in 2030 incidence vs. no intervention (positive = fewer cases)
# # Outcome 2: women-to-MSM relative impact ratio (women % change / MSM % change)
# get_outcome <- function(outcome, city, total_calc_results, sex_calc_results) {
#     if (outcome == 1) {
#         -1 * total_calc_results[YEAR, , INTERVENTION, city, "pct_change_incidence_vs_noint"]
#     } else {
#         women <- sex_calc_results[YEAR, "female", , INTERVENTION, city, "pct_change_incidence_vs_noint"]
#         msm   <- sex_calc_results[YEAR, "msm",    , INTERVENTION, city, "pct_change_incidence_vs_noint"]
#         msm[abs(msm) < MIN_MSM_CHANGE] <- NA
#         women / msm
#     }
# }
