# Sensitivity analysis (v2): shared settings and outcome definitions
#
# Sourced at the top of every step, so all steps use the SAME outcome definitions.
# Run from the jheem_analyses folder (same convention as analysis/intervention).
# Like the original scripts, this assumes ROOT.DIR, SHIELD.TEN.MSAS,
# load.calib.simsets() and extract.calib.simsets() are already available.

library(tidyverse)

# SETTINGS ----
CALIB.CODE   <- "calib.10.20.stage4"
locations    <- setNames(SHIELD.TEN.MSAS, SHIELD.TEN.MSAS)
BASE.PATH    <- paste0(ROOT.DIR, "/shield/outputs/", CALIB.CODE, "/")    # inputs (results arrays)
OUT.PATH     <- paste0(BASE.PATH, "sensitivity_v2/")                     # v2 outputs (originals are not overwritten)
dir.create(paste0(OUT.PATH, "figures/PRCC plots"), recursive = TRUE, showWarnings = FALSE)

YEAR         <- "2030"
INTERVENTION <- "doxy.cov.20"

# Outcome 2 is a ratio of % changes (women / MSM). When the MSM % change is
# close to 0 the ratio is unstable, so those sims are set to NA and dropped.
# Units are percentage points (pct_change_incidence_vs_noint = -30 means a 30% reduction).
MIN_MSM_CHANGE <- 1

# PRCC significance: p-values are adjusted for the number of parameters tested
# (per MSA and outcome) before applying the threshold.
P.ADJUST.METHOD   <- "BH"
P.VALUE.THRESHOLD <- 0.05

# Plot settings
TOP_BOTTOM_FRAC <- 0.2    # share of sims in the "top" and "bottom" groups
MIN_N_PARAMS    <- 5      # minimum number of parameters shown per plot
MEDIAN_GAP      <- c("1" = 5, "2" = 0.05)   # min |median top - median bottom| to show a parameter

# OUTCOMES ----
# Returns one value per simulation (original sim order) for one MSA.
# Outcome 1: % reduction in 2030 incidence vs. no intervention (positive = fewer cases)
# Outcome 2: women-to-MSM relative impact ratio (women % change / MSM % change)
get_outcome <- function(outcome, city, total_calc_results, sex_calc_results) {
    if (outcome == 1) {
        -1 * total_calc_results[YEAR, , INTERVENTION, city, "pct_change_incidence_vs_noint"]
    } else {
        women <- sex_calc_results[YEAR, "female", , INTERVENTION, city, "pct_change_incidence_vs_noint"]
        msm   <- sex_calc_results[YEAR, "msm",    , INTERVENTION, city, "pct_change_incidence_vs_noint"]
        msm[abs(msm) < MIN_MSM_CHANGE] <- NA
        women / msm
    }
}
