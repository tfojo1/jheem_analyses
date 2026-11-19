# PRCC sensitivity analysis (v3): settings and outcome definitions
#
# Sourced at the top of every step, so all steps use the SAME outcome definitions.
# Pipeline (v3): step1 sort sims -> step2 top/bottom 20% ranking -> step3 PRCC -> step4 tornado plots
# Run from the jheem_analyses folder (same convention as analysis/intervention).
# Packages and SHIELD base files are loaded by ../shared/sensitivity_setup.R, only when
# they are not already in the session (re-sourcing this file is cheap).
# Calibrated parameters come from intervention/01a_generate_raw_results_arrays.R (extracts/params/).
#
# v3 (2026-11-17): three outcomes, all = % reduction in 2030 incidence with
# doxy-PEP at 20% coverage vs. no doxy-PEP, within the same simulation:
#   total  : total population
#   msm    : MSM
#   female : women
# Positive values = fewer cases with doxy-PEP.

# SETUP (packages and SHIELD base files; loads only what is not already in the session) ----
# All packages used by steps 1-4 (PRCC is base R: calc_prcc_base() below)
SENS.PACKAGES <- c("tidyverse")
source('../jheem_analyses/applications/SHIELD/analysis/sensitivity_analyses/shared/sensitivity_setup.R')

# SETTINGS ----
CALIB.CODE   <- "calib.10.20.stage4"
N.SIM        <- 1000
# REBUILD.PARAM.CACHE <- FALSE    # TRUE = re-extract parameters from the simsets (e.g., after recalibration)

# TEST MODE: NULL = all 10 MSAs. To test, give one MSA name or code, e.g.
#   TEST.MSA <- names(SHIELD.TEN.MSAS)[1]
# Test outputs go to OUT.PATH/test/, so they never mix with the full run.
# TEST.MSA <- names(SHIELD.TEN.MSAS)[1]
TEST.MSA <- NULL

msas <- if (is.null(TEST.MSA)) SHIELD.TEN.MSAS else
    SHIELD.TEN.MSAS[names(SHIELD.TEN.MSAS) %in% TEST.MSA | SHIELD.TEN.MSAS %in% TEST.MSA]
if (length(msas) == 0) stop("TEST.MSA '", TEST.MSA, "' is not in SHIELD.TEN.MSAS")
locations    <- setNames(unname(msas), unname(msas))      # MSA codes
FILE.SUFFIX  <- if (is.null(TEST.MSA)) "" else "_test"
# locations    <- setNames(SHIELD.TEN.MSAS, SHIELD.TEN.MSAS)
BASE.PATH    <- paste0(ROOT.DIR, "/shield/outputs/", CALIB.CODE, "/")    # inputs (results arrays)
# V2.PATH      <- paste0(BASE.PATH, "sensitivity_v2/")                     # v2 outputs (no longer used)
OUT.PATH     <- paste0(BASE.PATH, "sensitivity_prcc2/", if (is.null(TEST.MSA)) "" else "test/")   # test runs go to a test/ subfolder
# OUT.PATH     <- paste0(BASE.PATH, "sensitivity_v3_3out", FILE.SUFFIX, "/")                # v3 outputs (v2 outputs are not overwritten)
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

# PRCC (base R; replaces epiR::epi.prcc) ----
# Partial rank correlation coefficient of each parameter with the outcome, adjusting
# for all other parameters. Same method as epi.prcc: rank-transform every column,
# then PRCC_j = -P[j, y] / sqrt(P[j, j] * P[y, y]), with P = inverse of the rank
# correlation matrix (identical to correlating the residuals of rank(x_j) and rank(y)
# after regressing each on the ranks of the other parameters).
# Test: t = r * sqrt(df / (1 - r^2)), df = n - 2 - q (q = number of other parameters);
# 95% CI by Fisher z with SE = 1 / sqrt(n - 3 - q).
# Input: data frame of parameters with the outcome as the LAST column (as epi.prcc).
# Output columns match epi.prcc 2.x: var, est, lower, upper, test.statistic, df, p.value.
calc_prcc_base <- function(dat, conf.level = 0.95) {
    ranks <- apply(as.matrix(dat), 2, rank, ties.method = "average")
    n     <- nrow(ranks)
    k     <- ncol(ranks) - 1                  # number of parameters
    y     <- k + 1
    P     <- tryCatch(solve(cor(ranks)),
                      error = function(e) stop("Rank correlation matrix is singular (perfectly collinear ",
                                               "parameters?): ", e$message))
    est   <- -P[1:k, y] / sqrt(diag(P)[1:k] * P[y, y])
    q     <- k - 1
    df    <- n - 2 - q
    tstat <- est * sqrt(df / (1 - est^2))
    zcrit <- qnorm(1 - (1 - conf.level) / 2)
    se.z  <- 1 / sqrt(n - 3 - q)
    data.frame(var            = colnames(dat)[1:k],
               est            = unname(est),
               lower          = tanh(atanh(unname(est)) - zcrit * se.z),
               upper          = tanh(atanh(unname(est)) + zcrit * se.z),
               test.statistic = unname(tstat),
               df             = df,
               p.value        = 2 * pt(-abs(unname(tstat)), df),
               row.names      = NULL)
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
