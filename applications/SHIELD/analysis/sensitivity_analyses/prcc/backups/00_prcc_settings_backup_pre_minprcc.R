# PRCC sensitivity analysis (v3): settings and outcome definitions
#
# Sourced at the top of every step, so all steps use the SAME outcome definitions.
# Pipeline (v3): step1 sort sims -> step2 top/bottom 20% ranking -> step3 PRCC -> step4 tornado plots
# Run from the jheem_analyses folder (same convention as analysis/intervention).
# Packages and SHIELD base files are loaded by ../shared/sensitivity_setup.R, only when
# they are not already in the session (re-sourcing this file is cheap).
# Calibrated parameters come from intervention/01a_generate_raw_results_arrays.R (extracts/params/).
#
# v3: five outcomes in 2030, doxy-PEP at 20% coverage vs. no doxy-PEP, within the same
# simulation: % reduction in incidence (total, MSM, women), the women-to-MSM relative
# impact ratio (rir) and women minus MSM (percentage points). See OUTCOMES below.

# SETUP (packages and SHIELD base files; loads only what is not already in the session) ----
# All packages used by steps 1-4 (PRCC is base R: calc_prcc_base() below)
SENS.PACKAGES <- c("tidyverse")
source('../jheem_analyses/applications/SHIELD/analysis/sensitivity_analyses/shared/sensitivity_setup.R')

# SETTINGS ----
CALIB.CODE   <- "calib.10.20.stage4"
N.SIM        <- 1000
YEAR         <- "2030"
INTERVENTION <- "doxy.cov.20"    # compared with no intervention ("noint") only

# TEST MODE: NULL = all 10 MSAs. To test, give one MSA name or code, e.g.
#   TEST.MSA <- names(SHIELD.TEN.MSAS)[1]
# Test outputs go to OUT.PATH/test/, so they never mix with the full run.
TEST.MSA <- NULL

msas <- if (is.null(TEST.MSA)) SHIELD.TEN.MSAS else
    SHIELD.TEN.MSAS[names(SHIELD.TEN.MSAS) %in% TEST.MSA | SHIELD.TEN.MSAS %in% TEST.MSA]
if (length(msas) == 0) stop("TEST.MSA '", TEST.MSA, "' is not in SHIELD.TEN.MSAS")
locations    <- setNames(unname(msas), unname(msas))      # MSA codes

BASE.PATH    <- paste0(ROOT.DIR, "/shield/outputs/", CALIB.CODE, "/")    # inputs (results arrays, extracts/params)
OUT.PATH     <- paste0(BASE.PATH, "sensitivity_prcc2/", if (is.null(TEST.MSA)) "" else "test/")
dir.create(paste0(OUT.PATH, "figures/tornado"), recursive = TRUE, showWarnings = FALSE)

# INFLUENTIAL PARAMETERS ----
# Top / bottom groups: sims with the highest / lowest TOP_BOTTOM_FRAC of each parameter's values.
# Standardized difference (std_diff) = |median top - median bottom| / SD of the outcome across all
# sims in that MSA (no units, so it compares across outcomes). A parameter stands out for an
# MSA x outcome when std_diff >= MIN_STD_DIFF.
TOP_BOTTOM_FRAC <- 0.2
MIN_STD_DIFF    <- 1

# PLOTS ----
# Which parameters each tornado panel shows (ordered by std_diff in both cases):
#   "sd"    : every parameter that stands out (std_diff >= MIN_STD_DIFF); the number varies by panel
#   "top_n" : a fixed number per panel, the TOP_N_PARAMS parameters with the largest std_diff
PLOT.FILTER      <- "top_n"
TOP_N_PARAMS     <- 10     # used only when PLOT.FILTER == "top_n"
if (!PLOT.FILTER %in% c("sd", "top_n")) stop("PLOT.FILTER must be \"sd\" or \"top_n\"")
FACET_NCOL       <- 4      # panels per row (one panel per outcome)
FIG_WIDTH        <- 18     # inches
FIG_IN_PER_PARAM <- 0.5    # figure height (inches) per parameter, in the tallest panel of each row

# OUTCOMES ----
# Names = outcome codes used in files and tables; values = panel titles (with units).
# All in YEAR, INTERVENTION vs. no intervention, within the same simulation:
#   total, msm, female : % reduction in incidence (positive = fewer cases with doxy-PEP)
#   rir                : women-to-MSM relative impact ratio = women % reduction / MSM % reduction;
#                        NA when MSM incidence did not fall (as in intervention/04_women_to_msm_impact_ratio.R)
#   women_minus_msm    : women % reduction - MSM % reduction (percentage points; negative = women
#                        benefit less than MSM), as in the GAM analysis
OUTCOMES <- c(total           = "Total population: % reduction",
              msm             = "MSM: % reduction",
              female          = "Women: % reduction",
              rir             = "Women-to-MSM relative impact ratio"
              # women_minus_msm = "Women minus MSM (percentage points)"
              )

# Include the fifth outcome (women minus MSM)? FALSE drops it from every step
INCLUDE.WOMEN.MINUS.MSM <- TRUE
if (!INCLUDE.WOMEN.MINUS.MSM) OUTCOMES <- OUTCOMES[names(OUTCOMES) != "women_minus_msm"]

# Returns one value per simulation (original sim order) for one MSA.
get_outcome <- function(outcome, city, total_calc_results, sex_calc_results) {
    reduction <- function(sex) -1 * sex_calc_results[YEAR, sex, , INTERVENTION, city, "pct_change_incidence_vs_noint"]
    if (outcome == "total") {
        -1 * total_calc_results[YEAR, , INTERVENTION, city, "pct_change_incidence_vs_noint"]
    } else if (outcome %in% c("msm", "female")) {
        reduction(outcome)
    } else if (outcome == "rir") {
        msm <- reduction("msm")
        rir <- reduction("female") / msm
        rir[msm <= 0] <- NA        # MSM incidence did not fall (as in 04_women_to_msm_impact_ratio.R)
        rir
    } else if (outcome == "women_minus_msm") {
        reduction("female") - reduction("msm")
    } else {
        stop("Unknown outcome: ", outcome)
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
# 95% CI by Fisher z with SE = 1 / sqrt(n - 3 - q) (q = number of other parameters).
# PRCCs are not tested against 0 (no p-values): with 1,000 sims almost any PRCC would be
# "significant"; parameters are selected by the standardized difference (MIN_STD_DIFF) instead.
# Input: data frame of parameters with the outcome as the LAST column (as epi.prcc).
# Output columns: var, est (PRCC), lower, upper (95% CI).
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
    zcrit <- qnorm(1 - (1 - conf.level) / 2)
    se.z  <- 1 / sqrt(n - 3 - q)
    data.frame(var            = colnames(dat)[1:k],
               est            = unname(est),
               lower          = tanh(atanh(unname(est)) - zcrit * se.z),
               upper          = tanh(atanh(unname(est)) + zcrit * se.z),
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
