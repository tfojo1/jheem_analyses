# GAM sensitivity analysis: settings, parameter groups and outcome paths
#
# Sourced at the top of every step, so all steps use the SAME MSAs, sims and groups.
# Pipeline: step1 parameter correlations -> step2 variance shares -> step3 heatmap
# (run_gam_all.R runs all three). Edit settings and PARAM_GROUPS here.
#
# Run from the jheem_analyses folder: every source() path is relative to it.
# To test on one MSA first, set TEST.MSA below (test outputs go to OUT.PATH/test/).
# Calibrated parameters come from intervention/01a_generate_raw_results_arrays.R (extracts/params/).

# SETUP (packages and SHIELD base files; loads only what is not already in the session) ----
# All packages used by steps 1-3, checked before anything runs
#   voi: evppivar(); mgcv and earth: its GAM and MARS fits; scales: heatmap legend (step 3)
SENS.PACKAGES <- c("tidyverse", "voi", "mgcv", "earth", "scales")
source('../jheem_analyses/applications/SHIELD/analysis/sensitivity_analyses/shared/sensitivity_setup.R')
# source('../jheem_analyses/commoncode/locations_of_interest.R')                               # SHIELD.TEN.MSAS
# # source('../jheem_analyses/applications/SHIELD/shield_specification.R')                       # model + ROOT.DIR
# source('../jheem_analyses/applications/SHIELD/analysis/calibration/calibration_helper_functions.R')  # load/extract simsets
#
# library(tidyverse)
# library(voi)      # install.packages("voi"); also installs mgcv and earth

# SETTINGS ----
CALIB.CODE   <- "calib.10.20.stage4"
N.SIM        <- 1000
EVAL.YEAR    <- "2030"
INTERVENTION <- "doxy.cov.20"      # doxy-PEP coverage scenario to analyse
MAX.GAM.PARS <- 2                  # groups with more parameters use "earth" (a multi-parameter
                                   # GAM uses a tensor-product smooth with too many terms for 1,000 sims)
SEED         <- 2026

# OUTCOMES (computed in step 2; order used in tables and figures) ----
# All in EVAL.YEAR, INTERVENTION vs. no intervention, within the same simulation:
#   Total, MSM, Women   : % reduction in incidence (positive = fewer cases with doxy-PEP)
#   Women-to-MSM ratio  : women % reduction / MSM % reduction; NA when MSM incidence did not
#                         fall (same definition as prcc/00_prcc_settings.R, outcome "rir")
#   Women minus MSM     : women % reduction - MSM % reduction (percentage points)
GAM_OUTCOMES <- c("Total", "MSM", "Women", "Women-to-MSM ratio", "Women minus MSM")

# Single-parameter figure (step 3): parameters shown per outcome panel, ranked by
# their median share across the MSAs analysed
TOP_N_SINGLE <- 10

# GROUP CHECK (step 1; see README, "Checking the groups") ----
# Two parameters strongly correlated in the posterior must be in the SAME group, otherwise both
# groups claim the same variance. Step 1 flags a pair of groups for merging when a parameter in
# one and a parameter in the other have |Spearman r| >= GROUP.CHECK.R in at least
# GROUP.CHECK.N.MSAS MSAs (or in every MSA analysed, if fewer). Step 2 stops until the check passes.
GROUP.CHECK.R        <- 0.7
GROUP.CHECK.N.MSAS   <- 3
GROUP.CHECK.OVERRIDE <- FALSE    # TRUE = let step 2 run anyway, after you have reviewed the flagged links

# TEST MODE: NULL = all 10 MSAs. To test, give one MSA name or code, e.g.
#   TEST.MSA <- names(SHIELD.TEN.MSAS)[1]
# Test outputs go to OUT.PATH/test/, so they never mix with the full run.
# TEST.MSA <- names(SHIELD.TEN.MSAS)[1]
TEST.MSA <- NULL #runs all MSAs

msas <- if (is.null(TEST.MSA)) SHIELD.TEN.MSAS else
    SHIELD.TEN.MSAS[names(SHIELD.TEN.MSAS) %in% TEST.MSA | SHIELD.TEN.MSAS %in% TEST.MSA]
if (length(msas) == 0) stop("TEST.MSA '", TEST.MSA, "' is not in SHIELD.TEN.MSAS")
locations   <- setNames(unname(msas), unname(msas))      # MSA codes
FILE.SUFFIX <- if (is.null(TEST.MSA)) "" else "_test"

BASE.PATH  <- paste0(ROOT.DIR, "/shield/outputs/", CALIB.CODE, "/")
OUT.PATH   <- paste0(BASE.PATH, "sensitivity_analysis_GAM/", if (is.null(TEST.MSA)) "" else "test/")   # test runs go to a test/ subfolder
# OUT.PATH   <- paste0(BASE.PATH, "sensitivity_analysis_GAM/")      # before: test runs used a "_test" file suffix
dir.create(OUT.PATH, recursive = TRUE, showWarnings = FALSE)
# CACHE.PATH <- paste0(OUT.PATH, "param_cache/")          # one small file per MSA
# dir.create(CACHE.PATH, recursive = TRUE, showWarnings = FALSE)

# PARAMETER GROUPS ----
# Regular expressions on parameter names; each parameter goes to the FIRST
# group it matches (order matters). Correlated parameters are kept together.
PARAM_GROUPS <- c(
    "MSM transmission: level and trend"       = "^global\\.transmission\\.rate\\.msm$|^transmission\\.rate\\.multiplier\\.msm[0-9]{4}$",
    "Heterosexual transmission: level and trend" = "^global\\.transmission\\.rate\\.het$|^transmission\\.rate\\.multiplier\\.heterosexual[0-9]{4}$",
    "Future transmission change"              = "^transmission\\.rate\\.future\\.change\\.mult$",
    "MSM transmission by age"                 = "^transmission\\.rate\\.multiplier\\.age[0-9]+\\.msm$",
    "Heterosexual transmission by age"        = "^transmission\\.rate\\.multiplier\\.age[0-9]+\\.heterosexual$",
    "Race: transmission and mixing"           = "^transmission\\.rate\\.multiplier\\.(black|hispanic|other)$|sexual\\.multi$",
    "Infectiousness by stage"                 = "transmissibility|^prop\\.early\\.latent",
    "MSM-female bridging"                     = "pairings\\.with\\.(female|msm|male)$",
    "Age mixing"                              = "^age\\.mixing\\.sd\\.mult$",
    "MSM population size"                     = "proportion\\.msm\\.of\\.male\\.mult$",
    "Screening: MSM"                          = "^screening\\.rate\\.multiplier\\.msm\\.|^msm\\.sti\\.screening\\.covid",
    "Screening: heterosexual"                 = "^screening\\.rate\\.multiplier\\.(female|heterosexual_male)|^heterosexual\\.sti\\.screening\\.covid",
    "Screening: race"                         = "^screening\\.rate\\.multiplier\\.(black|hispanic|other)$|^(black|hispanic|other)\\.sti\\.screening\\.covid",
    "Future screening change"                 = "^screening\\.rate\\.future\\.change\\.mult$",
    "Symptomatic care seeking"                = "careseeking",
    "Natural history and symptoms"            = "symptomatic",
    "Stage misclassification"                 = "misclassified",
    "HIV testing"                             = "syphilis\\.to\\.hiv\\.testing",
    "Initial conditions (1970)"               = "1970$",
    "Prenatal screening"                      = "trimester",
    "Demographics"                            = "aging|fertility|mortality|immigration"
)

# Fingerprint of the groups: step 2 checks that step 1 was run on exactly these groups
GROUPS.SIGNATURE <- paste(names(PARAM_GROUPS), PARAM_GROUPS, sep = " = ", collapse = " | ")

assign_groups <- function(param_names) {
    group <- sapply(param_names, function(p) {
        hit <- names(PARAM_GROUPS)[sapply(PARAM_GROUPS, grepl, x = p)]
        if (length(hit) == 0) "Unassigned" else hit[1]
    })
    if (any(group == "Unassigned")) {
        warning("Parameters not in any group (analysed as 'Unassigned'): ",
                paste(param_names[group == "Unassigned"], collapse = ", "))
    }
    split(param_names, group)
}
 