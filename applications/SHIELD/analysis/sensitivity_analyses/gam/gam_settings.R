# GAM-based sensitivity analysis: shared settings, parameter groups and parameter loader
# Sourced by 00_param_correlations.R and 01_gam_sensitivity.R so both use the same
# MSAs, sims and groups. Edit settings and PARAM_GROUPS here.
#
# Run from the jheem_analyses folder: every source() path is relative to it.
# To test on one MSA first, set TEST.MSA below (outputs get a "_test" suffix).
# Calibrated parameters come from intervention/01a_generate_raw_results_arrays.R (extracts/params/).

# SETUP (packages and SHIELD base files; loads only what is not already in the session) ----
# All packages used by scripts 00-02, checked before anything runs
#   voi: evppivar(); mgcv and earth: its GAM and MARS fits; scales: heatmap legend (02)
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

# TEST MODE: NULL = all 10 MSAs. To test, give one MSA name or code, e.g.
#   TEST.MSA <- names(SHIELD.TEN.MSAS)[1]
TEST.MSA <- names(SHIELD.TEN.MSAS)[1]

msas <- if (is.null(TEST.MSA)) SHIELD.TEN.MSAS else
    SHIELD.TEN.MSAS[names(SHIELD.TEN.MSAS) %in% TEST.MSA | SHIELD.TEN.MSAS %in% TEST.MSA]
if (length(msas) == 0) stop("TEST.MSA '", TEST.MSA, "' is not in SHIELD.TEN.MSAS")
locations   <- setNames(unname(msas), unname(msas))      # MSA codes
FILE.SUFFIX <- if (is.null(TEST.MSA)) "" else "_test"

BASE.PATH  <- paste0(ROOT.DIR, "/shield/outputs/", CALIB.CODE, "/")
OUT.PATH   <- paste0(BASE.PATH, "sensitivity_analysis_GAM/")
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

# LOAD ----
# Parameter matrix (n.sim x n.params) for each MSA: get_param_matrix(city) in
# shared/sensitivity_setup.R reads extracts/params/<MSA>.Rdata, saved by
# intervention/01a_generate_raw_results_arrays.R from the no-intervention simset (same sim
# order as the outcome arrays). It stops if a file is missing: run 01a first.

# PREVIOUS LOADER (kept for reference; replaced by the shared get_param_matrix()) ----
# # Parameter matrix (n.sim x n.params) for each MSA, loaded one MSA at a time.
# # Loaded simsets are KEPT in the session as 'calib.simsets' (the helper's own cache),
# # so a city that is already loaded is not read from disk again.
# # Each parameter matrix is also saved to disk, so later runs skip loading entirely.
# get_param_matrix <- function(city) {
#     cache.file <- paste0(CACHE.PATH, city, ".Rdata")
#     if (file.exists(cache.file)) return(get(load(cache.file)))
#
#     # Reuses (and adds to) a global 'calib.simsets' if one exists
#     calib.simsets <- load.calib.simsets(SHIELD.TEN.MSAS[SHIELD.TEN.MSAS == city], CALIB.CODE, N.SIM)
#     assign("calib.simsets", calib.simsets, envir = globalenv())
#
#     simset <- extract.calib.simsets(calib.simsets, city, CALIB.CODE, exact = T)[[1]]$full_simset
#     x <- t(simset$parameters)
#     colnames(x) <- simset$parameter.names
#
#     save(x, file = cache.file)
#     x
# }
