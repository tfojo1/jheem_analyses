# ****************************************************************************************************
# DIAGNOSTICS SETUP ----
# ****************************************************************************************************
# One place to choose the calibration and load what every diagnostic script needs.
# 1. Set CALIBRATION.CODE and LOCATIONS here once; no script sets its own.
# 2. Each script in mcmc/, fit/, prior/ and reports/ sources this file first.
# 3. The heavy files (specification, data managers, calibration register) load only once per R session.
#    Sourcing this file again only resets the settings below, so it is quick.
#
# USAGE (from jheem_analyses)
#   1. set CALIBRATION.CODE and LOCATIONS below
#   2. optional, to load everything ahead of time:
#        source("applications/SHIELD/analysis/diagnostics/diagnostics_setup.R")
#   3. run any diagnostic script; it sources this file itself
#   To switch calibration: change CALIBRATION.CODE here and run the script again.
#   To add a new calibration: register it in shield_calib_register.R; this file re-sources the register
#   when CALIBRATION.CODE is not registered yet.
# ****************************************************************************************************

## SETTINGS ----
CALIBRATION.CODE <- "calib.10.20.stage3"
LOCATIONS        <- NULL                         # NULL = all 10 cities; or e.g. c(Baltimore = "C.12580")

## SOURCES ----
# 1. specification: also sources shield_source_code.R (data managers, jheem root directory,
#    locations_of_interest.R, shield_calib_parameters.R)
# 2. calibration register: also sources shield_likelihoods.R; needed by the mixing scripts
#    (get.calibration.info) and re-sourced only when CALIBRATION.CODE is not registered yet
# 3. helpers: functions only, quick to re-source
DIAGNOSTICS.DIR <- "../jheem_analyses/applications/SHIELD/analysis/diagnostics"
if (!exists("SHIELD.SPECIFICATION")) source("../jheem_analyses/applications/SHIELD/shield_specification.R")
if (!exists("load.calib.simsets"))   source("../jheem_analyses/applications/SHIELD/analysis/calibration/calibration_helper_functions.R")
if (is.null(tryCatch(get.calibration.info(CALIBRATION.CODE, throw.error.if.missing = FALSE), error = function(e) NULL)))
    source("../jheem_analyses/applications/SHIELD/shield_calib_register.R")
source(file.path(DIAGNOSTICS.DIR, "helpers", "fit_helpers.R"))
source(file.path(DIAGNOSTICS.DIR, "helpers", "mcmc_helpers.R"))
library(ggplot2)
library(dplyr)
if (is.null(LOCATIONS)) LOCATIONS <- SHIELD.TEN.MSAS

## OUTPUT FOLDERS ----
# tables and plots from fit/ and prior/, one folder per calibration:
#   <jheem root>/shield/calibration_diagnostics/<calibration code>/
# 1. OUT.DIR      = the folder for CALIBRATION.CODE
# 2. out.dir(code) = the folder for any other code (for scripts that loop over CALIBRATION.CODES)
# 3. DATA.OUT.DIR = outputs from data-only scripts, which do not depend on a calibration
# the mixing scripts save under shield/mcmc_diagnostics/ (see helpers/mcmc_helpers.R)
out.dir <- function(code = CALIBRATION.CODE) {
    d <- file.path(get.jheem.root.directory(), "shield", "calibration_diagnostics", code)
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    d
}
OUT.DIR      <- out.dir(CALIBRATION.CODE)
DATA.OUT.DIR <- out.dir("data_only")

cat("Diagnostics setup:", CALIBRATION.CODE, "|", length(LOCATIONS), "locations | outputs in", OUT.DIR, "\n")
