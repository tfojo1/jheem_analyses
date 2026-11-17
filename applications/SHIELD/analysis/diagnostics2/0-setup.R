# ****************************************************************************************************
# DIAGNOSTICS2: SETUP ----
# ****************************************************************************************************
# One place for the settings every diagnostics2 script uses. Each script sources this file first.
# The heavy files (specification, calibration register) load only once per R session.
# Methods and assumptions: METHODS.md (same folder). Section numbers in the code refer to that file.
#
# OUTPUT FOLDERS (METHODS 10): everything for one calibration in one folder, so it can be deleted together
#   <jheem root>/shield/diagnostics/<calibration code>/
#       mcmc_cache/   slimmed MCMC per city, reused by every diagnostic
#       mixing/       the mixing report and its CSVs
# ****************************************************************************************************

## SETTINGS ----
CALIBRATION.CODE <- "calib.10.20.stage4"
LOCATIONS        <- NULL          # NULL = all 10 cities; or e.g. c(Baltimore = "C.12580")
MIXING.BURN.KEEP <- 0.5           # share of each chain used (the last part); 0.5 = second half (METHODS 1.3)

## SOURCES ----
DIAGNOSTICS2.DIR <- "../jheem_analyses/applications/SHIELD/analysis/diagnostics2"
if (!exists("SHIELD.SPECIFICATION")) source("../jheem_analyses/applications/SHIELD/shield_specification.R")
if (is.null(tryCatch(get.calibration.info(CALIBRATION.CODE, throw.error.if.missing = FALSE), error = function(e) NULL)))
    source("../jheem_analyses/applications/SHIELD/shield_calib_register.R")
library(bayesian.simulations)
source(file.path(DIAGNOSTICS2.DIR, "helpers", "mixing_stats.R"))
source(file.path(DIAGNOSTICS2.DIR, "helpers", "identifiability.R"))
source(file.path(DIAGNOSTICS2.DIR, "helpers", "report_text.R"))
source(file.path(DIAGNOSTICS2.DIR, "helpers", "report_html.R"))
if (is.null(LOCATIONS)) LOCATIONS <- SHIELD.TEN.MSAS

## FOLDERS ----
DIAGNOSTICS.ROOT <- file.path(get.jheem.root.directory(), "shield", "diagnostics")
# Caches built before diagnostics2 (files <calibration code>_<location>.rds). Read only if the new cache has no
# copy, and then copied into the new cache. Set to NULL to stop using it.
OLD.CACHE.DIR    <- file.path(get.jheem.root.directory(), "shield", "mcmc_diagnostics", "mcmc_cache")

# folder for one calibration: calibration.dir("mixing") -> .../diagnostics/<code>/mixing/
calibration.dir <- function(sub = "", code = CALIBRATION.CODE) {
    d <- file.path(DIAGNOSTICS.ROOT, code, sub)
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    d
}

cat("Diagnostics2 setup:", CALIBRATION.CODE, "|", length(LOCATIONS), "cities | outputs in",
    calibration.dir(), "\n")
