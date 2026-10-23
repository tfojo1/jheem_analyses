# ****************************************************************************************************
# CHECK PS:EL SPLIT ----
# ****************************************************************************************************
# Does the model split early diagnoses between P&S and EL the way the data does?
# (Moved out of the old check_prior_and_ps_el_split.R; the prior part is now prior/check_prior_position.R.)
#
# For each city in one or more calibrations:
# 1. PS:EL ratio = P&S diagnoses / EL diagnoses, summed over 2015-2021, model vs data
#    only years where both outcomes are observed are used, for model and data alike
#
# Simsets already in memory (calib.simsets) are reused; anything missing is loaded from file.
#
# USAGE
#   1. set CALIBRATION.CODE (and LOCATIONS) in diagnostics_setup.R
#      to compare several calibrations, add them to CALIBRATION.CODES below
#   2. from jheem_analyses:  source("applications/SHIELD/analysis/diagnostics/fit/check_ps_el_split.R")
# ****************************************************************************************************

## SOURCES ----
source("../jheem_analyses/applications/SHIELD/analysis/diagnostics/diagnostics_setup.R")

## SETTINGS ----
CALIBRATION.CODES <- CALIBRATION.CODE           # one or more codes, e.g. c(CALIBRATION.CODE, "calib.10.15.stage3")
YEARS             <- as.character(2015:2021)    # years for the PS:EL check

## LOAD SIMSETS ----
calib.simsets <- load.calib.simsets(locations         = LOCATIONS,
                                    calibration.codes = CALIBRATION.CODES)

## observed.by.year ----
# observed yearly counts for one outcome and city, averaged over data sources
observed.by.year <- function(outcome, location) {
    x <- SURVEILLANCE.MANAGER$pull(outcome, keep.dimensions = "year",
                                   dimension.values = list(location = location, year = YEARS))
    if (is.null(x)) return(setNames(rep(NA_real_, length(YEARS)), YEARS))
    v <- apply(x, "year", mean, na.rm = TRUE)
    v[YEARS]
}

## ps.el ----
# PS:EL over the years where both outcomes are observed, model and data
ps.el <- function(simset, location) {
    ps.obs <- observed.by.year("ps.syphilis.diagnoses", location)
    el.obs <- observed.by.year("early.syphilis.diagnoses", location)
    ok <- is.finite(ps.obs) & is.finite(el.obs)
    ps.sim <- apply(simset$get("diagnosis.ps", year = YEARS, keep.dimensions = "year"), 1, median)
    el.sim <- apply(simset$get("diagnosis.el.misclassified", year = YEARS, keep.dimensions = "year"), 1, median)
    c(model = sum(ps.sim[ok]) / sum(el.sim[ok]),
      data  = sum(ps.obs[ok]) / sum(el.obs[ok]))
}

## RUN ----
# one table per calibration code: one row per city; a city that fails is reported and skipped
for (code in CALIBRATION.CODES) {
    entries <- Filter(function(e) e$calib.code == code, calib.simsets)
    res <- lapply(entries, function(e) tryCatch(ps.el(e$full_simset, e$location.code),
                                                error = function(err) { cat("SKIPPED", e$location.name, ":", conditionMessage(err), "\n"); NULL }))
    names(res) <- sapply(entries, `[[`, "location.name")
    res <- Filter(Negate(is.null), res)
    if (length(res) == 0) next
    tab <- t(sapply(res, identity))
    tab <- cbind(tab, model.over.data = tab[, "model"] / tab[, "data"])
    cat("\n== ", code, " | PS:EL ", min(YEARS), "-", max(YEARS), ", model vs data ==\n", sep = "")
    print(round(tab, 2))
    write.csv(tab, file.path(out.dir(code), paste0("ps_el_split_", code, ".csv")))
}
