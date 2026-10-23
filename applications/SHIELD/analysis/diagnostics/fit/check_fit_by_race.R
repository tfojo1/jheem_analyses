# ****************************************************************************************************
# CHECK FIT BY RACE ----
# ****************************************************************************************************
# Model / data ratios of diagnoses by race (black, hispanic, other), by period and city.
#
# For each city in the calibration set in diagnostics_setup.R:
# 1. plots of diagnoses by race (saved by plot.calib.comparison)
# 2. for P&S, total, EL and late diagnoses: model / data by period for each race
#    1 = right, > 1 = model too high, < 1 = model too low
# 3. the same for each race relative to "other":
#    black.to.other = (model black / model other) / (data black / data other)
#    < 1 = the model's black diagnoses are too low relative to other; tells a disparity problem from an overall level problem
#    each table ends with: median across cities, and cities within 0.8-1.2
#
# Data by race for the 10 MSAs (syphilis-manager-v2026.10.09):
#   county aggregates only, 2019-2023, with gaps (suppressed years are left out, not counted as 0)
#   so the periods are 2019-2021 and 2022-2023 (2022-2023 is after the calibration period: an out-of-sample check)
#   "other" = white + asian + american indian + pacific islander (as in R/shield_ontology_mappings.R)
#
# Simsets already in memory (calib.simsets) are reused; anything missing is loaded from file.
#
# USAGE
#   1. set CALIBRATION.CODE (and LOCATIONS) in diagnostics_setup.R
#   2. from jheem_analyses:  source("applications/SHIELD/analysis/diagnostics/fit/check_fit_by_race.R")
#   3. outputs (CSV) go to the calibration folder: one table per outcome, plus one long file for sharing:
#      fit_by_race_all_<code>.csv (outcome, measure, period, city, value)
#   If step 0 stops with a compiled-code error, re-source shield_source_code.R; if that is not
#   enough, restart R without restoring the workspace and run again.
# ****************************************************************************************************

## SOURCES ----
# observed.by.race() is in helpers/fit_helpers.R
source("../jheem_analyses/applications/SHIELD/analysis/diagnostics/diagnostics_setup.R")

## SETTINGS ----
PERIODS  <- list("2019-2021" = 2019:2021, "2022-2023" = 2022:2023)   # 2022-2023: after the calibration period
OUTCOMES <- c(diagnosis.ps                 = "ps.syphilis.diagnoses",          # model outcome = data outcome
              diagnosis.total              = "total.syphilis.diagnoses",
              diagnosis.el.misclassified   = "early.syphilis.diagnoses",
              diagnosis.late.misclassified = "unknown.duration.or.late.syphilis.diagnoses")
RACES    <- c("black", "hispanic", "other")
MEASURES <- c(RACES, "black.to.other", "hispanic.to.other")
YEARS    <- as.character(min(unlist(PERIODS)):max(unlist(PERIODS)))
MAKE.PLOTS    <- TRUE
PLOT.OUTCOMES <- "diagnosis.ps"                   # can add any of names(OUTCOMES)
PLOT.YEARS    <- 2010:2025

## LOAD SIMSETS ----
calib.simsets <- load.calib.simsets(locations = LOCATIONS, calibration.codes = CALIBRATION.CODE)
entries <- Filter(function(e) e$calib.code == CALIBRATION.CODE, calib.simsets)
if (length(entries) == 0) stop("No simsets loaded for ", CALIBRATION.CODE)
names(entries) <- sapply(entries, `[[`, "location.name")

## 0. SESSION CHECK ----
check.session(entries[[1]]$location.code, keep.dimensions = c("year", "race"))

## 1. PLOTS ----
# diagnoses by race, one panel per race
if (MAKE.PLOTS)
    plot.calib.comparison(calib.simsets, calibration.codes = CALIBRATION.CODE, locations = LOCATIONS,
                          outcomes = PLOT.OUTCOMES, separate.by = "outcome",
                          facet.by = "race", years = PLOT.YEARS, save.dir = OUT.DIR)

## model.by.race ----
# model diagnoses (median over simulations) by year and race
model.by.race <- function(simset, sim.outcome) {
    s <- apply(simset$get(sim.outcome, year = YEARS, keep.dimensions = c("year", "race")), c("year", "race"), median)
    s[YEARS, RACES, drop = FALSE]
}

## ratios.by.period ----
# per period:
# 1. model / data for each race, over the years where that race is observed
# 2. black.to.other and hispanic.to.other, over the years where both races are observed
ratios.by.period <- function(model, obs) {
    ratio <- function(p, num, den = NULL) {
        y  <- intersect(as.character(p), rownames(obs))
        ok <- y[rowSums(is.na(obs[y, c(num, den), drop = FALSE])) == 0]
        if (length(ok) == 0) return(NA)
        m <- colSums(model[ok, c(num, den), drop = FALSE]); o <- colSums(obs[ok, c(num, den), drop = FALSE])
        if (is.null(den)) m[[num]] / o[[num]] else (m[[num]] / m[[den]]) / (o[[num]] / o[[den]])
    }
    t(sapply(PERIODS, function(p) c(black             = ratio(p, "black"),
                                    hispanic          = ratio(p, "hispanic"),
                                    other             = ratio(p, "other"),
                                    black.to.other    = ratio(p, "black", "other"),
                                    hispanic.to.other = ratio(p, "hispanic", "other"))))
}

## 2. MODEL / DATA BY RACE ----
# one table per outcome: rows = measure | period, columns = cities, then median and cities within 0.8-1.2
long <- list()
for (out in names(OUTCOMES)) {
    res <- lapply(entries, function(e) tryCatch({
        obs <- observed.by.race(OUTCOMES[[out]], e$location.code, years = YEARS)
        if (is.null(obs)) NULL else ratios.by.period(model.by.race(e$full_simset, out), obs)
    }, error = function(err) { message(e$location.name, " / ", out, ": ", conditionMessage(err)); NULL }))
    res <- Filter(Negate(is.null), res)
    if (length(res) == 0) next
    tab <- sapply(res, function(r) as.vector(r[, MEASURES]))
    rownames(tab) <- paste(rep(MEASURES, each = length(PERIODS)), "|", rep(names(PERIODS), times = length(MEASURES)))
    tab <- cbind(tab,
                 median        = apply(tab, 1, function(r) if (any(is.finite(r))) median(r[is.finite(r)]) else NA),
                 in.0.8.to.1.2 = apply(tab, 1, function(r) sum(r >= 0.8 & r <= 1.2, na.rm = TRUE)))
    cat("\n== ", out, " | model / data by race ==\n", sep = "")
    print(round(tab, 2))
    write.csv(tab, file.path(OUT.DIR, paste0("fit_by_race_", out, "_", CALIBRATION.CODE, ".csv")))
    for (city in names(res))
        long[[paste(out, city)]] <- data.frame(outcome = out,
                                               measure = rep(MEASURES, each = length(PERIODS)),
                                               period  = rep(names(PERIODS), times = length(MEASURES)),
                                               city    = city,
                                               value   = as.vector(res[[city]][, MEASURES]))
}
write.csv(do.call(rbind, long), file.path(OUT.DIR, paste0("fit_by_race_all_", CALIBRATION.CODE, ".csv")), row.names = FALSE)
