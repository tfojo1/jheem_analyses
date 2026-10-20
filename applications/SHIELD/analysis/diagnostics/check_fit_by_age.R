# ****************************************************************************************************
# CHECK FIT BY AGE ----
# ****************************************************************************************************
# Does the model over- or under-predict diagnoses at the youngest and oldest ages?
# Use it before changing the age priors: a multiplier sitting far below its prior mean only
# matters if the fit at that age is off.
#
# For each city in one calibration:
# 1. plots of each diagnosis outcome by age (saved by plot.calib.comparison)
# 2. a table of model / data by age group, 2019-2021
#    1 = right, > 1 = model too high, < 1 = model too low
#
# Simsets already in memory (calib.simsets) are reused; anything missing is loaded from file.
#
# USAGE
#   1. set CALIBRATION.CODE (and LOCATIONS if you want fewer cities) below
#   2. from jheem_analyses:  source("applications/SHIELD/analysis/diagnostics/check_fit_by_age.R")
#   If step 0 stops with a compiled-code error, re-source shield_source_code.R; if that is not
#   enough, restart R without restoring the workspace and run again.
# ****************************************************************************************************

## SETTINGS ----
CALIBRATION.CODE <- "calib.10.15.stage3"
LOCATIONS        <- NULL                         # NULL = all 10 cities; or e.g. c(Baltimore = "C.12580")
YEARS            <- as.character(2019:2021)      # MSA data by age exists only for these years
PLOT.YEARS       <- 2010:2025
MAKE.PLOTS       <- T
OUTCOMES <- c(diagnosis.total              = "total.syphilis.diagnoses",          # model outcome = data outcome
              diagnosis.ps                 = "ps.syphilis.diagnoses",
              diagnosis.el.misclassified   = "early.syphilis.diagnoses",
              diagnosis.late.misclassified = "unknown.duration.or.late.syphilis.diagnoses")
out.dir <- file.path(get.jheem.root.directory(), "shield", "calibration_diagnostics")
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)

## SOURCES ----
# only source what is not already loaded, so re-running in the same session is quick
if (!exists("SURVEILLANCE.MANAGER"))  source("../jheem_analyses/applications/SHIELD/shield_source_code.R")
if (!exists("SHIELD.SPECIFICATION"))  source("../jheem_analyses/applications/SHIELD/shield_specification.R")
if (!exists("SHIELD.TEN.MSAS"))       source("../jheem_analyses/commoncode/locations_of_interest.R")
if (!exists("load.calib.simsets"))    source("../jheem_analyses/applications/SHIELD/analysis/calibration/calibration_helper_functions.R")
if (is.null(LOCATIONS)) LOCATIONS <- SHIELD.TEN.MSAS

## LOAD SIMSETS ----
# load.calib.simsets() finds calib.simsets in the session, reuses those entries and reads only the missing ones from file
calib.simsets <- load.calib.simsets(locations = LOCATIONS, calibration.codes = CALIBRATION.CODE)
entries <- Filter(function(e) e$calib.code == CALIBRATION.CODE, calib.simsets)
if (length(entries) == 0) stop("No simsets loaded for ", CALIBRATION.CODE)

## 0. SESSION CHECK ----
# mapping data to the model's age groups uses compiled code; in a session restored from a saved
# workspace its address is empty and every pull fails with "NULL value passed as symbol address"
tryCatch(
    SURVEILLANCE.MANAGER$pull(OUTCOMES[[1]], keep.dimensions = c("year", "age"),
                              dimension.values = list(location = entries[[1]]$location.code, year = YEARS)),
    error = function(e) stop("Ontology mapping fails in this session (", conditionMessage(e), ").\n",
                             "Re-source shield_source_code.R, or restart R without restoring the workspace."))

## AGE BINS ----
# model and CDC data use different age groups; both nest into these bins by their lower age
BINS      <- c(0, 15, 20, 25, 35, 45, 55, 65, Inf)
BIN.NAMES <- c("0-14", "15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65+")
to.bin  <- function(age) as.character(cut(as.numeric(sub("^(\\d+).*", "\\1", age)), BINS, BIN.NAMES, right = FALSE))
bin.sum <- function(m) t(apply(m, 1, function(r) tapply(r, to.bin(names(r)), sum)))   # year x age  ->  year x bin

## 1. PLOTS & SAVE ----
# one figure per outcome, one panel per age; saved under calibrationPlots/comparison/<code>/by_outcome
if (MAKE.PLOTS)
    plot.calib.comparison(calib.simsets, calibration.codes = CALIBRATION.CODE, locations = LOCATIONS,
                          save.dir = out.dir,
                          outcomes = names(OUTCOMES), separate.by = "outcome",
                          facet.by = "age", years = PLOT.YEARS)

## ratio.by.age ----
# model / data by age bin for one city and outcome, summed over the years that have data in that bin
ratio.by.age <- function(e, sim.outcome, data.outcome) {
    d <- SURVEILLANCE.MANAGER$pull(data.outcome, keep.dimensions = c("year", "age"),
                                   dimension.values = list(location = e$location.code, year = YEARS))
    if (is.null(d)) return(NULL)
    d <- apply(d, c("year", "age"), mean, na.rm = TRUE)                     # average over sources
    d[is.nan(d)] <- NA
    d.yb <- bin.sum(d)
    s    <- e$full_simset$get(sim.outcome, year = YEARS, keep.dimensions = c("year", "age"))
    s.yb <- bin.sum(apply(s, c("year", "age"), median))[rownames(d.yb), colnames(d.yb), drop = FALSE]
    ok   <- !is.na(d.yb)
    colSums(ifelse(ok, s.yb, 0)) / colSums(ifelse(ok, d.yb, 0))
}

## 2. TABLE ----
# one table per outcome: rows = age bins, columns = cities; NA = no data for that bin
ratio.tables <- list()
for (out in names(OUTCOMES)) {
    res <- lapply(entries, function(e) tryCatch(ratio.by.age(e, out, OUTCOMES[[out]]),
                                                error = function(err) { message(e$location.name, ": ", conditionMessage(err)); NULL }))
    names(res) <- sapply(entries, `[[`, "location.name")
    res <- Filter(Negate(is.null), res)
    if (length(res) == 0) next
    tab <- sapply(res, function(r) r[BIN.NAMES])
    rownames(tab) <- BIN.NAMES
    ratio.tables[[out]] <- tab
    cat("\n== ", out, ": model / data by age, ", min(YEARS), "-", max(YEARS), " ==\n", sep = "")
    print(round(tab, 2))
}
# SAVE:
for (out in names(ratio.tables))
    write.csv(ratio.tables[[out]], file.path(out.dir, paste0("fit_by_age_", out, "_", CALIBRATION.CODE, ".csv")))


# Reading a value
# value	meaning
# 1.0	the model matches the data
# 1.5	the model predicts 50% more cases than observed
# 0.67	the model predicts two-thirds of the observed cases
# about 0.8–1.2	close enough; sources disagree by around that much
# Inf	the data has 0 cases there and the model has some; ignore it
# NaN	both are 0
# 0	the model has none and the data has some, e.g. EL at 0–14, where the model does no screening
# NA	no data for that age group in that city