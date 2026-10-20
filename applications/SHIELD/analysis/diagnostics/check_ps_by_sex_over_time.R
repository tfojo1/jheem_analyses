# ****************************************************************************************************
# CHECK PS BY SEX OVER TIME ----
# ****************************************************************************************************
# Does the model get the balance between MSM and heterosexual PS right over time?
# Male PS from 2000 on is mostly MSM, so the male:female split
# is the best data we have on MSM vs het timing.
#
# For each city in one calibration:
# 1. plots of PS diagnoses split by sex (saved by plot.calib.comparison)
# 2. tables of model / data by period, for male PS, female PS and the male:female ratio
#    1 = right, > 1 = model too high, < 1 = model too low
#    male.to.female below 1 = model's male PS too low relative to female (MSM not rising fast enough)
#
# Simsets already in memory (calib.simsets) are reused; anything missing is loaded from file.
#
# USAGE
#   1. set CALIBRATION.CODE (and LOCATIONS if you want fewer cities) below
#   2. from jheem_analyses:  source("applications/SHIELD/analysis/diagnostics/check_ps_by_sex_over_time.R")
#   If step 0 stops with a compiled-code error, re-source shield_source_code.R; if that is not
#   enough, restart R without restoring the workspace and run again.
# ****************************************************************************************************

## SETTINGS ----
CALIBRATION.CODE <- "calib.10.15.stage3"
LOCATIONS        <- NULL                         # NULL = all 10 cities; or e.g. c(Baltimore = "C.12580")
YEARS            <- as.character(1993:2021)      # MSA PS data by sex starts in 1993 (CDC surveillance reports)
PERIODS          <- list("1993-1999" = 1993:1999, "2000-2004" = 2000:2004, "2005-2009" = 2005:2009,
                         "2010-2014" = 2010:2014, "2015-2019" = 2015:2019, "2020-2021" = 2020:2021)
PLOT.YEARS       <- 1990:2025
MAKE.PLOTS       <- TRUE

## SOURCES ----
# only source what is not already loaded, so re-running in the same session is quick
if (!exists("SURVEILLANCE.MANAGER"))  source("../jheem_analyses/applications/SHIELD/shield_source_code.R")
if (!exists("SHIELD.SPECIFICATION"))  source("../jheem_analyses/applications/SHIELD/shield_specification.R")
if (!exists("SHIELD.TEN.MSAS"))       source("../jheem_analyses/commoncode/locations_of_interest.R")
if (!exists("load.calib.simsets"))    source("../jheem_analyses/applications/SHIELD/analysis/calibration/calibration_helper_functions.R")
if (is.null(LOCATIONS)) LOCATIONS <- SHIELD.TEN.MSAS

# output folder for plots and tables (after the sources, since it needs jheem2)
out.dir <- file.path(get.jheem.root.directory(), "shield", "calibration_diagnostics")
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)

## LOAD SIMSETS ----
# load.calib.simsets() finds calib.simsets in the session, reuses those entries and reads only the missing ones from file
calib.simsets <- load.calib.simsets(locations = LOCATIONS, calibration.codes = CALIBRATION.CODE)
entries <- Filter(function(e) e$calib.code == CALIBRATION.CODE, calib.simsets)
if (length(entries) == 0) stop("No simsets loaded for ", CALIBRATION.CODE)

## 0. SESSION CHECK ----
# mapping data to the model's strata uses compiled code; in a session restored from a saved
# workspace its address is empty and every pull fails with "NULL value passed as symbol address"
tryCatch(
    SURVEILLANCE.MANAGER$pull("ps.syphilis.diagnoses", keep.dimensions = c("year", "sex"),
                              dimension.values = list(location = entries[[1]]$location.code, year = YEARS)),
    error = function(e) stop("Ontology mapping fails in this session (", conditionMessage(e), ").\n",
                             "Re-source shield_source_code.R, or restart R without restoring the workspace."))

## 1. PLOTS ----
# PS diagnoses split by sex, one panel per city
if (MAKE.PLOTS)
    plot.calib.comparison(calib.simsets, calibration.codes = CALIBRATION.CODE, locations = LOCATIONS,
                          outcomes = "diagnosis.ps", separate.by = "outcome",
                          split.by = "sex", years = PLOT.YEARS, save.dir = out.dir)

## observed.ps.by.sex ----
# observed PS by sex for one city (years x male/female), averaged over every source that reports it
# read directly rather than with pull(): the CDC surveillance reports (1993-2019) also have an "msm"
# category, and pull() then returns only the county aggregates (2019 on). Male already includes MSM.
observed.ps.by.sex <- function(location) {
    d <- SURVEILLANCE.MANAGER$data$ps.syphilis.diagnoses$estimate
    by.source <- list()
    for (s in names(d)) for (o in names(d[[s]])) {
        a <- d[[s]][[o]]$year__location__sex                                  # year x location x sex
        if (is.null(a) || !location %in% dimnames(a)$location || !all(c("male", "female") %in% dimnames(a)$sex)) next
        m <- matrix(NA_real_, length(YEARS), 2, dimnames = list(YEARS, c("male", "female")))
        y <- intersect(YEARS, dimnames(a)$year)
        m[y, ] <- a[y, location, c("male", "female")]
        by.source[[paste(s, o)]] <- m
    }
    if (length(by.source) == 0) return(NULL)
    out <- apply(simplify2array(by.source), c(1, 2), mean, na.rm = TRUE)    # average over sources
    out[is.nan(out)] <- NA
    out
}

## ps.by.sex ----
# model / data for male PS, female PS and the male:female ratio, by period
# only years where both sexes are observed are used, for model and data alike
ps.by.sex <- function(e) {
    d <- observed.ps.by.sex(e$location.code)
    if (is.null(d)) return(NULL)
    s <- apply(e$full_simset$get("diagnosis.ps", year = YEARS, keep.dimensions = c("year", "sex")),
               c("year", "sex"), median)                                       # median over simulations
    yrs   <- intersect(YEARS, rownames(d))
    model <- cbind(male = s[yrs, "msm"] + s[yrs, "heterosexual_male"], female = s[yrs, "female"])
    obs   <- cbind(male = d[yrs, "male"], female = d[yrs, "female"])
    t(sapply(PERIODS, function(p) {
        y  <- intersect(as.character(p), yrs)
        ok <- y[!is.na(obs[y, "male"]) & !is.na(obs[y, "female"])]
        if (length(ok) == 0) return(c(male = NA, female = NA, male.to.female = NA))
        m <- colSums(model[ok, , drop = FALSE])
        o <- colSums(obs[ok, , drop = FALSE])
        c(male           = m[["male"]] / o[["male"]],
          female         = m[["female"]] / o[["female"]],
          male.to.female = (m[["male"]] / m[["female"]]) / (o[["male"]] / o[["female"]]))
    }))
}

## 2. TABLES ----
# one table per measure: rows = periods, columns = cities; NA = no data with both sexes in that period
res <- lapply(entries, function(e) tryCatch(ps.by.sex(e),
                                            error = function(err) { message(e$location.name, ": ", conditionMessage(err)); NULL }))
names(res) <- sapply(entries, `[[`, "location.name")
res <- Filter(Negate(is.null), res)

ratio.tables <- list()
for (what in c("male.to.female", "male", "female")) {
    tab <- sapply(res, function(r) r[, what])
    rownames(tab) <- names(PERIODS)
    ratio.tables[[what]] <- tab
    cat("\n== PS ", what, ": model / data by period ==\n", sep = "")
    print(round(tab, 2))
}

## SAVE ----
for (what in names(ratio.tables))
    write.csv(ratio.tables[[what]], file.path(out.dir, paste0("ps_by_sex_", what, "_", CALIBRATION.CODE, ".csv")))
