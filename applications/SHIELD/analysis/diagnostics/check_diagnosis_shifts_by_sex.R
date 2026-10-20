# ****************************************************************************************************
# CHECK DIAGNOSIS SHIFTS BY SEX ----
# ****************************************************************************************************
# How much did male and female diagnoses change in each period, and did men move away from women?
# Use it to check whether the spline points of a time multiplier (1970, 1990, 1995, 2000, 2010, 2022)
# can follow the shape of the data. Data only: no simsets needed.
#
# For each city and period (start year -> end year):
# 1. male.fold, female.fold = end / start for each sex
#    e.g. male 100 -> 300 gives 3; female 100 -> 100 gives 1
# 2. male.log, female.log = log(end / start)
#    0 = no change, +0.5 = about 65% rise, -0.5 = about 40% fall
# 3. relative = male.fold / female.fold = how much the male:female ratio changed
#    1 = the sexes moved together, 3 = men rose 3x relative to women, < 1 = women rose faster
# 4. for 10-year periods only, first.half.share = the part of the 10-year change in the
#    male:female ratio (log scale) that happened in the first 5 years
#    0.5 = steady change, which one linear step of the spline can draw
#    near 1 = almost all in the first half (sharp 2000-2005, flat 2005-2010), which needs a point in the middle
#    near 0 = almost all in the second half
#    NA when the 10-year change is under 10%, too small to split
#
# Start and end values are averaged over the years around them (YEAR.WINDOW) to damp year-to-year noise.
# e.g. YEAR.WINDOW = 1: the 2000 value is the mean of 1999, 2000 and 2001.
# Only years where both sexes are observed are used.
#
# USAGE
#   1. set OUTCOMES, LOCATIONS and the periods below
#   2. from jheem_analyses:  source("applications/SHIELD/analysis/diagnostics/check_diagnosis_shifts_by_sex.R")
# ****************************************************************************************************

## SETTINGS ----
OUTCOMES       <- c("ps.syphilis.diagnoses")     # can add "early.syphilis.diagnoses", "total.syphilis.diagnoses"
LOCATIONS      <- NULL                           # NULL = US + the 10 cities; or e.g. c(Baltimore = "C.12580")
YEAR.WINDOW    <- 1                              # 1 = mean of year-1..year+1 at each end; 0 = single years
PERIODS.5      <- list(c(1990, 1995), c(1995, 2000), c(2000, 2005), c(2005, 2010), c(2010, 2015), c(2015, 2020))
PERIODS.10     <- list(c(1990, 2000), c(1995, 2005), c(2000, 2010), c(2005, 2015), c(2010, 2020))
PERIODS.SPLINE <- list(c(1990, 1995), c(1995, 2000), c(2000, 2010), c(2010, 2022))  # steps between spline points

## SOURCES ----
# only source what is not already loaded, so re-running in the same session is quick
if (!exists("SURVEILLANCE.MANAGER")) source("../jheem_analyses/applications/SHIELD/shield_source_code.R")
if (!exists("SHIELD.TEN.MSAS"))      source("../jheem_analyses/commoncode/locations_of_interest.R")
if (is.null(LOCATIONS)) LOCATIONS <- c(US = "US", SHIELD.TEN.MSAS)

# output folder for tables (after the sources, since it needs jheem2)
out.dir <- file.path(get.jheem.root.directory(), "shield", "calibration_diagnostics")
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)

## observed.by.sex ----
# observed diagnoses by sex for one outcome and city (years x male/female), averaged over every source
# read directly rather than with pull(): the CDC surveillance reports also have an "msm" category,
# and pull() then returns only the county aggregates (2019 on). Male already includes MSM.
observed.by.sex <- function(outcome, location) {
    d <- SURVEILLANCE.MANAGER$data[[outcome]]$estimate
    if (is.null(d)) stop("No data in SURVEILLANCE.MANAGER for ", outcome)
    years <- as.character(1980:2025)
    by.source <- list()
    for (s in names(d)) for (o in names(d[[s]])) {
        a <- d[[s]][[o]]$year__location__sex                                  # year x location x sex
        if (is.null(a) || !location %in% dimnames(a)$location || !all(c("male", "female") %in% dimnames(a)$sex)) next
        m <- matrix(NA_real_, length(years), 2, dimnames = list(years, c("male", "female")))
        y <- intersect(years, dimnames(a)$year)
        m[y, ] <- a[y, location, c("male", "female")]
        by.source[[paste(s, o)]] <- m
    }
    if (length(by.source) == 0) return(NULL)
    out <- apply(simplify2array(by.source), c(1, 2), mean, na.rm = TRUE)    # average over sources
    out[is.nan(out)] <- NA
    out
}

## value.at ----
# male and female value at one year: mean over the window, using only years with both sexes observed
value.at <- function(d, year) {
    y  <- intersect(as.character((year - YEAR.WINDOW):(year + YEAR.WINDOW)), rownames(d))
    ok <- y[!is.na(d[y, "male"]) & !is.na(d[y, "female"])]
    if (length(ok) == 0) return(c(male = NA, female = NA))
    colMeans(d[ok, , drop = FALSE])
}

## shift ----
# change from start to end for each sex, and for the male:female ratio
shift <- function(d, start, end) {
    s    <- value.at(d, start)
    e    <- value.at(d, end)
    fold <- e / s
    c(male.start   = s[["male"]],    male.end   = e[["male"]],
      female.start = s[["female"]],  female.end = e[["female"]],
      male.fold    = fold[["male"]], female.fold = fold[["female"]],
      male.log     = log(fold[["male"]]), female.log = log(fold[["female"]]),
      mf.start     = s[["male"]] / s[["female"]], mf.end = e[["male"]] / e[["female"]],
      relative     = fold[["male"]] / fold[["female"]])
}

## 1. SHIFTS ----
# one row per outcome x city x period
PERIOD.SETS <- list("5-year" = PERIODS.5, "10-year" = PERIODS.10, "spline" = PERIODS.SPLINE)
rows <- list()
for (outcome in OUTCOMES) for (loc.name in names(LOCATIONS)) {
    d <- observed.by.sex(outcome, LOCATIONS[[loc.name]])
    if (is.null(d)) { message(loc.name, ": no ", outcome, " data by sex"); next }
    for (set in names(PERIOD.SETS)) for (p in PERIOD.SETS[[set]]) {
        r <- shift(d, p[1], p[2])
        # 10-year periods: how much of the change in the male:female ratio came in the first 5 years
        share <- NA
        if (set == "10-year") {
            first <- shift(d, p[1], p[1] + 5)[["relative"]]
            if (!is.na(r[["relative"]]) && abs(log(r[["relative"]])) >= log(1.1))
                share <- log(first) / log(r[["relative"]])
        }
        rows[[length(rows) + 1]] <- data.frame(outcome = outcome, location = loc.name, set = set,
                                               period = paste0(p[1], "-", p[2]), t(r),
                                               first.half.share = share)
    }
}
shifts <- do.call(rbind, rows)

## 2. TABLES ----
# rows = periods, columns = cities; NA = no data with both sexes at the start or end
show <- function(outcome, set, what) {
    x   <- shifts[shifts$outcome == outcome & shifts$set == set, ]
    tab <- tapply(x[[what]], list(factor(x$period, unique(x$period)), factor(x$location, unique(x$location))), identity)
    cat("\n== ", outcome, ", ", set, " periods: ", what, " ==\n", sep = "")
    print(round(tab, 2))
}
for (outcome in OUTCOMES) for (set in names(PERIOD.SETS)) {
    for (what in c("male.fold", "female.fold", "relative")) show(outcome, set, what)
    if (set == "10-year") show(outcome, set, "first.half.share")
}

## SAVE ----
write.csv(shifts, file.path(out.dir, "diagnosis_shifts_by_sex.csv"), row.names = FALSE)
cat("\nSaved to", file.path(out.dir, "diagnosis_shifts_by_sex.csv"), "\n")
