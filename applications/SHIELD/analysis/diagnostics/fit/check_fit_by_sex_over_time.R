# ****************************************************************************************************
# CHECK FIT BY SEX OVER TIME ----
# ****************************************************************************************************
# Model / data ratios of diagnoses for men and women, by period and city, plus two model-side
# summaries that help tell a transmission problem from a diagnosis (detection) problem.
# (This file replaces check_ps_by_sex_over_time.R: its plots are step 1, its tables are the P&S part of step 2.)
#
# For each city in the calibration set in diagnostics_setup.R:
# 1. plots of diagnoses split by sex (saved by plot.calib.comparison)
# 2. for P&S and total diagnoses (and EL, late): model / data by period for men, women, and men:women
#    1 = right, > 1 = model too high, < 1 = model too low
#    men:women = (model men / model women) / (data men / data women); < 1 = model's men too low relative to women
#    each table ends with: median across cities, cities within 0.8-1.2, and (per city) the median across periods
# 3. fitted transmission multipliers at the spline years (posterior medians), MSM, het and the MSM:het ratio
#    does the het multiplier fall after the 1990s, and does MSM:het rise after 2000?
# 4. model incidence and the share of infections diagnosed at P&S, by sex group, by period
#    if incidence follows the expected shape but the diagnosed share is off, the problem is detection
#    (care-seeking, screening, symptoms); if incidence itself is off, it is transmission.
#
# Data by sex for the 10 MSAs (syphilis-manager-v2026.10.09):
#   P&S: CDC surveillance reports 1993-2019 + county aggregates 2019-2023
#   total, EL, late: county aggregates 2019-2023 only
#   so total/EL/late tables have values only for 2019-2021 and 2022-2023 (2022-2023 is after the
#   calibration period: an out-of-sample check)
# "male" in the data is all men (MSM included); model men = msm + heterosexual_male.
#
# Simsets already in memory (calib.simsets) are reused; anything missing is loaded from file.
#
# USAGE
#   1. set CALIBRATION.CODE (and LOCATIONS) in diagnostics_setup.R
#   2. from jheem_analyses:  source("applications/SHIELD/analysis/diagnostics/fit/check_fit_by_sex_over_time.R")
#   3. outputs (CSV) go to shield/calibration_diagnostics/, including one long file for sharing:
#      fit_by_sex_all_<code>.csv (outcome, measure, period, city, value)
#   If step 0 stops with a compiled-code error, re-source shield_source_code.R; if that is not
#   enough, restart R without restoring the workspace and run again.
# ****************************************************************************************************

## SOURCES ----
source("../jheem_analyses/applications/SHIELD/analysis/diagnostics/diagnostics_setup.R")

## SETTINGS ----
PERIODS          <- list("1993-1999" = 1993:1999, "2000-2004" = 2000:2004, "2005-2009" = 2005:2009,
                         "2010-2014" = 2010:2014, "2015-2018" = 2015:2018, "2019-2021" = 2019:2021,
                         "2022-2023" = 2022:2023)  # 2022-2023: after the calibration period
CALIBRATION.PERIODS <- names(PERIODS)[1:6]        # periods used for the median across periods
OUTCOMES <- c(diagnosis.ps                 = "ps.syphilis.diagnoses",          # model outcome = data outcome
              diagnosis.total              = "total.syphilis.diagnoses",
              diagnosis.el.misclassified   = "early.syphilis.diagnoses",
              diagnosis.late.misclassified = "unknown.duration.or.late.syphilis.diagnoses")
SPLINE.YEARS <- c("1970", "1990", "1995", "2000", "2010", "2022")
YEARS        <- as.character(min(unlist(PERIODS)):max(unlist(PERIODS)))
MAKE.PLOTS    <- TRUE
PLOT.OUTCOMES <- "diagnosis.ps"                   # can add any of names(OUTCOMES)
PLOT.YEARS    <- 1990:2025

## LOAD SIMSETS ----
calib.simsets <- load.calib.simsets(locations = LOCATIONS, calibration.codes = CALIBRATION.CODE)
entries <- Filter(function(e) e$calib.code == CALIBRATION.CODE, calib.simsets)
if (length(entries) == 0) stop("No simsets loaded for ", CALIBRATION.CODE)
names(entries) <- sapply(entries, `[[`, "location.name")

## 0. SESSION CHECK ----
check.session(entries[[1]]$location.code)

## 1. PLOTS ----
# diagnoses split by sex, one panel per city
if (MAKE.PLOTS)
    plot.calib.comparison(calib.simsets, calibration.codes = CALIBRATION.CODE, locations = LOCATIONS,
                          outcomes = PLOT.OUTCOMES, separate.by = "outcome",
                          split.by = "sex", years = PLOT.YEARS, save.dir = OUT.DIR)

## model.by.sex ----
# model diagnoses (median over simulations) by year for men (msm + heterosexual_male) and women
model.by.sex <- function(simset, sim.outcome) {
    s <- apply(simset$get(sim.outcome, year = YEARS, keep.dimensions = c("year", "sex")), c("year", "sex"), median)
    cbind(male = s[YEARS, "msm"] + s[YEARS, "heterosexual_male"], female = s[YEARS, "female"])
}

## ratios.by.period ----
# model / data for men, women and men:women in each period, using only years where both sexes are observed
ratios.by.period <- function(model, obs) {
    t(sapply(PERIODS, function(p) {
        y  <- intersect(as.character(p), rownames(obs))
        ok <- y[!is.na(obs[y, "male"]) & !is.na(obs[y, "female"])]
        if (length(ok) == 0) return(c(men = NA, women = NA, men.to.women = NA))
        m <- colSums(model[ok, , drop = FALSE]); o <- colSums(obs[ok, , drop = FALSE])
        c(men          = m[["male"]] / o[["male"]],
          women        = m[["female"]] / o[["female"]],
          men.to.women = (m[["male"]] / m[["female"]]) / (o[["male"]] / o[["female"]]))
    }))
}

## add.summaries ----
# periods x cities  ->  adds columns median and in.0.8.to.1.2 (across cities),
# and a row with each city's median across the calibration periods
add.summaries <- function(tab) {
    tab <- rbind(tab, "median over periods" = apply(tab[CALIBRATION.PERIODS, , drop = FALSE], 2, median, na.rm = TRUE))
    cbind(tab,
          median         = apply(tab, 1, median, na.rm = TRUE),
          in.0.8.to.1.2  = apply(tab, 1, function(r) sum(r >= 0.8 & r <= 1.2, na.rm = TRUE)))
}

## 2. MODEL / DATA BY SEX ----
long <- list()
for (out in names(OUTCOMES)) {
    res <- lapply(entries, function(e) tryCatch({
        obs <- observed.by.sex(OUTCOMES[[out]], e$location.code, years = YEARS)
        if (is.null(obs)) NULL else ratios.by.period(model.by.sex(e$full_simset, out), obs)
    }, error = function(err) { message(e$location.name, " / ", out, ": ", conditionMessage(err)); NULL }))
    res <- Filter(Negate(is.null), res)
    if (length(res) == 0) next
    for (what in c("men", "women", "men.to.women")) {
        tab <- sapply(res, function(r) r[, what]); rownames(tab) <- names(PERIODS)
        tab <- add.summaries(tab)
        cat("\n== ", out, " | ", what, " | model / data ==\n", sep = "")
        print(round(tab, 2))
        write.csv(tab, file.path(OUT.DIR, paste0("fit_by_sex_", out, "_", what, "_", CALIBRATION.CODE, ".csv")))
        cities <- setdiff(colnames(tab), c("median", "in.0.8.to.1.2"))
        long[[paste(out, what)]] <- data.frame(outcome = out, measure = what,
                                               period = rep(rownames(tab), times = length(cities)),
                                               city = rep(cities, each = nrow(tab)),
                                               value = as.numeric(tab[, cities]))
    }
}
write.csv(do.call(rbind, long), file.path(OUT.DIR, paste0("fit_by_sex_all_", CALIBRATION.CODE, ".csv")), row.names = FALSE)

## 3. FITTED TRANSMISSION MULTIPLIERS BY SPLINE YEAR ----
# posterior medians per city; msm.to.het = MSM multiplier / het multiplier
trate <- lapply(c("msm", "heterosexual"), function(g)
    sapply(entries, function(e) {
        P <- e$full_simset$parameters
        apply(P[paste0("transmission.rate.multiplier.", g, SPLINE.YEARS), , drop = FALSE], 1, median)
    }))
names(trate) <- c("msm", "het")
for (g in names(trate)) rownames(trate[[g]]) <- SPLINE.YEARS
trate$msm.to.het <- trate$msm / trate$het
for (g in names(trate)) {
    cat("\n== Fitted transmission multiplier, ", g, " (posterior median) ==\n", sep = "")
    print(round(cbind(trate[[g]], median = apply(trate[[g]], 1, median)), 2))
    write.csv(trate[[g]], file.path(OUT.DIR, paste0("trate_multiplier_", g, "_", CALIBRATION.CODE, ".csv")))
}

## 4. INCIDENCE AND SHARE DIAGNOSED AT P&S, BY SEX GROUP ----
# incidence = new infections per period; diagnosed.share = P&S diagnoses / incidence (same period)
groups <- c("msm", "heterosexual_male", "female")
by.group <- lapply(entries, function(e) tryCatch({
    inc <- apply(e$full_simset$get("incidence",    year = YEARS, keep.dimensions = c("year", "sex")), c("year", "sex"), median)
    ps  <- apply(e$full_simset$get("diagnosis.ps", year = YEARS, keep.dimensions = c("year", "sex")), c("year", "sex"), median)
    list(incidence       = t(sapply(PERIODS, function(p) colSums(inc[as.character(p), groups, drop = FALSE]))),
         diagnosed.share = t(sapply(PERIODS, function(p) colSums(ps[as.character(p), groups, drop = FALSE]) /
                                                         colSums(inc[as.character(p), groups, drop = FALSE]))))
}, error = function(err) { message(e$location.name, " / incidence: ", conditionMessage(err)); NULL }))
by.group <- Filter(Negate(is.null), by.group)
for (what in c("incidence", "diagnosed.share")) for (g in groups) {
    tab <- sapply(by.group, function(x) x[[what]][, g])
    rownames(tab) <- names(PERIODS)
    cat("\n== Model ", what, " | ", g, " ==\n", sep = "")
    print(round(cbind(tab, median = apply(tab, 1, median)), if (what == "incidence") 0 else 3))
    write.csv(tab, file.path(OUT.DIR, paste0("model_", what, "_", g, "_", CALIBRATION.CODE, ".csv")))
}
