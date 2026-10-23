# ****************************************************************************************************
# CHECK WHERE THE FITTED PARAMETERS SIT IN THEIR PRIOR ----
# ****************************************************************************************************
# For each city in one or more calibrations:
# 1. where each fitted parameter sits in its prior (percentile of the posterior median among prior draws)
#    0 = bottom of the prior, 1 = top; below 2.5% or above 97.5% = pressed against the prior
# 2. the same for the transmission changes between spline years: MSM, het, and MSM relative to het
# (The P&S:EL check that used to be in this file is now fit/check_ps_el_split.R.)
#
# Simsets already in memory (calib.simsets) are reused; anything missing is loaded from file.
#
# USAGE
#   1. set CALIBRATION.CODE (and LOCATIONS) in diagnostics_setup.R
#      to compare several calibrations, add them to CALIBRATION.CODES below
#   2. from jheem_analyses:  source("applications/SHIELD/analysis/diagnostics/prior/check_prior_position.R")
# ****************************************************************************************************

## SOURCES ----
source("../jheem_analyses/applications/SHIELD/analysis/diagnostics/diagnostics_setup.R")

## SETTINGS ----
CALIBRATION.CODES <- CALIBRATION.CODE           # one or more codes, e.g. c(CALIBRATION.CODE, "calib.10.15.stage3")
FLAG              <- c(0.025, 0.975)            # outside this = pressed against the prior
N.PRIOR.DRAWS     <- 10000

## LOAD SIMSETS ----
# load.calib.simsets() finds calib.simsets in the session, reuses those entries and reads only the missing ones from file
calib.simsets <- load.calib.simsets(locations         = LOCATIONS,
                                    calibration.codes = CALIBRATION.CODES)

## PRIOR DRAWS ----
# draws from the full prior, one row per draw, one column per parameter
prior.names <- SHIELD.FULL.PARAMETERS.PRIOR@var.names
prior.draws <- generate.random.samples(SHIELD.FULL.PARAMETERS.PRIOR, n = N.PRIOR.DRAWS)
if (nrow(prior.draws) == length(prior.names) && ncol(prior.draws) != length(prior.names)) prior.draws <- t(prior.draws)
if (is.null(colnames(prior.draws))) colnames(prior.draws) <- prior.names

## percentile ----
# share of reference values at or below x
percentile <- function(x, ref) mean(ref <= x, na.rm = TRUE)

## transmission.changes ----
# log change in the MSM and het transmission multipliers between consecutive spline years
# get(name) returns one value per prior draw or per simulation
SPLINE.TIMES <- c("1970", "1990", "1995", "2000", "2010", "2022")
transmission.changes <- function(get) {
    out <- list()
    for (k in 2:length(SPLINE.TIMES)) {
        step  <- paste0(SPLINE.TIMES[k - 1], "-", SPLINE.TIMES[k])
        d.msm <- log(get(paste0("transmission.rate.multiplier.msm", SPLINE.TIMES[k]))) -
            log(get(paste0("transmission.rate.multiplier.msm", SPLINE.TIMES[k - 1])))
        d.het <- log(get(paste0("transmission.rate.multiplier.heterosexual", SPLINE.TIMES[k]))) -
            log(get(paste0("transmission.rate.multiplier.heterosexual", SPLINE.TIMES[k - 1])))
        out[[paste("change: msm", step)]]     <- d.msm
        out[[paste("change: het", step)]]     <- d.het
        out[[paste("change: msm-het", step)]] <- d.msm - d.het
    }
    out
}
prior.changes <- transmission.changes(function(n) prior.draws[, n])

## check.one.simset ----
# prior percentiles for one city's simset
check.one.simset <- function(simset) {
    params <- simset$parameters                       # parameters x simulations

    # 1. each calibrated parameter
    common <- intersect(rownames(params), colnames(prior.draws))
    p.params <- sapply(common, function(n) percentile(median(params[n, ]), prior.draws[, n]))

    # 2. transmission changes between spline years
    fit.changes <- transmission.changes(function(n) params[n, ])
    p.changes <- sapply(names(prior.changes), function(n) percentile(median(fit.changes[[n]]), prior.changes[[n]]))

    c(p.params, p.changes)
}

## RUN ----
# one table per calibration code; a city that fails is reported and skipped
for (code in CALIBRATION.CODES) {
    cat("\n**************** ", code, " ****************\n")
    entries <- Filter(function(e) e$calib.code == code, calib.simsets)
    checks  <- list()
    for (e in entries) {
        checks[[e$location.name]] <- tryCatch(check.one.simset(e$full_simset),
                                              error = function(err) { cat("SKIPPED", e$location.name, ":", conditionMessage(err), "\n"); NULL })
    }
    checks <- Filter(Negate(is.null), checks)
    if (length(checks) == 0) next

    # prior table: rows = parameters, columns = cities; only rows pressed against the prior in at least one city
    pct.table <- sapply(checks, function(x) x[names(checks[[1]])])
    n.flagged <- rowSums(pct.table < FLAG[1] | pct.table > FLAG[2], na.rm = TRUE)
    cat("\n== Pressed against the prior (percentile of posterior median; 0 = bottom, 1 = top) ==\n")
    print(round(pct.table[n.flagged > 0, , drop = FALSE][order(-n.flagged[n.flagged > 0]), , drop = FALSE], 3))

    # flag table: same rows, showing only the flagged cells
    # LOW = below FLAG[1], HIGH = above FLAG[2], blank = inside the prior's range
    flag.table <- ifelse(pct.table < FLAG[1], "LOW", ifelse(pct.table > FLAG[2], "HIGH", ""))
    flag.table <- cbind(flag.table,
                        n.low  = rowSums(flag.table == "LOW",  na.rm = TRUE),
                        n.high = rowSums(flag.table == "HIGH", na.rm = TRUE))
    flag.table <- flag.table[n.flagged > 0, , drop = FALSE][order(-n.flagged[n.flagged > 0]), , drop = FALSE]
    cat("\n== Flagged cells only (LOW < ", FLAG[1], ", HIGH > ", FLAG[2], ") ==\n", sep = "")
    print(noquote(flag.table))

    write.csv(pct.table,  file.path(out.dir(code), paste0("prior_percentiles_", code, ".csv")))
    write.csv(flag.table, file.path(out.dir(code), paste0("prior_flags_", code, ".csv")))
}
