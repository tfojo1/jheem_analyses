# ****************************************************************************************************
# 01b GENERATE CUSTOM OUTCOMES ----
# ****************************************************************************************************
# Step 01b of the workflow listed in 00_settings.R. Run it after 01a_generate_raw_results_arrays.R.
#
# WHAT IT DOES
#   1. Reads each raw array from step 01a (<strat>_raw_results.Rdata).
#   2. Builds the custom outcomes from it: rates, cumulative cases, and the change against two
#      references (year0 and no intervention).
#   3. Saves them as <strat>_calc_results.Rdata, next to the raw array.
#
#   The calc array has the same dimensions as the raw one, from YEAR0 onward:
#     year x [stratum] x sim x intervention x location x outcome
#
# SETTINGS
#   1. CALC.STRATA (below): which raw arrays to process. Only those calc files are written, so
#      the others are left alone.
#   2. YEAR0, NOINT and CALIB.NAME come from 00_settings.R.
#
# NAMING RULE: <summary>_<quantity>_vs_<reference>
#   1. quantity     incidence, diagnosis_total, diagnosis_ps (the counts, in the raw array)
#                   <q>_rate    per 100,000 population of that stratum
#                   cum_<q>     cumulative from YEAR0, with YEAR0 included
#   2. summary      no prefix     the value itself
#                   abs_change_   x - reference, in the same units as the quantity
#                   pct_change_   100 * (x - reference) / reference
#   3. reference    vs_year0      the same intervention in YEAR0
#                   vs_noint      the no-intervention scenario in the same year
#
#   Change is always x minus the reference, so a negative change means fewer cases.
#
# THE FOUR SETS OF OUTCOMES (each built for incidence, diagnosis_total and diagnosis_ps)
#   Worked example: Atlanta, incidence. 2022: 6,098 cases. 2030: 9,117 cases with no
#   intervention and 6,117 at 30% coverage. Population 6,606,000 in 2030.
#
#   1. LEVELS: the epidemic in one scenario, with no comparison.
#        <q>_rate      2030, no intervention: 100,000 * 9,117 / 6,606,000 = 138 per 100,000
#        cum_<q>       2022 + 2023 + ... + that year
#
#   2. CHANGE VS YEAR0: how the epidemic has moved since YEAR0, within one scenario.
#      With no intervention this is the epidemic's own trend.
#        abs_change_<q>_vs_year0         2030, no intervention: 9,117 - 6,098 = +3,019 cases
#        pct_change_<q>_vs_year0         100 * 3,019 / 6,098 = +49.5%
#        abs_change_<q>_rate_vs_year0    the same, on the rate
#        pct_change_<q>_rate_vs_year0    the same, on the rate. It differs from the count version
#                                        because the population grows.
#      Doxy-PEP starts after YEAR0, so every intervention has the same YEAR0 value, and all of
#      these are 0 in YEAR0.
#
#   3. CHANGE VS NO INTERVENTION: the effect of the intervention, in the same year.
#        abs_change_<q>_vs_noint         2030, 30% coverage: 6,117 - 9,117 = -3,000 cases
#        pct_change_<q>_vs_noint         100 * -3,000 / 9,117 = -32.9%
#        abs_change_<q>_rate_vs_noint    -3,000 per 6,606,000 = -45.4 per 100,000
#        abs_change_cum_<q>_vs_noint     the same on cumulative cases: the total effect from YEAR0
#        pct_change_cum_<q>_vs_noint     to that year
#      All of these are 0 in the noint slice. There is no pct change for the rate: doxy-PEP does
#      not change the population, so it equals pct_change_<q>_vs_noint.
#
#   4. OTHER
#        diagnosis_ps_per_incidence   P&S diagnoses per incident infection, same year and
#                                     scenario. A detection measure: 1,850 / 9,117 = 0.20.
#        diagnosis_el_per_incidence   the same for early latent. Only built when the raw array
#                                     has diagnosis.el.misclassified.
#        cum_doxy_coverage            doxy-PEP person-years from YEAR0. Total level only.
#        share_incidence              each stratum's % of all incidence. Stratified arrays only.
#                                     Sums to 100 across the strata.
#        share_incidence_among_male   each male group's % of male incidence. Sex array only.
#                                     Women are NA.
#
# SUMMARISING
#   Every outcome is computed within each sim. Summarise it as the median of that outcome.
#   Do not rebuild it from the medians of its parts: a median of differences is not a difference
#   of medians.
# ****************************************************************************************************

source('../jheem_analyses/applications/SHIELD/analysis/intervention/00_settings.R')


# ---- SETTINGS FOR THIS STEP ----
# Which raw arrays to process: any of the ones step 01a built ("total", "sex", "race", "age")
CALC.STRATA <- c("total", "sex","race")

# The quantities: the name used in the outcome names = the outcome name in the raw arrays
QUANTITIES <- c(incidence       = "incidence",
                diagnosis_total = "diagnosis.total",
                diagnosis_ps    = "diagnosis.ps")


# ****************************************************************************************************
# HELPERS ----
# ****************************************************************************************************
# Every helper takes and returns an array with named dimensions, and finds dimensions by name,
# never by position.

## subset_dim ----
# Keeps the given values of one dimension. Every dimension is kept, even if it has one value.
subset_dim <- function(x, dim.name, values) {
    idx <- rep(list(TRUE), length(dim(x)))
    idx[[match(dim.name, names(dimnames(x)))]] <- values
    do.call(`[`, c(list(x), idx, list(drop = FALSE)))
}

## drop_dim ----
# Takes one value of a dimension and removes that dimension.
drop_dim <- function(x, dim.name, value) {
    x    <- subset_dim(x, dim.name, value)
    keep <- names(dimnames(x)) != dim.name
    array(x, dim(x)[keep], dimnames(x)[keep])
}

## change ----
# The change of x against one slice of x, which is the reference:
#   change(x, "year", "2022", "abs")             x - x[2022], within each intervention
#   change(x, "intervention", "noint", "pct")    100 * (x - x[noint]) / x[noint], within each year
# The reference slice itself comes out as 0. A reference of 0 gives NA for pct, not Inf.
change <- function(x, ref.dim, ref.value, type = c("abs", "pct")) {
    type   <- match.arg(type)
    ref    <- drop_dim(x, ref.dim, ref.value)
    others <- which(names(dimnames(x)) != ref.dim)
    diff   <- sweep(x, others, ref, "-")
    if (type == "abs") return(diff)
    ref[ref == 0] <- NA
    100 * sweep(diff, others, ref, "/")
}

## cumulate ----
# Running total along year, which must be the first dimension. The first year is included, so the
# cumulative value in YEAR0 equals that year's value.
cumulate <- function(x) {
    stopifnot(names(dimnames(x))[1] == "year")
    # with year first, each column of this matrix is one series over the years
    m <- apply(matrix(x, nrow = dim(x)[1]), 2, cumsum)
    array(m, dim(x), dimnames(x))
}

## share ----
# Each stratum's % of the total over `groups`, within each year / sim / intervention / location.
# Strata outside `groups` are NA: they are not part of that total.
#   msm = 500, heterosexual_male = 200, female = 300
#   share(x, "sex")                                  msm 50, heterosexual_male 20, female 30
#   share(x, "sex", c("msm", "heterosexual_male"))   msm 71.4, heterosexual_male 28.6, female NA
share <- function(x, strat, groups = dimnames(x)[[strat]]) {
    strat.pos <- match(strat, names(dimnames(x)))
    others    <- seq_along(dim(x))[-strat.pos]
    total     <- apply(subset_dim(x, strat, groups), others, sum)
    total[total == 0] <- NA
    out <- 100 * sweep(x, others, total, "/")
    out[!(dimnames(x)[[strat]][slice.index(x, strat.pos)] %in% groups)] <- NA
    out
}

## read_rdata ----
# Loads one saved object and returns it, without leaving a copy in the workspace.
read_rdata <- function(file) get(load(file))


# ****************************************************************************************************
# BUILD THE CUSTOM OUTCOMES ----
# ****************************************************************************************************
## build_custom_outcomes ----
# raw: one raw array from step 01a. Returns the calc array, from YEAR0 onward:
#   year x [stratum] x sim x intervention x location x outcome
build_custom_outcomes <- function(raw) {

    # 1. Set up: the stratum dimension (none for total), the dimension order, the years
    strat <- setdiff(names(dimnames(raw)), c("year", "sim", "intervention", "location", "outcome"))
    dims  <- c("year", strat, "sim", "intervention", "location")
    year0 <- as.character(YEAR0)
    years <- as.character(as.integer(YEAR0):max(as.integer(dimnames(raw)$year)))
    has   <- function(o) o %in% dimnames(raw)$outcome

    # One raw outcome, from YEAR0 onward, with its dimensions in the order above
    take <- function(o) aperm(subset_dim(drop_dim(raw, "outcome", o), "year", years), dims)

    # 2. The quantities: counts, rates per 100,000, cumulative counts
    pop  <- take("population")
    x    <- lapply(QUANTITIES, take)
    rate <- lapply(x, function(v) 100000 * v / pop)
    cum  <- lapply(x, cumulate)

    out <- list()

    # 3. LEVELS
    for (q in names(x)) {
        out[[paste0(q, "_rate")]] <- rate[[q]]
        out[[paste0("cum_", q)]]  <- cum[[q]]
    }

    # 4. CHANGE VS YEAR0
    for (q in names(x)) {
        out[[paste0("abs_change_", q, "_vs_year0")]]      <- change(x[[q]],    "year", year0, "abs")
        out[[paste0("pct_change_", q, "_vs_year0")]]      <- change(x[[q]],    "year", year0, "pct")
        out[[paste0("abs_change_", q, "_rate_vs_year0")]] <- change(rate[[q]], "year", year0, "abs")
        out[[paste0("pct_change_", q, "_rate_vs_year0")]] <- change(rate[[q]], "year", year0, "pct")
    }

    # 5. CHANGE VS NO INTERVENTION
    for (q in names(x)) {
        out[[paste0("abs_change_", q, "_vs_noint")]]      <- change(x[[q]],    "intervention", NOINT, "abs")
        out[[paste0("pct_change_", q, "_vs_noint")]]      <- change(x[[q]],    "intervention", NOINT, "pct")
        out[[paste0("abs_change_", q, "_rate_vs_noint")]] <- change(rate[[q]], "intervention", NOINT, "abs")
        out[[paste0("abs_change_cum_", q, "_vs_noint")]]  <- change(cum[[q]],  "intervention", NOINT, "abs")
        out[[paste0("pct_change_cum_", q, "_vs_noint")]]  <- change(cum[[q]],  "intervention", NOINT, "pct")
    }

    # 6. OTHER
    incidence <- x$incidence
    incidence[incidence == 0] <- NA
    out$diagnosis_ps_per_incidence <- x$diagnosis_ps / incidence
    if (has("diagnosis.el.misclassified"))
        out$diagnosis_el_per_incidence <- take("diagnosis.el.misclassified") / incidence
    if (has("doxy.coverage"))
        out$cum_doxy_coverage <- cumulate(take("doxy.coverage"))
    if (length(strat) == 1) {
        out$share_incidence <- share(x$incidence, strat)
        male <- c("msm", "heterosexual_male")
        if (all(male %in% dimnames(raw)[[strat]]))
            out$share_incidence_among_male <- share(x$incidence, strat, male)
    }

    # 7. Stack into one array with outcome last. Every outcome must have the same dimensions.
    for (o in names(out))
        if (!identical(dimnames(out[[o]]), dimnames(pop))) stop("'", o, "' has the wrong dimensions")
    dn <- c(dimnames(pop), list(outcome = names(out)))
    dn$location <- dimnames(raw)$location    # keeps the city names on the location codes
    result <- array(unlist(out, use.names = FALSE), sapply(dn, length), dn)
    attr(result, "year0") <- year0
    result
}


# ****************************************************************************************************
# RUN ----
# ****************************************************************************************************
OUT.DIR <- shield.output.path(CALIB.NAME)

for (strat in CALC.STRATA) {
    raw.file  <- file.path(OUT.DIR, paste0(strat, "_raw_results.Rdata"))
    calc.name <- paste0(strat, "_calc_results")
    if (!file.exists(raw.file))
        stop("Missing ", raw.file, ". Run 01a_generate_raw_results_arrays.R with '", strat,
             "' in RUN.STRATA first.")

    print(paste0("Building ", calc.name, " ..."))
    assign(calc.name, build_custom_outcomes(read_rdata(raw.file)))
    save(list = calc.name, file = file.path(OUT.DIR, paste0(calc.name, ".Rdata")))
    print(paste0(calc.name, ".Rdata saved, ", dim(get(calc.name))[["outcome"]], " outcomes"))
}
