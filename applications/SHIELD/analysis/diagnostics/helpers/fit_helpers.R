# ****************************************************************************************************
# FIT DIAGNOSTICS: HELPER FUNCTIONS ----
# ****************************************************************************************************
# Functions only, shared by the scripts in fit/ and prior/. Sourced by diagnostics_setup.R.
# What they need when you call them: SURVEILLANCE.MANAGER (shield_source_code.R)
#
# Contents:
#  1. observed.by.sex()  - observed diagnoses by sex, years x male/female
#  2. observed.by.race() - observed diagnoses by race, years x black/hispanic/other
#  3. check.session()    - stops early if the ontology mapping fails in this session
# ****************************************************************************************************

## observed.by.sex ----
# observed diagnoses by sex for one outcome and city (years x male/female), averaged over every source
# that has both male and female
# read directly rather than with pull(): the CDC surveillance reports also have an "msm" category,
# and pull() then returns only the county aggregates (2019 on). Male already includes MSM.
observed.by.sex <- function(outcome, location, years = as.character(1980:2025)) {
    d <- SURVEILLANCE.MANAGER$data[[outcome]]$estimate
    if (is.null(d)) stop("No data in SURVEILLANCE.MANAGER for ", outcome)
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

## RACE.MAP ----
# data race -> model race, the same as cdc.to.shield.race1 in R/shield_ontology_mappings.R
RACE.MAP <- c("black/african american"                 = "black",
              "hispanic/latino"                        = "hispanic",
              "white"                                  = "other",
              "asian"                                  = "other",
              "american indian/alaska native"          = "other",
              "native hawaiian/other pacific islander" = "other")

## observed.by.race ----
# observed diagnoses by race for one outcome and city (years x black/hispanic/other), averaged over every source
# 1. other = white + asian + american indian + pacific islander, so it is NA in a year where any of them is
#    missing (suppressed), rather than too low
# 2. at MSA level (syphilis-manager-v2026.10.09) only the county aggregates have race: 2019-2023, with gaps
observed.by.race <- function(outcome, location, years = as.character(1980:2025)) {
    d <- SURVEILLANCE.MANAGER$data[[outcome]]$estimate
    if (is.null(d)) stop("No data in SURVEILLANCE.MANAGER for ", outcome)
    by.source <- list()
    for (s in names(d)) for (o in names(d[[s]])) {
        a <- d[[s]][[o]]$year__location__race                                 # year x location x race
        if (is.null(a) || !location %in% dimnames(a)$location || !all(names(RACE.MAP) %in% dimnames(a)$race)) next
        y <- intersect(years, dimnames(a)$year)
        x <- matrix(a[y, location, names(RACE.MAP), drop = FALSE], length(y), length(RACE.MAP),
                    dimnames = list(y, names(RACE.MAP)))                         # year x data race
        m <- matrix(NA_real_, length(years), 3, dimnames = list(years, c("black", "hispanic", "other")))
        for (r in colnames(m)) m[y, r] <- rowSums(x[, RACE.MAP == r, drop = FALSE])
        by.source[[paste(s, o)]] <- m
    }
    if (length(by.source) == 0) return(NULL)
    out <- apply(simplify2array(by.source), c(1, 2), mean, na.rm = TRUE)    # average over sources
    out[is.nan(out)] <- NA
    out
}

## check.session ----
# mapping data to the model's strata uses compiled code; in a session restored from a saved
# workspace its address is empty and every pull fails with "NULL value passed as symbol address"
check.session <- function(location, outcome = "ps.syphilis.diagnoses", keep.dimensions = c("year", "sex")) {
    tryCatch(
        SURVEILLANCE.MANAGER$pull(outcome, keep.dimensions = keep.dimensions,
                                  dimension.values = list(location = location, year = as.character(2019:2021))),
        error = function(e) stop("Ontology mapping fails in this session (", conditionMessage(e), ").\n",
                                 "Re-source shield_source_code.R, or restart R without restoring the workspace."))
    invisible(TRUE)
}
