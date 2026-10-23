# ****************************************************************************************************
# TEST: WHAT EHE'S PROPORTION-MSM LIKELIHOOD CALCULATES ----
# ****************************************************************************************************
# Runs EHE's own functions (not a copy) for one state and one MSA, and prints each step:
# 1. DATA  - what get.data.function prepares: Emory totals, BRFSS race values, their years, the sds
# 2. MODEL - what the simulation returns for those targets (optimized.get), with its shape
# 3. PAIRS - which data value is compared with which model value inside dnorm()
#            (R silently reuses the shorter vector when the two have different lengths)
# 4. VALUE - the number compute.function returns, recomputed by hand to check it,
#            next to what a log-likelihood (dnorm(..., log = TRUE)) would give
# 5. JHEEM - the value from the instantiated likelihood, to confirm it is the same number
#
# The likelihood is proportion.msm.likelihood.instructions in applications/EHE/ehe_likelihoods.R
#
# Run in a FRESH R session from jheem_analyses (EHE and SHIELD each load their own SURVEILLANCE.MANAGER).
# A real EHE simulation is used where one can be loaded (SIM.CODES below).
# Otherwise a made-up simulation is used; its shape copies what the code asks for, so treat it as an
# illustration and confirm with a real simulation.
# ****************************************************************************************************

## SETTINGS ----
LOCATIONS <- c(state = "MD", msa = "C.12580")          # a state has BRFSS race data; an MSA does not
SIM.CODES <- c(MD = "final.ehe.state", C.12580 = NA)    # calibration code for a real simulation; NA = made-up
SIM.N     <- c(MD = 1000, C.12580 = NA)                 # n.sim of the saved simset

## SOURCES ----
if (!exists("EHE.SPECIFICATION"))                     source("../jheem_analyses/applications/EHE/ehe_specification.R")
if (!exists("proportion.msm.likelihood.instructions")) source("../jheem_analyses/applications/EHE/ehe_likelihoods.R")
instr <- proportion.msm.likelihood.instructions

## get.sim ----
# first simulation of the saved simset, or NULL if there is none
get.sim <- function(loc) {
    if (is.na(SIM.CODES[[loc]])) return(NULL)
    ss <- tryCatch(retrieve.simulation.set("ehe", loc, SIM.CODES[[loc]], n.sim = SIM.N[[loc]]),
                   error = function(e) { message(loc, ": could not load simset (", conditionMessage(e), ")"); NULL })
    if (is.null(ss)) NULL else ss$subset(1)
}

## fake.sim ----
# made-up simulation: total 5.0% in every year; by race black 6%, hispanic 5%, other 4%
# the race array uses the same years as the totals, because that is what race.optimized.get.instr asks for
fake.sim <- function(data) {
    years <- names(data$totals)
    total <- setNames(rep(0.050, length(years)), years)
    race  <- NULL
    if (!is.null(data$by.race)) {
        races <- colnames(data$by.race)
        race  <- matrix(rep(c(black = 0.06, hispanic = 0.05, other = 0.04)[races], each = length(years)),
                        nrow = length(years), dimnames = list(year = years, race = races))
    }
    list(optimized.get = function(x) if (identical(x, data$total.optimized.get.instr)) total else race)
}

## pair.table ----
# which data value meets which model value inside dnorm(data, model, sd)
# R walks both arrays column by column and recycles the shorter one: rep_len() reproduces that
pair.table <- function(data.arr, model.arr) {
    d.dn <- dimnames(as.matrix(data.arr)); m.dn <- dimnames(as.matrix(model.arr))
    d.lab <- expand.grid(year = d.dn[[1]], race = if (length(d.dn) > 1 && !is.null(d.dn[[2]])) d.dn[[2]] else "total", stringsAsFactors = FALSE)
    m.lab <- expand.grid(year = m.dn[[1]], race = if (length(m.dn) > 1 && !is.null(m.dn[[2]])) m.dn[[2]] else "total", stringsAsFactors = FALSE)
    n <- max(length(data.arr), length(model.arr))
    out <- data.frame(data.year   = rep_len(d.lab$year, n), data.race  = rep_len(d.lab$race, n),
                      data.value  = round(100 * rep_len(c(data.arr), n), 2),
                      model.year  = rep_len(m.lab$year, n), model.race = rep_len(m.lab$race, n),
                      model.value = round(100 * rep_len(c(model.arr), n), 2))
    out$same.year <- out$data.year == out$model.year
    out$same.race <- out$data.race == out$model.race
    out
}

## RUN ----
options(width = 160)   # so the pair tables print on one line
for (loc in LOCATIONS) {
    cat("\n\n****************************************************************\n")
    cat("LOCATION:", loc, "\n")
    cat("****************************************************************\n")

    # 1. DATA ----
    data <- instr$get.data.function(version = "ehe", location = loc)
    cat("\n== 1. DATA ==\n")
    cat("Emory totals (% of men), by year:\n"); print(round(100 * data$totals, 2))
    if (is.null(data$by.race)) {
        cat("BRFSS by race: NULL -> the race part is skipped (race.d = 0)\n")
    } else {
        cat("BRFSS by race (% of men), year x race:", paste(dim(data$by.race), collapse = " x "), "\n")
        print(round(100 * data$by.race, 2))
    }
    cat("sd: total =", data$total.sd, " race =", data$race.sd, "\n")

    # 2. MODEL ----
    sim  <- get.sim(loc)
    real <- !is.null(sim)
    if (!real) sim <- fake.sim(data)
    cat("\n== 2. MODEL (", if (real) "real simulation" else "MADE-UP simulation", ") ==\n", sep = "")
    model.total <- sim$optimized.get(data$total.optimized.get.instr)
    cat("model total, shape:", paste(dim(as.matrix(model.total)), collapse = " x "), "\n"); print(round(100 * model.total, 2))
    model.race <- NULL
    if (!is.null(data$by.race)) {
        model.race <- sim$optimized.get(data$race.optimized.get.instr)
        cat("model by race, shape:", paste(dim(as.matrix(model.race)), collapse = " x "),
            "  <- compare with the data shape above\n")
        print(round(100 * model.race, 2))
    }

    # 3. PAIRS ----
    cat("\n== 3. PAIRS inside dnorm() ==\n")
    cat("total:\n"); print(pair.table(data$totals, model.total), row.names = FALSE)
    if (!is.null(model.race)) {
        p <- pair.table(data$by.race, model.race)
        cat("\nrace (", nrow(p), " comparisons; ", sum(!p$same.race), " compare DIFFERENT races, ",
            sum(!p$same.year), " compare different years):\n", sep = "")
        print(p, row.names = FALSE)
    }

    # 4. VALUE ----
    total.d  <- dnorm(data$totals, model.total, data$total.sd)
    race.d   <- if (is.null(model.race)) 0 else dnorm(data$by.race, model.race, data$race.sd)
    by.hand  <- sum(total.d, na.rm = TRUE) + sum(race.d, na.rm = TRUE)
    returned <- instr$compute.function(sim = sim, data = data, log = TRUE, weights = 1)
    as.log   <- sum(dnorm(data$totals, model.total, data$total.sd, log = TRUE), na.rm = TRUE) +
        (if (is.null(model.race)) 0 else sum(dnorm(data$by.race, model.race, data$race.sd, log = TRUE), na.rm = TRUE))
    cat("\n== 4. VALUE ==\n")
    cat("densities, total:", round(total.d, 3), "\n")
    if (!is.null(model.race)) cat("densities, race :", round(race.d, 3), "\n")
    cat("sum of densities (by hand)           :", round(by.hand, 4), "\n")
    cat("compute.function(log = TRUE) returns :", round(returned, 4), "\n")
    cat("same number?                         :", isTRUE(all.equal(by.hand, returned)), "\n")
    cat("a log-likelihood would be            :", round(as.log, 4), "\n")
    cat("largest possible density per point   : total", round(dnorm(0, 0, data$total.sd), 1),
        "| race", round(dnorm(0, 0, data$race.sd), 1), "\n")

    # 5. JHEEM ----
    if (real) {
        lik <- instr$instantiate.likelihood(version = "ehe", location = loc)
        cat("\n== 5. JHEEM ==\nlikelihood$compute(sim) returns:", round(lik$compute(sim, log = TRUE), 4), "\n")
    }
}
