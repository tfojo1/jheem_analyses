## ==========================================================================
## INTEGRATION TIER  |  the model run itself
## --------------------------------------------------------------------------
## WHAT THIS FILE COVERS
##   One real simulation at the prior medians, checked for the properties an
##   epidemic model cannot violate: positive population, no negative outcomes,
##   consistent accounting, determinism, and the right sign of response to the
##   transmission rate.
##
## RUN JUST THIS FILE
##   Rscript applications/SHIELD/tests/run_tests.R --filter=integration-engine
##
## WHY IT MATTERS
##   One real model run, checked for the properties an epidemic model cannot
##   violate. None of this has ever run in CI, so a change that makes the model
##   produce negative compartments or lose people is currently caught only when
##   somebody notices a strange plot weeks later.
## ==========================================================================

## Edition 3 gives `tolerance` its strict, relative-difference meaning.
## Without it test_dir() falls back to edition 2, where an 8% error passes a
## 5% tolerance.
local_edition(3)

skip_unless_stage("has.shield.helpers")

YEARS <- as.character(2010:2030)

test_that("the engine builds and runs at the prior medians", {
    ## The smoke test the whole suite hangs on: if this fails, nothing
    ## downstream means anything.
    s <- skip_unless_sim()
    expect_false(is.null(s$sim))
    expect_true(inherits(s$sim, "jheem.simulation.set"))
})

test_that("population is positive and finite in every year", {
    s <- skip_unless_sim()
    pop <- s$sim$get("population", year = YEARS, keep.dimensions = "year")

    expect_true(all(is.finite(pop)), info = "non-finite population")
    expect_true(all(pop > 0), info = "non-positive population")
})

test_that("population changes smoothly - no discontinuities between years", {
    ## A year-on-year jump of more than a few percent in a metro population is
    ## a bug in the demographic inputs, not demography. This is the check that
    ## catches an ontology mapping silently dropping or duplicating a stratum.
    s <- skip_unless_sim()
    pop <- as.vector(s$sim$get("population", year = YEARS, keep.dimensions = "year"))

    growth <- abs(diff(pop) / head(pop, -1))
    expect_lt(max(growth), 0.10)
})

test_that("no modelled quantity is negative", {
    ## Counts and rates alike. A negative compartment means the solver was
    ## handed an inconsistent rate somewhere.
    s <- skip_unless_sim()

    outcomes <- c("population", "infected", "uninfected", "incidence",
                  "prevalence", "diagnosis.total", "diagnosis.ps",
                  "sti.screening", "hiv.testing", "deaths",
                  "immigration", "emigration")

    for (outcome in outcomes) {
        vals <- tryCatch(s$sim$get(outcome, year = YEARS, keep.dimensions = "year"),
                         error = function(e) NULL)
        if (is.null(vals)) next
        expect_true(all(is.finite(vals)),
                    info = paste(outcome, "has non-finite values"))
        expect_true(all(vals >= 0),
                    info = paste(outcome, "has negative values; min was",
                                 signif(min(vals), 4)))
    }
})

test_that("infected plus uninfected accounts for the whole population", {
    ## The model's basic accounting identity. `population` is registered as the
    ## time-INTEGRATED average of point.population = infected + uninfected
    ## (shield_specification.R:1537-1556), while `infected` and `uninfected` are
    ## point-in-time compartment groups. So at a year mark the two differ by the
    ## within-year average-versus-endpoint gap - a fraction of a percent - and
    ## exact equality is the wrong expectation.
    ##
    ## What must hold is that the gap stays small and does not grow: a
    ## compartment added to one aggregate and not the other would show up as a
    ## widening divergence rather than a jump.
    s <- skip_unless_sim()

    total <- as.vector(s$sim$get("population", year = YEARS, keep.dimensions = "year"))
    infected <- as.vector(s$sim$get("infected", year = YEARS, keep.dimensions = "year"))
    uninfected <- as.vector(s$sim$get("uninfected", year = YEARS, keep.dimensions = "year"))

    relative.gap <- abs((infected + uninfected) - total) / total

    expect_true(all(relative.gap < 0.01),
                info = paste0("infected + uninfected departs from population by up to ",
                              signif(100 * max(relative.gap), 3), "%"))

    ## the gap must not widen over the run
    expect_lt(tail(relative.gap, 1), head(relative.gap, 1) * 2 + 1e-6)
})

test_that("prevalence never exceeds the population", {
    s <- skip_unless_sim()
    total <- as.vector(s$sim$get("population", year = YEARS, keep.dimensions = "year"))
    prev <- as.vector(s$sim$get("prevalence", year = YEARS, keep.dimensions = "year"))

    expect_true(all(prev <= total))
})

test_that("diagnoses never exceed incident infections plus existing prevalence", {
    ## You cannot diagnose more people than are infected.
    s <- skip_unless_sim()
    diag <- as.vector(s$sim$get("diagnosis.total", year = YEARS, keep.dimensions = "year"))
    prev <- as.vector(s$sim$get("prevalence", year = YEARS, keep.dimensions = "year"))
    inc  <- as.vector(s$sim$get("incidence", year = YEARS, keep.dimensions = "year"))

    expect_true(all(diag <= prev + inc))
})

test_that("primary-and-secondary diagnoses are a subset of all diagnoses", {
    s <- skip_unless_sim()
    total <- as.vector(s$sim$get("diagnosis.total", year = YEARS, keep.dimensions = "year"))
    ps <- as.vector(s$sim$get("diagnosis.ps", year = YEARS, keep.dimensions = "year"))

    expect_true(all(ps <= total * (1 + 1e-8)),
                info = "diagnosis.ps exceeds diagnosis.total")
})

test_that("MSM diagnoses are a subset of male diagnoses", {
    s <- skip_unless_sim()
    male <- as.vector(s$sim$get("diagnosis.ps.among.male", year = YEARS,
                                keep.dimensions = "year"))
    msm <- as.vector(s$sim$get("diagnosis.ps.among.msm", year = YEARS,
                               keep.dimensions = "year"))

    expect_true(all(msm <= male * (1 + 1e-8)),
                info = "MSM diagnoses exceed male diagnoses")
})

test_that("the model is deterministic for a given parameter vector", {
    ## Two runs of the same engine with the same parameters must agree exactly.
    ## Non-determinism here would make every calibration irreproducible.
    s <- skip_unless_sim()
    use_repo_root()

    again <- s$engine$run(s$params)
    expect_equal(
        as.vector(again$get("population", year = YEARS, keep.dimensions = "year")),
        as.vector(s$sim$get("population", year = YEARS, keep.dimensions = "year")),
        tolerance = 1e-12)
    expect_equal(
        as.vector(again$get("diagnosis.total", year = YEARS, keep.dimensions = "year")),
        as.vector(s$sim$get("diagnosis.total", year = YEARS, keep.dimensions = "year")),
        tolerance = 1e-12)
})

test_that("raising the transmission rate raises incidence", {
    ## A directional sanity check on the parameter that matters most. If the
    ## sign of this relationship ever flips, every calibration is meaningless
    ## and nothing else in this suite would notice.
    s <- skip_unless_sim()
    use_repo_root()

    trate.names <- grep("^global\\.transmission\\.rate", names(s$params), value = TRUE)
    skip_if(length(trate.names) == 0, "no global transmission rate parameter")

    higher <- s$params
    higher[trate.names] <- higher[trate.names] * 1.5

    hotter <- tryCatch(s$engine$run(higher), error = function(e) NULL)
    skip_if(is.null(hotter), "the engine did not converge at the raised rate")

    base.inc <- sum(s$sim$get("incidence", year = YEARS, keep.dimensions = "year"))
    hot.inc  <- sum(hotter$get("incidence", year = YEARS, keep.dimensions = "year"))

    expect_gt(hot.inc, base.inc)
})

test_that("the simulation covers every year the calibration asks for", {
    ## The registered calibrations run to 2030. A simulation that stops earlier
    ## produces NA targets that the likelihood may silently drop.
    s <- skip_unless_sim()
    pop <- s$sim$get("population", year = as.character(c(1990, 2010, 2030)),
                     keep.dimensions = "year")
    expect_equal(length(pop), 3)
    expect_true(all(is.finite(pop)))
})

test_that("every stratum of the population is non-empty", {
    ## An empty race/sex/age cell means the initial population was built from a
    ## mapping that lost a category - the model then has no one to infect there.
    s <- skip_unless_sim()
    pop <- s$sim$get("population", year = "2020",
                     keep.dimensions = c("age", "race", "sex"))

    expect_true(all(is.finite(pop)))
    empty <- which(pop <= 0, arr.ind = TRUE)
    expect_equal(nrow(empty), 0,
                 info = paste("empty population strata:", nrow(empty), "cells"))
})
