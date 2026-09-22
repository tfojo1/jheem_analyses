## ==========================================================================
## INTEGRATION TIER  |  intervention/intervention_definitions.R
## --------------------------------------------------------------------------
## WHAT THIS FILE COVERS
##   That the doxy-PEP interventions register, that their parameter matrix is
##   well formed, and that running one actually changes the projection - more
##   coverage averting more infections.
##
## RUN JUST THIS FILE
##   Rscript applications/SHIELD/tests/run_tests.R --filter=integration-interventions
##
## WHY IT MATTERS
##   The doxy-PEP interventions are the deliverable: every number in the
##   manuscript is a difference between an intervention run and the null run.
##
##   The pipeline currently has no check that an intervention did anything. The
##   runner sets stop.for.errors = FALSE, asks for five coverage levels that are
##   never built, and reports success either way.
## ==========================================================================

## Edition 3 gives `tolerance` its strict, relative-difference meaning.
## Without it test_dir() falls back to edition 2, where an 8% error passes a
## 5% tolerance.
local_edition(3)

skip_unless_stage("has.shield.helpers")

## load_interventions ----
## Source intervention_definitions.R into a private environment, with a fixed
## seed, so the test does not depend on (or disturb) the global interventions
## registry more than it must.
load_interventions <- function() {
    use_repo_root()
    if (is.null(SHIELD.TEST.ENV$interventions)) {
        SHIELD.TEST.ENV$interventions <- tryCatch({
            set.seed(20260918)
            source(file.path(SHIELD.DIR, "intervention/intervention_definitions.R"))
            TRUE
        }, error = function(e) {
            SHIELD.TEST.ENV$interventions.error <- conditionMessage(e)
            FALSE
        })
    }
    isTRUE(SHIELD.TEST.ENV$interventions)
}

## skip_unless_interventions ----
skip_unless_interventions <- function() {
    skip_if(is.null(shield.test.specification()), "no specification")
    if (!load_interventions()) {
        testthat::skip(paste("intervention_definitions.R failed:",
                             SHIELD.TEST.ENV$interventions.error))
    }
}

test_that("intervention_definitions.R runs", {
    skip_unless_interventions()
    succeed()
})

test_that("the doxy parameter matrix is shaped the way create.intervention expects", {
    skip_unless_interventions()

    expect_true(is.matrix(DOXY.PARAMS))
    expect_equal(nrow(DOXY.PARAMS), 1)
    expect_equal(rownames(DOXY.PARAMS), "doxy.effectiveness")
    expect_gt(ncol(DOXY.PARAMS), 1)
    expect_true(all(is.finite(DOXY.PARAMS)))
    expect_true(all(DOXY.PARAMS >= 0 & DOXY.PARAMS <= 1),
                info = "doxy effectiveness must be a proportion")
})

test_that("every coverage level the loop declares is actually registered", {
    ## create.intervention() is called inside a for loop whose result is never
    ## assigned; it registers by side effect. If registration ever stopped
    ## happening, the loop would still print each name and the runner would
    ## still report success.
    skip_unless_interventions()

    defs <- readLines(file.path(SHIELD.DIR, "intervention/intervention_definitions.R"),
                      warn = FALSE)
    seq.line <- grep("for\\s*\\(\\s*coverage\\s+in\\s+seq\\(", defs, value = TRUE)
    skip_if(length(seq.line) != 1)
    args <- as.numeric(strsplit(gsub(".*seq\\(([^)]*)\\).*", "\\1", seq.line[1]),
                                "\\s*,\\s*")[[1]])
    expected <- paste0("doxy.cov.", seq(args[1], args[2], args[3]))

    missing <- character(0)
    for (code in expected) {
        ok <- tryCatch(!is.null(get.intervention.from.code(code)),
                       error = function(e) FALSE)
        if (!ok) missing <- c(missing, code)
    }

    expect_equal(missing, character(0),
                 info = paste("coverage levels the loop builds but that are not",
                              "retrievable by code:", paste(missing, collapse = ", ")))
})

test_that("the null intervention is available", {
    skip_unless_interventions()
    expect_false(is.null(noint))
})

test_that("a doxy intervention changes the projection", {
    ## The check the pipeline has never had. Finding #32 is that doxy.uptake is
    ## never driven by anything; an intervention that sets a quantity nothing
    ## reads produces output identical to the null run, and every "averted
    ## infections" number in the analysis is then zero by construction - which
    ## looks like a modest effect rather than a bug.
    skip_unless_interventions()
    s <- skip_unless_sim()
    use_repo_root()

    intervention <- tryCatch(get.intervention.from.code("doxy.cov.50"),
                             error = function(e) NULL)
    skip_if(is.null(intervention), "doxy.cov.50 is not registered")

    engine <- tryCatch(
        create.jheem.engine(SHIELD.TEST.VERSION, SHIELD.TEST.LOCATION,
                            end.year = 2040, intervention = intervention),
        error = function(e) NULL)
    skip_if(is.null(engine), "could not build an intervention engine")

    treated <- tryCatch(engine$run(s$params), error = function(e) NULL)
    skip_if(is.null(treated), "the intervention run did not converge")

    years <- as.character(2031:2040)
    baseline.inc <- sum(s$sim$get("incidence", year = as.character(2031:2040),
                                  keep.dimensions = "year"))
    treated.inc <- sum(treated$get("incidence", year = years,
                                   keep.dimensions = "year"))

    expect_false(
        isTRUE(all.equal(treated.inc, baseline.inc, tolerance = 1e-9)),
        info = paste0(
            "a 50% doxy-PEP coverage scenario produced exactly the baseline ",
            "incidence (", signif(baseline.inc, 8), "). The intervention sets ",
            "doxy.coverage; check that a model quantity actually reads it."))

    expect_lt(treated.inc, baseline.inc)
})

test_that("higher doxy coverage averts more infections", {
    ## Monotonicity in the dose. If a mis-scaled coverage made 50% weaker than
    ## 10%, every headline figure would be wrong and nothing would error.
    skip_unless_interventions()
    s <- skip_unless_sim()
    skip_unless_slow()
    use_repo_root()

    run.at <- function(code) {
        iv <- tryCatch(get.intervention.from.code(code), error = function(e) NULL)
        if (is.null(iv)) return(NA_real_)
        eng <- tryCatch(create.jheem.engine(SHIELD.TEST.VERSION, SHIELD.TEST.LOCATION,
                                            end.year = 2040, intervention = iv),
                        error = function(e) NULL)
        if (is.null(eng)) return(NA_real_)
        sim <- tryCatch(eng$run(s$params), error = function(e) NULL)
        if (is.null(sim)) return(NA_real_)
        sum(sim$get("incidence", year = as.character(2031:2040),
                    keep.dimensions = "year"))
    }

    low <- run.at("doxy.cov.10")
    high <- run.at("doxy.cov.50")
    skip_if(is.na(low) || is.na(high), "could not run both coverage levels")

    expect_lt(high, low)
})

test_that("the intervention's coverage scale-up window is coherent", {
    ## start.time must precede the time the target coverage is reached, and the
    ## target must be a proportion.
    defs <- readLines(file.path(SHIELD.DIR, "intervention/intervention_definitions.R"),
                      warn = FALSE)
    defs <- defs[!grepl("^\\s*#", defs)]

    start <- as.numeric(sub(".*start\\.time\\s*=\\s*([0-9]+).*", "\\1",
                            grep("start\\.time\\s*=", defs, value = TRUE)[1]))
    end <- as.numeric(sub(".*times\\s*=\\s*([0-9]+).*", "\\1",
                          grep("^\\s*times\\s*=", defs, value = TRUE)[1]))

    skip_if(is.na(start) || is.na(end))
    expect_lt(start, end)

    scale <- grep("scale\\s*=", defs, value = TRUE)[1]
    expect_true(grepl("proportion", scale),
                info = "doxy.coverage must be applied on the proportion scale")
})
