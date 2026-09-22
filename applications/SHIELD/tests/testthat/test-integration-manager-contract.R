## ==========================================================================
## INTEGRATION TIER  |  the surveillance manager <-> model data contract
## --------------------------------------------------------------------------
## WHAT THIS FILE COVERS
##   That the loaded syphilis manager still provides every outcome the model
##   pulls, with every stratification the model pulls on.
##
## RUN JUST THIS FILE
##   Rscript applications/SHIELD/tests/run_tests.R --filter=integration-manager
##
## WHY IT MATTERS
##   The data contract between the surveillance manager and the model.
##
##   The manager is built by a separate pipeline with its own CI, and that CI
##   validates the manager against its own spec - it has no idea what the SHIELD
##   model actually pulls. So a manager can pass its own validation, be promoted,
##   and break every SHIELD run.
##
##   This is not hypothetical. The copy of the manager in cached/ is stratified
##   year__location for emory `proportion.msm`, while the release-tagged copy is
##   stratified year__location__sex. get.best.guess.msm.proportions() pulls with
##   sex = 'male', so against the first copy the pull returns NULL and
##   create.jheem.engine() fails for every location, with an error that names the
##   location rather than the missing stratification.
## ==========================================================================

## Edition 3 gives `tolerance` its strict, relative-difference meaning.
## Without it test_dir() falls back to edition 2, where an 8% error passes a
## 5% tolerance.
local_edition(3)

skip_unless_stage("has.surveillance.manager")

## Outcome -> the dimensions the model requires it to be stratified by.
## Every entry here is a pull that SHIELD code actually performs.
REQUIRED.OUTCOMES <- list(
    "proportion.msm"               = c("location", "sex"),
    "population"                   = c("location", "year"),
    "total.syphilis.diagnoses"     = c("location", "year"),
    "ps.syphilis.diagnoses"        = c("location", "year"),
    "early.syphilis.diagnoses"     = c("location", "year"),
    "prop.male.ps.diag.among.msm"  = c("location", "year"),
    "fertility.rate"               = c("location", "year"),
    "immigration"                  = c("location", "year"),
    "emigration"                   = c("location", "year"),
    "deaths"                       = c("location", "year")
)

test_that("the manager exposes every outcome the model pulls", {
    have <- names(SURVEILLANCE.MANAGER$data)
    missing <- setdiff(names(REQUIRED.OUTCOMES), have)

    expect_equal(
        missing, character(0),
        info = paste0("outcomes the SHIELD model pulls but the manager (",
                      SURVEILLANCE.MANAGER$name, ", tag ",
                      SHIELD.TEST.ENV$manager.tag %||% "latest",
                      ") does not provide: ", paste(missing, collapse = ", "))
    )
})

## outcome_dimensions ----
## All stratification keys present anywhere under an outcome, e.g.
## "year__location__sex" -> c("year", "location", "sex").
outcome_dimensions <- function(outcome) {
    node <- SURVEILLANCE.MANAGER$data[[outcome]]
    if (is.null(node)) return(character(0))
    keys <- character(0)
    collect <- function(x, depth) {
        if (depth == 3L) {        # estimate -> source -> ontology -> strata
            keys <<- c(keys, names(x))
        } else if (is.list(x)) {
            for (el in x) collect(el, depth + 1L)
        }
    }
    collect(node, 0L)
    unique(unlist(strsplit(keys, "__", fixed = TRUE)))
}

test_that("every required outcome carries the stratifications the model pulls on", {
    ## This is the check that catches a silently de-stratified outcome. A pull
    ## that filters on a dimension the data does not have returns NULL, and the
    ## caller reports a misleading error about the location.
    problems <- list()

    for (outcome in names(REQUIRED.OUTCOMES)) {
        if (!outcome %in% names(SURVEILLANCE.MANAGER$data)) next
        have <- outcome_dimensions(outcome)
        need <- REQUIRED.OUTCOMES[[outcome]]
        missing <- setdiff(need, have)
        if (length(missing)) {
            problems[[outcome]] <- paste0(
                outcome, " is missing ", paste(missing, collapse = ", "),
                " (has: ", paste(sort(have), collapse = ", "), ")")
        }
    }

    expect_equal(
        length(problems), 0,
        info = paste0("outcomes missing a stratification the model pulls on:\n  ",
                      paste(unlist(problems), collapse = "\n  "))
    )
})

test_that("proportion.msm resolves for every county of the test location", {
    ## The exact pull get.best.guess.msm.proportions() performs. Asserting it
    ## directly turns a confusing downstream failure into a clear one.
    skip_unless_stage("has.commoncode")

    counties <- locations::get.contained.locations(SHIELD.TEST.LOCATION, "county")
    expect_gt(length(counties), 0)

    pulled <- SURVEILLANCE.MANAGER$pull(
        outcome = "proportion.msm",
        dimension.values = list(location = counties, sex = "male"),
        sources = "emory")

    expect_false(
        is.null(pulled),
        info = paste0(
            "SURVEILLANCE.MANAGER$pull('proportion.msm', sex = 'male', ",
            "sources = 'emory') returned NULL for the counties of ",
            SHIELD.TEST.LOCATION, ". create.jheem.engine() will fail for every ",
            "location. Check that the emory slice is still stratified by sex."))

    skip_if(is.null(pulled))
    expect_setequal(dimnames(pulled)$location, counties)
    expect_false(any(is.na(apply(pulled, "location", mean, na.rm = TRUE))))
    vals <- as.vector(pulled)
    vals <- vals[!is.na(vals)]
    expect_true(all(vals >= 0 & vals <= 1),
                info = "proportion.msm must be a proportion")
})

test_that("the national series the historical penalty depends on is present", {
    ## shield_likelihoods.R reads total.syphilis.diagnoses for the US, 1970-1993,
    ## and immediately divides by the 1990 value. If the slice is absent the
    ## tryCatch returns a character string and the very next line does arithmetic
    ## on it, so sourcing the likelihoods fails with a type error rather than a
    ## useful message.
    series <- tryCatch(
        SURVEILLANCE.MANAGER$data$total.syphilis.diagnoses$estimate$
            cdc.sti.surveillance.reports$cdc.pdf.report$year__location[
                as.character(1970:1993), "US"],
        error = function(e) NULL)

    expect_false(is.null(series),
                 info = "the US 1970-1993 total.syphilis.diagnoses series is missing")
    skip_if(is.null(series))

    expect_length(series, length(1970:1993))
    expect_true(all(is.finite(series)))
    expect_gt(series[["1990"]], 0)

    ratio <- series / series[["1990"]]
    expect_true(all(ratio > 0))
    ## The historical penalty's sharpness is sd(log(ratio)); a degenerate
    ## series would make it 0 and the penalty infinitely sharp.
    expect_gt(sd(log(ratio)), 0)
})

test_that("the loaded manager is the one the tests intend to run against", {
    ## Recorded so a failure elsewhere can be attributed to the manager rather
    ## than to the model.
    expect_true(nzchar(SURVEILLANCE.MANAGER$name))
    cat("\n  manager: ", SURVEILLANCE.MANAGER$name,
        " | tag: ", SHIELD.TEST.ENV$manager.tag %||% "latest",
        " | outcomes: ", length(names(SURVEILLANCE.MANAGER$data)), "\n", sep = "")
    succeed()
})
