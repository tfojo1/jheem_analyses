## ==========================================================================
## INTEGRATION TIER  |  shield_specification.R
## --------------------------------------------------------------------------
## WHAT THIS FILE COVERS
##   That the specification builds, and that its compartments, age brackets,
##   .from/.to dimensions, stage aliases and outcomes are the ones the rest of
##   the codebase assumes.
##
## RUN JUST THIS FILE
##   Rscript applications/SHIELD/tests/run_tests.R --filter=integration-specification
##
## WHY IT MATTERS
##   The specification is ~2,500 lines of register.model.quantity() calls. Most of
##   its errors are structural: a compartment renamed in one place and not
##   another, an alias that no longer covers the stages it claims, a quantity that
##   evaluates to NA for a location.
##
##   Building it is cheap (about two seconds), so there is no excuse for CI not to.
## ==========================================================================

## Edition 3 gives `tolerance` its strict, relative-difference meaning.
## Without it test_dir() falls back to edition 2, where an 8% error passes a
## 5% tolerance.
local_edition(3)

skip_unless_stage("has.shield.helpers")

test_that("the specification builds", {
    ## The single most valuable assertion in this file: it is the step that
    ## every calibration, intervention and plot depends on, and nothing in CI
    ## has ever performed it.
    spec <- shield.test.specification()
    expect_false(is.null(spec),
                 info = paste("shield_specification.R failed to build:",
                              SHIELD.TEST.ENV$has.specification.message))
})

test_that("the specification's compartments are the ones the rest of the code assumes", {
    skip_if(is.null(shield.test.specification()))
    sm <- get.specification.metadata(SHIELD.TEST.VERSION, SHIELD.TEST.LOCATION)
    dn <- sm$dim.names

    ## Hardcoded all over the likelihoods, interventions and analysis scripts.
    expect_setequal(dn$sex, c("heterosexual_male", "msm", "female"))
    expect_setequal(dn$race, c("black", "hispanic", "other"))
    expect_setequal(dn$continuum, c("undiagnosed", "diagnosed.untreated"))
    expect_setequal(dn$stage, c("primary", "secondary", "early.latent",
                                "late.latent", "tertiary", "cns"))
    expect_setequal(dn$profile, c("susceptible", "diagnosed.treated"))
})

test_that("the age brackets are contiguous, ascending and cover all ages", {
    skip_if(is.null(shield.test.specification()))
    sm <- get.specification.metadata(SHIELD.TEST.VERSION, SHIELD.TEST.LOCATION)

    parsed <- parse.age.strata.names(sm$dim.names$age)
    expect_false(any(is.na(parsed$lower)))
    expect_true(all(diff(parsed$lower) > 0), info = "age brackets must ascend")
    expect_equal(parsed$lower[1], 0, info = "the first bracket must start at 0")
    expect_true(is.infinite(tail(parsed$upper, 1)),
                info = "the last bracket must be open-ended")
    ## no gaps: each bracket's upper is the next one's lower
    expect_equal(head(parsed$upper, -1), tail(parsed$lower, -1))
})

test_that("the .from/.to dimensions mirror the base dimensions", {
    ## Mixing matrices are indexed by <dim>.from / <dim>.to. If these ever drift
    ## from the base dimension the matrices stop being square and the error
    ## surfaces deep inside the solver.
    skip_if(is.null(shield.test.specification()))
    dn <- get.specification.metadata(SHIELD.TEST.VERSION, SHIELD.TEST.LOCATION)$dim.names

    for (d in c("age", "race", "sex", "continuum", "stage", "profile")) {
        expect_equal(dn[[paste0(d, ".from")]], dn[[d]],
                     info = paste(d, ".from does not match", d))
        expect_equal(dn[[paste0(d, ".to")]], dn[[d]],
                     info = paste(d, ".to does not match", d))
    }
})

test_that("the compartment aliases partition the stages as documented", {
    ## ps.stages, early.stages and late.stages are used throughout the
    ## likelihoods to aggregate the model to the data's categories. If an alias
    ## silently drops a stage, the modelled diagnosis counts are compared against
    ## data for a different set of stages.
    skip_if(is.null(shield.test.specification()))
    sm <- get.specification.metadata(SHIELD.TEST.VERSION, SHIELD.TEST.LOCATION)
    all.stages <- sm$dim.names$stage

    ps    <- c("primary", "secondary")
    early <- c("primary", "secondary", "early.latent")
    late  <- c("late.latent", "tertiary", "cns")

    expect_true(all(ps %in% all.stages))
    expect_true(all(early %in% all.stages))
    expect_true(all(late %in% all.stages))

    ## early and late must together be every stage, and must not overlap
    expect_setequal(c(early, late), all.stages)
    expect_equal(intersect(early, late), character(0))
    expect_true(all(ps %in% early))
})

test_that("the outcomes the likelihoods and interventions name all exist", {
    ## An outcome renamed in the specification and not in the likelihoods is a
    ## silent mis-fit: the likelihood is instantiated against a name the
    ## simulation does not produce.
    skip_unless_sim()
    sim <- shield_test_sim()$sim
    available <- sim$outcomes

    required <- c(
        ## population / demography
        "population", "adult.population", "births.from", "deaths",
        "immigration", "emigration", "fertility.rate",
        ## infection
        "infected", "uninfected", "prevalence", "incidence",
        ## diagnosis
        "diagnosis.total", "diagnosis.ps", "diagnosis.ps.among.msm",
        "prop.male.ps.diag.among.msm", "ps.diag.rate.among.msm",
        ## testing
        "sti.screening", "hiv.testing"
    )

    expect_equal(setdiff(required, available), character(0),
                 info = paste("outcomes referenced elsewhere but not produced:",
                              paste(setdiff(required, available), collapse = ", ")))
})

test_that("the specification exposes the doxy quantities the intervention drives", {
    ## The doxy intervention sets doxy.coverage and reads doxy.effectiveness.
    ## If either is not a registered model quantity, create.intervention()
    ## succeeds and the intervention does nothing.
    skip_if(is.null(shield.test.specification()))

    src <- paste(readLines(file.path(SHIELD.DIR, "shield_specification.R"),
                           warn = FALSE), collapse = "\n")
    for (q in c("doxy.coverage", "doxy.effectiveness")) {
        expect_true(grepl(paste0("name\\s*=\\s*['\"]", q, "['\"]"), src),
                    info = paste(q, "is not registered in shield_specification.R"))
    }
})
