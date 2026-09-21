## ==========================================================================
## INTEGRATION TIER  |  shield_likelihoods.R
## --------------------------------------------------------------------------
## WHAT THIS FILE COVERS
##   That every likelihood set the register names exists, instantiates, and
##   returns a finite log-likelihood that is higher for a better-fitting
##   simulation.
##
## RUN JUST THIS FILE
##   Rscript applications/SHIELD/tests/run_tests.R --filter=integration-likelihoods
##
## WHY IT MATTERS
##   The likelihoods decide what the calibration is fitting to. A likelihood that
##   instantiates but returns -Inf, or that silently drops every observation,
##   produces a chain that runs for 250,000 iterations and means nothing.
##
##   shield_likelihoods.R defines ~40 instruction objects and joins them into the
##   stage-0/1/2/3 sets that shield_calib_register.R registers. Every one of those
##   sets should at least instantiate and score a real simulation.
## ==========================================================================

## Edition 3 gives `tolerance` its strict, relative-difference meaning.
## Without it test_dir() falls back to edition 2, where an 8% error passes a
## 5% tolerance.
local_edition(3)

skip_unless_stage("has.shield.helpers")

test_that("shield_likelihoods.R sources", {
    ## Not a formality: it currently fails against the installed jheem2 because
    ## create.custom.likelihood.instructions() is called with `weights`, which
    ## only the dev clone accepts. test-integration-jheem2-api.R explains that
    ## case; this one records the consequence.
    skip_unless_likelihoods()
    succeed()
})

test_that("the national historical-diagnosis series is usable", {
    ## shield_likelihoods.R wraps the pull in tryCatch and returns a CHARACTER
    ## STRING on failure - then divides by it on the very next line. The guard
    ## therefore does nothing except change the error message. If the pull ever
    ## fails, this is what should report it.
    skip_unless_likelihoods()

    expect_true(is.numeric(NATIONAL_TOTAL_DIAGNOSIS_DATA),
                info = paste("the tryCatch fallback leaked a character value:",
                             paste(head(NATIONAL_TOTAL_DIAGNOSIS_DATA, 1), collapse = "")))
    expect_true(all(is.finite(NATIONAL_TOTAL_DIAGNOSIS_RATIO)))
    expect_true(all(NATIONAL_TOTAL_DIAGNOSIS_RATIO > 0))
    expect_equal(unname(NATIONAL_TOTAL_DIAGNOSIS_RATIO[["1990"]]), 1,
                 tolerance = 1e-12,
                 info = "the series is normalised to its 1990 value")
})

test_that("the historical penalty's bounds bracket the national trajectory", {
    ## min.ratio and max.ratio come from the national series itself, so every
    ## national year must fall inside them - otherwise the penalty would
    ## penalise the very trajectory it was derived from.
    skip_unless_likelihoods()

    min.r <- min(NATIONAL_TOTAL_DIAGNOSIS_RATIO, na.rm = TRUE)
    max.r <- max(NATIONAL_TOTAL_DIAGNOSIS_RATIO, na.rm = TRUE)

    expect_lt(min.r, max.r)
    expect_true(all(NATIONAL_TOTAL_DIAGNOSIS_RATIO >= min.r &
                        NATIONAL_TOTAL_DIAGNOSIS_RATIO <= max.r))

    sharpness <- sd(log(NATIONAL_TOTAL_DIAGNOSIS_RATIO), na.rm = TRUE)
    expect_true(is.finite(sharpness) && sharpness > 0)
})

## The joined instruction sets that shield_calib_register.R actually registers.
registered_instruction_names <- function() {
    f <- file.path(SHIELD.DIR, "shield_calib_register.R")
    lines <- readLines(f, warn = FALSE)
    lines <- lines[!grepl("^\\s*#", lines)]
    m <- regmatches(lines, regexpr("likelihood\\.instructions\\s*=\\s*([A-Za-z0-9._]+)", lines))
    unique(sub(".*=\\s*", "", m))
}

test_that("every likelihood set the register names exists", {
    skip_unless_likelihoods()

    named <- registered_instruction_names()
    expect_gt(length(named), 0)

    missing <- named[!vapply(named, exists, logical(1))]
    expect_equal(missing, character(0),
                 info = paste("registered calibrations name likelihood sets that",
                              "shield_likelihoods.R does not define:",
                              paste(missing, collapse = ", ")))
})

test_that("every registered likelihood set instantiates for the test location", {
    skip_unless_likelihoods()
    skip_unless_slow()
    use_repo_root()

    named <- registered_instruction_names()
    named <- named[vapply(named, exists, logical(1))]
    skip_if(length(named) == 0)

    failures <- character(0)
    for (nm in named) {
        err <- tryCatch({
            get(nm)$instantiate.likelihood(SHIELD.TEST.VERSION,
                                           SHIELD.TEST.LOCATION,
                                           verbose = FALSE)
            NA_character_
        }, error = function(e) conditionMessage(e))
        if (!is.na(err)) failures <- c(failures, paste0(nm, ": ", err))
    }

    expect_equal(failures, character(0),
                 info = paste0("likelihood sets that fail to instantiate:\n  ",
                               paste(failures, collapse = "\n  ")))
})

test_that("every registered likelihood set scores a real simulation finitely", {
    ## A -Inf or NaN log-likelihood at the prior medians means the sampler can
    ## never accept anything, and the chain silently produces nothing usable.
    skip_unless_likelihoods()
    s <- skip_unless_sim()
    use_repo_root()

    named <- registered_instruction_names()
    named <- named[vapply(named, exists, logical(1))]
    skip_if(length(named) == 0)

    problems <- character(0)
    for (nm in named) {
        value <- tryCatch({
            lik <- get(nm)$instantiate.likelihood(SHIELD.TEST.VERSION,
                                                  SHIELD.TEST.LOCATION,
                                                  verbose = FALSE)
            lik$compute(s$sim, log = TRUE)
        }, error = function(e) structure(NA_real_, msg = conditionMessage(e)))

        if (!is.numeric(value) || length(value) != 1 || !is.finite(value)) {
            problems <- c(problems, paste0(
                nm, " -> ",
                if (!is.null(attr(value, "msg"))) attr(value, "msg") else
                    paste(format(value), collapse = ", ")))
        }
    }

    expect_equal(problems, character(0),
                 info = paste0("likelihood sets that do not return a finite ",
                               "log-likelihood at the prior medians:\n  ",
                               paste(problems, collapse = "\n  ")))
})

test_that("a better-fitting simulation scores higher than a worse-fitting one", {
    ## The direction of the likelihood. If this were reversed - or if the
    ## likelihood ignored the simulation entirely - every check above would
    ## still pass.
    skip_unless_likelihoods()
    s <- skip_unless_sim()
    use_repo_root()

    named <- registered_instruction_names()
    named <- named[vapply(named, exists, logical(1))]
    skip_if(length(named) == 0)

    lik <- tryCatch(get(named[1])$instantiate.likelihood(
        SHIELD.TEST.VERSION, SHIELD.TEST.LOCATION, verbose = FALSE),
        error = function(e) NULL)
    skip_if(is.null(lik))

    base.score <- lik$compute(s$sim, log = TRUE)

    ## A simulation run at a badly wrong transmission rate should fit the
    ## surveillance data worse.
    trate.names <- grep("^global\\.transmission\\.rate", names(s$params), value = TRUE)
    skip_if(length(trate.names) == 0)

    wrong <- s$params
    wrong[trate.names] <- wrong[trate.names] * 3
    bad.sim <- tryCatch(s$engine$run(wrong), error = function(e) NULL)
    skip_if(is.null(bad.sim), "the engine did not converge at the distorted rate")

    bad.score <- lik$compute(bad.sim, log = TRUE)

    expect_lt(bad.score, base.score)
})

test_that("likelihood dimension lists use a consistent dimension order", {
    ## The basic likelihood instructions name dimensions as c("sex","race","age").
    ## Mixing orders between instructions that are joined together has bitten
    ## this codebase before (commit 2d041eea). This pins the convention.
    f <- file.path(SHIELD.DIR, "shield_likelihoods.R")
    src <- readLines(f, warn = FALSE)
    src <- src[!grepl("^\\s*#", src)]

    dim.lines <- grep("dimensions\\s*=\\s*c\\(", src, value = TRUE)
    skip_if(length(dim.lines) == 0)

    orders <- unique(gsub(".*dimensions\\s*=\\s*c\\(([^)]*)\\).*", "\\1", dim.lines))
    orders <- gsub("[\"' ]", "", orders)
    ## only consider the ones that name all three demographic dimensions
    demographic <- orders[grepl("sex", orders) & grepl("race", orders) &
                              grepl("age", orders)]
    skip_if(length(demographic) == 0)

    expect_length(unique(demographic), 1)
    expect_equal(unique(demographic), "sex,race,age",
                 info = paste("inconsistent dimension orders in shield_likelihoods.R:",
                              paste(unique(demographic), collapse = " | ")))
})
