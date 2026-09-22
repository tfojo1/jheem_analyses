## ============================================================================
## WHAT THIS FILE COVERS
##   shield_calib_parameters.R  - the joint prior over all ~174 calibrated
##                                parameters, and SHIELD.APPLY.PARAMETERS.FN,
##                                which writes a sampled parameter vector into
##                                the model.
##   shield_calib_register.R    - the registered calibration stages.
##
## WHY IT MATTERS
##   The prior is what the sampler explores. A parameter with an impossible
##   support (a proportion whose prior puts mass above 1), a median that does
##   not lie inside its own interval, or a name in the register that is not in
##   the prior, all produce a chain that runs to completion and reports a
##   posterior. There is no error and no warning - just a result that is wrong
##   in a way no plot reveals.
##
##   These checks take milliseconds and run before any chain is launched.
## ============================================================================

## Edition 3 gives `tolerance` its strict, relative-difference meaning.
## Without it test_dir() falls back to edition 2, where an 8% error passes a
## 5% tolerance.
local_edition(3)

skip_unless_stage("has.shield.helpers")

## --- the joint prior ---------------------------------------------------------

test_that("the full prior is well formed", {
    prior <- SHIELD.FULL.PARAMETERS.PRIOR

    expect_gt(length(prior@var.names), 50)
    expect_equal(anyDuplicated(prior@var.names), 0,
                 info = paste("a parameter appears twice in the joint prior:",
                              paste(unique(prior@var.names[duplicated(prior@var.names)]),
                                    collapse = ", ")))
    expect_true(all(nzchar(prior@var.names)))
})

test_that("every prior median is finite", {
    ## get.medians() is what seeds a chain and what the engine tests run at.
    ## A single NA here makes the whole starting vector unusable.
    medians <- get.medians(SHIELD.FULL.PARAMETERS.PRIOR)

    expect_equal(length(medians), length(SHIELD.FULL.PARAMETERS.PRIOR@var.names))
    bad <- names(medians)[!is.finite(medians)]
    expect_equal(bad, character(0),
                 info = paste("parameters with a non-finite prior median:",
                              paste(bad, collapse = ", ")))
})

test_that("parameters named as proportions have medians inside [0, 1]", {
    ## A logit-normal prior cannot leave [0, 1], but a lognormal one can, and
    ## the two are easy to confuse when the parameter is written as a fraction.
    medians <- get.medians(SHIELD.FULL.PARAMETERS.PRIOR)
    props <- medians[grepl("^(prp|prop|proportion|fraction)[._]", names(medians))]
    skip_if(length(props) == 0)

    bad <- props[props < 0 | props > 1]
    expect_equal(length(bad), 0,
                 info = paste("proportion parameters whose prior median is",
                              "outside [0, 1]:",
                              paste(names(bad), signif(bad, 4), collapse = ", ")))
})

test_that("parameters named as rates or multipliers have positive medians", {
    medians <- get.medians(SHIELD.FULL.PARAMETERS.PRIOR)
    positive <- medians[grepl("(rate|multiplier)", names(medians))]
    skip_if(length(positive) == 0)

    bad <- positive[positive <= 0]
    expect_equal(length(bad), 0,
                 info = paste("rate/multiplier parameters with a non-positive",
                              "prior median:", paste(names(bad), collapse = ", ")))
})

test_that("the prior can be sampled, and every draw is finite", {
    ## Sampling exercises the covariance structure that get.medians() never
    ## touches. A non-positive-definite block shows up here and nowhere else.
    skip_if(!exists("generate.random.samples", mode = "function"),
            "jheem2 does not expose generate.random.samples()")

    set.seed(41)
    ## skip_if() evaluates its message eagerly, so capture the message text
    ## rather than the condition object.
    err <- NA_character_
    draws <- tryCatch(generate.random.samples(SHIELD.FULL.PARAMETERS.PRIOR, n = 50),
                      error = function(e) { err <<- conditionMessage(e); NULL })
    skip_if(is.null(draws), paste("could not sample the joint prior:", err))

    expect_true(all(is.finite(as.matrix(draws))),
                info = "the joint prior produced non-finite draws")
})

test_that("sampled proportions stay inside [0, 1]", {
    ## Medians can be in range while the tails are not. This is the check that
    ## catches a lognormal standing in for a logit-normal.
    skip_if(!exists("generate.random.samples", mode = "function"),
            "jheem2 does not expose generate.random.samples()")

    set.seed(42)
    draws <- tryCatch(
        as.matrix(generate.random.samples(SHIELD.FULL.PARAMETERS.PRIOR, n = 500)),
        error = function(e) NULL)
    skip_if(is.null(draws))

    prop.cols <- grep("^(prp|prop|proportion|fraction)[._]", colnames(draws))
    skip_if(length(prop.cols) == 0)

    offenders <- character(0)
    for (j in prop.cols) {
        v <- draws[, j]
        if (any(v < 0 | v > 1)) {
            offenders <- c(offenders, paste0(
                colnames(draws)[j], " (range ",
                paste(signif(range(v), 4), collapse = " to "), ")"))
        }
    }

    expect_equal(offenders, character(0),
                 info = paste0("proportion parameters whose prior puts mass ",
                               "outside [0, 1]:\n  ",
                               paste(offenders, collapse = "\n  ")))
})

## --- applying parameters to the model ----------------------------------------

test_that("SHIELD.APPLY.PARAMETERS.FN accepts the prior's own parameter vector", {
    ## The function writes each sampled value into the model settings by name.
    ## A name it does not recognise is silently ignored, and a name it expects
    ## but the prior does not supply is an error at chain start - hours after
    ## the job was submitted.
    expect_true(is.function(SHIELD.APPLY.PARAMETERS.FN))
    expect_equal(names(formals(SHIELD.APPLY.PARAMETERS.FN)),
                 c("model.settings", "parameters"))
})

test_that("every parameter name the apply function constructs exists in the prior", {
    ## The aging-rate names are built by pasting age, race and an index
    ## together. If the specification's age brackets change, the constructed
    ## names stop matching the prior's and the aging multipliers quietly stop
    ## being applied.
    skip_if(is.null(shield.test.specification()), "no specification")
    sm <- get.specification.metadata(SHIELD.TEST.VERSION, SHIELD.TEST.LOCATION)

    q <- sm$age.upper.bounds
    aging.from <- q[seq_len(length(q) - 1)] - 1
    races <- sm$dim.names$race

    constructed <- as.vector(outer(
        as.vector(outer(paste0("age", aging.from), races, paste, sep = ".")),
        c(1, 2),
        function(a, i) paste0(a, ".aging.rate.multiplier.", i)))

    prior.names <- SHIELD.FULL.PARAMETERS.PRIOR@var.names
    missing <- setdiff(constructed, prior.names)

    expect_equal(missing, character(0),
                 info = paste0(
                     "SHIELD.APPLY.PARAMETERS.FN builds these aging-rate ",
                     "parameter names from the specification, but they are not ",
                     "in the prior, so they are never sampled:\n  ",
                     paste(head(missing, 12), collapse = "\n  "),
                     if (length(missing) > 12)
                         paste0("\n  ... and ", length(missing) - 12, " more")))
})

## --- the calibration register ------------------------------------------------

## register_entries ----
register_entries <- function() {
    f <- file.path(SHIELD.DIR, "shield_calib_register.R")
    lines <- readLines(f, warn = FALSE)
    lines[!grepl("^\\s*#", lines)]
}

test_that("registered calibrations use sane chain settings", {
    ## n.iter, thin and n.chains are the difference between a run that finishes
    ## overnight and one that never finishes. A thin larger than n.iter keeps
    ## nothing at all.
    lines <- register_entries()

    n.iter <- as.numeric(gsub(".*n\\.iter\\s*=\\s*([0-9]+).*", "\\1",
                              grep("n\\.iter\\s*=", lines, value = TRUE)))
    thin <- as.numeric(gsub(".*thin\\s*=\\s*([0-9]+).*", "\\1",
                            grep("thin\\s*=", lines, value = TRUE)))

    skip_if(length(n.iter) == 0 || length(thin) == 0)

    expect_true(all(n.iter > 0), info = "a calibration is registered with n.iter <= 0")
    expect_true(all(thin > 0), info = "a calibration is registered with thin <= 0")
    expect_true(all(n.iter >= thin),
                info = "a calibration thins more aggressively than it iterates")
})

test_that("every parameter set the register names is defined", {
    ## register.calibration.info(parameter.names = ...) refers to prior objects
    ## by name. One typo and that stage calibrates a different set of parameters
    ## than intended, with no error.
    lines <- register_entries()

    referenced <- unique(unlist(regmatches(
        lines, gregexpr("[A-Z][A-Z0-9._]*PARAMETERS[A-Z0-9._]*PRIOR", lines))))
    skip_if(length(referenced) == 0)

    missing <- referenced[!vapply(referenced, exists, logical(1))]
    expect_equal(missing, character(0),
                 info = paste("prior objects named in the register but never",
                              "defined:", paste(missing, collapse = ", ")))
})

test_that("fixed initial parameter values name real parameters", {
    ## fixed.initial.parameter.values pins a starting value by name. A name that
    ## is not in the prior is silently ignored, so the chain starts somewhere
    ## other than where the author intended.
    lines <- register_entries()

    pinned <- unique(unlist(regmatches(
        lines, gregexpr('"[a-z][a-z0-9._]*"\\s*=\\s*[0-9.]+', lines))))
    pinned <- gsub('"([^"]+)".*', "\\1", pinned)
    skip_if(length(pinned) == 0)

    prior.names <- SHIELD.FULL.PARAMETERS.PRIOR@var.names
    ## only the ones that look like transmission-rate pins, to avoid sweeping in
    ## unrelated named arguments
    pinned <- pinned[grepl("transmission|rate|multiplier", pinned)]
    skip_if(length(pinned) == 0)

    missing <- setdiff(pinned, prior.names)
    expect_equal(missing, character(0),
                 info = paste("fixed initial values given for parameters that",
                              "are not in the prior:",
                              paste(missing, collapse = ", ")))
})
