## ============================================================================
## WHAT THIS FILE COVERS
##   do.calculate.variance.parameters(), in
##   R/shield_calculating_error_terms_for_likelihoods.R
##
## WHY IT MATTERS
##   This function estimates how noisy the surveillance data are. Its output -
##   a coefficient of variation, and sometimes an exponent of variance - becomes
##   the `error.variance.term` on the basic likelihood instructions, which is
##   what decides how hard the calibration tries to match each data point.
##
##   Too small a CV and the sampler chases noise. Too large and the data stop
##   constraining the model at all. Either way the chain still runs and still
##   produces a posterior, so there is nothing to notice.
##
##   The estimator has a closed form, so it can be checked exactly against data
##   with a known error structure. That is what these tests do: generate errors
##   with a CV we chose, and confirm the function recovers it.
## ============================================================================

## Edition 3 gives `tolerance` its strict, relative-difference meaning.
## Without it test_dir() falls back to edition 2, where an 8% error passes a
## 5% tolerance.
local_edition(3)

skip_unless_stage("has.packages", "has.jheem2")

## The file's top level reads the surveillance manager, so source only the
## function definitions into a private environment.
error_terms_env <- function() {
    if (is.null(SHIELD.TEST.ENV$error.terms.env)) {
        f <- file.path(SHIELD.DIR, "R/shield_calculating_error_terms_for_likelihoods.R")
        env <- new.env(parent = globalenv())
        exprs <- parse(f)
        ## evaluate only the function definitions; skip the top-level calls that
        ## need SURVEILLANCE.MANAGER
        for (e in exprs) {
            if (is.call(e) && length(e) >= 3 &&
                as.character(e[[1]]) %in% c("<-", "=") &&
                is.call(e[[3]]) && identical(as.character(e[[3]][[1]]), "function")) {
                eval(e, envir = env)
            }
        }
        SHIELD.TEST.ENV$error.terms.env <- env
    }
    SHIELD.TEST.ENV$error.terms.env
}

do_calc <- function(...) {
    env <- error_terms_env()
    if (!exists("do.calculate.variance.parameters", envir = env)) {
        testthat::skip("do.calculate.variance.parameters() is not defined")
    }
    get("do.calculate.variance.parameters", envir = env)(...)
}

test_that("a known coefficient of variation is recovered", {
    ## Generate errors whose sd is exactly cv * x. The estimator
    ##   cv = sqrt(mean(e^2 / x^2))
    ## must return that cv.
    set.seed(31)
    x <- runif(20000, 200, 5000)
    true.cv <- 0.15
    e <- rnorm(length(x), mean = 0, sd = true.cv * x)

    est <- do_calc(e = e, x = x, output = "cv", verbose = FALSE)

    expect_true(is.numeric(est) && length(est) == 1)
    expect_equal(as.numeric(est), true.cv, tolerance = 0.03,
                 info = paste("estimated CV", signif(est, 4),
                              "for a true CV of", true.cv))
})

test_that("the estimated CV scales with the noise, not with the counts", {
    ## A coefficient of variation is by definition scale-free: multiplying every
    ## count and every error by the same factor must leave it unchanged. If it
    ## does change, the estimator is really returning an absolute sd.
    set.seed(32)
    x <- runif(10000, 100, 2000)
    e <- rnorm(length(x), 0, 0.2 * x)

    small <- do_calc(e = e, x = x, output = "cv", verbose = FALSE)
    large <- do_calc(e = e * 10, x = x * 10, output = "cv", verbose = FALSE)

    expect_equal(as.numeric(large), as.numeric(small), tolerance = 1e-8)
})

test_that("doubling the noise doubles the estimated CV", {
    set.seed(33)
    x <- runif(10000, 100, 2000)
    e <- rnorm(length(x), 0, 0.1 * x)

    base <- do_calc(e = e, x = x, output = "cv", verbose = FALSE)
    noisier <- do_calc(e = e * 2, x = x, output = "cv", verbose = FALSE)

    expect_equal(as.numeric(noisier), 2 * as.numeric(base), tolerance = 1e-8)
})

test_that("a known absolute sd is recovered", {
    ## The 'sd' branch: constant-variance errors, independent of the count.
    set.seed(34)
    x <- runif(20000, 200, 5000)
    true.sd <- 40
    e <- rnorm(length(x), 0, true.sd)

    est <- do_calc(e = e, x = x, output = "sd", verbose = FALSE)
    expect_equal(as.numeric(est), true.sd, tolerance = 0.05)
})

test_that("missing values are dropped rather than propagated", {
    ## Surveillance data have holes. An NA reaching the sum would make the whole
    ## error term NA, and an NA error term silently disables a likelihood.
    set.seed(35)
    x <- runif(5000, 200, 5000)
    e <- rnorm(length(x), 0, 0.15 * x)

    clean <- do_calc(e = e, x = x, output = "cv", verbose = FALSE)

    x.holes <- c(x, NA, 1000)
    e.holes <- c(e, 50, NA)
    holed <- do_calc(e = e.holes, x = x.holes, output = "cv", verbose = FALSE)

    expect_true(is.finite(holed), info = "NA input produced a non-finite CV")
    expect_equal(as.numeric(holed), as.numeric(clean), tolerance = 0.02)
})

test_that("zero counts do not produce an infinite CV", {
    ## The CV divides by x. A stratum reported as zero must be excluded, not
    ## turned into Inf.
    set.seed(36)
    x <- c(runif(5000, 200, 5000), 0, 0)
    e <- c(rnorm(5000, 0, 0.15 * 1000), 5, -5)

    est <- do_calc(e = e, x = x, output = "cv", verbose = FALSE)
    expect_true(is.finite(est), info = "a zero count produced a non-finite CV")
})

test_that("the CV the likelihoods actually use is in a plausible range", {
    ## shield_likelihoods.R computes a CV for ps.syphilis.diagnoses at source
    ## time and prints it. A CV outside roughly 1%-100% means the error model is
    ## either claiming the data are near-perfect or that they carry no
    ## information - both of which change the calibration completely.
    skip_unless_likelihoods()
    skip_if(!exists("diagnosis_cv"), "diagnosis_cv is not defined")

    expect_true(is.numeric(diagnosis_cv))
    expect_true(all(is.finite(diagnosis_cv)))
    expect_true(all(diagnosis_cv > 0.01 & diagnosis_cv < 1.0),
                info = paste("implausible diagnosis CV:",
                             paste(signif(diagnosis_cv, 4), collapse = ", ")))
})
