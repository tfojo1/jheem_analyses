## ==========================================================================
## UNIT TIER  |  intervention/doxy_effectiveness.R
## --------------------------------------------------------------------------
## WHAT THIS FILE COVERS
##   That the lognormal fitted to the published doxy-PEP relative risk actually
##   reproduces that RR and its 95% CI, and that the draws are seeded, capped
##   at 1, and usable as a proportion.
##
## RUN JUST THIS FILE
##   Rscript applications/SHIELD/tests/run_tests.R --filter=doxy
##
## WHY IT MATTERS
##   Doxy-PEP efficacy is the single number that drives every intervention result
##   in the manuscript. It is derived from one published RR and its 95% CI, so the
##   derivation has to reproduce that CI.
## ==========================================================================

## Edition 3 gives `tolerance` its strict, relative-difference meaning.
## Without it test_dir() falls back to edition 2, where an 8% error passes a
## 5% tolerance.
local_edition(3)

skip_unless_stage("has.packages", "has.standalone")

## The Luetkemeyer et al. estimate the model is built on.
RR.MEAN <- 0.20
RR.LO   <- 0.08
RR.HI   <- 0.48

test_that("the fitted lognormal reproduces the published confidence interval", {
    ## This is the whole contract of fit_rr_lognorm_from_mean_ci(): take a
    ## published RR and CI, return the lognormal that produced them.
    ##
    ## It currently does not. The function applies
    ##     meanlog = log(rr_mean) - 0.5 * sdlog^2
    ## which centres the distribution so that its ARITHMETIC MEAN is rr_mean.
    ## But a published RR with a symmetric-on-the-log-scale CI is the MEDIAN,
    ## not the arithmetic mean: here (log(0.08) + log(0.48)) / 2 = log(0.196),
    ## which is the reported 0.20 to within rounding. Subtracting the variance
    ## correction shifts the entire distribution down, so every simulated RR is
    ## too small and every efficacy (1 - RR) is too large.
    p <- fit_rr_lognorm_from_mean_ci(RR.MEAN, RR.LO, RR.HI)

    fitted.lo <- qlnorm(0.025, meanlog = p$meanlog, sdlog = p$sdlog)
    fitted.hi <- qlnorm(0.975, meanlog = p$meanlog, sdlog = p$sdlog)

    expect_equal(fitted.lo, RR.LO, tolerance = 0.05,
                 info = paste0("fitted 2.5% limit is ", signif(fitted.lo, 4),
                               ", published limit is ", RR.LO))
    expect_equal(fitted.hi, RR.HI, tolerance = 0.05,
                 info = paste0("fitted 97.5% limit is ", signif(fitted.hi, 4),
                               ", published limit is ", RR.HI))
})

test_that("the fitted sdlog matches the published interval width", {
    ## This part of the derivation is correct and worth pinning: sdlog is the
    ## log-scale CI width divided by the z-span.
    p <- fit_rr_lognorm_from_mean_ci(RR.MEAN, RR.LO, RR.HI)
    expect_equal(p$sdlog, (log(RR.HI) - log(RR.LO)) / (qnorm(0.975) - qnorm(0.025)),
                 tolerance = 1e-12)
})

test_that("the point estimate is recovered as the distribution's centre", {
    ## Whichever convention is chosen, exp(meanlog) must land on the published
    ## point estimate, because that is what the CI is centred on.
    p <- fit_rr_lognorm_from_mean_ci(RR.MEAN, RR.LO, RR.HI)
    expect_equal(exp(p$meanlog), RR.MEAN, tolerance = 0.05,
                 info = paste0("median of the fitted distribution is ",
                               signif(exp(p$meanlog), 4),
                               " but the published RR is ", RR.MEAN))
})

test_that("the fit is invariant to the scale of the RR", {
    ## A property test: multiplying the estimate and both limits by a constant
    ## must shift meanlog by log(constant) and leave sdlog alone.
    a <- fit_rr_lognorm_from_mean_ci(RR.MEAN, RR.LO, RR.HI)
    b <- fit_rr_lognorm_from_mean_ci(RR.MEAN * 3, RR.LO * 3, RR.HI * 3)

    expect_equal(b$sdlog, a$sdlog, tolerance = 1e-12)
    expect_equal(b$meanlog - a$meanlog, log(3), tolerance = 1e-12)
})

test_that("non-positive RRs are rejected", {
    expect_error(fit_rr_lognorm_from_mean_ci(0, RR.LO, RR.HI), "must be > 0")
    expect_error(fit_rr_lognorm_from_mean_ci(RR.MEAN, 0, RR.HI), "must be > 0")
    expect_error(fit_rr_lognorm_from_mean_ci(RR.MEAN, RR.LO, -1), "must be > 0")
})

test_that("draws are reproducible under set.seed", {
    set.seed(11)
    a <- draw_rr_lognorm(500, RR.MEAN, RR.LO, RR.HI)
    set.seed(11)
    b <- draw_rr_lognorm(500, RR.MEAN, RR.LO, RR.HI)
    expect_identical(a, b)
})

test_that("draws are capped at 1 and strictly positive", {
    set.seed(12)
    rr <- draw_rr_lognorm(5000, RR.MEAN, RR.LO, RR.HI, cap_at_one = TRUE)

    expect_length(rr, 5000)
    expect_true(all(rr > 0))
    expect_true(all(rr <= 1),
                info = "an RR above 1 becomes a negative efficacy in the intervention")

    ## Efficacy = 1 - RR must therefore be a valid proportion.
    eff <- 1 - rr
    expect_true(all(eff >= 0 & eff < 1))
})

test_that("uncapped draws can exceed 1, which is why the cap exists", {
    set.seed(13)
    rr <- draw_rr_lognorm(20000, RR.MEAN, RR.LO, RR.HI, cap_at_one = FALSE)
    expect_true(max(rr) > 1,
                info = "with this CI the lognormal has mass above RR = 1")
})

test_that("the sampled distribution matches the fitted one", {
    ## Guards against the draw function quietly using different parameters than
    ## the fit function returns.
    set.seed(14)
    rr <- draw_rr_lognorm(200000, RR.MEAN, RR.LO, RR.HI, cap_at_one = FALSE)
    p <- fit_rr_lognorm_from_mean_ci(RR.MEAN, RR.LO, RR.HI)

    expect_equal(mean(log(rr)), p$meanlog, tolerance = 0.02)
    expect_equal(sd(log(rr)), p$sdlog, tolerance = 0.02)
})

test_that("the efficacy values wired into the interventions are a valid proportion", {
    ## intervention_definitions.R builds DOXY.PARAMS from these draws. Whatever
    ## the centring convention, the values handed to the engine must be
    ## proportions, one row named doxy.effectiveness.
    set.seed(15)
    eff <- 1 - draw_rr_lognorm(1000, RR.MEAN, RR.LO, RR.HI, cap_at_one = TRUE)
    params <- matrix(eff, nrow = 1, dimnames = list("doxy.effectiveness", NULL))

    expect_equal(nrow(params), 1)
    expect_equal(rownames(params), "doxy.effectiveness")
    expect_equal(ncol(params), 1000)
    expect_true(all(params >= 0 & params <= 1))
})
