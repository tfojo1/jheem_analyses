## ==========================================================================
## UNIT TIER  |  R/shield_multivariate_spline_prior.R
## --------------------------------------------------------------------------
## WHAT THIS FILE COVERS
##   All three multivariate spline prior builders: that uncertainty accumulates
##   away from the baseline year in both directions, that the covariance matches
##   the closed-form random walk, and that the two implementations agree.
##
## RUN JUST THIS FILE
##   Rscript applications/SHIELD/tests/run_tests.R --filter=spline
##
## WHY IT MATTERS
##   The multivariate spline priors are the statistical heart of the calibration:
##   they decide how much the transmission rate is allowed to move between spline
##   years. A covariance built backwards is invisible in every plot and changes
##   every posterior.
##
##   These tests check the mathematical contract rather than frozen numbers, so
##   they stay meaningful when the knot years or sds change.
## ==========================================================================

## Edition 3 gives `tolerance` its strict, relative-difference meaning.
## Without it test_dir() falls back to edition 2, where an 8% error passes a
## 5% tolerance.
local_edition(3)

skip_unless_stage("has.packages", "has.jheem2", "has.standalone")

## --- shared expectations ------------------------------------------------------

## Variance of the log-value at each spline year, in spline.times order.
log_sds <- function(dist) sqrt(diag(dist@sigma))

## The defining property of a random-walk spline prior: uncertainty accumulates
## with distance from the baseline year, in BOTH directions. The baseline year
## itself carries only the baseline sd.
expect_accumulating_uncertainty <- function(dist, spline.times, baseline.year,
                                            n.parameters = 1) {
    sds <- log_sds(dist)
    ## variable order is parameter-within-year
    years <- rep(as.numeric(spline.times), each = n.parameters)
    base <- as.numeric(baseline.year)

    for (p in seq_len(n.parameters)) {
        idx <- seq(p, length(sds), by = n.parameters)
        yr  <- years[idx]
        sd  <- sds[idx]

        past   <- order(yr[yr < base], decreasing = TRUE)   # baseline outward
        future <- order(yr[yr > base])                       # baseline outward

        if (length(past) > 1) {
            expect_true(all(diff(sd[yr < base][past]) > 0),
                        info = "past-side sds must grow as you move away from baseline")
        }
        if (length(future) > 1) {
            expect_true(all(diff(sd[yr > base][future]) > 0),
                        info = "future-side sds must grow as you move away from baseline")
        }
        ## every non-baseline year must be wider than the baseline year
        expect_true(all(sd[yr != base] > sd[yr == base]),
                    info = "every non-baseline year must be wider than the baseline")
    }
}

## --- make.joint.mv.spline.prior (the one the live calibration uses) -----------

joint_args <- function(parameters = "trate",
                       logsd.deltas.past = c("1970" = log(2), "1990" = log(1.5), "1995" = log(1.2)),
                       logsd.deltas.future = c("2010" = log(1.2), "2017" = log(1.5)),
                       correlation = 0.7) {
    list(parameters = parameters,
         logmean.baseline = rep(log(2), length(parameters)),
         logsd.baseline = rep(0.5 * log(2), length(parameters)),
         logsd.deltas.past = logsd.deltas.past,
         logsd.deltas.future = logsd.deltas.future,
         spline.times = c(names(logsd.deltas.past), "2000", names(logsd.deltas.future)),
         correlation = correlation)
}

test_that("the joint prior accumulates uncertainty away from the baseline in both directions", {
    ## This is the property that was wrong: the past-side transformation matrix
    ## was built backwards, so 1995 (adjacent to baseline) was wider than 1970
    ## (five knots away). It reversed the direction of the prior's information.
    a <- joint_args()
    dist <- do.call(make.joint.mv.spline.prior, a)

    expect_accumulating_uncertainty(dist, a$spline.times, "2000", n.parameters = 1)
})

test_that("the joint prior is symmetric when past and future deltas mirror each other", {
    ## With mirrored deltas the prior must be mirrored. This catches a one-sided
    ## bug that a monotonicity check alone could miss.
    deltas <- c(log(2), log(1.5), log(1.2))
    a <- joint_args(
        logsd.deltas.past   = setNames(deltas, c("1970", "1990", "1995")),
        logsd.deltas.future = setNames(rev(deltas), c("2005", "2010", "2030"))
    )
    dist <- do.call(make.joint.mv.spline.prior, a)
    sds <- log_sds(dist)
    names(sds) <- a$spline.times

    ## 1995/2005 are one delta out, 1990/2010 two, 1970/2030 three
    expect_equal(unname(sds["1995"]), unname(sds["2005"]), tolerance = 1e-10)
    expect_equal(unname(sds["1990"]), unname(sds["2010"]), tolerance = 1e-10)
    expect_equal(unname(sds["1970"]), unname(sds["2030"]), tolerance = 1e-10)
})

test_that("the joint prior's variance matches the closed-form random walk", {
    ## var(log value at year y) = var(baseline) + sum of the variances of every
    ## delta between the baseline and y. That is the whole model, and it is
    ## cheap to assert exactly.
    a <- joint_args()
    dist <- do.call(make.joint.mv.spline.prior, a)
    sds <- log_sds(dist)
    names(sds) <- a$spline.times

    base.var <- a$logsd.baseline[1]^2
    past <- a$logsd.deltas.past[order(as.numeric(names(a$logsd.deltas.past)),
                                      decreasing = TRUE)]  # 1995, 1990, 1970
    expected <- base.var + cumsum(past^2)
    expect_equal(unname(sds[names(expected)]^2), unname(expected), tolerance = 1e-10)

    future <- a$logsd.deltas.future[order(as.numeric(names(a$logsd.deltas.future)))]
    expected.f <- base.var + cumsum(future^2)
    expect_equal(unname(sds[names(expected.f)]^2), unname(expected.f), tolerance = 1e-10)
})

test_that("the joint prior's medians equal the baseline mean at every spline year", {
    ## The deltas are centred at zero, so the marginal median at every year is
    ## exp(logmean.baseline) - the prior should not drift with time.
    a <- joint_args()
    dist <- do.call(make.joint.mv.spline.prior, a)
    expect_true(all(abs(dist@mu - a$logmean.baseline[1]) < 1e-12))
})

test_that("the joint prior produces a valid covariance matrix", {
    a <- joint_args()
    dist <- do.call(make.joint.mv.spline.prior, a)

    expect_false(any(is.na(dist@sigma)))
    expect_equal(dist@sigma, t(dist@sigma), tolerance = 1e-10)
    expect_true(all(eigen(dist@sigma, symmetric = TRUE, only.values = TRUE)$values > -1e-8),
                info = "covariance must be positive semi-definite")
})

test_that("correlation = 0 does not produce a NaN covariance", {
    ## The construction divides the diagonal by `correlation` to undo the
    ## off-diagonal scaling. Validation accepts any value in [-1, 1], so 0 is
    ## accepted and yields 0/0. The live call uses 0.7, which is why this has
    ## never been hit - it is one edit away from being hit.
    a <- joint_args(correlation = 0)
    ## The NaNs are caught downstream by the distribution constructor, so the
    ## symptom is an error rather than a bad object. Either way the call must
    ## succeed: correlation = 0 (independent parameters) is a legitimate model.
    dist <- tryCatch(do.call(make.joint.mv.spline.prior, a),
                     error = function(e) e)

    expect_false(
        inherits(dist, "error"),
        info = paste0(
            "correlation = 0 is accepted by validation but divides the ",
            "covariance diagonal by zero, giving NaN. Error was: ",
            if (inherits(dist, "error")) conditionMessage(dist) else "",
            "\nEither guard the zero case or reject it in validation."))

    if (!inherits(dist, "error")) expect_false(any(is.na(dist@sigma)))
})

test_that("multiple parameters are correlated within a year and independent at baseline", {
    a <- joint_args(parameters = c("trate.msm", "trate.het"), correlation = 0.7)
    dist <- do.call(make.joint.mv.spline.prior, a)

    n.par <- 2
    times <- a$spline.times
    idx <- function(param, year) which(times == year) * n.par - n.par + param

    ## baseline block: the two parameters are modelled as independent
    expect_equal(dist@sigma[idx(1, "2000"), idx(2, "2000")], 0, tolerance = 1e-12)

    ## a non-baseline year: the shared deltas induce positive covariance
    expect_gt(dist@sigma[idx(1, "1990"), idx(2, "1990")], 0)

    ## implied correlation between the two parameters' deltas is the requested one
    a0 <- joint_args(parameters = c("p1", "p2"), correlation = 0.7)
    d0 <- do.call(make.joint.mv.spline.prior, a0)
    cov12 <- d0@sigma[idx(1, "1995"), idx(2, "1995")]
    delta.var <- a0$logsd.deltas.past[["1995"]]^2
    expect_equal(cov12, 0.7 * delta.var, tolerance = 1e-10)
})

test_that("the joint prior names its variables parameter-within-year", {
    a <- joint_args(parameters = c("trate.msm", "trate.het"))
    dist <- do.call(make.joint.mv.spline.prior, a)

    expect_equal(dist@var.names,
                 paste0(rep(a$parameters, length(a$spline.times)),
                        rep(a$spline.times, each = length(a$parameters))))
    expect_equal(anyDuplicated(dist@var.names), 0)
})

test_that("the joint prior rejects malformed input", {
    a <- joint_args()

    expect_error(do.call(make.joint.mv.spline.prior,
                         modifyList(a, list(logsd.baseline = c(-1)))),
                 "positive")
    expect_error(do.call(make.joint.mv.spline.prior,
                         modifyList(a, list(correlation = 1.5))),
                 "between 1 and -1")
    ## a "past" delta that is actually later than the baseline year
    bad <- a
    bad$logsd.deltas.past <- c("1970" = log(2), "1990" = log(1.5), "2005" = log(1.2))
    bad$spline.times <- c("1970", "1990", "2000", "2005", "2010", "2017")
    expect_error(do.call(make.joint.mv.spline.prior, bad),
                 "baseline year must be later")

    ## spline.times that does not account for every delta plus one baseline
    bad2 <- a
    bad2$spline.times <- c(bad2$spline.times, "2020")
    expect_error(do.call(make.joint.mv.spline.prior, bad2),
                 "one value per spline time")
})

## --- make.mv.spline.prior -----------------------------------------------------

test_that("make.mv.spline.prior runs and accumulates uncertainty", {
    ## This function is not on the live calibration path, but it is exported in
    ## the same file and is the one a reader is most likely to reach for. It was
    ## left un-runnable by a paste from a diff view.
    spline.times <- c("1970", "1990", "2000", "2010", "2020")
    deltas <- c("1970" = log(2), "1990" = log(1.5),
                "2010" = log(1.5), "2020" = log(2))

    dist <- make.mv.spline.prior(
        parameters = "trate",
        logmean.baseline = log(2),
        logsd.baseline = 0.5 * log(2),
        logsd.deltas = deltas,
        spline.times = spline.times,
        baseline.year = "2000")

    expect_accumulating_uncertainty(dist, spline.times, "2000")
})

test_that("make.mv.spline.prior agrees with the joint version at correlation 1", {
    ## Two implementations of the same model must agree for a single parameter.
    spline.times <- c("1970", "1990", "2000", "2010", "2020")
    past   <- c("1970" = log(2), "1990" = log(1.5))
    future <- c("2010" = log(1.5), "2020" = log(2))

    single <- make.mv.spline.prior(
        parameters = "trate", logmean.baseline = log(2),
        logsd.baseline = 0.5 * log(2), logsd.deltas = c(past, future),
        spline.times = spline.times, baseline.year = "2000")

    joint <- make.joint.mv.spline.prior(
        parameters = "trate", logmean.baseline = log(2),
        logsd.baseline = 0.5 * log(2),
        logsd.deltas.past = past, logsd.deltas.future = future,
        spline.times = spline.times, correlation = 0.7)

    expect_equal(log_sds(single), log_sds(joint), tolerance = 1e-10)
    expect_equal(as.vector(single@mu), as.vector(joint@mu), tolerance = 1e-10)
})

## --- make.mv.spline.prior.five.points -----------------------------------------

test_that("the five-point prior accumulates uncertainty from its 2000 baseline", {
    dist <- make.mv.spline.prior.five.points(
        parameter = "trate",
        logmean00 = log(2), logsd00 = 0.5 * log(2),
        logsd.delta95 = log(1.2), logsd.delta90 = log(1.3),
        logsd.delta70 = log(2),
        logsd.delta10 = log(1.3), logsd.delta17 = log(1.2))

    expect_accumulating_uncertainty(
        dist, c("1970", "1990", "1995", "2000", "2010", "2017"), "2000")
    expect_equal(dist@var.names,
                 paste0("trate", c(1970, 1990, 1995, 2000, 2010, 2017)))
})
