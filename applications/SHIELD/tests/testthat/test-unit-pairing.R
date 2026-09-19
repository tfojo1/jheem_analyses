## ==========================================================================
## UNIT TIER  |  R/shield_inputManager_pairing.R
## --------------------------------------------------------------------------
## WHAT THIS FILE COVERS
##   The sexual mixing maths: observed/expected ratios, pairing proportions,
##   age-mixing matrices, the fitted age-difference model, and the two headerless
##   age CSVs the pairing manager reads.
##
## RUN JUST THIS FILE
##   Rscript applications/SHIELD/tests/run_tests.R --filter=pairing
##
## WHY IT MATTERS
##   The pairing manager turns published partnership counts into the mixing
##   matrices the transmission model runs on. Every function here has an exact
##   mathematical contract, and none of them needs a data manager.
## ==========================================================================

## Edition 3 gives `tolerance` its strict, relative-difference meaning.
## Without it test_dir() falls back to edition 2, where an 8% error passes a
## 5% tolerance.
local_edition(3)

skip_unless_stage("has.packages", "has.standalone")

## --- calculate.oe.ratios ------------------------------------------------------

test_that("observed/expected ratios are 1 everywhere under proportional mixing", {
    ## If partnerships are formed at random, observed equals expected by
    ## construction, so every O/E is exactly 1. This is the single strongest
    ## check on the definition of "expected".
    marginals <- c(black = 0.2, hispanic = 0.3, other = 0.5)
    col.totals <- c(black = 400, hispanic = 1000, other = 600)
    proportional <- outer(marginals, col.totals)

    oe <- calculate.oe.ratios(proportional)

    expect_equal(unname(oe), matrix(1, 3, 3), tolerance = 1e-12)
})

test_that("assortative mixing gives on-diagonal O/E above 1", {
    counts <- matrix(c(72, 33, 176,
                       56, 130, 562,
                       120, 301, 2023),
                     nrow = 3, byrow = TRUE,
                     dimnames = list(from = c("black", "hispanic", "other"),
                                     to   = c("black", "hispanic", "other")))
    oe <- calculate.oe.ratios(counts)

    expect_equal(dim(oe), c(3L, 3L))
    expect_true(all(oe > 0))
    expect_true(all(diag(oe) > 1),
                info = "same-race pairings should be over-represented")
})

test_that("O/E is invariant to rescaling the whole table", {
    counts <- matrix(c(72, 33, 176, 56, 130, 562, 120, 301, 2023),
                     nrow = 3, byrow = TRUE)
    expect_equal(calculate.oe.ratios(counts),
                 calculate.oe.ratios(counts * 7.5),
                 tolerance = 1e-12)
})

## --- get.pairing.proportions --------------------------------------------------

test_that("pairing proportions have columns summing to exactly 1", {
    ## The model's convention throughout is that a column is the distribution of
    ## partners for one receiving group, so it must be a probability vector.
    oe <- calculate.oe.ratios(matrix(c(72, 33, 176, 56, 130, 562, 120, 301, 2023),
                                     nrow = 3, byrow = TRUE))
    props <- get.pairing.proportions(oe, marginal.counts = c(1000, 1500, 6000))

    expect_equal(unname(colSums(props)), rep(1, 3), tolerance = 1e-12)
    expect_true(all(props >= 0))
})

test_that("pairing proportions reduce to the marginals when O/E is uniform", {
    ## No preferential mixing means partners are drawn in proportion to
    ## population share, regardless of the receiving group.
    oe <- matrix(1, 3, 3)
    marginals <- c(1000, 1500, 6000)
    props <- get.pairing.proportions(oe, marginal.counts = marginals)

    expected <- marginals / sum(marginals)
    for (j in 1:3) expect_equal(unname(props[, j]), unname(expected), tolerance = 1e-12)
})

test_that("pairing proportions reject non-conformable input", {
    expect_error(get.pairing.proportions(matrix(1, 3, 4), c(1, 2, 3)), "square")
    expect_error(get.pairing.proportions(matrix(1, 3, 3), c(1, 2)), "same as the dimension")
})

## --- get.age.mixing.proportions -----------------------------------------------

age_mixing_fixture <- function(sd.multiplier = 1) {
    age.cutoffs <- c(15, 20, 25, 30, 35, 40, 45, 50, 55, 65)
    get.age.mixing.proportions(
        age.delta.intercept.mean = 0,
        age.delta.slope.mean     = 0,
        age.delta.intercept.sd   = 5,
        age.delta.slope.sd       = 0,
        age.cutoffs              = age.cutoffs,
        age.labels               = paste0(head(age.cutoffs, -1), "-",
                                          tail(age.cutoffs, -1)),
        single.year.age.counts   = fixture_single_year_age_counts(),
        sd.multiplier            = sd.multiplier)
}

test_that("age mixing proportions have columns summing to 1", {
    ## Documented contract: "age.to columns summing to 1".
    m <- age_mixing_fixture()

    expect_equal(unname(colSums(m)), rep(1, ncol(m)), tolerance = 1e-10)
    expect_true(all(m >= 0))
    expect_equal(names(dimnames(m)), c("age.from", "age.to"))
})

test_that("age mixing is concentrated on the diagonal with no age preference", {
    ## With mean age difference 0, the most likely partner bracket is the
    ## receiver's own bracket.
    m <- age_mixing_fixture()
    for (j in seq_len(ncol(m))) {
        expect_equal(unname(which.max(m[, j])), j,
                     info = paste("column", j, "peaks in the wrong bracket"))
    }
})

test_that("a larger sd multiplier spreads contacts away from the diagonal", {
    ## sd.multiplier is a calibrated parameter, so its direction of effect must
    ## be right: bigger multiplier, flatter mixing.
    tight <- age_mixing_fixture(sd.multiplier = 0.5)
    loose <- age_mixing_fixture(sd.multiplier = 3)

    expect_true(all(diag(loose) < diag(tight)),
                info = "raising sd.multiplier must reduce same-age mixing")
})

test_that("a positive mean age delta shifts partners older", {
    age.cutoffs <- c(15, 20, 25, 30, 35, 40, 45, 50, 55, 65)
    labels <- paste0(head(age.cutoffs, -1), "-", tail(age.cutoffs, -1))
    mk <- function(intercept) get.age.mixing.proportions(
        age.delta.intercept.mean = intercept, age.delta.slope.mean = 0,
        age.delta.intercept.sd = 5, age.delta.slope.sd = 0,
        age.cutoffs = age.cutoffs, age.labels = labels,
        single.year.age.counts = fixture_single_year_age_counts(),
        sd.multiplier = 1)

    ## mean age of the partner distribution for a mid-range receiving bracket
    bracket.mids <- (head(age.cutoffs, -1) + tail(age.cutoffs, -1)) / 2
    mean.partner.age <- function(m, j) sum(m[, j] * bracket.mids)

    expect_gt(mean.partner.age(mk(5), 4), mean.partner.age(mk(0), 4))
    expect_lt(mean.partner.age(mk(-5), 4), mean.partner.age(mk(0), 4))
})

test_that("infinite age cutoffs are rejected", {
    ## The normal-difference integral is undefined at an open-ended top bracket,
    ## so this must fail loudly rather than return NaN.
    expect_error(
        get.age.mixing.proportions(0, 0, 5, 0,
                                   age.cutoffs = c(15, 20, Inf),
                                   age.labels = c("15-20", "20+"),
                                   single.year.age.counts = fixture_single_year_age_counts(),
                                   sd.multiplier = 1),
        "cannot be infinite")
})

## --- fit.age.model ------------------------------------------------------------

test_that("fit.age.model recovers a known linear age-difference model", {
    ## Generate partners from Delta = b0 + b1 * age + N(0, sd) and check the
    ## fitted coefficients come back.
    set.seed(21)
    n <- 20000
    age.ref <- runif(n, 18, 60)
    b0 <- -2; b1 <- 0.1
    age.partner <- age.ref + b0 + b1 * age.ref + rnorm(n, 0, 4)

    fit <- fit.age.model(age.ref, age.partner)

    expect_named(fit, c("mean.intercept", "mean.slope", "sd.intercept", "sd.slope"))
    expect_equal(unname(fit["mean.intercept"]), b0, tolerance = 0.3)
    expect_equal(unname(fit["mean.slope"]), b1, tolerance = 0.05)
    ## constant residual sd: the fitted sd slope should be near zero
    expect_equal(unname(fit["sd.slope"]), 0, tolerance = 0.05)
    expect_equal(unname(fit["sd.intercept"]), 4, tolerance = 0.6)
})

## --- the age CSVs the pairing manager reads -----------------------------------

test_that("the age-pairing CSVs are read without losing the first observation", {
    ## These files are headerless pairs of ages. Reading them with header = TRUE
    ## silently consumes the first partnership as a column name and coerces both
    ## columns to character, which then propagates into fit.age.model().
    dir <- file.path(SHIELD.DIR, "data_files/pairing")
    skip_if_not(dir.exists(dir))

    for (f in c("heterosexual_age.csv", "msm_age.csv")) {
        path <- file.path(dir, f)
        expect_true(file.exists(path), info = path)

        raw <- read.csv(path, header = FALSE)
        n.lines <- length(readLines(path, warn = FALSE))

        expect_equal(nrow(raw), n.lines,
                     info = paste(f, "lost a row: the file has no header"))
        expect_equal(ncol(raw), 2, info = paste(f, "should be two age columns"))
        expect_true(all(vapply(raw, is.numeric, logical(1))),
                    info = paste(f, "columns must be numeric; a header row would",
                                 "coerce them to character"))
        expect_false(any(is.na(raw)), info = paste(f, "contains NA ages"))
        expect_true(all(raw > 0 & raw < 120), info = paste(f, "has implausible ages"))
    }
})

test_that("the pairing manager builds and its proportions are well formed", {
    dir <- file.path(SHIELD.DIR, "data_files/pairing")
    skip_if_not(dir.exists(dir))

    mgr <- create.pairing.manager(dir = dir)

    ## the fitted age models must be finite
    for (nm in names(mgr$sex.age.models)) {
        fit <- mgr$sex.age.models[[nm]]
        expect_true(all(is.finite(fit)),
                    info = paste("non-finite age model coefficients for", nm))
        expect_gt(unname(fit["sd.intercept"]), 0)
    }

    ## same-race O/E ratios above 1 (assortative), as the comments claim
    for (nm in c("oe.sexual.byrace.bb", "oe.sexual.byrace.hh", "oe.sexual.byrace.oo")) {
        expect_gt(mgr[[nm]], 1)
    }
})
