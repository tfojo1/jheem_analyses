## ==========================================================================
## UNIT TIER  |  jheem2 spline after.modifier semantics
## --------------------------------------------------------------------------
## WHAT THIS FILE COVERS
##   The jheem2 behaviour SHIELD relies on to project the transmission and
##   STI-screening splines past their last knot: the calibrated future-change
##   multiplier m REPLACES the base after.modifier (it is not added to it),
##   and on a bounded-logit spline m is not capped at the spline's maximum.
##
## RUN JUST THIS FILE
##   Rscript applications/SHIELD/tests/run_tests.R --filter=functional-form-modifiers
##
## WHY IT MATTERS
##   In jheem2 1.12.3 the after.modifier of a spline is built on the spline's
##   own link and bounds. For STI screening (logit bounded to 0-0.9) that put
##   the future-change multiplier itself on a 0-0.9 logit: with the default
##   (additive) alphas the effective multiplier was 0.9*expit(0.223+log m),
##   i.e. 0.44 for m = 0.75, and with overwrite = TRUE any m >= 0.9 stopped the
##   calibration with an error. jheem2 dev @ 9578726 (1 Oct 2026) builds the
##   change links from the knot bounds instead, so the modifier can be
##   unbounded. These tests fail with an explanation on an older jheem2.
## ==========================================================================

## Edition 3 gives `tolerance` its strict, relative-difference meaning.
local_edition(3)

skip_unless_stage("has.packages", "has.jheem2")

## --- helpers --------------------------------------------------------------------

## jheem2.internal ----
## The clone is sourced into the global environment; the package keeps these
## functions in its namespace. Resolve either way.
jheem2.internal <- function(name) {
    if (exists(name, mode = "function", envir = globalenv()))
        get(name, envir = globalenv())
    else
        utils::getFromNamespace(name, "jheem2")
}

## ff_project ----
## Project a functional form with 'all' alphas (scalar per alpha name).
ff_project <- function(ff, values, years) {
    create.alphas <- jheem2.internal("create.functional.form.alphas")
    set.main      <- jheem2.internal("set.alpha.main.effect.values")
    mdn <- ff$minimum.dim.names
    if (is.null(mdn)) mdn <- list()
    alphas <- lapply(ff$alpha.names, function(nm) {
        a <- create.alphas(ff, nm, maximum.dim.names = mdn)
        if (nm %in% names(values))
            a <- set.main(a, dimension = "all", dimension.values = "all", values = values[[nm]])
        a
    })
    names(alphas) <- ff$alpha.names
    setNames(unlist(ff$project(years = years, alphas = alphas)), years)
}

bounded.logit  <- function(p, max = 0.9) log(p / (max - p))
bounded.expit  <- function(x, max = 0.9) max * stats::plogis(x)

jheem2.label <- function() paste0("(jheem2 in use: ", SHIELD.TEST.ENV$jheem2.source, " ",
                                  SHIELD.TEST.ENV$jheem2.version, ")")

## screening_like_ff ----
## Same settings as get_sti_screening_functional_form(), with fixed knot values.
screening_like_ff <- function() {
    create.linear.spline.functional.form(
        knot.times  = c("2010" = 2010, "2020" = 2020),
        knot.values = list("2010" = 0.05, "2020" = 0.07),
        link = "logit", knot.link = "logit", knots.are.on.transformed.scale = FALSE,
        min = 0, max = 0.9,
        after.time = 2030, after.modifier = 0.5,
        overwrite.modifiers.with.alphas = TRUE,
        modifier.link = "identity", modifier.min = 0, modifier.max = Inf,
        after.modifier.increasing.change.link = "logit",
        after.modifier.decreasing.change.link = "logit")
}

## --- tests ----------------------------------------------------------------------

test_that("a bounded-logit spline accepts an unbounded after.modifier", {
    ## jheem2 1.12.3 builds the change links with modifier.min/modifier.max, so
    ## modifier.max = Inf makes the logit change link invalid and the
    ## specification fails to build at the STI-screening element.
    ff <- tryCatch(screening_like_ff(), error = function(e) conditionMessage(e))
    expect_true(inherits(ff, "functional.form"),
                info = paste0(
                    "Building the STI-screening-style spline failed: ", if (is.character(ff)) ff, "\n",
                    "SHIELD's screening spline needs jheem2 dev @ 9578726 (1 Oct 2026) or later, ",
                    "which builds the after.modifier change links from the knot bounds ",
                    jheem2.label()))
})

test_that("on a bounded logit, m scales the last-interval change and is not capped at 0.9", {
    ff <- tryCatch(screening_like_ff(), error = function(e) NULL)
    skip_if(is.null(ff), paste("screening-style spline does not build", jheem2.label()))

    for (m in c(0.25, 0.75, 1.5, 2.5)) {
        got <- ff_project(ff, list(after.modifier = m), 2030)
        expected <- bounded.expit(bounded.logit(0.07) + m * (bounded.logit(0.07) - bounded.logit(0.05)))
        expect_equal(unname(got), expected, tolerance = 1e-8,
                     info = paste0("m = ", m, ": logit(p2030) should equal logit(p2020) + m * ",
                                   "[logit(p2020) - logit(p2010)] on the 0-0.9 bounded logit ",
                                   jheem2.label()))
    }
})

test_that("with overwrite.modifiers.with.alphas = TRUE the calibrated m replaces the base value", {
    ## The default (FALSE) ADDS the alpha to the base after.modifier: with the
    ## base of 0.5 the model used 0.5 + m (mean 1.25) instead of m (mean 0.75).
    make <- function(overwrite) create.natural.spline.functional.form(
        knot.times = c("2010" = 2010, "2022" = 2022),
        knot.values = list("2010" = 0, "2022" = 0), knots.are.on.transformed.scale = TRUE,
        knot.link = "log", link = "identity", min = 0,
        after.time = 2034, after.modifier = 0.5,
        overwrite.modifiers.with.alphas = overwrite, modifiers.apply.to.change = TRUE,
        after.modifier.increasing.change.link = "identity",
        after.modifier.decreasing.change.link = "log")

    vals <- list(`2010` = 1.2, `2022` = 2.4, after.modifier = 0.75)
    expect_equal(unname(ff_project(make(TRUE), vals, 2034)), 2.4 + 0.75 * 1.2, tolerance = 1e-8,
                 info = paste("overwrite = TRUE should use a = m", jheem2.label()))
    expect_equal(unname(ff_project(make(FALSE), vals, 2034)), 2.4 + (0.5 + 0.75) * 1.2, tolerance = 1e-8,
                 info = paste("documents the additive default: a = base + m", jheem2.label()))
})
