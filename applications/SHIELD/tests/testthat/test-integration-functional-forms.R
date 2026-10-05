## ==========================================================================
## INTEGRATION TIER  |  the specification's time-varying functional forms
## --------------------------------------------------------------------------
## WHAT THIS FILE COVERS
##   The real functional forms the specification builds for the elements whose
##   future trend is calibrated (transmission MSM / heterosexual, STI
##   screening), and the way SHIELD.APPLY.PARAMETERS.FN writes alphas into them:
##     * the calibrated future-change multiplier REPLACES the base after.modifier
##     * the projection interval equals the last knot interval
##     * after.time = last knot + m * (last-interval change), on the change link
##     * no alpha is written twice to the same element / knot / dimension value
##     * the female screening multiplier reaches females and heterosexual men,
##       and not MSM
##
## RUN JUST THIS FILE
##   Rscript applications/SHIELD/tests/run_tests.R --filter=integration-functional-forms
##
## WHY IT MATTERS
##   All of these failed silently in the September 2026 calibration
##   (calib.9.23.stage3.pk): the future multiplier was added to 0.5, a
##   12-year change was projected over 8 years, the STI-screening multiplier
##   was capped at 0.9, and a later alpha call overwrote
##   screening.rate.multiplier.heterosexuals.2020 so that it had no effect at
##   all. None of these change a fit statistic or a plot until the projections
##   look wrong.
## ==========================================================================

local_edition(3)

skip_unless_stage("has.shield.helpers")

## --- what the specification is expected to do -----------------------------------
## Elements whose after.modifier is calibrated, the parameter that sets it, and
## the link on which the future change is applied (increasing / decreasing trend).
FUTURE.CHANGE.ELEMENTS <- list(
    transmission.rate.msm = list(
        parameter = "transmission.rate.future.change.mult",
        increasing = "identity", decreasing = "log", max = Inf),
    transmission.rate.heterosexual = list(
        parameter = "transmission.rate.future.change.mult",
        increasing = "identity", decreasing = "log", max = Inf),
    rate.sti.screening.over.14.without.covid = list(
        parameter = "screening.rate.future.change.mult",
        increasing = "logit", decreasing = "logit", max = 0.9)
)

## --- helpers --------------------------------------------------------------------

jheem2.internal <- function(name) {
    if (exists(name, mode = "function", envir = globalenv()))
        get(name, envir = globalenv())
    else
        utils::getFromNamespace(name, "jheem2")
}

jheem2.label <- function() paste0("(jheem2 in use: ", SHIELD.TEST.ENV$jheem2.source, " ",
                                  SHIELD.TEST.ENV$jheem2.version, ")")

## shield_test_kernel ----
## The specification kernel holds each element's evaluated functional form
## (including the ones built by get.functional.form.function, which need the
## data managers). Built once per run.
shield_test_kernel <- function() {
    use_repo_root()
    if (is.null(SHIELD.TEST.ENV$ff.kernel)) {
        if (is.null(shield.test.specification())) return(NULL)
        SHIELD.TEST.ENV$ff.kernel <- tryCatch(
            jheem2.internal("create.jheem.kernel")(SHIELD.TEST.VERSION, SHIELD.TEST.LOCATION),
            error = function(e) { SHIELD.TEST.ENV$ff.kernel.error <- conditionMessage(e); NA })
    }
    if (identical(SHIELD.TEST.ENV$ff.kernel, NA)) NULL else SHIELD.TEST.ENV$ff.kernel
}

skip_unless_kernel <- function() {
    skip_unless_slow()
    k <- shield_test_kernel()
    if (is.null(k))
        testthat::skip(paste("could not build the specification kernel:",
                             SHIELD.TEST.ENV$ff.kernel.error, jheem2.label()))
    k
}

element_ff <- function(kernel, element) kernel$element.backgrounds[[element]]$functional.form

## ff_project ----
## Project with 'all' alphas; returns a list (one array or scalar per year).
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
    setNames(ff$project(years = years, alphas = alphas), years)
}

## link_fn ----
link_fn <- function(type, max) switch(type,
    identity = function(x) x,
    log      = function(x) log(x),
    logit    = function(x) log(x / (max - x)))

## --- the functional forms -------------------------------------------------------

test_that("each calibrated future-change spline overwrites (does not add to) its after.modifier", {
    kernel <- skip_unless_kernel()
    for (element in names(FUTURE.CHANGE.ELEMENTS)) {
        ff <- element_ff(kernel, element)
        expect_false(is.null(ff), info = paste0("no functional form for '", element, "'"))
        if (is.null(ff)) next
        expect_true("after.modifier" %in% ff$alpha.names,
                    info = paste0("'", element, "' has no after.modifier alpha"))
        expect_false(isTRUE(ff$alphas.are.additive[["after.modifier"]]),
                     info = paste0(
                         "'", element, "': after.modifier alphas are ADDED to the base value, so ",
                         FUTURE.CHANGE.ELEMENTS[[element]]$parameter, " = m is applied as base + m. ",
                         "Set overwrite.modifiers.with.alphas = TRUE in its functional form."))
    }
})

test_that("each future projection interval equals the last knot interval", {
    ## The after.modifier multiplies the change over the LAST knot interval and
    ## applies it over (after.time - last knot). If the two differ, m no longer
    ## means "fraction of the past pace": with a 12-year interval projected over
    ## 8 years, m = 0.75 is 1.125x the past annual pace.
    kernel <- skip_unless_kernel()
    for (element in names(FUTURE.CHANGE.ELEMENTS)) {
        ff <- element_ff(kernel, element)
        if (is.null(ff)) next                                  # reported by the first test
        kt <- sort(ff$knot.times)                              # includes after.time
        n <- length(kt)
        last.interval   <- kt[n - 1] - kt[n - 2]
        future.interval <- kt[n] - kt[n - 1]
        expect_equal(unname(future.interval), unname(last.interval),
                     info = paste0("'", element, "': last knot interval is ", last.interval,
                                   " years but after.time is ", future.interval,
                                   " years after the last knot, so the projected annual pace is ",
                                   round(last.interval / future.interval, 2), " x m times the past pace"))
    }
})

test_that("the after.time knot is last knot + m x (last-interval change) on the change link", {
    kernel <- skip_unless_kernel()
    for (element in names(FUTURE.CHANGE.ELEMENTS)) {
        spec <- FUTURE.CHANGE.ELEMENTS[[element]]
        ff <- element_ff(kernel, element)
        if (is.null(ff)) next                                  # reported by the first test
        kt <- sort(ff$knot.times)
        n <- length(kt)
        knot.names <- names(kt)[c(n - 2, n - 1)]          # penultimate, last real knot
        years <- unname(kt[c(n - 2, n - 1, n)])

        ## a rising and a falling trend between the last two knots (alphas are
        ## multipliers on the knot scale: rate multipliers or odds ratios)
        for (trend in list(c(1, 1.5), c(1.5, 1))) {
            for (m in c(0.25, 0.75, 1.5)) {
                vals <- setNames(list(trend[1], trend[2], m), c(knot.names, "after.modifier"))
                p <- ff_project(ff, vals, years)
                prev <- as.numeric(p[[1]]); last <- as.numeric(p[[2]]); after <- as.numeric(p[[3]])
                rising <- last >= prev
                ok <- vapply(seq_along(last), function(i) {
                    g <- link_fn(if (rising[i]) spec$increasing else spec$decreasing, spec$max)
                    isTRUE(all.equal(g(after[i]), g(last[i]) + m * (g(last[i]) - g(prev[i])),
                                     tolerance = 1e-8))
                }, logical(1))
                expect_true(all(ok), info = paste0(
                    "'", element, "', m = ", m, ", trend ", paste(trend, collapse = " -> "), ": ",
                    sum(!ok), " of ", length(ok), " cells do not follow ",
                    "g(after) = g(last) + m * (g(last) - g(prev)). ", jheem2.label()))
            }
        }
    }
})

## --- the apply function ---------------------------------------------------------

test_that("SHIELD.APPLY.PARAMETERS.FN never writes the same alpha twice", {
    ## jheem2 stores main-effect alphas by (element, alpha, dimension, value).
    ## A second write to the same slot REPLACES the first, so the first
    ## parameter silently has no effect. This is how
    ## screening.rate.multiplier.heterosexuals.2020 was lost in calib.9.23.
    skip_if(is.null(shield.test.specification()), "no specification")
    sm <- get.specification.metadata(SHIELD.TEST.VERSION, SHIELD.TEST.LOCATION)

    log <- new.env(); log$keys <- character(0)
    record <- function(element.name, alpha.name, dimension, dim.values) {
        log$keys <- c(log$keys, paste(element.name, alpha.name, dimension, dim.values, sep = " | "))
    }
    mocks <- list(
        set.element.functional.form.main.effect.alphas = function(model.settings, element.name, alpha.name,
                                                                  values, dimension,
                                                                  applies.to.dimension.values = names(values), ...)
            record(element.name, alpha.name, dimension, as.character(applies.to.dimension.values)),
        set.element.functional.form.interaction.alphas = function(model.settings, element.name, alpha.name,
                                                                  value, applies.to.dimension.values, ...)
            record(element.name, alpha.name, "interaction",
                   paste(names(applies.to.dimension.values), unlist(applies.to.dimension.values),
                         sep = "=", collapse = "&")))
    apply.fn <- SHIELD.APPLY.PARAMETERS.FN
    environment(apply.fn) <- list2env(mocks, parent = environment(SHIELD.APPLY.PARAMETERS.FN))

    params <- get.medians(SHIELD.FULL.PARAMETERS.PRIOR)
    apply.fn(list(specification.metadata = sm), params)

    dups <- unique(log$keys[duplicated(log$keys)])
    expect_true(length(log$keys) > 0, info = "the mocked apply function recorded no alpha calls")
    expect_equal(dups, character(0), info = paste0(
        "these element | alpha | dimension | value slots are written more than once; ",
        "only the last write takes effect:\n  ", paste(head(dups, 15), collapse = "\n  ")))
})

test_that("the female screening multiplier reaches females and heterosexual men, not MSM", {
    ## Doubling screening.rate.multiplier.female.2010 must double the bounded
    ## odds of screening in 2010 for females and heterosexual men (whose
    ## multiplier is female x rel.female) and leave MSM unchanged.
    s <- skip_unless_sim()
    use_repo_root()
    param <- "screening.rate.multiplier.female.2010"
    skip_if_not(param %in% names(s$params), paste(param, "is not in the prior"))

    element <- "rate.sti.screening.over.14.without.covid"
    get.2010 <- function(params) {
        s$engine$crunch(parameters = params)
        v <- s$engine$extract.quantity.values()[[element]]
        if (is.null(v)) testthat::skip(paste0("'", element, "' is not in the engine's crunched quantity values"))
        yrs <- as.numeric(names(v))
        v[[which.min(abs(yrs - 2010))]]
    }
    p2 <- s$params; p2[param] <- 2 * p2[param]
    r1 <- get.2010(s$params)
    r2 <- get.2010(p2)
    withr::defer(s$engine$crunch(parameters = s$params))

    bounded.odds <- function(r) { p <- 1 - exp(-r); p / (0.9 - p) }   # element is a rate: r = -log(1 - p)
    or <- bounded.odds(r2) / bounded.odds(r1)
    sex.dim <- which(names(dimnames(or)) == "sex")
    by.sex <- apply(or, sex.dim, function(x) range(x, na.rm = TRUE))

    expect_equal(unname(by.sex[, "female"]), c(2, 2), tolerance = 1e-6,
                 info = "female screening odds should double")
    expect_equal(unname(by.sex[, "heterosexual_male"]), c(2, 2), tolerance = 1e-6,
                 info = "heterosexual-male screening = female.t x rel.female, so its odds should double too")
    expect_equal(unname(by.sex[, "msm"]), c(1, 1), tolerance = 1e-6,
                 info = "MSM screening must not respond to the female multiplier")
})
