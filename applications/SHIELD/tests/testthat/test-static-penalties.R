## ==========================================================================
## STATIC TIER  |  shield_likelihoods.R (the two custom penalties)
## --------------------------------------------------------------------------
## WHAT THIS FILE COVERS
##   The growth penalty's denominator, the historical penalty's ineffective
##   tryCatch guard, and whether the penalty's sharpness is documented. These
##   live in the source text, so they need no data manager to check.
##
## RUN JUST THIS FILE
##   Rscript applications/SHIELD/tests/run_tests.R --filter=static-penalties
##
## WHY IT MATTERS
##   Two custom penalty likelihoods carry known statistical problems that live in
##   the source text rather than in any value the model produces, so they can be
##   checked without a data manager, a specification or a jheem2 that matches.
##
##   Each test below also demonstrates the size of the effect numerically, so the
##   failure message argues its own case rather than just pointing at a line.
## ==========================================================================

## Edition 3 gives `tolerance` its strict, relative-difference meaning.
## Without it test_dir() falls back to edition 2, where an 8% error passes a
## 5% tolerance.
local_edition(3)

likelihood_source <- function() {
    f <- file.path(SHIELD.DIR, "shield_likelihoods.R")
    skip_if_not(file.exists(f))
    lines <- readLines(f, warn = FALSE)
    lines[!grepl("^\\s*#", lines)]
}

test_that("the growth penalty does not add 1 to the denominator", {
    ## penalty.ps.diag.growth.likelihood forms a 10-year ratio as
    ##     vals[end] / (vals[start] + 1)
    ## The `+ 1` was added to avoid dividing by zero in an empty stratum, but it
    ## is not a guard - it is a bias, and it is concentrated exactly where the
    ## data are thinnest.
    ##
    ## For the total (tens of thousands of diagnoses) the distortion is nil. For
    ## a small race/sex stratum it is not:
    ##
    ##   2020 count   true ratio   ratio with + 1   understated by
    ##            2          5.0             3.33            33 %
    ##            5          5.0             4.17            17 %
    ##           20          5.0             4.76             5 %
    ##         2000          5.0             5.00             0 %
    ##
    ## Because the penalty scores log(ratio) against a lognormal, a systematic
    ## downward shift in the small strata pulls those strata toward the "no
    ## growth" end of the prior - the opposite of the intent, and strongest for
    ## the groups the analysis most wants to resolve.
    ##
    ## A floor - pmax(denominator, eps) - removes the division hazard without
    ## touching any stratum that has data.
    src <- likelihood_source()

    ## demonstrate the bias so the failure is self-explanatory
    ratio.true <- function(end, start) end / start
    ratio.plus1 <- function(end, start) end / (start + 1)
    small <- c(2, 5, 20, 2000)
    understatement <- 1 - ratio.plus1(small * 5, small) / ratio.true(small * 5, small)
    expect_gt(understatement[1], 0.3)   # the bias is real at n = 2
    expect_lt(understatement[4], 0.001) # and absent at n = 2000

    offenders <- grep("\\[\\s*as\\.character\\(start_year\\)\\s*,?\\s*\\]?\\s*\\+\\s*1\\s*\\)",
                      src, value = TRUE)

    expect_equal(
        length(offenders), 0,
        info = paste0(
            "the growth penalty still adds 1 to the 10-year denominator:\n  ",
            paste(trimws(offenders), collapse = "\n  "),
            "\nAt a 2020 count of 2 this understates the ratio by ",
            round(100 * understatement[1]), "%, at 5 by ",
            round(100 * understatement[2]), "%, and at 2000 by ~0%. ",
            "Use pmax(denominator, eps) or skip empty strata instead.")
    )
})

test_that("the historical-penalty guard actually guards", {
    ## NATIONAL_TOTAL_DIAGNOSIS_DATA is wrapped in tryCatch whose error handler
    ## returns a character string, and the NEXT line divides by it. So when the
    ## pull fails, sourcing still dies - just with
    ## "non-numeric argument to binary operator" instead of the message the
    ## handler was written to produce. The guard adds nothing.
    src <- likelihood_source()

    handler.line <- grep('error\\s*=\\s*function\\(e\\)\\s*\\{?\\s*"', src)
    skip_if(length(handler.line) == 0, "no string-returning tryCatch handler found")

    ## Does anything between the handler and the first use check the type?
    first.use <- grep("NATIONAL_TOTAL_DIAGNOSIS_DATA\\s*/", src)
    skip_if(length(first.use) == 0)

    between <- src[seq(min(handler.line), min(first.use))]
    guarded <- any(grepl("is\\.numeric|is\\.character|stopifnot|if\\s*\\(", between))

    expect_true(
        guarded,
        info = paste0(
            "shield_likelihoods.R:", min(handler.line), " returns a character ",
            "string when the national series is missing, and line ",
            min(first.use), " divides by it without checking. The tryCatch ",
            "changes the error message and nothing else - either stop() with a ",
            "clear message or fall back to a usable default.")
    )
})

test_that("the historical penalty's sharpness is documented as a deliberate choice", {
    ## The penalty's sdlog moved from a hand-picked 0.347 to a data-derived
    ## sd(log(ratio)) = 0.215. That is defensible, but it is the spread of the
    ## national trajectory rather than a measurement-error scale, and any stage-3
    ## run launched before that change used the old value - so old and new runs
    ## are not comparable. The choice needs to be written down next to the code
    ## that makes it.
    f <- file.path(SHIELD.DIR, "shield_likelihoods.R")
    skip_if_not(file.exists(f))
    lines <- readLines(f, warn = FALSE)

    sdlog.lines <- grep("sdlog\\s*=\\s*sd\\(log\\(|data\\$sdlog", lines)
    skip_if(length(sdlog.lines) == 0, "the penalty no longer derives sdlog")

    ## a comment within five lines above the first derivation
    window <- lines[max(1, min(sdlog.lines) - 5):min(sdlog.lines)]
    documented <- any(grepl("^\\s*#", window) &
                          grepl("sd|spread|sharp|scale|error", window, ignore.case = TRUE))

    expect_true(
        documented,
        info = paste0(
            "shield_likelihoods.R:", min(sdlog.lines), " sets the historical ",
            "penalty's sharpness from the national trajectory's own spread, ",
            "with no comment saying so. Record that this is a trajectory spread ",
            "rather than a measurement-error scale, and that runs before the ",
            "change used 0.347.")
    )
})

test_that("custom penalty compute functions guard against a zero denominator", {
    ## Every custom likelihood in the file divides by something drawn from the
    ## simulation. A stratum that is empty in the denominator year yields Inf or
    ## NaN, which propagates to the whole chain's log-likelihood.
    src <- likelihood_source()

    divisions <- grep("vals\\[|_vals\\[", src)
    skip_if(length(divisions) == 0)

    has.guard <- any(grepl("pmax\\(|ifelse\\(|is\\.finite\\(|na\\.rm\\s*=\\s*T", src))
    expect_true(has.guard,
                info = "no finiteness or floor guard anywhere in the custom penalties")
})
