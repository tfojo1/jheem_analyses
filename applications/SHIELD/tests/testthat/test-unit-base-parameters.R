## ==========================================================================
## UNIT TIER  |  shield_base_parameters.R
## --------------------------------------------------------------------------
## WHAT THIS FILE COVERS
##   The fixed natural-history parameters: that proportions are in [0, 1], rates
##   are non-negative, durations are in years and match their own comments, and
##   stated confidence intervals bracket their values.
##
## RUN JUST THIS FILE
##   Rscript applications/SHIELD/tests/run_tests.R --filter=base-parameters
##
## WHY IT MATTERS
##   The fixed (non-calibrated) natural-history parameters. Nothing recalculates
##   these, nothing plots them, and a wrong unit here is silent: the model just
##   runs a different disease.
## ==========================================================================

## Edition 3 gives `tolerance` its strict, relative-difference meaning.
## Without it test_dir() falls back to edition 2, where an 8% error passes a
## 5% tolerance.
local_edition(3)

skip_unless_stage("has.packages", "has.standalone")

test_that("every base parameter is a single finite number", {
    v <- SHIELD_BASE_PARAMETER_VALUES

    expect_true(is.numeric(v))
    expect_gt(length(v), 30)
    expect_false(any(is.na(v)), info = paste("NA parameters:",
                                             paste(names(v)[is.na(v)], collapse = ", ")))
    expect_true(all(is.finite(v)), info = paste("non-finite parameters:",
                                                paste(names(v)[!is.finite(v)], collapse = ", ")))
    expect_equal(anyDuplicated(names(v)), 0,
                 info = paste("a parameter is defined twice:",
                              paste(unique(names(v)[duplicated(names(v))]), collapse = ", ")))
})

test_that("every parameter named as a proportion lies in [0, 1]", {
    v <- SHIELD_BASE_PARAMETER_VALUES
    props <- v[grepl("^(prp|prop|prob)\\.", names(v))]
    expect_gt(length(props), 5)

    bad <- props[props < 0 | props > 1]
    expect_equal(length(bad), 0,
                 info = paste("proportions outside [0, 1]:",
                              paste(names(bad), signif(bad, 4), collapse = ", ")))
})

test_that("every parameter named as a rate or relative risk is non-negative", {
    v <- SHIELD_BASE_PARAMETER_VALUES
    rates <- v[grepl("^(rate|rr)\\.", names(v))]
    expect_gt(length(rates), 5)

    bad <- rates[rates < 0]
    expect_equal(length(bad), 0,
                 info = paste("negative rates/relative risks:",
                              paste(names(bad), collapse = ", ")))
})

test_that("stage durations are expressed in years", {
    ## The engine's rates are per year and each duration is used as
    ## 1 / duration, so a duration written in months or weeks silently
    ## multiplies the exit rate by 12 or 52.
    ##
    ## duration.tertiary and duration.cns are both commented "average of 1
    ## month" and stored as 1/12 - correct in years. This test pins that
    ## convention so the next edit cannot quietly write `1` and mean a month.
    v <- SHIELD_BASE_PARAMETER_VALUES
    durations <- v[grepl("^duration\\.", names(v))]
    expect_gt(length(durations), 3)

    ## Untreated syphilis stages last from a couple of weeks to a few decades.
    ## Anything under a week or over a century is a unit error, not a parameter.
    bad <- durations[durations <= 1 / 52 | durations > 100]
    expect_equal(length(bad), 0,
                 info = paste0("durations that are not plausible years: ",
                               paste(names(bad), signif(bad, 4), collapse = ", ")))
})

test_that("the documented duration comments agree with the stored values", {
    ## Parses the "# N weeks/months/years" comment beside each duration and
    ## checks it against the value. This is the check that catches a scale label
    ## and its number drifting apart.
    f <- file.path(SHIELD.DIR, "shield_base_parameters.R")
    src <- paste(readLines(f, warn = FALSE), collapse = "\n")

    ## add.parameter(..., 'duration.x', <value>, ...) # <comment>
    blocks <- regmatches(src, gregexpr(
        "add\\.parameter\\([^,]+,\\s*'(duration\\.[A-Za-z._]+)',\\s*\\n?\\s*([^,]+),[^\\n]*#([^\\n]*)",
        src, perl = TRUE))[[1]]
    skip_if(length(blocks) == 0, "could not parse the duration definitions")

    unit.years <- c(week = 1 / 52, weeks = 1 / 52,
                    month = 1 / 12, months = 1 / 12,
                    year = 1, years = 1)

    for (b in blocks) {
        name  <- sub(".*'(duration\\.[A-Za-z._]+)'.*", "\\1", b)
        value <- SHIELD_BASE_PARAMETER_VALUES[[name]]
        comment <- sub(".*#", "", b)

        ## first "<number> <unit>" or "<a>-<b> <unit>" in the comment
        m <- regmatches(comment, regexpr(
            "([0-9]+(\\.[0-9]+)?)\\s*(-|to)?\\s*([0-9]+(\\.[0-9]+)?)?\\s*(week|month|year)s?",
            comment, perl = TRUE))
        if (!length(m)) next

        unit <- sub(".*?(week|month|year)s?.*", "\\1", m)
        nums <- as.numeric(regmatches(m, gregexpr("[0-9]+(\\.[0-9]+)?", m))[[1]])
        stated.years <- range(nums) * unname(unit.years[unit])

        expect_true(
            value >= stated.years[1] / 1.5 && value <= stated.years[2] * 1.5,
            info = paste0(name, " is stored as ", signif(value, 4),
                          " years but its comment says \"", trimws(m), "\" (",
                          paste(signif(stated.years, 4), collapse = " to "),
                          " years)")
        )
    }
})

test_that("relative risks of congenital syphilis are ordered by prenatal care timing", {
    ## Earlier prenatal care must carry a lower risk, and no care is the
    ## reference at 1. A transposed pair here would reverse the headline
    ## prenatal-screening result.
    v <- SHIELD_BASE_PARAMETER_VALUES
    first  <- v[["rr.congenital.syphilis.prenatal.care.first.trimester"]]
    second <- v[["rr.congenital.syphilis.prenatal.care.second.trimester"]]
    third  <- v[["rr.congenital.syphilis.prenatal.care.third.trimester"]]
    none   <- v[["rr.congenital.syphilis.no.prenatal.care"]]

    expect_equal(none, 1)
    expect_lt(first, second)
    expect_lt(second, third)
    expect_lt(third, none)
})

test_that("vertical transmission is more likely from early than late syphilis", {
    v <- SHIELD_BASE_PARAMETER_VALUES
    expect_gt(v[["prob.vertical.transmission.mothers.early.syphilis"]],
              v[["prob.vertical.transmission.mothers.late.syphilis"]])
})

test_that("the stage distribution of traced contacts leaves a valid remainder", {
    ## shield_specification.R:1251 derives the late-latent share as
    ##   1 - primary - secondary - early.latent
    ## so these three must sum to strictly less than 1. If an edit pushes the
    ## sum past 1, late latent becomes a negative proportion and the contact
    ## tracing arm starts removing people from the model instead of treating
    ## them - with no error anywhere.
    v <- SHIELD_BASE_PARAMETER_VALUES
    shares <- c(v[["prp.infected.contacts.in.primary"]],
                v[["prp.infected.contacts.in.secondary"]],
                v[["prp.infected.contacts.in.early.latent"]])

    expect_true(all(shares > 0 & shares < 1))
    expect_lt(sum(shares), 1)

    derived.late.latent <- 1 - sum(shares)
    expect_gt(derived.late.latent, 0)
})

test_that("the base parameter table is internally consistent", {
    ## add.parameter() writes to four parallel structures; a typo in one name
    ## leaves them ragged.
    p <- SHIELD_BASE_PARAMETER
    expect_setequal(names(p$values), names(p$ci.lower))
    expect_setequal(names(p$values), names(p$ci.upper))
    expect_setequal(names(p$values), names(p$citation))
})

test_that("stated confidence intervals actually bracket their value", {
    ## Most entries carry the placeholder 0, 0 - those are not claims and are
    ## skipped. Any entry that does state an interval must be well formed and
    ## must contain the point estimate.
    p <- SHIELD_BASE_PARAMETER
    stated <- names(p$values)[p$ci.lower != 0 | p$ci.upper != 0]
    skip_if(length(stated) == 0, "no base parameter states a CI")

    for (nm in stated) {
        lo <- p$ci.lower[[nm]]; hi <- p$ci.upper[[nm]]; v <- p$values[[nm]]
        expect_lte(lo, hi, label = paste0(nm, " CI bounds are reversed"))
        expect_true(v >= lo && v <= hi,
                    info = paste0(nm, ": value ", signif(v, 4),
                                  " lies outside its stated CI [",
                                  signif(lo, 4), ", ", signif(hi, 4), "]"))
    }
})
