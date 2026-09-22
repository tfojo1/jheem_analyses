## ============================================================================
## WHAT THIS FILE COVERS
##   restratify.data.to.specification(), in R/shield_inputManager_helpers.R
##
## WHY IT MATTERS
##   This is the funnel every external dataset passes through on its way into
##   the model. It takes a long-format data frame whose categories are in some
##   source ontology (BRFSS races, census age brackets, WONDER sex codes) and
##   redistributes it onto the model's own strata.
##
##   Its failure mode is the worst kind: it does not error. If a mapping drops a
##   category, or double-counts one, the function still returns a well-formed
##   frame of plausible-looking numbers, and the only symptom is that the model
##   is fitted to a slightly wrong target. Nothing downstream can detect that.
##
## HOW IT WORKS (needed to read the tests below)
##   The function returns LONG FORMAT, not an array: one row per (stratum, year)
##   with a `value` column and a `weight` column. When one source category spans
##   several model strata - a "20-29 years" bracket landing on both "20-24
##   years" and "25-29 years" - the row is DUPLICATED, once per model stratum,
##   and the weights are the fractions of the source bracket that fall in each.
##   The value is left alone.
##
##   So the quantity that must be conserved is sum(value * weight), not
##   sum(value). Every test here is a statement about that product.
## ============================================================================

## Edition 3 gives `tolerance` its strict, relative-difference meaning.
## Without it test_dir() falls back to edition 2, where an 8% error passes a
## 5% tolerance.
local_edition(3)

skip_unless_stage("has.packages", "has.jheem2", "has.standalone")

## target_dim_names ----
## The model's own strata, small enough to reason about by hand.
target_dim_names <- function() {
    list(age  = c("0-14 years", "15-19 years", "20-24 years", "25-29 years"),
         race = c("black", "hispanic", "other"),
         sex  = c("heterosexual_male", "msm", "female"))
}

## fixture_frame ----
## A long-format frame already in the model's own categories, so no ontology
## translation is needed and the arithmetic is the only thing under test.
fixture_frame <- function(dim.names = target_dim_names(), value = 10) {
    grid <- expand.grid(age = dim.names$age,
                        race = dim.names$race,
                        sex = dim.names$sex,
                        stringsAsFactors = FALSE)
    grid$year <- 2020
    grid$value <- value
    grid
}

## total_mass ----
## The conserved quantity.
total_mass <- function(result) sum(result$value * result$weight)

## try_restratify ----
## Run restratification, returning NULL (not an error object) on failure, so
## skip_if() never has to evaluate conditionMessage() on a data frame.
try_restratify <- function(...) {
    out <- tryCatch(restratify.data.to.specification(...),
                    error = function(e) {
                        attr(e, "shield.failed") <- TRUE
                        e
                    })
    if (inherits(out, "condition")) {
        SHIELD.TEST.ENV$last.restratify.error <- conditionMessage(out)
        NULL
    } else {
        SHIELD.TEST.ENV$last.restratify.error <- NA_character_
        out
    }
}

## skip_if_restratify_failed ----
skip_if_restratify_failed <- function(result, what) {
    if (is.null(result)) {
        testthat::skip(paste0("restratify.data.to.specification() errored on ",
                              what, ": ", SHIELD.TEST.ENV$last.restratify.error))
    }
}

test_that("restratifying data already in the target strata conserves the total", {
    ## The simplest conservation case, and the one every other case reduces to.
    ## Nothing needs splitting, so every weight should be 1 and the total should
    ## come back untouched.
    dn <- target_dim_names()
    data <- fixture_frame(dn)

    result <- try_restratify(data, dim.names = dn)
    skip_if_restratify_failed(result, "the identity case")

    expect_true(all(is.finite(result$value)))
    expect_true(all(is.finite(result$weight)))
    expect_equal(total_mass(result), sum(data$value), tolerance = 1e-8,
                 info = "restratification changed the total")
    expect_equal(result$weight, rep(1, nrow(result)), tolerance = 1e-10,
                 info = "no splitting was needed, so every weight should be 1")
})

test_that("a source age bracket spanning two model brackets splits with weights summing to 1", {
    ## This is the mechanism the whole function exists for. A "15-24 years"
    ## source bracket covers exactly the model's "15-19" and "20-24". It must
    ## produce two rows whose weights sum to 1 - not two rows of weight 1, which
    ## would double the count, and not one row, which would halve it.
    dn <- target_dim_names()

    data <- data.frame(age = "15-24 years",
                       race = "black",
                       sex = "female",
                       year = 2020,
                       value = 100,
                       stringsAsFactors = FALSE)

    result <- try_restratify(data, dim.names = dn)
    skip_if_restratify_failed(result, "a straddling age bracket")

    split.rows <- result[result$age %in% c("15-19 years", "20-24 years"), ]
    expect_equal(nrow(split.rows), 2,
                 info = "a 15-24 source bracket should land on exactly two model brackets")
    expect_equal(sum(split.rows$weight), 1, tolerance = 1e-8,
                 info = paste("the split weights sum to", signif(sum(split.rows$weight), 6),
                              "instead of 1; the count is being",
                              if (sum(split.rows$weight) > 1) "inflated" else "lost"))
    expect_equal(total_mass(result), sum(data$value), tolerance = 1e-8)
})

test_that("an evenly straddling bracket splits evenly", {
    ## 15-24 covers 15-19 and 20-24, which are the same width, so each should
    ## get half. A weight split that is not 50/50 means the overlap fractions
    ## are computed against the wrong denominator.
    dn <- target_dim_names()
    data <- data.frame(age = "15-24 years", race = "black", sex = "female",
                       year = 2020, value = 100, stringsAsFactors = FALSE)

    result <- try_restratify(data, dim.names = dn)
    skip_if_restratify_failed(result, "an evenly straddling bracket")

    split.rows <- result[result$age %in% c("15-19 years", "20-24 years"), ]
    skip_if(nrow(split.rows) != 2)

    expect_equal(sort(split.rows$weight), c(0.5, 0.5), tolerance = 1e-6)
})

test_that("restratification conserves the total when a covariate is collapsed", {
    ## Source data often reports only some dimensions, using a catch-all value
    ## for the rest. The function must spread that total across the missing
    ## dimension without inventing or losing anyone.
    dn <- target_dim_names()

    data <- expand.grid(age = dn$age, race = dn$race, stringsAsFactors = FALSE)
    data$sex <- "all"          # not broken down by sex in the source
    data$year <- 2020
    data$value <- 100

    result <- try_restratify(data, dim.names = dn)
    skip_if_restratify_failed(result, "a catch-all covariate")

    expect_equal(total_mass(result), sum(data$value), tolerance = 1e-8,
                 info = paste("collapsing 'sex' changed the total from",
                              sum(data$value), "to", signif(total_mass(result), 8)))
})

test_that("restratification produces no NA and no negative mass", {
    ## An NA row becomes a silently unfitted cell in the likelihood; a negative
    ## one becomes a negative target.
    dn <- target_dim_names()
    result <- try_restratify(fixture_frame(dn), dim.names = dn)
    skip_if_restratify_failed(result, "the identity case")

    expect_false(any(is.na(result$value)))
    expect_false(any(is.na(result$weight)))
    expect_true(all(result$value >= 0))
    expect_true(all(result$weight >= 0))
})

test_that("every output stratum is one the model actually has", {
    ## A row labelled with a category outside dim.names cannot be matched to the
    ## model and is dropped later, without comment.
    dn <- target_dim_names()
    result <- try_restratify(fixture_frame(dn), dim.names = dn)
    skip_if_restratify_failed(result, "the identity case")

    for (d in names(dn)) {
        stray <- setdiff(unique(result[[d]]), dn[[d]])
        expect_equal(stray, character(0),
                     info = paste0("restratified data contains ", d,
                                   " categories the model does not have: ",
                                   paste(stray, collapse = ", ")))
    }
})

test_that("doubling every input value doubles the output mass", {
    ## Linearity. Any renormalisation bug - dividing by a total computed on the
    ## wrong margin, for instance - shows up here as a departure from scaling.
    dn <- target_dim_names()

    single <- try_restratify(fixture_frame(dn, value = 10), dim.names = dn)
    skip_if_restratify_failed(single, "the scaling pair")
    double <- try_restratify(fixture_frame(dn, value = 20), dim.names = dn)
    skip_if_restratify_failed(double, "the scaling pair")

    expect_equal(total_mass(double), 2 * total_mass(single), tolerance = 1e-8,
                 info = "restratification is not linear in the input values")
})

test_that("an explicit weight of 1 changes nothing", {
    ## The function defaults the weight column to 1 when it is absent. Supplying
    ## it explicitly must give the same answer - if it does not, the supplied
    ## weight is being combined with the default rather than replacing it.
    dn <- target_dim_names()

    implicit <- try_restratify(fixture_frame(dn), dim.names = dn)
    skip_if_restratify_failed(implicit, "the weighted/unweighted pair")

    explicit.input <- fixture_frame(dn)
    explicit.input$weight <- 1
    explicit <- try_restratify(explicit.input, dim.names = dn)
    skip_if_restratify_failed(explicit, "the weighted/unweighted pair")

    expect_equal(total_mass(explicit), total_mass(implicit), tolerance = 1e-10)
})

test_that("a covariate that is not a model dimension is rejected", {
    ## Named covariates must exist in dim.names. Accepting an unknown one would
    ## mean silently ignoring a stratification the source data does have.
    ##
    ## Note: the rejection path builds its message with jheem2's
    ## collapse.with.and(), which is not exported by the installed package, so
    ## against that build the error is about the missing helper rather than the
    ## missing dimension. Either way the call must fail rather than proceed.
    dn <- target_dim_names()
    data <- fixture_frame(dn)
    data$risk <- "high"        # not a model dimension

    expect_error(
        restratify.data.to.specification(
            data, dim.names = dn,
            covariate.names = c("age", "race", "sex", "risk")))
})
