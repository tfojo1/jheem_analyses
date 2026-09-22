## ==========================================================================
## INTEGRATION TIER  |  R/shield_specification_helpers.R
## --------------------------------------------------------------------------
## WHAT THIS FILE COVERS
##   The helpers that turn census and surveillance data into model arrays:
##   population by sex, sexual mixing, mortality, fertility, aging, migration,
##   race mixing and the 1970 seeding proportions.
##
## RUN JUST THIS FILE
##   Rscript applications/SHIELD/tests/run_tests.R --filter=spec-helpers
##
## WHY IT MATTERS
##   The specification helpers turn census and surveillance data into the arrays
##   the specification registers. They are the layer where a wrong subtraction or
##   a mis-ordered ontology produces numbers that are wrong but plausible, so they
##   never surface as an error.
## ==========================================================================

## Edition 3 gives `tolerance` its strict, relative-difference meaning.
## Without it test_dir() falls back to edition 2, where an 8% error passes a
## 5% tolerance.
local_edition(3)

skip_unless_stage("has.shield.helpers")

LOC <- SHIELD.TEST.LOCATION

## spec_meta ----
spec_meta <- function() {
    skip_if(is.null(shield.test.specification()))
    get.specification.metadata(SHIELD.TEST.VERSION, LOC)
}

## --- population by sex --------------------------------------------------------

test_that("heterosexual male counts are male counts minus MSM counts", {
    ## This helper used to return the MSM counts themselves, so the
    ## heterosexual-male age distribution was the MSM age distribution and the
    ## two groups mixed identically. The fix is a subtraction of two arrays, and
    ## a subtraction is only correct if the arrays are conformable and aligned -
    ## which is exactly what has never been exercised end to end.
    sm <- spec_meta()

    male <- get.male.single.year.age.counts(LOC)
    msm  <- get.msm.single.year.age.counts(LOC, sm)
    het  <- get.heterosexual_male.single.year.age.counts(LOC, sm)

    expect_equal(dim(male), dim(msm),
                 info = "male and MSM count arrays are not conformable")
    expect_equal(dimnames(male)$age, dimnames(msm)$age,
                 info = "male and MSM arrays are indexed by different ages")
    expect_equal(as.vector(het), as.vector(male) - as.vector(msm),
                 tolerance = 1e-10)
})

test_that("heterosexual male counts are non-negative and smaller than all males", {
    ## A negative count here would mean the MSM proportion exceeds 1 for some
    ## age, which the solver would happily integrate.
    sm <- spec_meta()
    male <- get.male.single.year.age.counts(LOC)
    het  <- get.heterosexual_male.single.year.age.counts(LOC, sm)

    expect_true(all(het >= 0),
                info = paste("negative heterosexual-male counts at ages:",
                             paste(names(het)[het < 0], collapse = ", ")))
    expect_true(all(het <= male))
    expect_gt(sum(het), 0)
})

test_that("MSM are a plausible minority of men at every age", {
    sm <- spec_meta()
    male <- get.male.single.year.age.counts(LOC)
    msm  <- get.msm.single.year.age.counts(LOC, sm)

    adult <- as.numeric(names(male)) >= 18
    share <- as.vector(msm)[adult] / as.vector(male)[adult]

    expect_true(all(is.finite(share)))
    expect_true(all(share >= 0 & share <= 0.5),
                info = paste0("implausible MSM share of adult men: range [",
                              paste(signif(range(share), 3), collapse = ", "), "]"))
})

test_that("female and male population counts are positive and finite", {
    sm <- spec_meta()
    female <- get.n.initial.female.population(LOC, sm)
    male   <- get.n.initial.male.population(LOC, sm)

    for (nm in c("female", "male")) {
        x <- get(nm)
        expect_true(all(is.finite(x)), info = paste(nm, "has non-finite counts"))
        expect_true(all(x >= 0), info = paste(nm, "has negative counts"))
        expect_gt(sum(x), 0)
    }
})

## --- sexual mixing ------------------------------------------------------------

test_that("age contact proportions sum to 1 for every receiving bracket", {
    ## Same contract as the unit-level mixing test, but through the real
    ## specification metadata and real census age counts.
    sm <- spec_meta()

    availability <- get.sexual.availability()

    matrices <- list(
        female = get.female.sexual.age.contact.proportions(
            LOC, age.mixing.sd.mult = 1,
            single.year.female.age.counts = get.female.single.year.age.counts(LOC),
            single.year.age.sexual.availability = availability,
            specification.metadata = sm),
        msm = get.msm.sexual.age.contact.proportions(
            LOC, age.mixing.sd.mult = 1,
            single.year.msm.age.counts = get.msm.single.year.age.counts(LOC, sm),
            single.year.age.sexual.availability = availability,
            specification.metadata = sm),
        heterosexual_male = get.heterosexual_male.sexual.age.contact.proportions(
            LOC, age.mixing.sd.mult = 1,
            single.year.heterosexual_male.age.counts =
                get.heterosexual_male.single.year.age.counts(LOC, sm),
            single.year.age.sexual.availability = availability,
            specification.metadata = sm)
    )

    for (nm in names(matrices)) {
        m <- matrices[[nm]]
        expect_true(all(is.finite(m)), info = paste(nm, "has non-finite mixing"))
        expect_true(all(m >= 0), info = paste(nm, "has negative mixing"))
        expect_equal(unname(colSums(m)), rep(1, ncol(m)), tolerance = 1e-8,
                     info = paste(nm, "age.to columns must sum to 1"))
        expect_equal(unname(dim(m)), rep(length(sm$dim.names$age), 2),
                     info = paste(nm, "mixing matrix is not age x age"))
    }
})

test_that("sexual availability is a proportion, zero in childhood", {
    avail <- get.sexual.availability()

    expect_true(all(avail >= 0 & avail <= 1),
                info = "sexual availability must be a proportion")
    expect_true(all(avail[as.character(0:12)] == 0),
                info = "the model assumes no sexual activity under 13")
    expect_gt(avail[["25"]], 0.9)
})

## --- demographic rates --------------------------------------------------------

test_that("mortality rates are finite, non-negative and below 1 per year", {
    sm <- spec_meta()
    rates <- get.general.mortality.rates(LOC, sm)

    expect_true(all(is.finite(rates)), info = "non-finite mortality rates")
    expect_true(all(rates >= 0), info = "negative mortality rates")
    expect_true(all(rates < 1),
                info = paste("mortality rate at or above 1 per year - a unit error;",
                             "max was", signif(max(rates), 4)))
})

test_that("mortality rises with age", {
    ## A monotone gradient in age is the one thing every life table agrees on,
    ## and a reversed ontology mapping would break it.
    sm <- spec_meta()
    rates <- get.general.mortality.rates(LOC, sm)
    skip_if(is.null(dimnames(rates)$age), "mortality rates are not age-stratified")

    by.age <- apply(rates, "age", mean)
    expect_gt(tail(by.age, 1), head(by.age, 1))
})

test_that("fertility rates are finite, non-negative and confined to fertile ages", {
    sm <- spec_meta()
    rates <- get.fertility.rates.from.census(LOC, sm)

    expect_true(all(is.finite(rates)), info = "non-finite fertility rates")
    expect_true(all(rates >= 0))

    skip_if(is.null(dimnames(rates)$age))
    non.fertile <- intersect(NON.FERTILE.AGES, dimnames(rates)$age)
    skip_if(length(non.fertile) == 0)

    outside <- rates[, non.fertile, drop = FALSE]
    expect_true(all(outside == 0),
                info = "non-zero fertility outside the fertile age range")
})

test_that("aging rates are positive and no faster than one bracket per year", {
    ## The aging rate out of a bracket is roughly 1 / bracket width. A rate
    ## above 1 means people leave a bracket faster than time passes.
    ## get.empiric.aging.rates() wraps the numbers in a spline functional form,
    ## so the raw rates come from the do.* worker.
    sm <- spec_meta()
    rates <- unlist(do.get.empiric.aging.rates(LOC, sm))

    expect_true(all(is.finite(rates)), info = "non-finite aging rates")
    expect_true(all(rates >= 0))
    expect_true(all(rates <= 1),
                info = paste("aging rate above 1 per year; max was",
                             signif(max(rates), 4)))
})

test_that("migration rates are finite and non-negative", {
    sm <- spec_meta()
    for (fn in list(get.immigration.rates, get.emigration.rates)) {
        rates <- fn(LOC, sm)
        expect_true(all(is.finite(rates)))
        expect_true(all(rates >= 0))
    }
})

## --- race mixing --------------------------------------------------------------

test_that("race population counts are positive for every modelled race", {
    sm <- spec_meta()
    counts <- get.race.population.counts(LOC, sm)

    expect_setequal(dimnames(counts)$race, sm$dim.names$race)
    expect_true(all(counts > 0),
                info = "a modelled race has zero population in this location")
})

test_that("oes.to.proportions normalises race mixing to sum to 1", {
    ## Note the convention here is the opposite of get.pairing.proportions():
    ## this function divides by rowSums, so it is the ROWS that sum to 1, while
    ## the pairing and age-mixing matrices normalise columns. Both conventions
    ## are in use in the same model; this test pins the one this function
    ## actually implements so a change of direction cannot pass unnoticed.
    races <- c("black", "hispanic", "other")
    oe <- matrix(c(3.76, 0.5, 0.4,
                   0.5, 2.19, 0.6,
                   0.4, 0.6, 1.55), nrow = 3, byrow = TRUE,
                 dimnames = list(race.from = races, race.to = races))
    pop <- c(black = 100000, hispanic = 150000, other = 600000)

    props <- oes.to.proportions(oe, pop)

    expect_true(all(is.finite(props)))
    expect_true(all(props >= 0))
    expect_equal(unname(rowSums(props)), rep(1, 3), tolerance = 1e-12)

    ## with uniform O/E the mixing reduces to population shares
    flat <- matrix(1, 3, 3, dimnames = list(race.from = races, race.to = races))
    expect_equal(unname(oes.to.proportions(flat, pop)[1, ]),
                 unname(pop / sum(pop)), tolerance = 1e-12)
})

## --- 1970 seeding -------------------------------------------------------------

test_that("the 1970 stage seeding proportions are valid proportions", {
    ## These set the initial infected distribution in 1970 and are never
    ## calibrated against anything, so nothing else would notice if one were
    ## out of range.
    for (fn in list(get_popProp_primary_diag_1970,
                    get_popProp_secondary_diag_1970,
                    get_popProp_el_diag_1970,
                    get_popProp_lu_diag_1970)) {
        v <- fn(LOC)
        expect_true(all(is.finite(v)))
        expect_true(all(v >= 0 & v <= 1),
                    info = "1970 seeding proportions must lie in [0, 1]")
    }
})
