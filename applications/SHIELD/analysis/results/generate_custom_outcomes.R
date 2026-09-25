
# number of cases averted (incidence and diagnosis)
# cumulative cases (relative to a start year that user specifies) (incidence and diagnosis)
# percent reduction in cumulative cases (incidence and diagnosis)
# percent reduction in cases (incidence and diagnosis)

source('../jheem_analyses/applications/SHIELD/analysis/shield_output_paths.R')

CALIB.NAME <- "calib.8.21.stage3.az"
NOINT = "noint"

## subset_array ----
# Need to find a single home for this
# Also change "dim_indices" to "dimension.values", because that's what it is
subset_array <- function(arr, dim_indices, drop = FALSE) {
    # dim_indices: named list where names are dimension *names*
    # (matching names(dimnames(arr))) and values are the indices
    # you want to keep along that dimension.
    
    dn <- dimnames(arr)
    if (is.null(dn) || is.null(names(dn))) {
        stop("Array must have named dimnames to subset by dimension name.")
    }
    
    nd <- length(dim(arr))
    args <- rep(list(TRUE), nd)              # default: keep everything
    
    target_pos <- match(names(dim_indices), names(dn))
    if (any(is.na(target_pos))) {
        missing_names <- names(dim_indices)[is.na(target_pos)]
        stop("Dimension name(s) not found: ", paste(missing_names, collapse = ", "))
    }
    
    args[target_pos] <- dim_indices
    
    do.call(`[`, c(list(arr), args, list(drop = drop)))
}

# ****************************************************************************
# WHY THESE ARE WRITTEN WITH aperm() AND NOT apply()
#
# Every calculation below is a whole-array operation in disguise. `x[noint] - x`
# across the intervention dimension is one vectorised subtraction once
# `intervention` is the LAST dimension: R then recycles the length-n vector of
# noint values across the intervention blocks.
#
# The old apply() versions built one closure call per cell -- profiling showed
# 61% of the runtime inside apply's own machinery and only 21% doing the
# arithmetic. These versions do the same arithmetic in whole-array form.
#
# aperm() is also SAFER than the old array(apply(...), dn) pattern: it derives
# the permutation from the dimension NAMES, so it cannot silently transpose the
# result the way a mismatched hand-passed `dn.one.outcome` could.
# ****************************************************************************

## .dims_last ----
#' Move the named dimensions to the end, leaving the others in their order
#' @noRd
.dims_last <- function(arr, last.dims) {
    others <- setdiff(names(dimnames(arr)), last.dims)
    aperm(arr, c(others, last.dims))
}

## .outcome_order ----
#' The dimension order every custom outcome is returned in
#' @noRd
.outcome_order <- function(arr) {
    strat <- setdiff(names(dimnames(arr)),
                     c("year", "sim", "intervention", "location", "outcome"))
    c("year", strat, "sim", "intervention", "location")
}

## .as_array ----
#' Rebuild an array from a bare vector, given the target dimnames
#' @noRd
.as_array <- function(v, dn) array(v, sapply(dn, length), dn)


#' Cases averted: the no-intervention arm minus this arm, in the SAME year
#'
#' averted(y, arm) = x(y, "noint") - x(y, arm)
#'
#' Sign convention: POSITIVE means the intervention prevented cases. The noint
#' slice is therefore exactly 0 by construction, not NA.
#'
#' The comparator is the counterfactual in the SAME year, not the baseline
#' year. That is a different question from calculate_pct_reduction_versus_year()
#' below, and the two diverge sharply when the untreated epidemic is growing:
#' Doxy-PEP can avert most of the projected infections while incidence still
#' sits above its 2022 level.
#'
#' WORKED EXAMPLE (Atlanta, 2030, incidence)
#'   incidence["2030", sim, "noint",       "C.12060"] = 9,117
#'   incidence["2030", sim, "doxy.cov.30", "C.12060"] = 6,117
#'   num_incidence_averted                            = 9,117 - 6,117 = 3,000
#'   i.e. 3,000 infections that would have occurred in 2030 did not.
#'
#' Computed WITHIN each sim, so the median of this outcome is a median of
#' differences -- the right summary, since the two arms are perfectly
#' correlated across sims (same posterior parameter draw).

## calculate_averted_count ----
calculate_averted_count <- function(arr, dn.one.outcome = NULL, noint = NOINT) {
    # dn.one.outcome is no longer needed -- the order comes from the dimnames.
    # It is kept so the existing call sites do not have to change.
    p  <- .dims_last(arr, "intervention")
    nv <- as.vector(subset_array(p, list(intervention = noint)))
    aperm(array(nv - as.vector(p), dim(p), dimnames(p)), .outcome_order(arr))
}

#' Percent of the counterfactual burden averted, in the SAME year
#'
#' pct_averted(y, arm) = 100 * (x(y,"noint") - x(y,arm)) / x(y,"noint")
#'
#' Positive means prevented. Bounded above by 100; negative only if the
#' intervention made things worse.
#'
#' WORKED EXAMPLE (Atlanta, 2030, incidence)
#'   100 * (9,117 - 6,117) / 9,117 = 32.9%
#'   i.e. one third of the infections projected for 2030 without Doxy-PEP.
#'
#' CAVEAT: no zero-denominator guard. If a sim has zero incidence under noint
#' this returns NaN (0/0) or Inf. Safe for incidence and diagnoses in these
#' MSAs, where noint burden is never zero, but it is NOT safe if this is ever
#' applied to a rare stratum. Contrast calculate_ratio_of_outcomes() and
#' calculate_ratio_versus_year(), which do guard their denominators.

## calculate_averted_pct ----
calculate_averted_pct <- function(arr, dn.one.outcome = NULL, noint = NOINT) {
    p  <- .dims_last(arr, "intervention")
    nv <- as.vector(subset_array(p, list(intervention = noint)))
    aperm(array(100 * (nv - as.vector(p)) / nv, dim(p), dimnames(p)),
          .outcome_order(arr))
}

#' One outcome per 100,000 of another, in the same year and the same arm
#'
#' rate(y, arm) = 100000 * num(y, arm) / den(y, arm)
#'
#' This is a LEVEL, not a comparison: it describes one scenario on its own and
#' involves no counterfactual. The 100,000 multiplier is hard-coded, which is
#' why the outcomes built from it are all named rate_*_per_pop /
#' _ppy_doxy. When the ratio is not a population rate -- P&S diagnoses per
#' incident infection, say -- use calculate_ratio_of_outcomes(), which applies
#' no multiplier.
#'
#' WORKED EXAMPLE (Atlanta, 2030, no intervention)
#'   incidence  = 9,117 ; population = 6,606,000
#'   rate_incidence_per_pop = 100000 * 9,117 / 6,606,000 = 138 per 100,000 py
#'   which is the value in Table 1.
#'
#' Numerator and denominator are both taken from the SAME arm, so the rate
#' reflects any population change the intervention causes. No zero-denominator
#' guard: a population denominator cannot be zero here.

## calculate_rate ----
calculate_rate <- function(arr, num.outcome, denom.outcome, dn.one.outcome = NULL) {
    p   <- .dims_last(arr, "outcome")
    num <- as.vector(subset_array(p, list(outcome = num.outcome)))
    den <- as.vector(subset_array(p, list(outcome = denom.outcome)))
    dn  <- dimnames(p)[setdiff(names(dimnames(p)), "outcome")]
    aperm(.as_array(100000 * num / den, dn), .outcome_order(arr))
}

## calculate_ratio_of_outcomes ----
#' Ratio of one outcome to another, in the same year
#'
#' Returns num/den on its own scale -- no multiplier, so the value is the
#' actual ratio (0.17, not 17 or 17000). Use calculate_rate() instead when the
#' denominator is a population and a per-100,000 rate is what is wanted.
#'
#' Computed WITHIN each sim, so summarise it as a median of ratios, not as a
#' ratio of medians. A zero denominator yields NA rather than Inf, which would
#' otherwise propagate into the median: unlike a population denominator, an
#' epidemic quantity such as incidence can legitimately be zero in a sim.
#'
#' WORKED EXAMPLE (Atlanta, 2030, no intervention)
#'   diagnosis.ps = 1,850 ; incidence = 9,117
#'   ratio_ps_diagnosis_to_incidence = 1,850 / 9,117 = 0.203
#'   -> about one in five incident infections was detected while still in the
#'      primary or secondary stage. This is a DETECTION property of the
#'      modelled care cascade, fixed at calibration; it is not an intervention
#'      effect and Doxy-PEP barely moves it.
calculate_ratio_of_outcomes <- function(arr, num.outcome, denom.outcome,
                                        dn.one.outcome = NULL) {
    p   <- .dims_last(arr, "outcome")
    num <- as.vector(subset_array(p, list(outcome = num.outcome)))
    den <- as.vector(subset_array(p, list(outcome = denom.outcome)))
    den[den == 0] <- NA_real_
    dn  <- dimnames(p)[setdiff(names(dimnames(p)), "outcome")]
    aperm(.as_array(num / den, dn), .outcome_order(arr))
}

#' Cases averted per 100,000 of a denominator -- averted COUNT over a LEVEL
#'
#' rate_averted(y, arm) = 100000 * ( num(y,"noint") - num(y,arm) ) / den(y,arm)
#'
#' Read it as: "of every 100,000 <denominator>, how many cases did this arm
#' prevent this year". The numerator is a DIFFERENCE between arms; the
#' denominator is the LEVEL in the arm being evaluated (not in noint). For
#' Doxy-PEP that distinction is immaterial -- the drug does not change
#' population size -- but it matters if this is reused for an intervention
#' that does.
#'
#' WHAT IT MEANS FOR INCIDENCE, by denominator (Atlanta, 2030, doxy.cov.30,
#' 3,000 infections averted):
#'
#'   denom.outcome = "population"      population     = 6,606,000
#'     rate_incidence_averted_per_pop  = 1e5 * 3,000 / 6,606,000 = 45.4
#'     -> 45 infections averted per 100,000 people in the MSA. Comparable
#'        across cities of different size; the population-level impact.
#'
#'   (A "population.msm" denominator was available here until 2026-09-08. It
#'   was REMOVED: it paired an ALL-SEX numerator with an MSM-only denominator,
#'   so rate_incidence_averted_per_msm overstated the MSM rate by exactly the
#'   spillover multiplier (~1.3-1.5x), and by a different amount in every MSA,
#'   so it was not even rank-preserving across cities. The stratum-correct
#'   quantity is rate_incidence_averted_per_pop read off the SEX-LEVEL array,
#'   where numerator and denominator are both taken from the same stratum.
#'   The same removal was made for the diagnosis.total and diagnosis.ps
#'   versions.)
#'
#'   (A "doxy.coverage" denominator was available here until 2026-09-08 and
#'   has been REMOVED, along with the three single-year outcomes built from
#'   it. A same-year ratio cannot attribute a transmission effect correctly --
#'   see "WHY THERE IS NO SINGLE-YEAR *_averted_ppy_doxy OUTCOME" at the foot
#'   of this file. Efficiency is answered by the CUMULATIVE outcome
#'   rate_cum_incidence_averted_ppy_doxy instead.)
#'
#' Same-year comparison against noint, computed within sim.

## calculate_rate_averted ----
calculate_rate_averted <- function(arr, num.outcome, denom.outcome,
                                   dn.one.outcome = NULL, noint = NOINT) {
    p   <- .dims_last(arr, c("intervention", "outcome"))
    num <- subset_array(p, list(outcome = num.outcome))     # outcome kept, length 1
    den <- subset_array(p, list(outcome = denom.outcome))
    nv  <- as.vector(subset_array(num, list(intervention = noint)))
    dn  <- dimnames(p)[setdiff(names(dimnames(p)), "outcome")]
    aperm(.as_array(100000 * (nv - as.vector(num)) / as.vector(den), dn),
          .outcome_order(arr))
}

## calculate_strata_share ----
#' Share of an outcome contributed by each stratum, as a percentage
#'
#' Collapses the stratification dimension to build a denominator, then divides
#' every stratum by it. The values therefore sum to 100 ACROSS the strata named
#' in `denominator.strata`; any stratum outside that set is returned as NA,
#' which is how the male-only share is left blank for women.
#'
#' The denominator is formed WITHIN each year / sim / intervention / location,
#' so the composition is scenario-specific: you can see whether Doxy-PEP shifts
#' who is acquiring infection, not just how many.
#'
#' Dimension order in equals dimension order out. Internally the stratification
#' dimension is moved to the end with aperm(), which makes the denominator
#' recycle across the strata blocks correctly and cannot silently transpose the
#' result the way a hand-built array() can.
#'
#' @param arr One outcome, dims year x <stratification> x sim x intervention x location.
#' @param denominator.strata Strata that make up the denominator. NULL (the
#'   default) uses every stratum, giving each stratum's share of the total.
#'
#' WORKED EXAMPLE (one MSA, one year, one arm, incidence by sex)
#'   msm = 500 ; heterosexual_male = 200 ; female = 300
#'
#'   denominator.strata = NULL (all three)   -> pct_incidence_share
#'     msm 50.0 , heterosexual_male 20.0 , female 30.0     (sums to 100)
#'
#'   denominator.strata = c("msm","heterosexual_male")     -> pct_male_incidence_share
#'     msm 500/700 = 71.4 , heterosexual_male 200/700 = 28.6 , female NA
#'     (the two male strata sum to 100; women are NA, not 0 -- they are
#'      outside this composition, not a zero part of it)
calculate_strata_share <- function(arr, denominator.strata = NULL) {
    
    strat <- setdiff(names(dimnames(arr)), c("year", "sim", "intervention", "location"))
    if (length(strat) != 1)
        stop("calculate_strata_share() needs exactly one stratification dimension; found: ",
             if (length(strat) == 0) "none" else paste(strat, collapse = ", "))
    
    levs <- dimnames(arr)[[strat]]
    if (is.null(denominator.strata)) denominator.strata <- levs
    missing.strata <- setdiff(denominator.strata, levs)
    if (length(missing.strata) > 0)
        stop("Stratum/strata not present in '", strat, "': ",
             paste(missing.strata, collapse = ", "))
    
    others <- setdiff(names(dimnames(arr)), strat)
    p      <- aperm(arr, c(others, strat))          # stratification dimension last
    
    # one denominator per year / sim / intervention / location, summed as whole
    # blocks rather than one closure call per cell
    denom <- Reduce(`+`, lapply(denominator.strata, function(s)
        as.vector(subset_array(p, stats::setNames(list(s), strat)))))
    denom[denom == 0] <- NA_real_                   # 0/0 would be a silent NaN
    
    # with the strata last, the array is one block per stratum, so a vector of
    # length prod(dim(others)) recycles across them exactly
    out <- 100 * p / as.vector(denom)
    
    # strata outside the denominator are not part of this composition
    keep <- levs %in% denominator.strata
    if (any(!keep)) {
        block <- prod(dim(p)[-length(dim(p))])
        out[rep(!keep, each = block)] <- NA_real_
    }
    
    aperm(out, names(dimnames(arr)))
}


#' Running total along `year`, within each sim / arm / location / stratum
#'
#' cum(y) = sum of x over baseline.year .. y
#'
#' THE BASELINE YEAR IS INCLUDED. The array reaching this function has already
#' been trimmed to baseline.year:end_year by the driver, so cum(baseline.year)
#' equals that year's value, not zero.
#'
#' That is harmless for the *_averted outcomes -- baseline.year contributes the
#' same amount to both arms and cancels in the difference -- but it does inflate
#' the DENOMINATOR of pct_cum_incidence_averted, because 2022 burden is counted
#' in the cumulative total while no intervention was operating that year
#' (Doxy-PEP starts in 2023). The effect is small but it is a dilution, and it
#' is worth stating in a methods section rather than being discovered by a
#' reader.
#'
#' WORKED EXAMPLE (Atlanta, no intervention, baseline.year = 2022)
#'   incidence 2022, 2023, 2024 = 6,098 ; 6,600 ; 7,100   (2022 is real)
#'   num_cum_incidence["2022"] = 6,098
#'   num_cum_incidence["2023"] = 12,698
#'   num_cum_incidence["2024"] = 19,798
#'
#' `stratification.dimensions` is vestigial: the running sum is along `year`
#' and every other dimension is carried through untouched, so strata are
#' accumulated separately without needing to be named.

## calculate_cumulative ----
calculate_cumulative <- function(arr, stratification.dimensions = NULL) {
    # Cumulative sum along `year`. Reshaping to a matrix with year down the
    # rows lets apply() walk plain columns instead of indexing four array
    # margins -- ~4x faster, and because it is still cumsum() doing the work
    # the results are BIT-identical to the previous version. (A hand-written
    # row-addition loop is marginally faster still, but cumsum() accumulates
    # in long double, so the loop differs in the last bit or two.)
    # `stratification.dimensions` is no longer needed and is kept only so the
    # existing call sites do not have to change.
    others <- setdiff(names(dimnames(arr)), "year")
    p <- aperm(arr, c("year", others))
    m <- apply(matrix(as.vector(p), nrow = dim(p)[1]), 2, cumsum)
    aperm(array(as.vector(m), dim(p), dimnames(p)), names(dimnames(arr)))
}

#' Percent change from the SAME arm's value in a baseline year
#'
#' pct_reduction(y, arm) = 100 * ( x(yr, arm) - x(y, arm) ) / x(yr, arm)
#'
#' SIGN TRAP: this is a REDUCTION, so it is POSITIVE when the outcome has
#' fallen below its baseline-year value and NEGATIVE when the epidemic has
#' grown. For syphilis in nine of the ten MSAs it is negative under every
#' coverage level, which is the point of reporting it alongside the averted
#' outcomes: the two answer different questions.
#'
#' The comparator is the same ARM in year `yr`, not noint. Since no
#' intervention is running in baseline.year, every arm shares that value, so
#' the arms are still comparable with each other.
#'
#' WORKED EXAMPLE (Atlanta, incidence rate per 100,000)
#'   2022 = 101 ; 2030 = 138
#'   100 * (101 - 138) / 101 = -36.6%
#'   -> incidence is 37% ABOVE its 2022 level, not 37% below it.
#'
#' Use calculate_ratio_versus_year() instead when growth is the quantity of
#' interest: it returns 138/101 = 1.37, which reads more naturally.
#'
#' The `noint` argument is accepted but NOT USED -- this function never
#' references the no-intervention arm. It is kept only so existing call sites
#' do not have to change.

## calculate_pct_reduction_versus_year ----
calculate_pct_reduction_versus_year <- function(arr, yr, dn.one.outcome = NULL,
                                                noint = NOINT) {
    p  <- .dims_last(arr, "year")
    yv <- as.vector(subset_array(p, list(year = yr)))
    aperm(array(100 * (yv - as.vector(p)) / yv, dim(p), dimnames(p)),
          .outcome_order(arr))
}

## calculate_ratio_versus_year ----
#' Ratio of an outcome to its own value in a baseline year (fold-change)
#'
#' Returns x_year / x_baseline.year, computed WITHIN each sim / intervention /
#' location / stratum, so the median and credible interval are taken over the
#' distribution of the RATIO. This is not the same as dividing the median in
#' the target year by the median in the baseline year (a ratio of medians);
#' the median of ratios is the correct summary when the numerator and
#' denominator are correlated across sims, as they are here.
#'
#' By construction the value in `yr` is exactly 1. A baseline of zero yields
#' NA rather than Inf.
#'
#' Companion to calculate_pct_reduction_versus_year(), which returns
#' 100 * (baseline - x) / baseline and is therefore NEGATIVE for a growing
#' epidemic. Use this function when the quantity of interest is growth.
#'
#' WORKED EXAMPLE (Atlanta, incidence rate per 100,000)
#'   2022 = 101 ; 2030 = 138
#'   ratio_rate_incidence_per_pop_vs_baseline["2030"] = 138 / 101 = 1.37
#'   -> a 1.4-fold rise. The same fact that
#'      calculate_pct_reduction_versus_year() reports as -36.6%.
calculate_ratio_versus_year <- function(arr, yr, dn.one.outcome = NULL) {
    p  <- .dims_last(arr, "year")
    yv <- as.vector(subset_array(p, list(year = yr)))
    yv[yv == 0] <- NA_real_
    aperm(array(as.vector(p) / yv, dim(p), dimnames(p)),
          .outcome_order(arr))
}


#' Build every derived outcome from a raw results array
#'
#' Takes the raw simulation array, trims it to baseline.year onward, and
#' returns an array of the same shape whose `outcome` dimension holds the
#' derived quantities instead of the raw ones. The raw outcomes are NOT carried
#' through, to keep the object small -- pass both arrays to the table builders
#' when you need raw and derived side by side.
#'
#' TWO CODE PATHS, chosen automatically:
#'   * total level      (no stratification dimension) -- gets the outcomes whose
#'     denominators are total-level only: population.msm and doxy.coverage,
#'     hence every *_ppy_doxy outcome, plus num_cum_doxy_coverage.
#'   * stratified level (e.g. a `sex` dimension) -- gets pct_incidence_share and
#'     pct_male_incidence_share instead, and NOT the doxy-denominated outcomes,
#'     because doxy.coverage is not sex-stratified in the raw arrays.
#'
#' THE THREE COMPARATORS. Every derived outcome answers one of three questions,
#' and mixing them up is the main way to misread this array:
#'   1. vs the no-intervention arm, SAME year   -> *_averted
#'   2. vs the SAME arm in baseline.year        -> *_vs_baseline,
#'                                                 *_reduction_vs_baseline
#'   3. no comparator, just a level             -> rate_*_per_pop, ratio_*_to_*
#'
#' The baseline year used for (2) is recorded on the result as
#' attr(x, "baseline.year").
#'
#' EVERYTHING IS COMPUTED WITHIN SIM, so each derived outcome has a full
#' posterior distribution and should be summarised as a median (and quantiles)
#' OF THE DERIVED QUANTITY -- never by combining medians of its parts. A median
#' of ratios is not a ratio of medians.
#'
#' @param raw_results Array with named dimnames including year, sim,
#'   intervention, location, outcome, plus at most one stratification
#'   dimension. NOTE: the stratification dimension is detected via
#'   names(dim(raw_results)), so the `dim` attribute must carry names -- it
#'   does when the array is built as array(x, sapply(dn, length), dn), which is
#'   how generate_total_results_array.R builds it.
#' @param baseline.year The year comparator (2) is measured against, and the
#'   first year kept in the output.

## calculate_custom_outcomes ----
calculate_custom_outcomes <- function(raw_results, baseline.year, debug=F) {
    if (debug) browser()
    
    # A3: take the LARGEST year, not the year that happens to sit in the last
    # position. These are identical only while the year dimnames are sorted.
    end_year <- max(as.integer(dimnames(raw_results)$year))
    baseline.year <- as.integer(baseline.year)
    if (!as.character(baseline.year) %in% dimnames(raw_results)$year)
        stop("baseline.year ", baseline.year, " is not among the years in raw_results (",
             min(as.integer(dimnames(raw_results)$year)), "-", end_year, ").")
    YEARS_TO_KEEP <- as.character(baseline.year:end_year)
    
    # Every subset below uses drop = TRUE, which collapses ANY length-1
    # dimension, not only `outcome`. A run with a single location, sim or year
    # therefore silently loses that dimension and dies much later inside
    # aperm() with "'perm' is of wrong length", which says nothing useful.
    # Fail here instead.
    .core <- c("year", "sim", "intervention", "location")
    .thin <- .core[vapply(.core, function(d) length(dimnames(raw_results)[[d]]),
                          integer(1)) == 1]
    if (length(.thin))
        stop("Dimension(s) with only one level: ", paste(.thin, collapse = ", "),
             ". subset_array(..., drop = TRUE) would collapse them and the ",
             "outcome arrays would stop lining up. Keep at least two levels ",
             "in each of year, sim, intervention and location.")
    
    STRATIFICATION_DIMENSIONS = setdiff(names(dim(raw_results)), c("year", "sim", "intervention", "location", "outcome"))
    if (length(STRATIFICATION_DIMENSIONS)==0) STRATIFICATION_DIMENSIONS <- NULL
    
    print("Re-ordering results to have outcome last")
    
    # This has to have all outcomes we'll ever use for calculations (it gets extended just below)
    # Subsetting now lets us shrink the array and speed up the runtime
    CHOSEN_OUTCOMES <- c("incidence", "diagnosis.total", "diagnosis.ps", "population")
    if (is.null(STRATIFICATION_DIMENSIONS))
        CHOSEN_OUTCOMES <- c(CHOSEN_OUTCOMES, "population.msm", "doxy.coverage")
    # Early-latent diagnoses are a calibration target but are not carried by
    # every results array, so they are opt-in: the EL ratio outcome below is
    # only produced when the raw array actually has them.
    has.el <- "diagnosis.el.misclassified" %in% dimnames(raw_results)$outcome
    if (has.el) CHOSEN_OUTCOMES <- c(CHOSEN_OUTCOMES, "diagnosis.el.misclassified")
    
    results <- subset_array(raw_results, list(year = YEARS_TO_KEEP, outcome = CHOSEN_OUTCOMES))
    # aperm(), not apply(..., function(x) x): the same permutation in C instead
    # of one closure call per cell (measured ~400x on a 2.5M-element array)
    results <- aperm(results,
                     c("year", STRATIFICATION_DIMENSIONS, "sim", "intervention",
                       "location", "outcome"))
    
    # Intervention is on the front because most of the apply's will leave it on the front
    dn_one_outcome <- dimnames(results)[c("intervention", "year", STRATIFICATION_DIMENSIONS, "sim","location")]
    
    # But if we're doing something based on year (like percent reduction relative to 2022), year will remain in front
    dn_one_outcome_yr_first <- dimnames(results)[c("year", STRATIFICATION_DIMENSIONS, "sim","intervention", "location")]
    
    print("Calculating incidence outcomes")
    num_incidence_averted <-
        calculate_averted_count(subset_array(results, list(outcome = "incidence"), drop=T),
                                dn_one_outcome)
    pct_incidence_averted <-
        calculate_averted_pct(subset_array(results, list(outcome = "incidence"), drop=T),
                              dn_one_outcome)
    rate_incidence_averted_per_pop <-
        calculate_rate_averted(subset_array(results, list(outcome = c("incidence", "population")), drop=T),
                               num.outcome = "incidence",
                               denom.outcome = "population",
                               dn_one_outcome)
    pct_incidence_reduction_vs_baseline <-
        calculate_pct_reduction_versus_year(subset_array(results, list(outcome = "incidence"), drop=T),
                                            as.character(baseline.year),
                                            dn_one_outcome_yr_first)
    rate_incidence_per_pop <- 
        calculate_rate(subset_array(results, list(outcome = c("incidence","population")), drop=T), 
                       num.outcome = "incidence",
                       denom.outcome = "population",
                       dn_one_outcome)
    
    ratio_incidence_vs_baseline <-
        calculate_ratio_versus_year(subset_array(results, list(outcome = "incidence"), drop=T),
                                    as.character(baseline.year),
                                    dn_one_outcome_yr_first)
    ratio_rate_incidence_per_pop_vs_baseline <-
        calculate_ratio_versus_year(rate_incidence_per_pop,
                                    as.character(baseline.year),
                                    dn_one_outcome_yr_first)
    
    print("Calculating total diagnosis outcomes")
    num_diagnosis_total_averted <-
        calculate_averted_count(subset_array(results, list(outcome = "diagnosis.total"), drop=T),
                                dn_one_outcome)
    
    pct_diagnosis_total_averted <-
        calculate_averted_pct(subset_array(results, list(outcome = "diagnosis.total"), drop=T),
                              dn_one_outcome)
    rate_diagnosis_total_averted_per_pop <-
        calculate_rate_averted(subset_array(results, list(outcome = c("diagnosis.total", "population")), drop=T),
                               num.outcome = "diagnosis.total",
                               denom.outcome = "population",
                               dn_one_outcome)
    pct_diagnosis_total_reduction_vs_baseline <-
        calculate_pct_reduction_versus_year(subset_array(results, list(outcome = "diagnosis.total"), drop=T),
                                            as.character(baseline.year),
                                            dn_one_outcome_yr_first)
    rate_diagnosis_total_per_pop <- 
        calculate_rate(subset_array(results, list(outcome = c("diagnosis.total","population")), drop=T), 
                       num.outcome = "diagnosis.total",
                       denom.outcome = "population",
                       dn_one_outcome)
    
    ratio_diagnosis_total_vs_baseline <-
        calculate_ratio_versus_year(subset_array(results, list(outcome = "diagnosis.total"), drop=T),
                                    as.character(baseline.year),
                                    dn_one_outcome_yr_first)
    ratio_rate_diagnosis_total_per_pop_vs_baseline <-
        calculate_ratio_versus_year(rate_diagnosis_total_per_pop,
                                    as.character(baseline.year),
                                    dn_one_outcome_yr_first)
    
    print("Calculating PS diagnosis outcomes")
    num_diagnosis_ps_averted <-
        calculate_averted_count(subset_array(results, list(outcome = "diagnosis.ps"), drop=T),
                                dn_one_outcome)
    pct_diagnosis_ps_averted <-
        calculate_averted_pct(subset_array(results, list(outcome = "diagnosis.ps"), drop=T),
                              dn_one_outcome)
    rate_diagnosis_ps_averted_per_pop <-
        calculate_rate_averted(subset_array(results, list(outcome = c("diagnosis.ps", "population")), drop=T),
                               num.outcome = "diagnosis.ps",
                               denom.outcome = "population",
                               dn_one_outcome)
    pct_diagnosis_ps_reduction_vs_baseline <-
        calculate_pct_reduction_versus_year(subset_array(results, list(outcome = "diagnosis.ps"), drop=T),
                                            as.character(baseline.year),
                                            dn_one_outcome_yr_first)
    rate_diagnosis_ps_per_pop <- 
        calculate_rate(subset_array(results, list(outcome = c("diagnosis.ps","population")), drop=T), 
                       num.outcome = "diagnosis.ps",
                       denom.outcome = "population",
                       dn_one_outcome)
    
    ratio_diagnosis_ps_vs_baseline <-
        calculate_ratio_versus_year(subset_array(results, list(outcome = "diagnosis.ps"), drop=T),
                                    as.character(baseline.year),
                                    dn_one_outcome_yr_first)
    ratio_rate_diagnosis_ps_per_pop_vs_baseline <-
        calculate_ratio_versus_year(rate_diagnosis_ps_per_pop,
                                    as.character(baseline.year),
                                    dn_one_outcome_yr_first)
    
    # ---- early-stage diagnoses relative to incident infection --------------
    # Diagnoses in year t of infections acquired long before t cannot speak to
    # detection of year-t incidence. P&S is the stage closest to acquisition
    # (~0-6 months), so this ratio is only second-order biased by the
    # infection-to-diagnosis lag, where diagnosis.total/incidence is not.
    # Same denominator for both, so early syphilis is the sum of the two.
    ratio_ps_diagnosis_to_incidence <-
        calculate_ratio_of_outcomes(subset_array(results, list(outcome = c("diagnosis.ps","incidence")), drop=T),
                                    num.outcome   = "diagnosis.ps",
                                    denom.outcome = "incidence",
                                    dn_one_outcome)
    if (has.el)
        ratio_el_diagnosis_to_incidence <-
        calculate_ratio_of_outcomes(subset_array(results, list(outcome = c("diagnosis.el.misclassified","incidence")), drop=T),
                                    num.outcome   = "diagnosis.el.misclassified",
                                    denom.outcome = "incidence",
                                    dn_one_outcome)
    
    if (is.null(STRATIFICATION_DIMENSIONS)) {
        print("Calculating cumulative doxy coverage")
        num_cum_doxy_coverage <-
            calculate_cumulative(subset_array(results, list(outcome = "doxy.coverage"), drop=T), stratification.dimensions = STRATIFICATION_DIMENSIONS)
    }
    
    # Cumulative incidence averted
    print("Calculating cumulative incidence outcomes")
    num_cum_incidence <-
        calculate_cumulative(subset_array(results, list(outcome = "incidence"), drop=T), stratification.dimensions = STRATIFICATION_DIMENSIONS)
    num_cum_incidence_averted <-
        calculate_averted_count(num_cum_incidence, dn_one_outcome)
    pct_cum_incidence_averted <-
        calculate_averted_pct(num_cum_incidence, dn_one_outcome)
    if (is.null(STRATIFICATION_DIMENSIONS)) {
        rate_cum_incidence_averted_ppy_doxy <-
            100000 * num_cum_incidence_averted / num_cum_doxy_coverage
    }
    
    # Cumulative diagnoses averted
    print("Calculating cumulative total diagnosis outcomes")
    num_cum_diagnosis_total <-
        calculate_cumulative(subset_array(results, list(outcome = "diagnosis.total"), drop=T), stratification.dimensions = STRATIFICATION_DIMENSIONS)
    num_cum_diagnosis_total_averted <-
        calculate_averted_count(num_cum_diagnosis_total, dn_one_outcome)
    pct_cum_diagnosis_total_averted <-
        calculate_averted_pct(num_cum_diagnosis_total, dn_one_outcome)
    if (is.null(STRATIFICATION_DIMENSIONS)) {
        rate_cum_diagnosis_total_averted_ppy_doxy <-
            100000 * num_cum_diagnosis_total_averted / num_cum_doxy_coverage
    }
    
    print("Calculating cumulative PS diagnosis outcomes")
    num_cum_diagnosis_ps <-
        calculate_cumulative(subset_array(results, list(outcome = "diagnosis.ps"), drop=T), stratification.dimensions = STRATIFICATION_DIMENSIONS)
    num_cum_diagnosis_ps_averted <-
        calculate_averted_count(num_cum_diagnosis_ps, dn_one_outcome)
    pct_cum_diagnosis_ps_averted <-
        calculate_averted_pct(num_cum_diagnosis_ps, dn_one_outcome)
    if (is.null(STRATIFICATION_DIMENSIONS)) {
        # A1: was num_cum_diagnosis_ps (the cumulative count) instead of the
        # averted count, unlike its incidence and total-diagnosis siblings.
        rate_cum_diagnosis_ps_averted_ppy_doxy <-
            100000 * num_cum_diagnosis_ps_averted / num_cum_doxy_coverage
    }
    
    # ---- who is acquiring infection: composition across strata ------------
    # Stratified results only -- the total-level array has no strata to
    # compare. `pct_incidence_share` sums to 100 across the strata;
    # `pct_male_incidence_share` is defined for the male strata only and is
    # NA for women.
    MALE.STRATA <- c("msm", "heterosexual_male")
    has.male.strata <- FALSE
    
    if (!is.null(STRATIFICATION_DIMENSIONS)) {
        print("Calculating incidence composition across strata")
        incidence.by.strata <- subset_array(results, list(outcome = "incidence"), drop = T)
        
        pct_incidence_share <- calculate_strata_share(incidence.by.strata)
        
        strat.levels    <- unlist(dimnames(results)[STRATIFICATION_DIMENSIONS], use.names = FALSE)
        has.male.strata <- all(MALE.STRATA %in% strat.levels)
        if (has.male.strata)
            pct_male_incidence_share <-
            calculate_strata_share(incidence.by.strata,
                                   denominator.strata = MALE.STRATA)
        else
            message("Skipping pct_male_incidence_share: '",
                    paste(setdiff(MALE.STRATA, strat.levels), collapse = "', '"),
                    "' not among the stratification levels.")
    }
    
    # ------------------------------------------------------------------
    # Combine, but DO NOT INCLUDE ORIGINAL OUTCOMES (to keep it smaller)
    #
    # Every outcome name below is also the name of the variable holding it,
    # so mget() fetches the arrays in exactly the order the names are listed.
    # There is no second, hand-maintained list of objects that can drift out
    # of step with this one.
    # ------------------------------------------------------------------
    dn_w_custom <- dimnames(results)
    # Add names of locations back in
    dn_w_custom[["location"]] <- dimnames(raw_results)[["location"]]
    
    if (is.null(STRATIFICATION_DIMENSIONS)) {
        outcome.names <- c(
            # single scenario-not comparison:
            
            "rate_incidence_per_pop",
            "ratio_incidence_vs_baseline", #count fold-change
            "ratio_rate_incidence_per_pop_vs_baseline", #rate fold-change
            "pct_incidence_reduction_vs_baseline",
            #
            "rate_diagnosis_total_per_pop",
            "ratio_diagnosis_total_vs_baseline", #count fold-change
            "ratio_rate_diagnosis_total_per_pop_vs_baseline", #rate fold-change
            "pct_diagnosis_total_reduction_vs_baseline",
            #
            "rate_diagnosis_ps_per_pop",
            "ratio_diagnosis_ps_vs_baseline",#count fold-change
            "ratio_rate_diagnosis_ps_per_pop_vs_baseline", #rate fold-change
            "pct_diagnosis_ps_reduction_vs_baseline",
            
            # Detection rate:
            "ratio_ps_diagnosis_to_incidence",
            
            # Cumulative 
            "num_cum_doxy_coverage",
            "num_cum_incidence",
            "num_cum_diagnosis_total",
            "num_cum_diagnosis_ps",
            
            #Averted: 
            "num_incidence_averted",
            "pct_incidence_averted",
            "rate_incidence_averted_per_pop", # absolute reduction in incidence rate 
            #
            "num_diagnosis_total_averted",
            "pct_diagnosis_total_averted",
            "rate_diagnosis_total_averted_per_pop",
            #
            "num_diagnosis_ps_averted",
            "pct_diagnosis_ps_averted",
            "rate_diagnosis_ps_averted_per_pop",
            
            # Cumulative averted
            "num_cum_incidence_averted",
            "pct_cum_incidence_averted",
            "rate_cum_incidence_averted_ppy_doxy", #total infections averted per 100,000 person-years on doxy by that year
            #            
            "num_cum_diagnosis_total_averted",
            "pct_cum_diagnosis_total_averted",
            "rate_cum_diagnosis_total_averted_ppy_doxy",
            #         
            "num_cum_diagnosis_ps_averted",
            "pct_cum_diagnosis_ps_averted",
            "rate_cum_diagnosis_ps_averted_ppy_doxy"
        )
    } else {
        # SAME GROUPING AND ORDER AS THE TOTAL-LEVEL BLOCK ABOVE, so the two
        # arrays can be read side by side. The differences are structural, not
        # stylistic, and are marked inline:
        #   - no *_ppy_doxy and no num_cum_doxy_coverage: doxy.coverage is not
        #     sex-stratified in the raw arrays, so there is no denominator here
        #   - pct_incidence_share / pct_male_incidence_share exist ONLY here:
        #     a composition needs strata to compose
        outcome.names <- c(
            # single scenario-not comparison:
            
            "rate_incidence_per_pop", 
            "ratio_incidence_vs_baseline",#count fold-change
            "ratio_rate_incidence_per_pop_vs_baseline", #rate fold-change
            "pct_incidence_reduction_vs_baseline",
            #
            "rate_diagnosis_total_per_pop",
            "ratio_diagnosis_total_vs_baseline",#count fold-change
            "ratio_rate_diagnosis_total_per_pop_vs_baseline", #rate fold-change
            "pct_diagnosis_total_reduction_vs_baseline",
                        #
            "rate_diagnosis_ps_per_pop",
            "ratio_diagnosis_ps_vs_baseline",#count fold-change
            "ratio_rate_diagnosis_ps_per_pop_vs_baseline", #rate fold-change
            "pct_diagnosis_ps_reduction_vs_baseline",
            # Detection rate:
            "ratio_ps_diagnosis_to_incidence",
            
            # Composition across strata (STRATIFIED ONLY - no total equivalent):
            "pct_incidence_share",
            
            # Cumulative
            # (num_cum_doxy_coverage is total-level only)
            "num_cum_incidence",
            "num_cum_diagnosis_total",
            "num_cum_diagnosis_ps",
            
            #Averted:
            "num_incidence_averted",
            "pct_incidence_averted",
            "rate_incidence_averted_per_pop", # absolute reduction in incidence rate, within this stratum
            #
            "num_diagnosis_total_averted",
            "pct_diagnosis_total_averted",
            "rate_diagnosis_total_averted_per_pop",
            #
            "num_diagnosis_ps_averted",
            "pct_diagnosis_ps_averted",
            "rate_diagnosis_ps_averted_per_pop",

          
            # Cumulative averted
            # (the rate_cum_*_ppy_doxy versions are total-level only)
            "num_cum_incidence_averted",
            "pct_cum_incidence_averted",
            #
            "num_cum_diagnosis_total_averted",
            "pct_cum_diagnosis_total_averted",
            #
            "num_cum_diagnosis_ps_averted",
            "pct_cum_diagnosis_ps_averted"
        )
        # Only present when the strata are the sexes. Appended here rather
        # than sitting beside pct_incidence_share in the composition group
        # above, so it lands at the END of the outcome dimension whenever it
        # applies -- the grouping above describes the fixed outcomes only.
        if (has.male.strata)
            outcome.names <- c(outcome.names, "pct_male_incidence_share")
    }
    # only present when the raw results carry early-latent diagnoses
    if (has.el)
        outcome.names <- c(outcome.names, "ratio_el_diagnosis_to_incidence")
    
    dn_w_custom[["outcome"]] <- outcome.names
    
    outcome.arrays <- mget(outcome.names, envir = environment())
    
    # ---- shape check: every outcome must be one full slice, in the same
    # ---- dimension order, or array() below would silently transpose it
    slice.names <- setdiff(names(dn_w_custom), "outcome")
    slice.dim   <- sapply(dn_w_custom[slice.names], length)
    for (nm in outcome.names) {
        got <- outcome.arrays[[nm]]
        if (!identical(names(dimnames(got)), slice.names))
            stop("Outcome '", nm, "' has dimensions in the order ",
                 paste(names(dimnames(got)), collapse = ", "),
                 " but ", paste(slice.names, collapse = ", "), " was expected.")
        if (!identical(unname(dim(got)), unname(slice.dim)))
            stop("Outcome '", nm, "' is ", paste(dim(got), collapse = "x"),
                 " but ", paste(slice.dim, collapse = "x"), " was expected.")
    }
    
    results_w_custom <- array(
        unlist(outcome.arrays, use.names = FALSE),
        sapply(dn_w_custom, length),
        dn_w_custom
    )
    
    # record which year the pct_*_reduction_vs_baseline outcomes are relative to
    attr(results_w_custom, "baseline.year") <- as.character(baseline.year)
    
    results_w_custom
}


# ----------------------------------------------------------------------------
# OUTCOME NAMING CONVENTION
#   num_*   a count               (cases, diagnoses, doxy person-years)
#   pct_*   a percentage          (0-100)
#   rate_*  a rate per 100,000, with the denominator spelled out:
#             _per_pop    per 100,000 population
#             (there is deliberately NO _per_msm variant: pairing an all-sex
#              numerator with an MSM denominator is not an MSM rate. For a
#              stratum-specific rate use rate_*_per_pop on the SEX-LEVEL
#              array, where both parts come from the same stratum, and which
#              works for female and heterosexual_male as well as msm.)
#             _ppy_doxy   per 100,000 doxy-PEP person-years
#   *_share  a percentage of a stratum total rather than of a comparator:
#             pct_incidence_share       each stratum's share of ALL incidence
#                                       (the three sexes sum to 100)
#             pct_male_incidence_share  each male stratum's share of MALE
#                                       incidence (msm + heterosexual_male sum
#                                       to 100; women are NA)
#           Both are stratified-results only, and are computed WITHIN each
#           intervention, so they show whether Doxy-PEP shifts the composition.
#   ratio_* a unitless ratio. The suffix says what the comparator is:
#             _vs_baseline   the SAME outcome in `baseline.year` (a
#                            fold-change: 1 = no change, 2 = doubled,
#                            0.75 = a 25% decline)
#             _to_<outcome>  a DIFFERENT outcome in the SAME year, e.g.
#                            ratio_ps_diagnosis_to_incidence
#           Both are computed within each sim, so summarise them as a
#           MEDIAN OF RATIOS, not a ratio of medians.
#   *_averted            relative to the "noint" comparator
#   *_reduction_vs_baseline  relative to `baseline.year`, recorded on the
#                            returned array as attr(x, "baseline.year")
#
# NOTE: the *_ppy_doxy outcomes are NaN in the "noint" slice by construction
# (doxy coverage is zero there, so the denominator is zero).
# ----------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# WHY THERE IS NO SINGLE-YEAR *_averted_ppy_doxy OUTCOME
#
# Removed 2026-09-08. Three outcomes existed here and are gone:
#   rate_incidence_averted_ppy_doxy
#   rate_diagnosis_total_averted_ppy_doxy
#   rate_diagnosis_ps_averted_ppy_doxy
#
# Each divided ONE YEAR'S averted cases by THAT SAME YEAR'S Doxy-PEP
# person-years. That attribution does not hold for an intervention that works
# through transmission. Doxycycline delivered in 2025 prevents infections in
# 2025 and then -- by removing infectious person-time from the sexual network
# -- also in 2026, 2027 and beyond. A same-year ratio credits the 2025 drug
# with only the 2025 benefit, and later credits the 2030 drug with benefit
# that accumulated from every earlier year of suppression.
#
# So it is biased in OPPOSITE directions at the two ends of a scale-up: it
# understates efficiency early, while the indirect effect is still arriving,
# and overstates it late, once the epidemic has been suppressed for years by
# drug given earlier. On an illustrative linear ramp the annual ratio rose
# 4.1-fold across 2023-2030 while the cumulative ratio moved 3.5-fold, and the
# two disagreed by 16% in the final year -- the annual version flattering the
# intervention.
#
# USE THE CUMULATIVE OUTCOME INSTEAD:
#
#   rate_cum_incidence_averted_ppy_doxy
#       = 1e5 * num_cum_incidence_averted / num_cum_doxy_coverage
#
# All the drug given over the window, over all the infections prevented over
# the same window. Both sides span the same period, so nothing is credited to
# the wrong year. Its MARGINAL counterpart -- what the next increment of
# coverage buys per additional person-year of drug -- is what a programme
# actually decides on, and is built in the analysis scripts by differencing
# adjacent coverage levels rather than as an outcome here.
#
# NOTE THE ASYMMETRY with rate_incidence_averted_per_pop, which IS reported
# annually and should be. A risk difference in 2030 is a legitimate
# single-year statement: its denominator is the population alive that year,
# not a cumulative input whose earlier units are still paying off.
#
# doxy.coverage is still pulled into CHOSEN_OUTCOMES at the total level -- it
# is needed for num_cum_doxy_coverage and the three cumulative rate_cum_*
# outcomes. Only the single-year ratios are gone.
# ----------------------------------------------------------------------------

# Verify accuracy of these transformations
# total_raw_results["2026", 3, "noint", "C.12580", "incidence"] -
#     total_raw_results["2026", 3, "doxy.cov.10", "C.12580", "incidence"] ==
#     total_calc_results["2026", 3, "doxy.cov.10", "C.12580", "num_incidence_averted"]

if (1==1) {
    # The calc arrays are written next to the raw ones, in the calibration's output folder.
    OUT.DIR <- shield.output.path(CALIB.NAME, create = TRUE)
    
    if(1==2){
        source ("../jheem_analyses/applications/SHIELD/shield_specification.R")
        print(ROOT.DIR)
        # only the raw arrays exist at this point -- this script writes the calc ones
        RAW <- load.shield.results(CALIB.NAME, which = c("total_raw", "sex_raw"))
        total_raw_results <- RAW$total_raw
        sex_raw_results   <- RAW$sex_raw
    }
    
    total_calc_results <- calculate_custom_outcomes(total_raw_results, baseline.year = "2022")
    save(total_calc_results,
         file = file.path(OUT.DIR, "total_calc_results.Rdata"))
    
    sex_calc_results <- calculate_custom_outcomes(sex_raw_results, baseline.year = "2022")
    save(sex_calc_results,
         file = file.path(OUT.DIR, "sex_calc_results.Rdata"))
}

#cumulative incidence : add a baseline year: 2022 update all instances 
#filter calculated outcomes to post 2022
# double checking ppy calculation 
# add incidence rate, and total diagnosis rate
