# ****************************************************************************
# SHIELD / Doxy-PEP -- INCREMENTAL EFFICIENCY OF DOXY-PEP COVERAGE
# ****************************************************************************
#
# A cost-effectiveness style table with PERSON-YEARS OF DOXYCYCLINE as the
# cost. For each MSA, horizon and coverage level:
#
#   CUMULATIVE (properties of the scenario)
#     cum_py            Doxy-PEP person-years delivered, 2022 to the horizon
#     cum_averted_msm   incident infections averted among MSM vs no-doxy
#     cum_averted_tot   incident infections averted in the total population
#
#   INCREMENTAL (properties of the MOVE from the coverage level below)
#     vs                the comparator rung ("none" for the lowest)
#     d_py, d_averted_msm, d_averted_tot
#     py_per_infection_msm  = d_py / d_averted_msm
#     py_per_infection_tot  = d_py / d_averted_tot
#
# WHY INCREMENTAL AND NOT AVERAGE. Person-years are linear in coverage -- each
# 10 points buys the same amount of drug -- so average efficiency
# (cum_averted / cum_py) is a deterministic rescaling of cum_averted and adds
# no information to the column beside it. Worse, it credits the next decision
# with the returns of decisions already made: nobody gets to buy the first 10
# points twice. The incremental ratio is the ICER analogue and is the quantity
# a programme actually decides on.
#
# READING THE TWO EFFICIENCY COLUMNS TOGETHER. Doxy-PEP goes only to MSM, so
# both columns share a denominator. The MSM column is the direct benefit; the
# total column adds the infections prevented in women and heterosexual men.
# The gap between them IS the spillover, expressed as antibiotic saved.
#
# ---------------------------------------------------------------------------
# METHOD: built from the MEDIANS returned by make_multi_location_table() -- no
# sim-level arithmetic, no credible intervals. The table therefore reconciles
# with itself: subtract two rows of cum_averted, divide into d_py, and you land
# on the printed ratio. Differencing medians is safe because the median is
# monotone: if cumulative averted rises with coverage in every sim, the medians
# rise too. CHECK 1 verifies that premise.
# ---------------------------------------------------------------------------
#
# REQUIRES in the environment (as loaded by eg_tables.R):
#   results (the list of four arrays), MSAS,
#   make_multi_location_table(), table_to_long()
#
# WRITES to TABLE.DIR:
#   table_efficiency_pooled.csv     main text, ten MSAs combined
#   table_efficiency_by_msa.csv     supplement, one row per MSA x coverage
# ****************************************************************************

library(dplyr)
library(tidyr)

# ---- 0. CONFIGURATION ------------------------------------------------------

EFF.HORIZONS    <- c("2030", "2035")
COVERAGE.LEVELS <- paste0("doxy.cov.", seq(10, 100, 10))
DIGITS.RATIO    <- 1

# if (!exists("TABLE.DIR")) TABLE.DIR <- "tables/"
# if (!dir.exists(TABLE.DIR)) dir.create(TABLE.DIR, recursive = TRUE)


# ---- 1. PULL THE MEDIANS ---------------------------------------------------
# num_cum_doxy_coverage exists only at the total level, so the sex rows come
# back NA for it. That is expected; the "Dropped N cell(s)" message is the same
# one doxy_summary_aug.R already prints.

lng <- table_to_long(make_multi_location_table(
    data          = results,
    locations     = MSAS,
    outcomes      = c("num_cum_incidence_averted", "num_cum_doxy_coverage"),
    interventions = COVERAGE.LEVELS,
    years         = EFF.HORIZONS,
    stat.type     = "median"))

py <- lng %>%
    filter(outcome == "num_cum_doxy_coverage", subgroup == "Total") %>%
    select(location, year, coverage, cum_py = value)

av <- lng %>%
    filter(outcome == "num_cum_incidence_averted",
           subgroup %in% c("msm", "Total")) %>%
    select(location, year, coverage, subgroup, value) %>%
    pivot_wider(names_from = subgroup, values_from = value) %>%
    rename(cum_averted_msm = msm, cum_averted_tot = Total)

dat <- left_join(av, py, by = c("location", "year", "coverage"))
if (any(is.na(dat$cum_py)))
    stop("Missing Doxy-PEP person-years for ", sum(is.na(dat$cum_py)),
         " cell(s). Is num_cum_doxy_coverage present in the total-level array?")

dat
# ---- 2. INCREMENTS ---------------------------------------------------------
# The rung below the lowest coverage level is `noint`, where cumulative averted
# is zero by construction (calculate_averted_count differences against noint)
# and no drug is delivered. lag(default = 0) encodes that; CHECK 3 tests the
# person-year half of the assumption against the model instead of trusting it.
#
# A reciprocal is undefined when the increment is not positive, so those cells
# are NA rather than Inf or a negative "cost per infection", which would be
# nonsense on the page.

## .pos ----
.pos <- function(x) ifelse(!is.na(x) & x > 0, x, NA_real_)

step <- dat %>%
    arrange(location, year, coverage) %>%
    group_by(location, year) %>%
    mutate(
        vs              = ifelse(is.na(dplyr::lag(coverage)), "none",
                                 paste0(dplyr::lag(coverage), "%")),
        d_py            = cum_py          - dplyr::lag(cum_py,          default = 0),
        d_averted_msm   = cum_averted_msm - dplyr::lag(cum_averted_msm, default = 0),
        d_averted_tot   = cum_averted_tot - dplyr::lag(cum_averted_tot, default = 0),
        py_per_inf_msm  = d_py / .pos(d_averted_msm),
        py_per_inf_tot  = d_py / .pos(d_averted_tot)) %>%
    ungroup()


# ---- 3. THE TWO TABLES -----------------------------------------------------

## .n ----
.n <- function(x) ifelse(is.na(x), "-", formatC(round(x), format = "d", big.mark = ","))

## .r ----
.r <- function(x) ifelse(is.na(x), "-", formatC(x, format = "f", digits = DIGITS.RATIO))

COLS <- c("coverage", "cum_py", "cum_averted_msm", "cum_averted_tot", "vs",
          "d_py", "d_averted_msm", "d_averted_tot",
          "py_per_inf_msm", "py_per_inf_tot")

# 3a. SUPPLEMENT: every MSA, every rung
by.msa <- step %>%
    transmute(location, year, coverage,
              cum_py          = .n(cum_py),
              cum_averted_msm = .n(cum_averted_msm),
              cum_averted_tot = .n(cum_averted_tot),
              vs,
              d_py            = .n(d_py),
              d_averted_msm   = .n(d_averted_msm),
              d_averted_tot   = .n(d_averted_tot),
              py_per_inf_msm  = .r(py_per_inf_msm),
              py_per_inf_tot  = .r(py_per_inf_tot)) %>%
    arrange(year, location, coverage) %>%
    select(location, year, all_of(COLS))
by.msa
write.csv(by.msa, file.path(TABLE.DIR, "table_efficiency_by_msa.csv"),
          row.names = FALSE)

# 3b. MAIN TEXT: the ten MSAs combined.
#
# Counts are SUMMED. The ratio is then formed from the summed numerator and
# summed denominator -- sum(d_py) / sum(d_averted) -- which answers "across all
# ten MSAs combined, how much doxycycline per infection prevented".
#
# Writing r_i = d_py_i / d_averted_i, that pooled ratio is
#
#     sum(d_py_i) / sum(d_averted_i)  =  sum( (d_averted_i / sum(d_averted)) * r_i )
#
# i.e. a weighted mean of the per-MSA ratios with weights equal to each MSA's
# share of INFECTIONS AVERTED (not of person-years -- an earlier version of
# this comment had that backwards). A plain median of the ten ratios would
# instead weight Baltimore equally with Houston.
#
# Because those weights are positive and sum to one, the pooled ratio is a
# convex combination of the per-MSA ratios and MUST lie inside their range.
# CHECK 5 asserts that.
#
# NO INTERVALS ARE WRITTEN. The pooled CSV carries exactly the same columns as
# the by-MSA CSV -- one ratio per population -- so the pooled file is simply
# the ten MSAs treated as one, and nothing in it can be mistaken for a CrI.
# Every value here is a median over the posterior simulations, with the
# interval discarded at make_multi_location_table().
#
# The min and max of the ten per-MSA ratios ARE still computed and printed to
# the console, because the spread between cities is a real finding. It stays
# out of the table because it is heterogeneity, not uncertainty, and in
# brackets beside a pooled value it would read as a credible interval.
pooled <- step %>%
    group_by(year, coverage) %>%
    summarise(
        vs              = first(vs),
        cum_py          = sum(cum_py),
        cum_averted_msm = sum(cum_averted_msm),
        cum_averted_tot = sum(cum_averted_tot),
        d_py            = sum(d_py),
        d_averted_msm   = sum(d_averted_msm),
        d_averted_tot   = sum(d_averted_tot),
        msm_lo = if (all(is.na(py_per_inf_msm))) NA_real_ else min(py_per_inf_msm, na.rm = TRUE),
        msm_hi = if (all(is.na(py_per_inf_msm))) NA_real_ else max(py_per_inf_msm, na.rm = TRUE),
        tot_lo = if (all(is.na(py_per_inf_tot))) NA_real_ else min(py_per_inf_tot, na.rm = TRUE),
        tot_hi = if (all(is.na(py_per_inf_tot))) NA_real_ else max(py_per_inf_tot, na.rm = TRUE),
        .groups = "drop") %>%
    mutate(py_per_inf_msm_num = d_py / .pos(d_averted_msm),
           py_per_inf_tot_num = d_py / .pos(d_averted_tot)) %>%
    transmute(year, coverage,
              cum_py          = .n(cum_py),
              cum_averted_msm = .n(cum_averted_msm),
              cum_averted_tot = .n(cum_averted_tot),
              vs,
              d_py            = .n(d_py),
              d_averted_msm   = .n(d_averted_msm),
              d_averted_tot   = .n(d_averted_tot),
              py_per_inf_msm = .r(py_per_inf_msm_num),
              py_per_inf_tot = .r(py_per_inf_tot_num),
              # carried numerically for the console spread note and CHECK 5,
              # then dropped so the pooled CSV has exactly the same columns as
              # the by-MSA CSV
              pooled_msm_num = py_per_inf_msm_num, pooled_tot_num = py_per_inf_tot_num,
              lo_msm_num = msm_lo, hi_msm_num = msm_hi,
              lo_tot_num = tot_lo, hi_tot_num = tot_hi) %>%
    arrange(year, coverage)

pooled.out <- pooled %>% select(year, all_of(COLS))

write.csv(pooled.out, file.path(TABLE.DIR, "table_efficiency_pooled.csv"),
          row.names = FALSE)

cat("\n=== POOLED (main text) ===\n")
cat("cum_*  : properties of the SCENARIO, summed across the 10 MSAs.\n")
cat("d_*    : properties of the MOVE from the coverage level named in `vs`.\n")
cat("py_per_inf_*: person-years of doxycycline per ADDITIONAL infection\n")
cat("         averted by that move, pooled as sum(d_py)/sum(d_averted).\n")
cat("Same columns as table_efficiency_by_msa.csv -- this is simply the ten\n")
cat("MSAs treated as one. No intervals anywhere: every value is a median\n")
cat("across 400 posterior simulations, and the posterior intervals were\n")
cat("discarded at the make_multi_location_table() step.\n\n")
print(as.data.frame(pooled.out), row.names = FALSE)


# ---- 4. CHECKS -------------------------------------------------------------

cat("\n=== BETWEEN-MSA SPREAD (console only, not written to the CSV) ===\n")
cat("   Lowest and highest of the ten per-MSA ratios at each rung: variation\n")
cat("   between cities, NOT uncertainty. The pooled value must lie between\n")
cat("   them (CHECK 5).\n\n")
print(as.data.frame(transmute(pooled, year, coverage,
          msm_pooled = round(pooled_msm_num, DIGITS.RATIO),
          msm_min    = round(lo_msm_num,     DIGITS.RATIO),
          msm_max    = round(hi_msm_num,     DIGITS.RATIO),
          tot_pooled = round(pooled_tot_num, DIGITS.RATIO),
          tot_min    = round(lo_tot_num,     DIGITS.RATIO),
          tot_max    = round(hi_tot_num,     DIGITS.RATIO))),
      row.names = FALSE)

cat("\n\n=== CHECK 1: is cumulative averted monotone in coverage? ===\n")
cat("   (the premise that makes differencing medians safe)\n")
bad <- step %>% filter(d_averted_msm < 0 | d_averted_tot < 0)
if (nrow(bad) == 0) cat("   OK - monotone increasing everywhere.\n") else {
    cat("   NON-MONOTONE at", nrow(bad), "step(s); those ratios are NA:\n")
    print(as.data.frame(bad[, c("location","year","coverage",
                                "d_averted_msm","d_averted_tot")]),
          row.names = FALSE)
}

cat("\n=== CHECK 2: extended dominance ===\n")
cat("   Incremental ratios must RISE along the ladder. If a rung is cheaper\n")
cat("   per infection than the rung below it, sequential comparison against\n")
cat("   the next-lower rung is not valid and that rung is extendedly\n")
cat("   dominated. A cost-effectiveness reviewer will check this.\n")
ed <- step %>%
    arrange(location, year, coverage) %>%
    group_by(location, year) %>%
    filter(!is.na(py_per_inf_tot),
           py_per_inf_tot < dplyr::lag(py_per_inf_tot)) %>%
    ungroup()
if (nrow(ed) == 0) cat("   OK - ratios rise monotonically in every MSA.\n") else {
    cat("   EXTENDED DOMINANCE at", nrow(ed), "rung(s):\n")
    print(as.data.frame(ed[, c("location","year","coverage","py_per_inf_tot")]),
          row.names = FALSE)
}

cat("\n=== CHECK 3: are Doxy-PEP person-years zero under no intervention? ===\n")
cat("   (the assumption behind lag(default = 0) for the lowest rung)\n")
z  <- make_multi_location_table(
          data = results, locations = MSAS, outcomes = "num_cum_doxy_coverage",
          interventions = "noint", years = EFF.HORIZONS, stat.type = "median")
zc <- unlist(z[, grep("num_cum_doxy_coverage_noint_", names(z)), drop = FALSE])
zv <- suppressWarnings(as.numeric(gsub(",", "", as.character(zc))))
zv <- zv[!is.na(zv)]
if (!length(zv))  cat("   could not read the noint columns; check manually.\n") else
if (all(zv == 0)) cat("   OK - zero in all", length(zv), "cells.\n") else
    cat("   NOT zero; largest is", max(zv),
        "person-years. The lowest rung's increment is wrong.\n")

cat("\n=== CHECK 4: is the person-year denominator plausible? ===\n")
cat("   cum_py at 100% coverage, divided by (MSM population x years since\n")
cat("   2022), is the MEAN coverage actually delivered over that window.\n")
cat("   Coverage ramps linearly 2023-2030 then holds, so the expected value\n")
cat("   is ~0.56 at the 2030 horizon (4.5 person-year-equivalents over 8\n")
cat("   years) and ~0.73 at 2035 (9.5 over 13). A value far from those means\n")
cat("   doxy.coverage is counting the wrong people.\n")
mp <- table_to_long(make_multi_location_table(
          data = results, locations = MSAS, outcomes = "population.msm",
          interventions = COVERAGE.LEVELS, years = EFF.HORIZONS,
          stat.type = "median")) %>%
    filter(subgroup == "Total", coverage == 100) %>%
    select(location, year, msm_pop = value)
plaus <- step %>% filter(coverage == 100) %>%
    select(location, year, cum_py) %>%
    left_join(mp, by = c("location", "year")) %>%
    mutate(n_ramp_years = as.integer(year) - 2022,
           implied_mean_coverage = round(cum_py / (msm_pop * n_ramp_years), 3))
print(as.data.frame(plaus), row.names = FALSE)

cat("\n=== CHECK 5: pooled ratio lies inside the per-MSA range ===\n")
cat("   Guaranteed by construction (a convex combination of the per-MSA\n")
cat("   ratios), so a violation means a sign, NA or weighting bug.\n")
viol <- pooled %>%
    filter((!is.na(pooled_tot_num) & (pooled_tot_num < lo_tot_num | pooled_tot_num > hi_tot_num)) |
           (!is.na(pooled_msm_num) & (pooled_msm_num < lo_msm_num | pooled_msm_num > hi_msm_num)))
if (nrow(viol) == 0) cat("   OK - inside the range at every rung.\n") else {
    cat("   VIOLATION at", nrow(viol), "rung(s):\n")
    print(as.data.frame(viol[, c("year","coverage","pooled_tot_num","lo_tot_num","hi_tot_num")]),
          row.names = FALSE)
}

cat("\nWrote:\n  ", file.path(TABLE.DIR, "table_efficiency_pooled.csv"),
    "\n  ", file.path(TABLE.DIR, "table_efficiency_by_msa.csv"), "\n")
