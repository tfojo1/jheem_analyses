# ****************************************************************************
# SHIELD / Doxy-PEP -- driver for the manuscript tables
# ****************************************************************************
#
# This file used to carry its own copies of subset_array(), get_stats(),
# make_single_location_table(), make_multi_location_table(), resolve_locations()
# and save_table_csv(). Those copies were stale. They have been removed and
# this file now sources the one live definition of each.
#
# Everything below the `if (1 == 2)` guard is example calls, not code that
# runs on source(). It used to be `if (1 == 1)`, so sourcing this file ran the
# whole analysis and wrote CSVs as a side effect.
# ****************************************************************************

library(tidyverse)
source('../jheem_analyses/commoncode/locations_of_interest.R')
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source("../jheem_analyses/applications/SHIELD/shield_calib_register.R")
# ROOT.DIR # is set by the specification

# the table and figure functions
source('../jheem_analyses/applications/SHIELD/analysis/intervention/intervention_helper_functions.R')
# output folders for a calibration: shield.output.path(), shield.table.path(), load.shield.results()
 

# ****************************************************************************
# LOAD RESULTS
# ****************************************************************************
# Four arrays, two stratification levels x two kinds:
#
#                     raw outcomes            calculated outcomes
#   total level       total_raw_results       total_calc_results
#   sex level         sex_raw_results         sex_calc_results
#
# Pass all four to `data` and ask for outcomes from either kind. Arrays at the
# same level are merged (each outcome is taken from whichever one has it);
# arrays at different levels become different rows. An outcome that exists at
# one level but not the other -- num_cum_doxy_coverage is total-only -- comes
# back as NA for the level that lacks it, and the function says so.
# ****************************************************************************

# Set the calibration once. Everything else is built from it.
CALIB.NAME  <- "calib.8.21.stage3.az"
TABLE.DIR   <- shield.table.path(CALIB.NAME, create = TRUE)

ALL.RESULTS <- load.shield.results(CALIB.NAME)

COVERAGE.LEVELS <- paste0("doxy.cov.", seq(10, 100, 10))


# ****************************************************************************
# EXAMPLES
# ****************************************************************************
if (1 == 2) {

    # ---- one city, raw and calculated outcomes side by side ---------------
    # diagnosis.total comes from the raw arrays, pct_incidence_averted from
    # the calculated ones. You no longer have to build two tables and join.
    atlanta <- make_multi_location_table(
        data          = ALL.RESULTS,
        locations     = "Atlanta",
        outcomes      = c("diagnosis.total", "pct_incidence_averted"),
        interventions = c("noint", "doxy.cov.50", "doxy.cov.100"),
        years         = c("2022", "2026", "2035"),
        stat.type     = "median.ci"
    )

    # ---- % incidence averted, ten cities, total + sex ---------------------
    make_multi_location_table(
        data          = ALL.RESULTS,
        locations     = names(SHIELD.TEN.MSAS),
        outcomes      = c("pct_cum_incidence_averted"),
        interventions = COVERAGE.LEVELS,
        years         = c("2035"),
        stat.type     = "median",
        save          = TRUE,
        save.dir      = TABLE.DIR,
        filename      = "multi.loc_pct.cum.inc.averted_2035"
    )

    # ---- totals only ------------------------------------------------------
    # Same columns as the table above, including `subgroup`, which holds
    # "Total" throughout. Tables built from different inputs now stack.
    make_multi_location_table(
        data          = ALL.RESULTS[c("total_raw", "total_calc")],
        locations     = names(SHIELD.TEN.MSAS),
        outcomes      = c("pct_cum_incidence_averted"),
        interventions = COVERAGE.LEVELS,
        years         = c("2035"),
        stat.type     = "median",
        save          = TRUE,
        save.dir      = TABLE.DIR,
        filename      = "multi.loc_pct.cum.inc.averted_2035_total"
    )

    # ---- % diagnoses averted ----------------------------------------------
    make_multi_location_table(
        data          = ALL.RESULTS,
        locations     = names(SHIELD.TEN.MSAS),
        outcomes      = c("pct_diagnosis_total_averted"),
        interventions = COVERAGE.LEVELS,
        years         = c("2035"),
        stat.type     = "median",
        save          = TRUE,
        save.dir      = TABLE.DIR,
        filename      = "multi.loc_pct.diag.averted_2035"
    )

    # ---- cumulative incidence averted -------------------------------------
    make_multi_location_table(
        data          = ALL.RESULTS,
        locations     = names(SHIELD.TEN.MSAS),
        outcomes      = c("num_cum_incidence_averted"),
        interventions = COVERAGE.LEVELS,
        years         = c("2035"),
        stat.type     = "median",
        save          = TRUE,
        save.dir      = TABLE.DIR,
        filename      = "multi.loc_cum.inc.averted_2035"
    )

    # ---- cumulative diagnoses averted -------------------------------------
    make_multi_location_table(
        data          = ALL.RESULTS,
        locations     = names(SHIELD.TEN.MSAS),
        outcomes      = c("num_cum_diagnosis_total_averted"),
        interventions = COVERAGE.LEVELS,
        years         = c("2035"),
        stat.type     = "median",
        save          = TRUE,
        save.dir      = TABLE.DIR,
        filename      = "multi.loc_cum.diag.averted_2035"
    )

    # ---- annual incidence averted -----------------------------------------
    make_multi_location_table(
        data          = ALL.RESULTS,
        locations     = names(SHIELD.TEN.MSAS),
        outcomes      = c("num_incidence_averted"),
        interventions = COVERAGE.LEVELS,
        years         = c("2035"),
        stat.type     = "median",
        save          = TRUE,
        save.dir      = TABLE.DIR,
        filename      = "multi.loc_inc.averted_2035"
    )

    # ---- doxy person-years: total level only ------------------------------
    # num_cum_doxy_coverage does not exist at the sex level, so the sex rows
    # come back NA and a note says which array each outcome came from.
    make_multi_location_table(
        data          = ALL.RESULTS,
        locations     = names(SHIELD.TEN.MSAS),
        outcomes      = c("num_cum_incidence_averted", "num_cum_doxy_coverage"),
        interventions = COVERAGE.LEVELS,
        years         = c("2035"),
        stat.type     = "median"
    )

    # ---- one stratum, interventions down the rows -------------------------
    make_multi_location_table(
        data          = ALL.RESULTS,
        locations     = "Atlanta",
        outcomes      = c("diagnosis.total", "pct_incidence_averted"),
        interventions = c("noint", "doxy.cov.50", "doxy.cov.100"),
        years         = c("2022", "2026", "2035"),
        row.vars        = "intervention",
        filter.by.strat = "msm",
        stat.type       = "median.ci"
    )
}
