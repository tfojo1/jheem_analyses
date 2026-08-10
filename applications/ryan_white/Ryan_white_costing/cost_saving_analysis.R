library(dplyr)
library(tidyr)
library(ggplot2)

# source("applications/ryan_white/ryan_white_main.R")
# source(
#     "applications/ryan_white/Ryan_white_costing/FSS_pricing_2026_pulldown.R"
# )

# load(
#     "/Users/ryanforster/code/ADAP_input_state_costing2026_2026-07-08.Rdata"
# )

# =================================================
# General economic assumptions
# =================================================

discount_rate <- 0.03

# CMS projected annual growth
INFLATION_RATE_DRUG <- 0.054
INFLATION_RATE_CARE <- 0.056

# Costs are expressed relative to the start of 2026.
# Therefore, 2026 receives a discount factor of 1.
discount_factors <- tibble(
    year = 2026:2035,
    year_index = 1:10,
    discount_factor =
        1 / (1 + discount_rate)^(year_index - 1),
    inflation_factor_drug =
        (1 + INFLATION_RATE_DRUG)^(year_index - 1),
    inflation_factor_care =
        (1 + INFLATION_RATE_CARE)^(year_index - 1)
)

# =================================================
# CD4-stratified routine-care costs
# =================================================

cd4_strata <- tibble(
    stratum = c(
        "CD4 >500",
        "CD4 200-500",
        "CD4 <200"
    ),
    wt = c(
        0.54,
        0.37,
        0.09
    ),
    cost_on_art = c(
        1650,
        2290,
        16800
    )
)

# Weighted average annual routine-care cost
# for someone receiving ART.
cost_on_art_wtd <- sum(
    cd4_strata$wt *
        cd4_strata$cost_on_art
)

# Drug-cost sensitivity tiers
cost_drug_low <-
    art_cost_class_weighted_overall$low

cost_drug_median <-
    art_cost_class_weighted_overall$median

cost_drug_high <-
    art_cost_class_weighted_overall$high

# =================================================
# Re-engagement parameters
# =================================================
#
# Each study is represented using:
#
# F_k(t) = pi_k * (1 - exp(-lambda_k * t))
#
# The primary unadjusted function is the equally
# weighted average of Helleberg study.
# Other parameterizations exist though:
# 1. Byrd private insurance
# 2. Byrd Medicaid
#
# The ADAP disruption multiplier will subsequently
# reduce the asymptote for each state and simulation.
# The timing parameters are left unchanged.
# =================================================

# Byrd: privately insured
byrd_private_pi_return <- 0.70

byrd_private_median_return_months <- 3.2

byrd_private_lambda_return <-
    log(2) /
    (byrd_private_median_return_months / 12)

# Byrd: Medicaid insured
byrd_medicaid_pi_return <- 0.59

byrd_medicaid_median_return_months <- 3.5

byrd_medicaid_lambda_return <-
    log(2) /
    (byrd_medicaid_median_return_months / 12)

# Helleberg
hel_pi <- 0.87
hel_lambda <- 1.2

# Unadjusted average eventual-return proportion
pi_return_unadjusted <-
        hel_pi


pi_return_unadjusted



F_cum_unadjusted <- function(t) {
    
        hel_pi *
            (1 - exp(-hel_lambda * t)) 
}

horizon_years <- 10

# =================================================
# Reshape JHEEM results
# =================================================

df <- as.data.frame.table(
    total.results,
    responseName = "value"
) %>%
    rename(
        year = year,
        sim = sim,
        outcome = outcome,
        location = location,
        intervention = intervention
    ) %>%
    mutate(
        year =
            as.integer(
                as.character(year)
            ),
        
        sim =
            as.integer(
                as.character(sim)
            ),
        
        value =
            as.numeric(value),
        
        location =
            trimws(
                as.character(location)
            ),
        
        outcome =
            trimws(
                as.character(outcome)
            ),
        
        intervention =
            trimws(
                as.character(intervention)
            )
    )

# =================================================
# Medicaid expansion status
# =================================================

state_expansion <- tibble::tribble(
    ~location, ~medicaid_expansion,
    "AL", 0,
    "AR", 1,
    "AZ", 1,
    "CA", 1,
    "CO", 1,
    "FL", 0,
    "GA", 0,
    "IL", 1,
    "IN", 1,
    "KY", 1,
    "LA", 1,
    "MA", 1,
    "MD", 1,
    "MI", 1,
    "MN", 1,
    "MO", 1,
    "MS", 0,
    "NC", 1,
    "NJ", 1,
    "NV", 1,
    "NY", 1,
    "OH", 1,
    "OK", 1,
    "PA", 1,
    "SC", 0,
    "TN", 0,
    "TX", 0,
    "VA", 1,
    "WA", 1,
    "WI", 0,
    "DC", 1
)

# =================================================
# Extract sampled ADAP-loss effects
# =================================================

parameter_df <- as.data.frame.table(
    all.parameters,
    responseName = "value"
) %>%
    rename(
        parameter = parameter,
        sim = simulation,
        location = location,
        intervention = intervention
    ) %>%
    mutate(
        sim =
            as.integer(
                as.character(sim)
            ),
        
        location =
            trimws(
                as.character(location)
            ),
        
        parameter =
            as.character(parameter),
        
        intervention =
            trimws(
                as.character(intervention)
            ),
        
        value =
            as.numeric(value)
    )

adap_loss_parameters <- parameter_df %>%
    filter(
        intervention == "adap.100.end.26",
        
        parameter %in% c(
            "lose.adap.expansion.effect",
            "lose.adap.nonexpansion.effect"
        )
    ) %>%
    dplyr::select(
        location,
        sim,
        parameter,
        value
    ) %>%
    pivot_wider(
        names_from = parameter,
        values_from = value
    ) %>%
    left_join(
        state_expansion,
        by = "location"
    ) %>%
    mutate(
        fraction_adap_losing_suppression =
            if_else(
                medicaid_expansion == 1,
                lose.adap.expansion.effect,
                lose.adap.nonexpansion.effect
            ),
        
        fraction_adap_losing_suppression =
            pmin(
                pmax(
                    fraction_adap_losing_suppression,
                    0
                ),
                1
            )
    ) %>%
    dplyr::select(
        location,
        sim,
        medicaid_expansion,
        fraction_adap_losing_suppression
    )

# =================================================
# 2025 baseline care fraction and ADAP dependence
# =================================================

baseline_2025 <- df %>%
    filter(
        year == 2025,
        intervention == "noint",
        
        outcome %in% c(
            "suppression",
            "adap.suppression",
            "diagnosed.prevalence"
        )
    ) %>%
    dplyr::select(
        location,
        sim,
        outcome,
        value
    ) %>%
    pivot_wider(
        names_from = outcome,
        values_from = value
    ) %>%
    mutate(
        care_fraction_2025 =
            suppression /
            diagnosed.prevalence,
        
        adap_share_suppressed_2025 =
            adap.suppression /
            suppression
    ) %>%
    dplyr::select(
        location,
        sim,
        care_fraction_2025,
        adap_share_suppressed_2025
    )

# =================================================
# Adjusted care fraction and return multiplier
# =================================================
#
# The same multiplier is applied to:
#
# 1. immediate ART initiation; and
# 2. the eventual return-to-care asymptote.
#
# multiplier =
#
# 1 - ADAP share suppressed *
#     fraction of ADAP suppression lost
#
# For "Total", the model retains the baseline care
# fraction and assigns a multiplier of 1 because the
# aggregate does not have one sampled state-specific
# ADAP loss parameter.
# =================================================

adjusted_care_draws <- baseline_2025 %>%
    left_join(
        adap_loss_parameters,
        by = c(
            "location",
            "sim"
        )
    ) %>%
    mutate(
        adap_disruption_multiplier =
            if_else(
                location == "Total",
                1,
                1 -
                    adap_share_suppressed_2025 *
                    fraction_adap_losing_suppression
            ),
        
        adap_disruption_multiplier =
            pmin(
                pmax(
                    adap_disruption_multiplier,
                    0
                ),
                1
            ),
        
        care_fraction_post_adap =
            care_fraction_2025 *
            adap_disruption_multiplier,
        
        care_fraction_post_adap =
            pmin(
                pmax(
                    care_fraction_post_adap,
                    0
                ),
                1
            ),
        
        # State- and draw-specific asymptote
        pi_return_post_adap =
            pi_return_unadjusted *
            adap_disruption_multiplier
    )

# Check for missing adjusted values
missing_adjusted_care <- adjusted_care_draws %>%
    filter(
        is.na(care_fraction_post_adap) |
            is.na(adap_disruption_multiplier) |
            is.na(pi_return_post_adap)
    )

if (nrow(missing_adjusted_care) > 0) {
    
    warning(
        paste0(
            nrow(missing_adjusted_care),
            " location/simulation rows have missing ",
            "post-ADAP care or return parameters."
        )
    )
}

# =================================================
# State- and draw-specific re-engagement schedules
# =================================================
#
# F_post,s,i(t) =
#
# disruption multiplier_s,i *
# average[F_H(t), F_private(t), F_Medicaid(t)]
#
# Thus:
#
# asymptote_s,i =
# 0.7167 * disruption multiplier_s,i
# =================================================

reengage_schedule <- adjusted_care_draws %>%
    dplyr::select(
        location,
        sim,
        adap_disruption_multiplier,
        pi_return_post_adap
    ) %>%
    distinct() %>%
    crossing(
        year_offset = 0:horizon_years
    ) %>%
    mutate(
        F_cum_unadjusted =
            F_cum_unadjusted(
                year_offset
            ),
        
        F_cum =
            adap_disruption_multiplier *
            F_cum_unadjusted,
        
        F_cum =
            pmin(
                pmax(
                    F_cum,
                    0
                ),
                1
            )
    ) %>%
    group_by(
        location,
        sim
    ) %>%
    arrange(
        year_offset,
        .by_group = TRUE
    ) %>%
    mutate(
        incr_return =
            F_cum -
            lag(
                F_cum,
                default = 0
            ),
        
        # Proportion off ART at the beginning
        # of each follow-up interval.
        still_offart =
            1 -
            lag(
                F_cum,
                default = 0
            )
    ) %>%
    ungroup()

# Optional schedule checks
reengage_schedule_check <- reengage_schedule %>%
    group_by(
        location,
        sim
    ) %>%
    summarise(
        min_increment =
            min(
                incr_return,
                na.rm = TRUE
            ),
        
        max_F_cum =
            max(
                F_cum,
                na.rm = TRUE
            ),
        
        final_F_cum =
            F_cum[
                which.max(
                    year_offset
                )
            ],
        
        expected_asymptote =
            first(
                pi_return_post_adap
            ),
        
        .groups = "drop"
    )

if (
    any(
        reengage_schedule_check$min_increment < -1e-10,
        na.rm = TRUE
    )
) {
    
    warning(
        "Some re-engagement schedules have negative increments."
    )
}

# =================================================
# Excess new diagnoses
# =================================================

new_excess <- df %>%
    filter(
        year >= 2026,
        year <= 2035,
        outcome == "new",
        
        intervention %in% c(
            "noint",
            "adap.100.end.26"
        )
    ) %>%
    dplyr::select(
        location,
        sim,
        year,
        intervention,
        value
    ) %>%
    pivot_wider(
        names_from = intervention,
        values_from = value
    ) %>%
    mutate(
        excess_new =
            `adap.100.end.26` -
            noint
    ) %>%
    left_join(
        adjusted_care_draws %>%
            dplyr::select(
                location,
                sim,
                care_fraction_2025,
                adap_share_suppressed_2025,
                medicaid_expansion,
                fraction_adap_losing_suppression,
                adap_disruption_multiplier,
                care_fraction_post_adap,
                pi_return_post_adap
            ),
        by = c(
            "location",
            "sim"
        )
    ) %>%
    mutate(
        immediate_starts =
            excess_new *
            care_fraction_post_adap,
        
        not_starting_now =
            excess_new -
            immediate_starts
    ) %>%
    arrange(
        location,
        sim,
        year
    )

# =================================================
# Excess incidence
# =================================================

excess_incidence <- df %>%
    filter(
        year >= 2026,
        year <= 2035,
        outcome == "incidence",
        
        intervention %in% c(
            "noint",
            "adap.100.end.26"
        )
    ) %>%
    dplyr::select(
        location,
        sim,
        year,
        intervention,
        value
    ) %>%
    pivot_wider(
        names_from = intervention,
        values_from = value
    ) %>%
    mutate(
        excess_incidence =
            `adap.100.end.26` -
            noint
    ) %>%
    dplyr::select(
        location,
        sim,
        year,
        excess_incidence
    ) %>%
    arrange(
        location,
        sim,
        year
    )

new_excess <- new_excess %>%
    left_join(
        excess_incidence,
        by = c(
            "location",
            "sim",
            "year"
        )
    )

# =================================================
# Expand non-starter cohorts across follow-up years
# =================================================
#
# Do not use crossing(reengage_schedule) here because
# the schedule now varies by location and simulation.
# A keyed many-to-many join is required.
# =================================================

nonstarter_followup <- new_excess %>%
    dplyr::select(
        location,
        sim,
        index_year = year,
        not_starting_now
    ) %>%
    left_join(
        reengage_schedule,
        by = c(
            "location",
            "sim"
        ),
        relationship = "many-to-many"
    ) %>%
    mutate(
        year =
            index_year +
            year_offset,
        
        delayed_starts =
            not_starting_now *
            incr_return,
        
        offart_pyears =
            not_starting_now *
            still_offart
    ) %>%
    filter(
        year >= 2026,
        year <= 2035
    )

# Delayed starts occurring at least one year after
# the cohort's index year.
lagged_starts <- nonstarter_followup %>%
    filter(
        year_offset >= 1
    ) %>%
    group_by(
        location,
        sim,
        year
    ) %>%
    summarise(
        delayed_starts =
            sum(
                delayed_starts,
                na.rm = TRUE
            ),
        
        .groups = "drop"
    )

# Person-years among excess diagnosed people who
# have not yet initiated ART.
offart_stock <- nonstarter_followup %>%
    group_by(
        location,
        sim,
        year
    ) %>%
    summarise(
        offart_pyears =
            sum(
                offart_pyears,
                na.rm = TRUE
            ),
        
        .groups = "drop"
    )

# =================================================
# Total starts and recurring ART burden
# =================================================

start_paths <- new_excess %>%
    dplyr::select(
        location,
        sim,
        year,
        excess_new,
        excess_incidence,
        care_fraction_2025,
        care_fraction_post_adap,
        adap_disruption_multiplier,
        pi_return_post_adap,
        immediate_starts,
        not_starting_now
    ) %>%
    left_join(
        lagged_starts,
        by = c(
            "location",
            "sim",
            "year"
        )
    ) %>%
    left_join(
        offart_stock,
        by = c(
            "location",
            "sim",
            "year"
        )
    ) %>%
    mutate(
        delayed_starts =
            coalesce(
                delayed_starts,
                0
            ),
        
        offart_pyears =
            coalesce(
                offart_pyears,
                0
            ),
        
        total_starts =
            immediate_starts +
            delayed_starts
    ) %>%
    arrange(
        location,
        sim,
        year
    )

# =================================================
# Normalize routine-care costs to 2026 dollars
# =================================================

CPI_2023 <- 549.084
CPI_2026 <- 591.677

deflator_2023_to_2026 <-
    CPI_2026 /
    CPI_2023

cost_drug_low_2026 <-
    cost_drug_low

cost_drug_median_2026 <-
    cost_drug_median

cost_drug_high_2026 <-
    cost_drug_high

cost_on_art_wtd_2026 <-
    cost_on_art_wtd *
    deflator_2023_to_2026

# =================================================
# Cost scenarios
# =================================================

cost_grid <- tibble(
    cost_scenario = factor(
        c(
            "Low cost",
            "Median cost",
            "High cost"
        ),
        levels = c(
            "Low cost",
            "Median cost",
            "High cost"
        )
    ),
    
    annual_drug_cost = c(
        cost_drug_low_2026,
        cost_drug_median_2026,
        cost_drug_high_2026
    )
)

# =================================================
# Annual incremental treatment costs
# =================================================
#
# Annual on-ART cost per person:
#
# drug cost +
# CD4-weighted routine-care cost
#
# People remain in the active excess ART stock after
# initiation, so active_excess_on_art is cumulative.
# =================================================

inc_cost_grid <- start_paths %>%
    crossing(
        cost_grid
    ) %>%
    left_join(
        discount_factors,
        by = "year"
    ) %>%
    mutate(
        total_on_art_cost_pp_inflated =
            (
                annual_drug_cost *
                    inflation_factor_drug
            ) +
            (
                cost_on_art_wtd_2026 *
                    inflation_factor_care
            )
    ) %>%
    arrange(
        location,
        sim,
        cost_scenario,
        year
    ) %>%
    group_by(
        location,
        sim,
        cost_scenario
    ) %>%
    mutate(
        active_excess_on_art =
            cumsum(
                total_starts
            ),
        
        annual_on_art_cost =
            active_excess_on_art *
            total_on_art_cost_pp_inflated,
        
        annual_incremental_cost =
            annual_on_art_cost,
        
        annual_incremental_cost_disc =
            annual_incremental_cost *
            discount_factor,
        
        cumulative_incremental_cost =
            cumsum(
                annual_incremental_cost_disc
            )
    ) %>%
    ungroup()

# =================================================
# Ryan White funding
# =================================================
#
# Funding values are converted from 2025 dollars
# into 2026 dollars before projection.
# =================================================

CPI_2025 <- 580.498
CPI_2026 <- 591.677

deflator_2025_to_2026 <-
    CPI_2026 /
    CPI_2025

rw_funding <- read.csv(
    paste0(
        "../jheem_analyses/applications/",
        "ryan_white/Ryan_white_costing/",
        "rw_funding_by_state.csv"
    ),
    stringsAsFactors = FALSE
) %>%
    mutate(
        location =
            trimws(
                as.character(location)
            ),
        
        across(
            c(
                part_a,
                part_b,
                part_c,
                part_d,
                part_f,
                adap
            ),
            as.numeric
        ),
        
        across(
            c(
                part_a,
                part_b,
                part_c,
                part_d,
                part_f,
                adap
            ),
            ~ .x *
                deflator_2025_to_2026
        ),
        
        annual_rwhap_total =
            rowSums(
                across(
                    c(
                        part_a,
                        part_b,
                        part_c,
                        part_d,
                        part_f
                    )
                ),
                na.rm = TRUE
            ),
        
        annual_drug_only =
            adap
    ) %>%
    dplyr::select(
        location,
        annual_rwhap_total,
        annual_drug_only
    ) %>%
    crossing(
        year = 2026:2035
    )

rw_funding_cum <- rw_funding %>%
    left_join(
        discount_factors,
        by = "year"
    ) %>%
    arrange(
        location,
        year
    ) %>%
    group_by(
        location
    ) %>%
    mutate(
        cumulative_rwhap_total =
            cumsum(
                annual_rwhap_total *
                    discount_factor
            ),
        
        cumulative_drug_only =
            cumsum(
                annual_drug_only *
                    discount_factor
            )
    ) %>%
    ungroup()

# =================================================
# Compare incremental costs with Ryan White funding
# =================================================

compare_with_rw <- inc_cost_grid %>%
    left_join(
        rw_funding_cum,
        by = c(
            "location",
            "year"
        )
    ) %>%
    mutate(
        gap_vs_total_cumulative =
            cumulative_incremental_cost -
            cumulative_rwhap_total,
        
        gap_vs_drug_cumulative =
            cumulative_incremental_cost -
            cumulative_drug_only
    )

View(compare_with_rw)

