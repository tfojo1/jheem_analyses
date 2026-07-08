library(dplyr)
library(tidyr)
library(ggplot2)


discount_rate <- 0.03  # 3% annual, per standard CEA/health economic convention
INFLATION_RATE_DRUG <- 0.054   # CMS Rx drug spending growth 2026-2032
INFLATION_RATE_CARE <- 0.056   # CMS NHE overall growth 2023-2032

# Year 2026 = year 1, so discount factor = 1/(1+r)^(t-1) or 1/(1+r)^t
# Convention: costs incurred during year t are discounted to start of 2026
discount_factors <- tibble(
    year             = 2026:2035,
    year_index       = 1:10,           # 1 = 2026
    discount_factor  = 1 / (1 + discount_rate)^(year_index - 1),
    inflation_factor_drug  = (1 + INFLATION_RATE_DRUG)^(year_index - 1),
    inflation_factor_care  = (1 + INFLATION_RATE_CARE)^(year_index - 1)  
)

# -------------------------------------------------
# CD4 stratified cost structure
# Unsuppressed population distributional weights are held constant.
# On-ART and off-ART costs are weighted averages across strata.
# Source: Neilan 2020 (distribution), Jones 2025 (costs)
# -------------------------------------------------
cd4_strata <- tibble(
    stratum       = c("CD4 >500", "CD4 200-500", "CD4 <200"),
    wt            = c(0.54, 0.37, 0.09),   # % of pop
    cost_on_art   = c(1650, 2290, 16800),  # annual routine care on ART ($/yr)
)

# Weighted average annual cost for someone ON ART
# Uses suppressed-population weights (on-ART distribution)
cost_on_art_wtd <- sum(cd4_strata$wt * cd4_strata$cost_on_art)


# ART cost tiers from the original code now refer to the drug/treatment
# component layered on top of routine care. If the intent is to preserve
# the low/median/high gradient as a sensitivity on the ART drug cost,
# keep them; otherwise replace with cost_on_art_wtd throughout.
# Here they are retained as a sensitivity gradient on top of routine care,
# so total on-ART cost = routine care (CD4-weighted) + ART drug tier.
# If your original cost_low/median/high already included routine care,
# replace `annual_cost + cost_on_art_wtd` with just `annual_cost` below
# and set cost_low/median/high to the drug-only figures.

cost_drug_low    <- 18500
cost_drug_median <- 33000
cost_drug_high   <- 47400

# -------------------------------------------------
# Re-engagement hazard: mixture model
# F(t) = pi * (1 - exp(-lambda * t))
# -------------------------------------------------
pi_reengage     <- 0.86
lambda_reengage <- 1.2
horizon_years   <- 10

F_cum <- function(t) pi_reengage * (1 - exp(-lambda_reengage * t))

reengage_schedule <- tibble(
    year_offset  = 0:horizon_years,
    F_cum        = F_cum(year_offset),
    incr_return  = F_cum - lag(F_cum, default = 0),
    still_offart = 1 - lag(F_cum, default = 0)
)

# -------------------------------------------------
# Reshape array
# -------------------------------------------------
df <- as.data.frame.table(total.results, responseName = "value") %>%
    rename(
        year         = year,
        sim          = sim,
        outcome      = outcome,
        location     = location,
        intervention = intervention
    ) %>%
    mutate(
        year         = as.integer(as.character(year)),
        sim          = as.integer(as.character(sim)),
        value        = as.numeric(value),
        location     = trimws(as.character(location)),
        outcome      = trimws(as.character(outcome)),
        intervention = trimws(as.character(intervention))
    )

# -------------------------------------------------
# 2025 baseline care fraction
# -------------------------------------------------
baseline_2025 <- df %>%
    filter(
        year == 2025,
        intervention == "noint",
        outcome %in% c("suppression", "diagnosed.prevalence")
    ) %>%
    dplyr::select(location, sim, outcome, value) %>%
    pivot_wider(names_from = outcome, values_from = value) %>%
    mutate(care_fraction_2025 = suppression / diagnosed.prevalence) %>%
    dplyr::select(location, sim, care_fraction_2025)

# -------------------------------------------------
# Excess new diagnoses split into immediate starters / non-starters
# -------------------------------------------------
new_excess <- df %>%
    filter(
        year >= 2026, year <= 2035,
        outcome == "new",
        intervention %in% c("noint", "adap.100.end.26")
    ) %>%
    dplyr::select(location, sim, year, intervention, value) %>%
    pivot_wider(names_from = intervention, values_from = value) %>%
    mutate(excess_new = `adap.100.end.26` - noint) %>%
    left_join(baseline_2025, by = c("location", "sim")) %>%
    mutate(
        immediate_starts = excess_new * care_fraction_2025,
        not_starting_now = excess_new - immediate_starts
    ) %>%
    arrange(location, sim, year)

# -------------------------------------------------
# Expand non-starter cohorts across follow-up years
# -------------------------------------------------
nonstarter_followup <- new_excess %>%
    dplyr::select(location, sim, index_year = year, not_starting_now) %>%
    crossing(reengage_schedule) %>%
    mutate(
        year           = index_year + year_offset,
        delayed_starts = not_starting_now * incr_return,
        offart_pyears  = not_starting_now * still_offart
    ) %>%
    filter(year >= 2026, year <= 2035)

lagged_starts <- nonstarter_followup %>%
    filter(year_offset >= 1) %>%
    group_by(location, sim, year) %>%
    summarise(delayed_starts = sum(delayed_starts, na.rm = TRUE), .groups = "drop")

offart_stock <- nonstarter_followup %>%
    group_by(location, sim, year) %>%
    summarise(offart_pyears = sum(offart_pyears, na.rm = TRUE), .groups = "drop")

# -------------------------------------------------
# Total starts and recurring cost burden
# -------------------------------------------------
start_paths <- new_excess %>%
    dplyr::select(location, sim, year, excess_new, immediate_starts, not_starting_now) %>%
    left_join(lagged_starts, by = c("location", "sim", "year")) %>%
    left_join(offart_stock,  by = c("location", "sim", "year")) %>%
    mutate(
        delayed_starts = coalesce(delayed_starts, 0),
        offart_pyears  = coalesce(offart_pyears, 0),
        total_starts   = immediate_starts + delayed_starts
    ) %>%
    arrange(location, sim, year)


# -------------------------------------------------
# Normalize source costs to 2026 USD
# BLS CPI Medical Care: 2023 base → 2026 USD
# -------------------------------------------------
CPI_2023 <- 549.084
CPI_2026 <- 591.677
deflator_2023_to_2026 <- CPI_2026 / CPI_2023   # 1.07757

cost_drug_low_2026    <- cost_drug_low    
cost_drug_median_2026 <- cost_drug_median 
cost_drug_high_2026   <- cost_drug_high   
cost_on_art_wtd_2026  <- cost_on_art_wtd  * deflator_2023_to_2026

# -------------------------------------------------
# Cost grid
# ART drug tiers are the sensitivity axis.
# Routine care costs (CD4-weighted) are added on top and are fixed
# across the drug-cost gradient.
# -------------------------------------------------
cost_grid <- tibble(
    cost_scenario = factor(
        c("Low cost", "Median cost", "High cost"),
        levels = c("Low cost", "Median cost", "High cost")
    ),
    annual_drug_cost = c(cost_drug_low, cost_drug_median, cost_drug_high)
)

# -------------------------------------------------
#  Expand across cost gradient and compute annual incremental costs.
#
#   On-ART annual cost per person  = drug tier + CD4-weighted routine care (on-ART)
#   
#
#   annual_incremental_cost = (cumulative people on ART) * total_on_art_cost_pp
#                           
# -------------------------------------------------
inc_cost_grid <- start_paths %>%
    crossing(cost_grid) %>%
    left_join(discount_factors, by = "year") %>%
    mutate(
        # Inflate drug costs at CMS Rx rate, routine care at CMS NHE rate
        total_on_art_cost_pp_inflated = (annual_drug_cost * inflation_factor_drug) +
            (cost_on_art_wtd   * inflation_factor_care)
    ) %>%
    arrange(location, sim, cost_scenario, year) %>%
    group_by(location, sim, cost_scenario) %>%
    mutate(
        active_excess_on_art         = cumsum(total_starts),
        annual_on_art_cost           = active_excess_on_art * total_on_art_cost_pp_inflated,
        annual_incremental_cost      = annual_on_art_cost,
        annual_incremental_cost_disc = annual_incremental_cost * discount_factor,
        cumulative_incremental_cost  = cumsum(annual_incremental_cost_disc)
    ) %>%
    ungroup()

# -------------------------------------------------
# Ryan White funding
# -------------------------------------------------

# -------------------------------------------------
# Ryan White funding — inflate 2025 USD → 2026 USD
# BLS CPI Medical Care: one-year forward adjustment
# CMS projections then take over from 2026 onward
# -------------------------------------------------
CPI_2025 <- 580.498
CPI_2026 <- 591.677
deflator_2025_to_2026 <- CPI_2026 / CPI_2025   # 1.01926

rw_funding <- read.csv("../jheem_analyses/applications/ryan_white/Ryan_white_costing/rw_funding_by_state.csv", stringsAsFactors = FALSE) %>%
    mutate(
        location = trimws(as.character(location)),
        across(c(part_a, part_b, part_c, part_d, part_f, adap), as.numeric),
        across(c(part_a, part_b, part_c, part_d, part_f, adap),
               ~ .x * deflator_2025_to_2026),
        annual_rwhap_total = rowSums(across(c(part_a, part_b, part_c, part_d, part_f)), na.rm = TRUE),
        annual_drug_only   = adap
    ) %>%
    dplyr::select(location, annual_rwhap_total, annual_drug_only) %>%
    crossing(year = 2026:2035)


rw_funding <- read.csv("../jheem_analyses/applications/ryan_white/Ryan_white_costing/rw_funding_by_state.csv", stringsAsFactors = FALSE) %>%
    mutate(
        location = trimws(as.character(location)),
        across(c(part_a, part_b, part_c, part_d, part_f, adap), as.numeric),
        annual_rwhap_total = rowSums(across(c(part_a, part_b, part_c, part_d, part_f)), na.rm = TRUE),
        annual_drug_only   = adap
    ) %>%
    dplyr::select(location, annual_rwhap_total, annual_drug_only) %>%
    crossing(year = 2026:2035)

rw_funding_cum <- rw_funding %>%
    left_join(discount_factors, by = "year") %>%
    arrange(location, year) %>%
    group_by(location) %>%
    mutate(
        cumulative_rwhap_total = cumsum(annual_rwhap_total * discount_factor),
        cumulative_drug_only   = cumsum(annual_drug_only   * discount_factor)
    ) %>%
    ungroup()

compare_with_rw <- inc_cost_grid %>%
    left_join(rw_funding_cum, by = c("location", "year")) %>%
    mutate(
        gap_vs_total_cumulative = cumulative_incremental_cost - cumulative_rwhap_total,
        gap_vs_drug_cumulative  = cumulative_incremental_cost - cumulative_drug_only
    )







