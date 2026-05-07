library(dplyr)
library(tidyr)
library(ggplot2)

cost_low    <- 32560
cost_median <- 40460
cost_high   <- 76760

# -------------------------------------------------
# Re-engagement hazard: mixture model
# F(t) = pi * (1 - exp(-lambda * t))
#   pi      = fraction of non-starters who EVER start ART
#   lambda  = annual hazard among the eventual starters
# Derived from Helleberg 2012:
#   F(1) = 0.60, F(5) = 0.86 (asymptote)
# -------------------------------------------------
pi_reengage     <- 0.86
lambda_reengage <- 1.2
horizon_years   <- 10  # how many years to follow each non-starter cohort

F_cum <- function(t) pi_reengage * (1 - exp(-lambda_reengage * t))

# Year-by-year incremental return fraction:
# incr_return[k] = F(k) - F(k-1) = fraction of original non-starter
# cohort that starts ART during year_offset k.
reengage_schedule <- tibble(
    year_offset = 1:horizon_years,
    incr_return = F_cum(year_offset) - F_cum(year_offset - 1)
)

# -------------------------------------------------
# 1. Reshape array
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
# 2. 2025 baseline care fraction
# -------------------------------------------------
baseline_2025 <- df %>%
    filter(
        year == 2025,
        intervention == "noint",
        outcome %in% c("suppression", "diagnosed.prevalence")
    ) %>%
    select(location, sim, outcome, value) %>%
    pivot_wider(names_from = outcome, values_from = value) %>%
    mutate(
        care_fraction_2025 = suppression / diagnosed.prevalence
    ) %>%
    select(location, sim, care_fraction_2025)

# -------------------------------------------------
# 3. Excess new diagnoses by sim-year, split into immediate
#    starters and non-starters
# -------------------------------------------------
new_excess <- df %>%
    filter(
        year >= 2026, year <= 2035,
        outcome == "new",
        intervention %in% c("noint", "adap.100.end.26")
    ) %>%
    select(location, sim, year, intervention, value) %>%
    pivot_wider(names_from = intervention, values_from = value) %>%
    mutate(
        excess_new = `adap.100.end.26` - noint
    ) %>%
    left_join(baseline_2025, by = c("location", "sim")) %>%
    mutate(
        immediate_starts = excess_new * care_fraction_2025,
        not_starting_now = excess_new - immediate_starts
    ) %>%
    arrange(location, sim, year)

# -------------------------------------------------
# 4. Delayed starts via mixture model.
#    Each non-starter cohort (indexed by diagnosis year) is followed
#    independently. In year `year + year_offset`, a fraction
#    `incr_return[year_offset]` of that cohort starts ART.
# -------------------------------------------------
lagged_starts <- new_excess %>%
    select(location, sim, year, not_starting_now) %>%
    crossing(reengage_schedule) %>%
    mutate(
        delayed_starts = not_starting_now * incr_return,
        year           = year + year_offset
    ) %>%
    filter(year >= 2026, year <= 2035) %>%
    group_by(location, sim, year) %>%
    summarise(
        delayed_starts = sum(delayed_starts, na.rm = TRUE),
        .groups = "drop"
    )

# -------------------------------------------------
# 5. Total starts and recurring cost burden
# -------------------------------------------------
start_paths <- new_excess %>%
    select(location, sim, year, excess_new, immediate_starts, not_starting_now) %>%
    left_join(lagged_starts, by = c("location", "sim", "year")) %>%
    mutate(
        delayed_starts = coalesce(delayed_starts, 0),
        total_starts   = immediate_starts + delayed_starts
    ) %>%
    arrange(location, sim, year)

# -------------------------------------------------
# 6. Deterministic cost gradient
# -------------------------------------------------
cost_grid <- tibble(
    cost_scenario = factor(
        c("Low cost", "Median cost", "High cost"),
        levels = c("Low cost", "Median cost", "High cost")
    ),
    annual_cost = c(cost_low, cost_median, cost_high)
)

# -------------------------------------------------
# 7. Expand across cost gradient and compute
#    recurring annual costs from accumulated burden
# -------------------------------------------------
inc_cost_grid <- start_paths %>%
    crossing(cost_grid) %>%
    arrange(location, sim, cost_scenario, year) %>%
    group_by(location, sim, cost_scenario) %>%
    mutate(
        active_excess_on_art        = cumsum(total_starts),
        annual_incremental_cost     = active_excess_on_art * annual_cost,
        cumulative_incremental_cost = cumsum(annual_incremental_cost)
    ) %>%
    ungroup()

# -------------------------------------------------
# 8. Ryan White funding by state
# -------------------------------------------------
rw_funding <- read.csv("~/code/rw_funding_by_state.csv", stringsAsFactors = FALSE) %>%
    mutate(
        location = trimws(as.character(location)),
        across(c(part_a, part_b, part_c, part_d, part_f), as.numeric),
        annual_rwhap_total = rowSums(across(c(part_a, part_b, part_c, part_d, part_f)), na.rm = TRUE),
        annual_drug_only   = part_b
    ) %>%
    select(location, annual_rwhap_total, annual_drug_only) %>%
    crossing(year = 2026:2035)

rw_funding_cum <- rw_funding %>%
    arrange(location, year) %>%
    group_by(location) %>%
    mutate(
        cumulative_rwhap_total = cumsum(annual_rwhap_total),
        cumulative_drug_only   = cumsum(annual_drug_only)
    ) %>%
    ungroup()

compare_with_rw <- inc_cost_grid %>%
    left_join(rw_funding_cum, by = c("location", "year")) %>%
    mutate(
        gap_vs_total_cumulative = cumulative_incremental_cost - cumulative_rwhap_total,
        gap_vs_drug_cumulative  = cumulative_incremental_cost - cumulative_drug_only
    )

# -------------------------------------------------
# 9. Florida cumulative plot
# -------------------------------------------------
fl_band <- compare_with_rw %>%
    filter(location == "FL") %>%
    group_by(year) %>%
    summarise(
        p05_all = quantile(cumulative_incremental_cost, 0.05, na.rm = TRUE),
        p25_all = quantile(cumulative_incremental_cost, 0.25, na.rm = TRUE),
        p75_all = quantile(cumulative_incremental_cost, 0.75, na.rm = TRUE),
        p95_all = quantile(cumulative_incremental_cost, 0.95, na.rm = TRUE),
        cumulative_rwhap_total = first(cumulative_rwhap_total),
        cumulative_drug_only   = first(cumulative_drug_only),
        .groups = "drop"
    )

fl_median_line <- compare_with_rw %>%
    filter(location == "FL", cost_scenario == "Median cost") %>%
    group_by(year) %>%
    summarise(
        p50_median_cost = median(cumulative_incremental_cost, na.rm = TRUE),
        .groups = "drop"
    )

fl_plot_df <- fl_band %>%
    left_join(fl_median_line, by = "year")

p_fl <- ggplot(fl_plot_df, aes(x = year)) +
    geom_ribbon(
        aes(ymin = p05_all / 1e6, ymax = p95_all / 1e6),
        alpha = 0.10
    ) +
    geom_ribbon(
        aes(ymin = p25_all / 1e6, ymax = p75_all / 1e6),
        alpha = 0.20
    ) +
    geom_line(
        aes(y = p50_median_cost / 1e6, color = "Cumulative Care Cost of Newly Diagnosed HIV Starting ART."),
        linewidth = 1.2
    ) +
    geom_line(
        aes(y = cumulative_drug_only / 1e6, color = "RWHAP Part-B Spending"),
        linewidth = 1.2,
        linetype = 3
    ) +
    labs(
        x = NULL,
        y = "Cumulative cost (Millions USD)",
        color = NULL,
        title = "Florida",
        subtitle = "Bands reflect transmission + cost-gradient uncertainty"
    ) +
    theme_bw()

print(p_fl)

# -------------------------------------------------
# 10. State boxplot at 2035: NET savings from cutting Part B
# -------------------------------------------------
plot_2035 <- compare_with_rw %>%
    filter(year == 2035, location != "Total") %>%
    mutate(
        location = trimws(as.character(location)),
        net_savings_partb = cumulative_incremental_cost - cumulative_drug_only
    ) %>%
    filter(tolower(location) != "total")

state_order <- plot_2035 %>%
    group_by(location) %>%
    summarise(
        med2035 = median(net_savings_partb, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    arrange(desc(med2035)) %>%
    pull(location)

p_state <- ggplot(
    plot_2035 %>%
        mutate(location = factor(location, levels = state_order)),
    aes(x = location, y = net_savings_partb / 1e6)
) +
    geom_boxplot(fill = "grey80", outlier.shape = NA) +
    scale_x_discrete(drop = TRUE) +
    labs(
        x = NULL,
        y = "Net savings from Keeping ADAP, 
        Only accounting for additional HIV infections,
        cumulative through 2035 (millions USD)"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_state)
