library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(patchwork)

BASELINE.YEAR <- "2025"

##============================================================================##
################################### Figure 1####################################
##============================================================================##

# =============================================================================
# PANEL A: Florida cumulative cost vs ADAP line plot
# =============================================================================

fl_band <- compare_with_rw %>%
    filter(location == "FL") %>%
    group_by(year) %>%
    summarise(
        p05_all                = quantile(cumulative_incremental_cost, 0.05, na.rm = TRUE),
        p25_all                = quantile(cumulative_incremental_cost, 0.25, na.rm = TRUE),
        p75_all                = quantile(cumulative_incremental_cost, 0.75, na.rm = TRUE),
        p95_all                = quantile(cumulative_incremental_cost, 0.95, na.rm = TRUE),
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

fl_plot_df <- fl_band %>% left_join(fl_median_line, by = "year")

p_A <- ggplot(fl_plot_df, aes(x = year)) +
    geom_ribbon(aes(ymin = p05_all / 1e9, ymax = p95_all / 1e9),
                fill = "#696969", alpha = 0.10) +
    geom_ribbon(aes(ymin = p25_all / 1e9, ymax = p75_all / 1e9),
                fill = "#36454F", alpha = 0.20) +
    geom_line(aes(y = p50_median_cost / 1e9,
                  color = "Cumulative HIV Care Cost\nFor Excess Incident Infections,\nfollowing ADAP Elimination"),
              linewidth = 1.2) +
    geom_line(aes(y = cumulative_drug_only / 1e9,
                  color = "Cumulative ADAP Spending"),
              linewidth = 1.2, linetype = 3) +
    scale_color_manual(values = c(
        "Cumulative HIV Care Cost\nFor Excess Incident Infections,\nfollowing ADAP Elimination"  = "red",
        "Cumulative ADAP Spending"  = "black"
    )) +
    scale_x_continuous(breaks = 2026:2035) +
    labs(
        x     = NULL,
        y     = "Cumulative Cost\n(Billions 2026 USD)",
        color = NULL,
        tag   = "A"
    ) +
    theme_bw() +
    theme(
        legend.position  = "bottom",
        axis.text.x      = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        plot.tag         = element_text(size = 10, face = "bold")
    )

## =============================================================================
# PANEL B: State-level boxplot of ratio at 2035 by Medicaid expansion
# =============================================================================
# -------------------------------------------------
# Medicaid expansion status (as of 2025)
# Source: KFF State Health Facts
# -------------------------------------------------
medicaid_expansion <- tibble(
    location = c(
        # Expansion states
        "CA", "NY", "FL", "TX", "IL", "PA", "OH", "MI", "GA", "NC",
        "VA", "WA", "MA", "MD", "CO", "MN", "OR", "NJ", "CT", "HI",
        "RI", "NM", "NV", "AK", "AR", "AZ", "DE", "IA", "IN", "KY",
        "LA", "ME", "MT", "ND", "NH", "NE", "OK", "SD", "UT", "VT",
        "WV", "WI", "DC",
        # Non-expansion states
        "AL", "MS", "TN", "SC", "KS", "MO", "WY", "TX", "FL", "GA",
        "NC", "ID"
    ),
    expanded = c(
        # Expansion
        rep(TRUE, 43),
        # Non-expansion
        rep(FALSE, 12)
    )
) %>%
    # Some states appear in both lists above for illustration — deduplicate
    # using the definitive assignment below instead
    distinct(location, .keep_all = TRUE)

# Definitive list — cleaner to just specify non-expansion states explicitly
non_expansion <- c("AL", "FL", "GA", "ID", "KS", "MS", "NC", "SC",
                   "SD", "TN", "TX", "WI", "WY")
# Note: FL, GA, NC, SD, WI flipped to expansion by 2025 in some scenarios —
# update this vector to match the policy year you are modeling.

medicaid_expansion <- tibble(location = unique(compare_with_rw$location)) %>%
    mutate(
        expanded       = !(location %in% non_expansion),
        expansion_label = if_else(expanded,
                                  "Medicaid expansion",
                                  "Non-expansion")
    )

# Abbreviation → full name lookup (built-in vectors + DC, PR if needed)
abb_to_name <- setNames(state.name, state.abb)
abb_to_name <- c(abb_to_name, DC = "District of Columbia", PR = "Puerto Rico")

expansion_colors <- c(
    "Medicaid expansion" = "#2e6b75",
    "Non-expansion"      = "#a8cdd1"
)

box_df <- compare_with_rw %>%
    filter(
        year == 2035,
        !location %in% c("Total", "total")
    ) %>%
    mutate(
        ratio = (cumulative_incremental_cost - cumulative_drug_only) / cumulative_drug_only,
        state_full = dplyr::coalesce(abb_to_name[location], location)
    ) %>%
    left_join(medicaid_expansion, by = "location")

state_order <- box_df %>%
    group_by(state_full) %>%
    summarise(med_ratio = median(ratio, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(med_ratio)) %>%
    pull(state_full)

p_B <- ggplot(
    box_df %>% mutate(state_full = factor(state_full, levels = state_order)),
    aes(x = state_full, y = ratio, fill = expansion_label)
) +
    geom_boxplot(outlier.shape = NA, alpha = 0.85) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey40", linewidth = 0.5) +
    scale_fill_manual(values = expansion_colors) +
    #scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_x_discrete(drop = TRUE) +
    labs(
        x    = NULL,
        y    = "Net Cost of ADAP Elimination\nto ADAP Expenditure Ratio
        ",
        fill = NULL,
        tag  = "B"
    ) +
    theme_bw() +
    theme(
        axis.text.x      = element_text(angle = 45, hjust = 1),
        axis.title       = element_text(size = 9),
        axis.text.y      = element_text(size = 8),
        panel.grid.minor = element_blank(),
        legend.position  = "top",
        plot.tag         = element_text(size = 10, face = "bold")
    )

# =============================================================================
# COMBINE WITH PATCHWORK
# =============================================================================

p_fig1 <- p_A / p_B +
    plot_layout(heights = c(1, 1.2))

print(p_fig1)

##============================================================================##
################################### Figure 2####################################
##============================================================================##

# =============================================================================
# RATIO METRIC + CUMULATIVE ADAP SPENDING PER CAPITA AT 2035
# =============================================================================
savings_2035 <- compare_with_rw %>%
    filter(
        year          == 2035,
        cost_scenario == "Median cost",
        !location %in% c("Total", "total")
    ) %>%
    group_by(location) %>%
    summarise(
        med_ratio         = median(
            (cumulative_incremental_cost - cumulative_drug_only) / cumulative_drug_only,
            na.rm = TRUE
        ),
        med_adap_spending = median(
            cumulative_drug_only,
            na.rm = TRUE
        ),
        .groups = "drop"
    )


# =============================================================================
# A. Relative ADAP Suppression (% of Suppressed PWH on ADAP)
# =============================================================================
adap_pct_df <- df %>%
    filter(
        year         == 2025,
        intervention == "noint",
        outcome      %in% c("adap.suppression", "suppression")
    ) %>%
    dplyr::select(location, sim, outcome, value) %>%
    pivot_wider(names_from = outcome, values_from = value) %>%
    mutate(prop_suppressed_on_adap = adap.suppression / suppression) %>%
    group_by(location) %>%
    summarise(
        avg_prop_suppressed_on_adap = mean(prop_suppressed_on_adap, na.rm = TRUE),
        .groups      = "drop"
    ) %>%
    filter(location != "Total")

# =============================================================================
# B. AVERAGE TRANSMISSION RATE IN 2025
# =============================================================================
total.prevalence <- apply(
    total.results[BASELINE.YEAR, , "diagnosed.prevalence", , "noint"],
    c("sim", "location"),
    sum,
    na.rm = TRUE
)

total.suppression <- apply(
    total.results[BASELINE.YEAR, , "suppression", , "noint"],
    c("sim", "location"), sum, na.rm = TRUE
)

trate_mat <- total.sexual.transmission[BASELINE.YEAR, , , "noint"] / (total.prevalence - total.suppression) # minus suppressed in denominator
avg_trate <- apply(trate_mat, "location", mean, na.rm = TRUE)

trate_df <- tibble(
    location              = names(avg_trate),
    avg_transmission_rate = as.numeric(avg_trate)
) %>% filter(location != "Total")

# =============================================================================
# C. VIRAL SUPPRESSION (% of diagnosed PLWH suppressed)
# =============================================================================
suppression_pct_df <- df %>%
    filter(
        year         == 2025,
        intervention == "noint",
        outcome      %in% c("suppression", "diagnosed.prevalence")
    ) %>%
    dplyr::select(location, sim, outcome, value) %>%
    pivot_wider(names_from = outcome, values_from = value) %>%
    mutate(suppression_pct = suppression / diagnosed.prevalence) %>%
    group_by(location) %>%
    summarise(
        avg_suppression_pct = mean(suppression_pct, na.rm = TRUE),
        .groups             = "drop"
    ) %>%
    filter(location != "Total")

# =============================================================================
# D. URBANICITY
# =============================================================================
model_states <- compare_with_rw %>%
    filter(year == 2035, !location %in% c("Total", "total")) %>%
    distinct(location) %>%
    pull(location)

state_urbanicity <- get.urbanicity.metric(
    locations = model_states,
    years     = 2021,
    outcome   = "diagnosed.prevalence"
)

urbanicity_df <- tibble(
    location   = names(state_urbanicity),
    urbanicity = as.numeric(state_urbanicity)
) %>% filter(!is.na(urbanicity))

# =============================================================================
#  ASSEMBLE PLOT DATA
# =============================================================================
plot_df <- savings_2035 %>%
    left_join(adap_pct_df,        by = "location") %>%
    left_join(trate_df,           by = "location") %>%
    left_join(suppression_pct_df, by = "location") %>%
    left_join(urbanicity_df,      by = "location") %>%
    left_join(medicaid_expansion, by = "location") %>%
    filter(!location %in% c("Total", "total"))

# =============================================================================
#  SPEARMAN CORRELATIONS
# =============================================================================
make_corr_label <- function(x, y, data) {
    ct <- cor.test(data[[x]], data[[y]], method = "spearman", exact = FALSE)
    sprintf("\u03c1 = %.2f", ct$estimate, ct$p.value)
}

label_adap        <- make_corr_label("avg_prop_suppressed_on_adap",          "med_ratio", plot_df)
label_trate       <- make_corr_label("avg_transmission_rate",  "med_ratio", plot_df)
label_suppression <- make_corr_label("avg_suppression_pct",   "med_ratio", plot_df)
label_urban       <- make_corr_label("urbanicity",          "med_ratio", plot_df)


# =============================================================================
#  SHARED AESTHETICS
# =============================================================================
expansion_colors <- c(
    "Medicaid expansion" = "#2e6b75",
    "Non-expansion"      = "#a8cdd1"
)

shared_theme <- theme_bw() +
    theme(
        legend.position  = "none",
        axis.title       = element_text(size = 9),
        axis.text        = element_text(size = 8),
        panel.grid.minor = element_blank(),
        plot.tag         = element_text(size = 10, face = "bold")
    )

# =============================================================================
# PANEL FUNCTION
#     size_var / size_lab control what the bubble size encodes
# =============================================================================
make_panel <- function(data,
                       x_var,
                       x_lab,
                       corr_label,
                       corr_pos = "topleft",
                       pct_x    = FALSE,
                       size_var = "med_new_dx",
                       size_lab = "New diagnoses\nin 2025") {
    
    x_range <- range(data[[x_var]], na.rm = TRUE)
    y_range <- range(data$med_ratio, na.rm = TRUE)
    
    ann_x     <- if (corr_pos == "topleft")
        x_range[1] + 0.02 * diff(x_range)
    else
        x_range[2] - 0.02 * diff(x_range)
    ann_hjust <- if (corr_pos == "topleft") 0 else 1
    ann_y     <- y_range[2]
    
    p <- ggplot(data, aes(
        x    = .data[[x_var]],
        y    = med_ratio,
        size = .data[[size_var]],
        fill = expansion_label
    )) +
        geom_hline(yintercept = 0, linetype = "dashed",
                   color = "grey50", linewidth = 0.4) +
        geom_point(shape = 21, color = "white", alpha = 0.88) +
        geom_text_repel(
            aes(label = location),
            size          = 2.6,
            show.legend   = FALSE,
            box.padding   = 0.3,
            point.padding = 0.2,
            max.overlaps  = 20,
            segment.color = "grey60"
        ) +
        annotate(
            "label",
            x          = ann_x,
            y          = ann_y,
            label      = corr_label,
            hjust      = ann_hjust,
            vjust      = 1,
            size       = 2.8,
            color      = "grey20",
            fill       = "white",
            label.size = 0.25
        ) +
        scale_fill_manual(values = expansion_colors, name = "Medicaid Expansion Status") +
        scale_size_continuous(
            name   = size_lab,
            range  = c(2, 10),
            labels = scales::label_dollar(scale = 1e-9, suffix = "B")
        ) +
        #scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        labs(
            x    = x_lab,
            y    = "Net Cost of ADAP Elimination\nto ADAP Expenditure Ratio",
      fill = NULL
        ) +
        shared_theme
    
    if (pct_x)
        p <- p + scale_x_continuous(labels = scales::percent_format(accuracy = 1))
    
    p <- p + 
        shared_theme +
        guides(
            fill = guide_legend(
                override.aes = list(size = 5, shape = 22),
                theme = theme(
                    legend.background = element_rect(color = "black", fill = NA, linewidth = 0.4),
                    legend.margin      = margin(5, 8, 5, 8)
                )
            ),
            size = guide_legend(
                override.aes = list(
                    fill  = "grey50",
                    color = "grey30",
                    shape = 21
                ),
                theme = theme(
                    legend.background = element_rect(color = "black", fill = NA, linewidth = 0.4),
                    legend.margin      = margin(5, 8, 5, 8)
                )
            )
        )
}

# =============================================================================
# 12. BUILD PANELS: sized by cumulative ADAP spending
# =============================================================================
p_adap_v2 <- make_panel(
    data       = plot_df,
    x_var      = "avg_prop_suppressed_on_adap",
    x_lab      = "Proportion of Suppressed PWH on ADAP, %",
    corr_label = label_adap,
    corr_pos   = "topleft",
    pct_x      = TRUE,
    size_var   = "med_adap_spending",
    size_lab = "Cum. ADAP spending\nthrough 2035 (USD)"
) + labs(tag = "C")

p_trate_v2 <- make_panel(
    data       = plot_df,
    x_var      = "avg_transmission_rate",
    x_lab      = "Average Transmission Rate",
    corr_label = label_trate,
    corr_pos   = "topleft",
    size_var   = "med_adap_spending",
    size_lab = "Cum. ADAP spending\nthrough 2035 (USD)"
) + labs(tag = "A")

p_suppression_v2 <- make_panel(
    data       = plot_df,
    x_var      = "avg_suppression_pct",
    x_lab      = "PWH who are Virally Suppressed, %",
    corr_label = label_suppression,
    corr_pos   = "topleft",
    pct_x      = TRUE,
    size_var   = "med_adap_spending",
    size_lab = "Cum. ADAP spending\nthrough 2035 (USD)"
) + labs(tag = "B")

p_urban_v2 <- make_panel(
    data       = plot_df %>% filter(!is.na(urbanicity)),
    x_var      = "urbanicity",
    x_lab      = "Diagnosed HIV-weighted Urbanicity",
    corr_label = label_urban,
    corr_pos   = "topleft",
    pct_x      = TRUE,
    size_var   = "med_adap_spending",
    size_lab = "Cum. ADAP spending\nthrough 2035 (USD)"
) + labs(tag = "D")

# =============================================================================
# 13. COMBINE AND PRINT BOTH FIGURES
# =============================================================================

p_combined_f2 <- (p_trate_v2 | p_suppression_v2 ) / (p_adap_v2  | p_urban_v2) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom", legend.box = "horizontal")

print(p_combined_f2)




