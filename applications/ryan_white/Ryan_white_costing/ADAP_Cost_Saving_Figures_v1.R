library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(cowplot)

BASELINE.YEAR <- "2025"

##============================================================================##
################################### Figure 1 ####################################
##============================================================================##

# =============================================================================
# Total ADAP spending, computed just-in-time for plotting
# (not stored back into compare_with_rw / rw_funding_cum upstream)
# =============================================================================
total_drug_only <- compare_with_rw %>%
    filter(location != "Total") %>%
    distinct(location, year, cumulative_drug_only) %>%
    group_by(year) %>%
    summarise(cumulative_drug_only = sum(cumulative_drug_only, na.rm = TRUE), .groups = "drop")

# =============================================================================
# SHARED COLOR SCALE across FL and Total panels
#   Cost line color varies by geography; spending line is always black dotted
# =============================================================================
shared_colors <- c(
    "FL - Cumulative Excess\nHIV Healthcare System Cost"        = "#1f4e9c",  # blue
    "US Total - Cumulative Excess\nHIV Healthcare System Cost"  = "#d62728",  # red
    "Cumulative ADAP Spending"             = "black"
)

# =============================================================================
# PANEL A/B: Reusable cumulative cost vs ADAP line plot builder
#   Now supports an optional subtitle (e.g. "Florida", "US Total")
# =============================================================================
make_cumcost_panel <- function(data, loc, tag_label, drug_only_override = NULL,
                               cost_label, subtitle = NULL) {
    
    band_df <- data %>%
        filter(location == loc) %>%
        group_by(year) %>%
        summarise(
            p05_all = quantile(cumulative_incremental_cost, 0.05, na.rm = TRUE),
            p25_all = quantile(cumulative_incremental_cost, 0.25, na.rm = TRUE),
            p75_all = quantile(cumulative_incremental_cost, 0.75, na.rm = TRUE),
            p95_all = quantile(cumulative_incremental_cost, 0.95, na.rm = TRUE),
            .groups = "drop"
        )
    
    if (!is.null(drug_only_override)) {
        drug_only_line <- drug_only_override
    } else {
        drug_only_line <- data %>%
            filter(location == loc) %>%
            group_by(year) %>%
            summarise(cumulative_drug_only = first(cumulative_drug_only), .groups = "drop")
    }
    
    band_df <- band_df %>% left_join(drug_only_line, by = "year")
    
    median_line <- data %>%
        filter(location == loc, cost_scenario == "Median cost") %>%
        group_by(year) %>%
        summarise(
            p50_median_cost = median(cumulative_incremental_cost, na.rm = TRUE),
            .groups = "drop"
        )
    
    plot_df <- band_df %>% left_join(median_line, by = "year")
    
    ggplot(plot_df, aes(x = year)) +
        geom_ribbon(aes(ymin = p05_all / 1e9, ymax = p95_all / 1e9),
                    fill = "#696969", alpha = 0.10) +
        geom_ribbon(aes(ymin = p25_all / 1e9, ymax = p75_all / 1e9),
                    fill = "#36454F", alpha = 0.20) +
        geom_line(aes(y = p50_median_cost / 1e9, color = cost_label),
                  linewidth = 1.2) +
        geom_line(aes(y = cumulative_drug_only / 1e9, color = "Cumulative ADAP Spending"),
                  linewidth = 1.2, linetype = 3) +
        scale_color_manual(values = shared_colors, drop = FALSE) +
        scale_x_continuous(breaks = 2026:2035) +
        labs(
            x        = NULL,
            y        = "Cumulative Cost\n(Billions 2026 USD)",
            color    = NULL,
            tag      = tag_label,
            subtitle = subtitle
        ) +
        theme_bw() +
        theme(
            legend.position  = "none",
            axis.text.x      = element_text(angle = 45, hjust = 1),
            axis.text.y      = element_text(size = 8),
            panel.grid.minor = element_blank(),
            plot.tag         = element_text(size = 10, face = "bold"),
            plot.subtitle    = element_text(size = 9, hjust = 0.5)
        )
}

p_A <- make_cumcost_panel(
    compare_with_rw, "FL", "A",
    cost_label = "FL - Cumulative Excess\nHIV Healthcare System Cost",
    subtitle   = "Florida"
)

p_B <- make_cumcost_panel(
    compare_with_rw, "Total", "B",
    drug_only_override = total_drug_only,
    cost_label = "US Total - Cumulative Excess\nHIV Healthcare System Cost",
    subtitle   = "US Total"
)

## =============================================================================
# PANEL C: State-level boxplot of ratio at 2035 by Medicaid expansion,
#          plus a pinned-last "US Total" box (shares panel B's red)
# =============================================================================
non_expansion <- c("AL", "FL", "GA", "ID", "KS", "MS", "NC", "SC",
                   "SD", "TN", "TX", "WI", "WY")
# Note: FL, GA, NC, SD, WI flipped to expansion by 2025 in some scenarios —
# update this vector to match the policy year you are modeling.

medicaid_expansion <- tibble(location = unique(compare_with_rw$location)) %>%
    mutate(
        expanded        = !(location %in% non_expansion),
        expansion_label = if_else(expanded, "Medicaid expansion", "Non-expansion")
    )

abb_to_name <- setNames(state.name, state.abb)
abb_to_name <- c(abb_to_name, DC = "District of Columbia", PR = "Puerto Rico")

expansion_colors <- c(
    "Medicaid expansion" = "#2e6b75",
    "Non-expansion"      = "#a8cdd1",
    "US Total"           = "#d62728"   # same red as US Total line in panel B
)

# --- State-level ratios (unchanged; excludes any "Total" row from source data) ---
box_df <- compare_with_rw %>%
    filter(
        year == 2035,
        !location %in% c("Total", "total")
    ) %>%
    mutate(
        ratio      = (cumulative_incremental_cost - cumulative_drug_only) / cumulative_drug_only,
        is_total   = FALSE,
        state_full = dplyr::coalesce(abb_to_name[location], location)
    ) %>%
    left_join(medicaid_expansion, by = "location")

# --- US Total ratios: reuse the SAME per-sim/cost_scenario incremental cost
#     that panel B already plots (location == "Total", straight from the
#     simulation output) — only the denominator gets swapped for
#     total_drug_only, since that's the piece that didn't line up before.
us_drug_2035 <- total_drug_only %>%
    filter(year == 2035) %>%
    pull(cumulative_drug_only)

us_total_box_df <- compare_with_rw %>%
    filter(location == "Total", year == 2035) %>%
    mutate(
        ratio           = (cumulative_incremental_cost - us_drug_2035) / us_drug_2035,
        is_total        = TRUE,
        state_full      = "US Total",
        expansion_label = "US Total"
    )

box_df <- bind_rows(box_df, us_total_box_df)

# order states by median ratio, but exclude US Total from the sort — it's pinned last
state_order <- box_df %>%
    filter(!is_total) %>%
    group_by(state_full) %>%
    summarise(med_ratio = median(ratio, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(med_ratio)) %>%
    pull(state_full)

state_order <- c(state_order, "US Total")

p_C <- ggplot(
    box_df %>% mutate(state_full = factor(state_full, levels = state_order)),
    aes(x = state_full, y = ratio, fill = expansion_label)
) +
    geom_boxplot(outlier.shape = NA, alpha = 0.85) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey40", linewidth = 0.5) +
    scale_fill_manual(values = expansion_colors) +
    scale_x_discrete(drop = TRUE) +
    scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 1), 
                       breaks = c(0, 1, 2, 5, 10, 20)) +
    labs(
        x    = NULL,
        y    = "Net Cost of ADAP Elimination\nto ADAP Expenditure Ratio",
        fill = NULL,
        tag  = "C"
    ) +
    theme_bw() +
    theme(
        axis.text.x      = element_text(size = 8, angle = 45, hjust = 1),
        axis.title       = element_text(size = 9),
        axis.text.y      = element_text(size = 8),
        panel.grid.minor = element_blank(),
        legend.position  = "none",
        plot.tag         = element_text(size = 10, face = "bold")
    )

# =============================================================================
# BUILD LEGENDS MANUALLY TO GUARANTEE NO DUPLICATE ENTRIES
# =============================================================================

# --- Legend 1: cost/spending lines (3 entries, exactly, correct linetypes) ---

legend_df <- tibble(
    x = 1, y = 1,
    label = factor(
        c("FL - Cumulative Excess\nHIV Healthcare System Cost",
          "US Total - Cumulative Excess\nHIV Healthcare System Cost",
          "Cumulative ADAP Spending"),
        levels = c("FL - Cumulative Excess\nHIV Healthcare System Cost",
                   "US Total - Cumulative Excess\nHIV Healthcare System Cost",
                   "Cumulative ADAP Spending")
    ),
    lty = factor(c(1, 1, 3))  # solid, solid, dotted
)

p_legend_source <- ggplot(legend_df, aes(x, y, color = label)) +
    geom_line(aes(linetype = label), linewidth = 1.2) +
    scale_color_manual(values = shared_colors) +
    scale_linetype_manual(values = c(
        "FL - Cumulative Excess\nHIV Healthcare System Cost"       = 1,
        "US Total - Cumulative Excess\nHIV Healthcare System Cost" = 1,
        "Cumulative ADAP Spending"             = 3
    )) +
    labs(color = NULL, linetype = NULL) +
    guides(
        color = guide_legend(
            override.aes = list(
                linetype  = c(1, 1, 3),
                color     = c("#1f4e9c", "#d62728", "black"),
                linewidth = 1.2,
                fill      = NA
            )
        ),
        linetype = "none"
    ) +
    theme_void() +
    theme(
        legend.position     = "bottom",
        legend.background   = element_rect(color = "black", fill = "white", linewidth = 0.4),
        legend.key          = element_rect(fill = "white", color = NA),
        legend.margin       = margin(5, 8, 5, 8),
        legend.text         = element_text(size = 8)
    )

lines_legend <- cowplot::get_legend(p_legend_source)

# --- Legend 2: Medicaid expansion / US Total fill (from panel C, boxed to match) ---
p_C_legend_source <- p_C +
    theme(
        legend.position    = "bottom",
        legend.background  = element_rect(color = "black", fill = NA, linewidth = 0.4),
        legend.margin      = margin(5, 8, 5, 8),
        legend.text        = element_text(size = 8)
    )

fill_legend <- cowplot::get_legend(p_C_legend_source)

# --- Combine both legends side by side ---
combined_legend <- cowplot::plot_grid(
    lines_legend, fill_legend,
    nrow = 1
)

# =============================================================================
# COMBINE MAIN PANELS (no legends) + MANUAL LEGEND ROW
# =============================================================================
p_top  <- p_A | p_B
p_main <- p_top / p_C +
    plot_layout(heights = c(1, 1.3))

p_fig1 <- cowplot::plot_grid(
    p_main,
    combined_legend,
    ncol = 1,
    rel_heights = c(1, 0.12)
)

print(p_fig1)


dc_df <- box_df %>% filter(state_full == "District of Columbia")
dc_range <- range(boxplot.stats(dc_df$ratio)$stats)  # gets whisker-to-whisker range, ignoring outliers


p_C_dc_inset <- ggplot(dc_df, aes(x = state_full, y = ratio)) +
    geom_boxplot(outlier.shape = NA, fill = "#2e6b75", alpha = 0.85, width = 0.4) +
    coord_cartesian(ylim = c(-1.05,-0.8)) +
    labs(x = NULL, y = NULL) +
    theme_bw(base_size = 7) +
    theme(
        axis.text.x = element_text(size = 6),
        plot.background = element_rect(color = "black", fill = "white"),
        plot.margin = margin(4, 8, 4, 8)
    )

p_C_final <- p_C + inset_element(
    p_C_dc_inset,
    left = 0.80, bottom = 0.55, right = 0.94, top = 0.95
)


p_top  <- p_A | p_B
p_main <- p_top / p_C_final +
    plot_layout(heights = c(1, 1.3))

p_fig1 <- cowplot::plot_grid(
    p_main,
    combined_legend,
    ncol = 1,
    rel_heights = c(1, 0.12)
)

print(p_fig1)

##============================================================================##
################################### Figure 2 (updated) ##########################
##============================================================================##

# =============================================================================
# ADAP CLIENT COUNT (2025 baseline, noint) -- denominator for per-client spending
# =============================================================================
adap_clients_df <- df %>%
    filter(year == 2025, intervention == "noint", outcome == "adap.clients") %>%
    group_by(location) %>%
    summarise(avg_adap_clients = mean(value, na.rm = TRUE), .groups = "drop") %>%
    filter(location != "Total")

# =============================================================================
# RATIO METRIC + CUMULATIVE ADAP SPENDING AT 2035
# =============================================================================
savings_2035 <- compare_with_rw %>%
    filter(
        year == 2035,
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
# A. Relative ADAP Suppression (% of Suppressed PWH on ADAP) -- unchanged
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
# B. [REPLACED] 2025 Annual ADAP Spending per Client
#     Previously: viral suppression % (avg_suppression_pct)
#     ASSUMPTION: cumulative_drug_only at year==2025 == annual 2025 spending
#     (only holds if the model horizon starts in 2025 -- confirm this).
# =============================================================================
annual_spending_per_client_df <- rw_funding_cum %>%
    filter(year == 2026, !location %in% c("Total", "total")) %>%
    dplyr::select(location, annual_drug_only) %>%
    left_join(adap_clients_df, by = "location") %>%
    mutate(
        avg_annual_spending_per_client = annual_drug_only / avg_adap_clients
    ) %>%
    filter(location != "Total")

# =============================================================================
# C. AVERAGE TRANSMISSION RATE IN 2025 -- unchanged
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

trate_mat <- total.sexual.transmission[BASELINE.YEAR, , , "noint"] / (total.prevalence - total.suppression)
avg_trate <- apply(trate_mat, "location", mean, na.rm = TRUE)

trate_df <- tibble(
    location              = names(avg_trate),
    avg_transmission_rate = as.numeric(avg_trate)
) %>% filter(location != "Total")

# =============================================================================
# D. URBANICITY -- unchanged
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
# ASSEMBLE PLOT DATA
# =============================================================================
plot_df <- savings_2035 %>%
    left_join(adap_pct_df,                    by = "location") %>%
    left_join(trate_df,                       by = "location") %>%
    left_join(annual_spending_per_client_df,   by = "location") %>%  # <-- REPLACED suppression_pct_df
    left_join(urbanicity_df,                  by = "location") %>%
    left_join(medicaid_expansion,             by = "location") %>%
    filter(!location %in% c("Total", "total"))

# =============================================================================
# SPEARMAN CORRELATIONS
# =============================================================================
make_corr_label <- function(x, y, data) {
    ct <- cor.test(data[[x]], data[[y]], method = "spearman", exact = FALSE)
    sprintf("\u03c1 = %.2f", ct$estimate, ct$p.value)
}

label_adap      <- make_corr_label("avg_prop_suppressed_on_adap",         "med_ratio", plot_df)
label_trate     <- make_corr_label("avg_transmission_rate",               "med_ratio", plot_df)
label_spending  <- make_corr_label("avg_annual_spending_per_client", "med_ratio", plot_df)  # <-- REPLACED label_suppression
label_urban     <- make_corr_label("urbanicity",                         "med_ratio", plot_df)

# =============================================================================
# SHARED AESTHETICS -- unchanged
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
# PANEL FUNCTION -- added x_scale/x_suffix so dollar-valued predictors format
# correctly (previous version only handled pct_x)
# =============================================================================
make_panel <- function(data,
                       x_var,
                       x_lab,
                       corr_label,
                       corr_pos = "topleft",
                       pct_x    = FALSE,
                       x_scale  = 1,
                       x_suffix = "",
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
        labs(
            x    = x_lab,
            y    = "Net Cost of ADAP Elimination\nto ADAP Expenditure Ratio",
            fill = NULL
        ) +
        shared_theme
    
    if (pct_x)
        p <- p + scale_x_continuous(labels = scales::percent_format(accuracy = 1))
    else if (x_scale != 1 || x_suffix != "")
        p <- p + scale_x_continuous(labels = scales::label_dollar(scale = x_scale, suffix = x_suffix))
    
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
    
    p
}

# =============================================================================
# BUILD PANELS
# =============================================================================
p_adap_v2 <- make_panel(
    data       = plot_df,
    x_var      = "avg_prop_suppressed_on_adap",
    x_lab      = "Proportion of Suppressed PWH on ADAP, %",
    corr_label = label_adap,
    corr_pos   = "topright",
    pct_x      = TRUE,
    size_var   = "med_adap_spending",
    size_lab   = "Cum. ADAP spending\nthrough 2035 (USD)"
) + labs(tag = "C")

p_trate_v2 <- make_panel(
    data       = plot_df,
    x_var      = "avg_transmission_rate",
    x_lab      = "Average Transmission Rate",
    corr_label = label_trate,
    corr_pos   = "topright",
    size_var   = "med_adap_spending",
    size_lab   = "Cum. ADAP spending\nthrough 2035 (USD)"
) + labs(tag = "A")

# [REPLACED] p_suppression_v2 -> p_spending_v2
p_spending_v2 <- make_panel(
    data       = plot_df,
    x_var      = "avg_annual_spending_per_client",
    x_lab      = "ADAP Spending per Client (USD)",
    corr_label = label_spending,
    corr_pos   = "topright",
    x_scale    = 1e-3, x_suffix = "K",
    size_var   = "med_adap_spending",
    size_lab   = "Cum. ADAP spending\nthrough 2035 (USD)"
) + labs(tag = "B")

p_urban_v2 <- make_panel(
    data       = plot_df %>% filter(!is.na(urbanicity)),
    x_var      = "urbanicity",
    x_lab      = "Diagnosed HIV-weighted Urbanicity",
    corr_label = label_urban,
    corr_pos   = "topright",
    pct_x      = TRUE,
    size_var   = "med_adap_spending",
    size_lab   = "Cum. ADAP spending\nthrough 2035 (USD)"
) + labs(tag = "D")

# =============================================================================
# COMBINE AND PRINT
# =============================================================================
p_combined_f2 <- (p_trate_v2 | p_spending_v2) / (p_adap_v2 | p_urban_v2) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom", legend.box = "horizontal")

print(p_combined_f2)




