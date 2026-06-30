# =============================================================================
# Word Table: State-Level National Summary of ADAP Elimination Impact at 2035
#
# One row per state + a "Total (US)" row; columns:
#   State | New Cases Attributable to ADAP Elimination (med [95% UI])
#         | Person-Years on ART (med [95% UI])
#         | Cum. Cost of ART Care (med [95% UI])
#         | Cum. ADAP Spending Avoided
#         | Net Cost (med [95% UI])
#         | Net Cost / ADAP Spending ratio (med [95% UI])
#
# METHODOLOGY:
#   - Ratio formula = (Cost - ADAP) / ADAP throughout.
#   - Cost scenario is FULLY POOLED: all three cost scenarios (Low/Median/
#     High) are pooled together with all sims into one combined distribution.
#     Both the median AND the 95% UI come from this pooled distribution 
#   - "Total (US)" row assumes INDEPENDENCE across states: built via
#     bootstrap resampling (B draws), independently resampling each state's
#     pooled (scenario x sim) value and summing across the 30 states.
#
# Styling mirrors Ryan_White_Costing.docx:
#   Dark navy header (#1F4E79), white 7pt bold
#   Alternating white / light-blue (#D6E4F0) rows, 7.5pt
#   Red text (#C00000) for negative net-cost rows
#   Light grey (#AAAAAA) borders, landscape US Letter, narrow margins
#
# Assumes in scope:
#   new_excess      – cols: location, sim, year, excess_new
#   start_paths     – cols: location, sim, year, total_starts, offart_pyears
#   compare_with_rw – cols: location, sim, year, cost_scenario,
#                           cumulative_incremental_cost, cumulative_drug_only
# =============================================================================

library(officer)
library(flextable)
library(dplyr)

# ── basic formatters ─────────────────────────────────────────────────────────

fmt_dollar <- function(x) {
    if (is.na(x)) return("NA")
    abs_x <- abs(x)
    if (abs_x >= 1e9) {
        s <- sprintf("$%.2fB", abs_x / 1e9)
    } else {
        s <- sprintf("$%.0fM", abs_x / 1e6)
    }
    if (x < 0) paste0("\u2212", s) else s
}

fmt_ratio <- function(x) {
    if (is.na(x)) return("NA")
    s <- sprintf("%.2f", abs(x))
    if (x < 0) paste0("\u2212", s) else s
}

fmt_n <- function(x, comma = TRUE) {
    if (is.na(x)) return("NA")
    format(round(x), big.mark = if (comma) "," else "", scientific = FALSE, trim = TRUE)
}

# Median [lo–hi] using a chosen formatter. Renamed from `fmt_ci` to avoid
# colliding with the differently-signatured fmt_ci() used in other scripts.
fmt_ci_state <- function(med, lo, hi, fmt_fn) {
    if (is.na(med) || is.na(lo) || is.na(hi)) return("NA")
    paste0(fmt_fn(med), " [", fmt_fn(lo), "\u2013", fmt_fn(hi), "]")
}

# ── new cases & person-years (scenario-independent) ─────────────────────────
# For a single state (locs length 1): direct empirical median/quantiles.
# For the Total row (locs = all states): independent bootstrap sum.
compute_count_metrics <- function(locs, final_year, new_excess, start_paths,
                                  B = 100000, seed = 123) {
    
    if (length(locs) == 1) {
        inf <- new_excess %>%
            filter(location == locs, year <= final_year) %>%
            group_by(sim) %>%
            summarise(cum_excess = sum(excess_new, na.rm = TRUE), .groups = "drop")
        
        py <- start_paths %>%
            filter(location == locs, year <= final_year) %>%
            group_by(sim) %>%
            arrange(year, .by_group = TRUE) %>%
            mutate(active_on_art = cumsum(total_starts)) %>%
            summarise(py_on_art = sum(active_on_art, na.rm = TRUE), .groups = "drop")
        
        return(list(
            inf = tibble(
                med = median(inf$cum_excess, na.rm = TRUE),
                lo  = quantile(inf$cum_excess, 0.025, na.rm = TRUE),
                hi  = quantile(inf$cum_excess, 0.975, na.rm = TRUE)
            ),
            py = tibble(
                med = median(py$py_on_art, na.rm = TRUE),
                lo  = quantile(py$py_on_art, 0.025, na.rm = TRUE),
                hi  = quantile(py$py_on_art, 0.975, na.rm = TRUE)
            )
        ))
    }
    
    # ── Total row: independent bootstrap across all states ──────────────────
    set.seed(seed)
    
    inf_by_loc <- new_excess %>%
        filter(location %in% locs, year <= final_year) %>%
        group_by(location, sim) %>%
        summarise(cum_excess = sum(excess_new, na.rm = TRUE), .groups = "drop")
    
    py_by_loc <- start_paths %>%
        filter(location %in% locs, year <= final_year) %>%
        group_by(location, sim) %>%
        arrange(year, .by_group = TRUE) %>%
        mutate(active_on_art = cumsum(total_starts)) %>%
        summarise(py_on_art = sum(active_on_art, na.rm = TRUE), .groups = "drop")
    
    inf_totals <- numeric(B)
    py_totals  <- numeric(B)
    
    for (loc in locs) {
        inf_vals <- inf_by_loc %>% filter(location == loc) %>% pull(cum_excess)
        py_vals  <- py_by_loc  %>% filter(location == loc) %>% pull(py_on_art)
        
        if (length(inf_vals) == 0 || length(py_vals) == 0) {
            warning("No data for location: ", loc, " — skipping in bootstrap total")
            next
        }
        
        inf_totals <- inf_totals + sample(inf_vals, B, replace = TRUE)
        py_totals  <- py_totals  + sample(py_vals,  B, replace = TRUE)
    }
    
    list(
        inf = tibble(
            med = median(inf_totals, na.rm = TRUE),
            lo  = quantile(inf_totals, 0.025, na.rm = TRUE),
            hi  = quantile(inf_totals, 0.975, na.rm = TRUE)
        ),
        py = tibble(
            med = median(py_totals, na.rm = TRUE),
            lo  = quantile(py_totals, 0.025, na.rm = TRUE),
            hi  = quantile(py_totals, 0.975, na.rm = TRUE)
        )
    )
}

# ── cost / net cost / ratio metrics — FULLY POOLED across cost scenarios ────
# All three cost scenarios (Low/Median/High) are pooled with sims into one
# combined distribution. Median AND 95% UI both come from that pooled
# distribution — no scenario-specific central estimate.
# For a single state (locs length 1): pooled empirical median/quantiles.
# For the Total row (locs = all states): independent bootstrap across states,
#   each draw sampling from the pooled (scenario x sim) distribution per state.
compute_cost_metrics <- function(locs, final_year, compare_with_rw, B = 100000, seed = 123) {
    
    scenarios <- levels(compare_with_rw$cost_scenario)
    
    # ADAP avoided: deterministic, scenario-independent, summed across locs
    adap_by_loc <- compare_with_rw %>%
        filter(location %in% locs, year == final_year) %>%
        group_by(location) %>%
        summarise(adap_loc = first(cumulative_drug_only), .groups = "drop")
    
    total_adap <- sum(adap_by_loc$adap_loc, na.rm = TRUE)
    
    # ── single state: pool all scenario x sim draws directly ────────────────
    if (length(locs) == 1) {
        
        adap_val <- adap_by_loc$adap_loc[1]
        
        pooled <- compare_with_rw %>%
            filter(location == locs, year == final_year, cost_scenario %in% scenarios) %>%
            mutate(net_cost = cumulative_incremental_cost - adap_val)
        
        ratio_vals <- if (total_adap == 0 || is.na(total_adap)) {
            rep(NA_real_, nrow(pooled))
        } else {
            pooled$net_cost / total_adap
        }
        
        return(list(
            total_adap = total_adap,
            cost  = list(med = median(pooled$cumulative_incremental_cost, na.rm = TRUE),
                         lo  = quantile(pooled$cumulative_incremental_cost, 0.025, na.rm = TRUE),
                         hi  = quantile(pooled$cumulative_incremental_cost, 0.975, na.rm = TRUE)),
            net   = list(med = median(pooled$net_cost, na.rm = TRUE),
                         lo  = quantile(pooled$net_cost, 0.025, na.rm = TRUE),
                         hi  = quantile(pooled$net_cost, 0.975, na.rm = TRUE)),
            ratio = list(med = median(ratio_vals, na.rm = TRUE),
                         lo  = quantile(ratio_vals, 0.025, na.rm = TRUE),
                         hi  = quantile(ratio_vals, 0.975, na.rm = TRUE))
        ))
    }
    
    # ── Total row: independent bootstrap across states, pooling scenarios ───
    set.seed(seed)
    
    pooled_by_loc <- compare_with_rw %>%
        filter(location %in% locs, year == final_year, cost_scenario %in% scenarios) %>%
        left_join(adap_by_loc, by = "location") %>%
        mutate(net_cost = cumulative_incremental_cost - adap_loc)
    
    cost_totals <- numeric(B)
    net_totals  <- numeric(B)
    
    for (loc in locs) {
        cp_vals <- pooled_by_loc %>% filter(location == loc) %>% pull(cumulative_incremental_cost)
        np_vals <- pooled_by_loc %>% filter(location == loc) %>% pull(net_cost)
        
        if (length(cp_vals) == 0) {
            warning("No cost data for location: ", loc, " — skipping in bootstrap total")
            next
        }
        
        cost_totals <- cost_totals + sample(cp_vals, B, replace = TRUE)
        net_totals  <- net_totals  + sample(np_vals, B, replace = TRUE)
    }
    
    ratio_totals <- if (total_adap == 0 || is.na(total_adap)) {
        rep(NA_real_, B)
    } else {
        net_totals / total_adap
    }
    
    list(
        total_adap = total_adap,
        cost  = list(med = median(cost_totals, na.rm = TRUE),
                     lo  = quantile(cost_totals, 0.025, na.rm = TRUE),
                     hi  = quantile(cost_totals, 0.975, na.rm = TRUE)),
        net   = list(med = median(net_totals, na.rm = TRUE),
                     lo  = quantile(net_totals, 0.025, na.rm = TRUE),
                     hi  = quantile(net_totals, 0.975, na.rm = TRUE)),
        ratio = list(med = median(ratio_totals, na.rm = TRUE),
                     lo  = quantile(ratio_totals, 0.025, na.rm = TRUE),
                     hi  = quantile(ratio_totals, 0.975, na.rm = TRUE))
    )
}

# ── row builder used for both states and the US total ───────────────────────
build_summary_row <- function(
        locs, label,
        final_year,
        new_excess, start_paths, compare_with_rw,
        B = 10000, seed = 123
) {
    
    counts <- compute_count_metrics(locs, final_year, new_excess, start_paths, B = B, seed = seed)
    costs  <- compute_cost_metrics(locs, final_year, compare_with_rw, B = B, seed = seed)
    
    tibble(
        location      = label,
        net_med_raw   = costs$net$med,
        ratio_med_raw = ifelse(is.na(costs$ratio$med), 0, costs$ratio$med),
        State         = label,
        `New HIV Cases\n(Cum. through 2035)` = fmt_ci_state(counts$inf$med, counts$inf$lo, counts$inf$hi, fmt_n),
        `Person-Years\non ART`               = fmt_ci_state(counts$py$med,  counts$py$lo,  counts$py$hi,  fmt_n),
        `Cum. ART Care\nCost`                = fmt_ci_state(costs$cost$med,  costs$cost$lo,  costs$cost$hi,  fmt_dollar),
        `Cum. ADAP\nSpending Avoided`        = fmt_dollar(costs$total_adap),
        `Net Cost\n(Care - ADAP)`            = fmt_ci_state(costs$net$med,   costs$net$lo,   costs$net$hi,   fmt_dollar),
        `Net Cost /\nADAP Spending (ratio)`  = fmt_ci_state(costs$ratio$med, costs$ratio$lo, costs$ratio$hi, fmt_ratio)
    )
}

# ── build summary data frame ─────────────────────────────────────────────────

build_state_summary <- function(
        final_year          = 2035,
        new_excess,
        start_paths,
        compare_with_rw,
        exclude_locations   = c("Total", "total"),
        include_total_row   = TRUE,
        total_label         = "Total (US)",
        B                   = 100000,
        seed                = 123
) {
    
    locations <- compare_with_rw %>%
        filter(year == final_year, !location %in% exclude_locations) %>%
        distinct(location) %>%
        pull(location) %>%
        sort()
    
    state_rows <- purrr::map_dfr(locations, function(loc) {
        build_summary_row(
            locs = loc, label = loc,
            final_year = final_year,
            new_excess = new_excess, start_paths = start_paths,
            compare_with_rw = compare_with_rw, B = B, seed = seed
        )
    })
    
    if (include_total_row) {
        total_row <- build_summary_row(
            locs = locations, label = total_label,
            final_year = final_year,
            new_excess = new_excess, start_paths = start_paths,
            compare_with_rw = compare_with_rw, B = B, seed = seed
        )
        state_rows <- bind_rows(state_rows, total_row)
    }
    
    state_rows
}

# ── flextable renderer ────────────────────────────────────────────────────────

make_state_flextable <- function(tbl_df, total_label = "Total (US)") {
    
    total_row_idx <- which(tbl_df$State == total_label)
    has_total     <- length(total_row_idx) == 1
    
    neg_net_rows   <- which(tbl_df$net_med_raw   < 0)
    neg_ratio_rows <- which(tbl_df$ratio_med_raw < 0)
    
    display_df <- tbl_df %>%
        select(-location, -net_med_raw, -ratio_med_raw)
    
    col_names <- names(display_df)
    n_cols    <- length(col_names)
    n_rows    <- nrow(display_df)
    
    stopifnot("Width array must match column count" = n_cols == 7L)
    col_widths_dxa <- c(600, 1950, 1550, 1900, 1900, 1950, 1950)
    col_widths_in  <- col_widths_dxa / 1440
    
    grey_border  <- fp_border(color = "#aaaaaa", width = 0.5)
    thick_border <- fp_border(color = "#1f4e79", width = 1.5)
    
    ft <- flextable(display_df) %>%
        set_table_properties(layout = "fixed", width = 1) %>%
        width(j = seq_len(n_cols), width = col_widths_in) %>%
        bg(part = "header", bg = "#1f4e79") %>%
        color(part = "header", color = "#ffffff") %>%
        bold(part = "header", bold = TRUE) %>%
        fontsize(part = "header", size = 7) %>%
        font(part = "header", fontname = "Arial") %>%
        align(part = "header", align = "center") %>%
        valign(part = "header", valign = "bottom") %>%
        padding(part = "header",
                padding.top = 4, padding.bottom = 4,
                padding.left = 3, padding.right = 3) %>%
        bg(part = "body", bg = "white") %>%
        fontsize(part = "body", size = 7.5) %>%
        font(part = "body", fontname = "Arial") %>%
        align(part = "body", align = "center") %>%
        align(part = "body", j = 1, align = "left") %>%
        padding(part = "body",
                padding.top = 3, padding.bottom = 3,
                padding.left = 4, padding.right = 3) %>%
        border_outer(part = "all",   border = grey_border) %>%
        border_inner_h(part = "all", border = grey_border) %>%
        border_inner_v(part = "all", border = grey_border)
    
    # Alternating row shading, skipping the Total row
    state_idx <- setdiff(seq_len(n_rows), total_row_idx)
    row_fills <- ifelse(seq_along(state_idx) %% 2 == 1, "ffffff", "d6e4f0")
    for (k in seq_along(state_idx)) {
        ft <- bg(ft, i = state_idx[k], bg = paste0("#", row_fills[k]), part = "body")
    }
    
    # Total row styling: bold, grey fill, thick top border
    if (has_total) {
        ft <- bg(ft, i = total_row_idx, bg = "#d9d9d9", part = "body")
        ft <- bold(ft, i = total_row_idx, j = seq_len(n_cols), bold = TRUE, part = "body")
        ft <- border(ft, i = total_row_idx, border.top = thick_border, part = "body")
    }
    
    # Red text for negative net-cost values
    net_col <- which(col_names == "Net Cost\n(Care - ADAP)")
    if (length(neg_net_rows) > 0 && length(net_col) > 0) {
        ft <- color(ft, i = neg_net_rows, j = net_col, color = "#c00000", part = "body")
    }
    
    # Red text for negative ratio values
    if (length(neg_ratio_rows) > 0) {
        ft <- color(ft, i = neg_ratio_rows, j = n_cols, color = "#c00000", part = "body")
    }
    
    # State column: bold for all
    ft <- bold(ft, j = 1, bold = TRUE, part = "body")
    
    ft
}

# ── Word document writer ──────────────────────────────────────────────────────

write_state_summary_word <- function(
        final_year          = 2035,
        new_excess,
        start_paths,
        compare_with_rw,
        exclude_locations   = c("Total", "total"),
        include_total_row   = TRUE,
        total_label         = "Total (US)",
        B                   = 10000,
        seed                = 123,
        output_path         = "ADAP_state_summary_2035.docx"
) {
    
    tbl_df <- build_state_summary(
        final_year        = final_year,
        new_excess        = new_excess,
        start_paths       = start_paths,
        compare_with_rw   = compare_with_rw,
        exclude_locations = exclude_locations,
        include_total_row = include_total_row,
        total_label       = total_label,
        B                 = B,
        seed              = seed
    )
    
    ft <- make_state_flextable(tbl_df, total_label = total_label)
    
    title_style <- fp_text(bold = TRUE, font.size = 11, font.family = "Arial")
    title_para  <- fpar(
        ftext(
            sprintf("Table: State-Level Impact of ADAP Elimination, Cumulative Through %d", final_year),
            title_style
        )
    )
    
    footnote_style <- fp_text(font.size = 7, font.family = "Arial", italic = TRUE)
    footnote_para  <- fpar(
        ftext(paste0(
            "Median [95% uncertainty interval] shown for stochastic quantities. ",
            "Cum. ADAP Spending Avoided is deterministic (point estimate). ",
            "For cost, net cost, and ratio columns, both the median and 95% interval are computed by pooling all ",
            "draws across all three cost scenarios (Low/Median/High) together with all simulations \u2014 cost ",
            "scenario is treated as an additional source of uncertainty, with no scenario-specific central estimate. ",
            "Net Cost = Cumulative ART care cost \u2212 Cumulative ADAP spending avoided; ",
            "Ratio = Net Cost / Cumulative ADAP Spending Avoided; ",
            "negative values (red) indicate ADAP savings exceed downstream care costs. ",
            "\"", total_label, "\" sums each quantity across all 30 states, assuming independence across states: ",
            "the national interval is constructed via bootstrap resampling (B = ", B, " draws), independently ",
            "resampling each state's pooled (scenario x sim) value and summing across states. ",
            "Costs in ", final_year, " USD."
        ), footnote_style)
    )
    
    doc <- read_docx() %>%
        body_set_default_section(
            prop_section(
                page_size = page_size(
                    orient = "landscape",
                    width  = 11,
                    height = 8.5
                ),
                page_margins = page_mar(
                    top    = 0.5, bottom = 0.5,
                    left   = 0.5, right  = 0.5,
                    header = 0.2, footer = 0.2,
                    gutter = 0
                )
            )
        ) %>%
        body_add_fpar(title_para) %>%
        body_add_par("", style = "Normal") %>%
        body_add_flextable(ft) %>%
        body_add_par("", style = "Normal") %>%
        body_add_fpar(footnote_para)
    
    print(doc, target = output_path)
    message("Saved: ", output_path)
    invisible(output_path)
}

# =============================================================================
# USAGE
# =============================================================================
write_state_summary_word(
    final_year      = 2035,
    new_excess      = new_excess,
    start_paths     = start_paths,
    compare_with_rw = compare_with_rw,
    output_path     = "~/ADAP_state_summary_2035.docx"
)
#
# To return the data frame without writing:
#
# tbl <- build_state_summary(
#     final_year          = 2035,
#     cost_scenario_label = "Median cost",
#     new_excess          = new_excess,
#     start_paths         = start_paths,
#     compare_with_rw     = compare_with_rw
# )



# =============================================================================
# Supplemental Florida in-depth Summary Word Table: New Initiators to ART 
# after ADAP cut
#   - Dark navy header (#1F4E79), white text, 7pt bold
#   - Alternating white / light-blue (#D6E4F0) data rows, 7.5pt
#   - Red text (#C00000) for median negative savings values
#   - Light grey (#AAAAAA) cell borders throughout
#   - Landscape US Letter, narrow margins
# =============================================================================

# -------------------------------------------------
# Helpers: safe number formatting (avoids integer overflow for large $)
# -------------------------------------------------
fmt_num <- function(x, dollar = FALSE, comma = TRUE) {
    s <- format(round(x), big.mark = if (comma) "," else "",
                scientific = FALSE, trim = TRUE)
    if (dollar) {
        s_stripped <- sub("^-", "", s)
        s <- ifelse(x < 0, paste0("$\u2212", s_stripped), paste0("$", s_stripped))
    }
    s
}

fmt_ci <- function(med, lo, hi, comma = TRUE, dollar = FALSE) {
    med_s <- fmt_num(med, dollar = dollar, comma = comma)
    lo_s  <- fmt_num(lo,  dollar = dollar, comma = comma)
    hi_s  <- fmt_num(hi,  dollar = dollar, comma = comma)
    paste0(med_s, " [", lo_s, "\u2013", hi_s, "]")
}

# -------------------------------------------------
# Build the summary table from model outputs.
# Replace the block below with your actual inc_cost_grid /
# compare_with_rw objects once the model has run.
# The expected inputs are:
#   new_excess        – sim-level data with columns:
#                       location, sim, year, excess_new,
#                       immediate_starts, not_starting_now
#   start_paths       – adds delayed_starts, offart_pyears, total_starts
#   compare_with_rw   – adds cumulative cost + RW funding columns
#   rw_funding        – annual_rwhap_total by location/year (deterministic)
#
# Aggregation is done separately for each column so that the
# median and 5th/95th percentiles are taken across sims at the
# *median* cost scenario (Median cost) for cost columns.
# -------------------------------------------------

build_art_table <- function(location_filter,
                            new_excess,
                            start_paths,
                            compare_with_rw,
                            rw_funding,
                            cost_scenario_label = "Median cost") {
    
    years <- 2026:2035
    
    # --- count columns (no cost gradient, take median + 5/95 across sims) ---
    counts <- new_excess %>%
        filter(location == location_filter, year %in% years) %>%
        left_join(
            start_paths %>%
                select(location, sim, year, delayed_starts, total_starts),
            by = c("location", "sim", "year")
        ) %>%
        group_by(year) %>%
        summarise(
            exc_new_med = median(excess_new,       na.rm = TRUE),
            exc_new_lo  = quantile(excess_new,     0.05, na.rm = TRUE),
            exc_new_hi  = quantile(excess_new,     0.95, na.rm = TRUE),
            exc_dx_med  = median(excess_new,       na.rm = TRUE),   # same as new in this model
            exc_dx_lo   = quantile(excess_new,     0.05, na.rm = TRUE),
            exc_dx_hi   = quantile(excess_new,     0.95, na.rm = TRUE),
            imm_med     = median(immediate_starts, na.rm = TRUE),
            imm_lo      = quantile(immediate_starts, 0.05, na.rm = TRUE),
            imm_hi      = quantile(immediate_starts, 0.95, na.rm = TRUE),
            lag_med     = median(delayed_starts,   na.rm = TRUE),
            lag_lo      = quantile(delayed_starts, 0.05, na.rm = TRUE),
            lag_hi      = quantile(delayed_starts, 0.95, na.rm = TRUE),
            tot_med     = median(total_starts,     na.rm = TRUE),
            tot_lo      = quantile(total_starts,   0.05, na.rm = TRUE),
            tot_hi      = quantile(total_starts,   0.95, na.rm = TRUE),
            .groups = "drop"
        )
    
    # active on ART = cumulative total_starts (median scenario)
    active <- start_paths %>%
        filter(location == location_filter, year %in% years) %>%
        group_by(sim) %>%
        arrange(year) %>%
        mutate(active_on_art = cumsum(total_starts)) %>%
        ungroup() %>%
        group_by(year) %>%
        summarise(
            act_med = median(active_on_art, na.rm = TRUE),
            act_lo  = quantile(active_on_art, 0.05, na.rm = TRUE),
            act_hi  = quantile(active_on_art, 0.95, na.rm = TRUE),
            .groups = "drop"
        )
    
    # --- cost columns (median cost scenario, median + 5/95 across sims) ---
    cost_data <- compare_with_rw %>%
        filter(
            location == location_filter,
            year %in% years,
            cost_scenario == cost_scenario_label
        ) %>%
        group_by(year) %>%
        summarise(
            ann_cost_med  = median(annual_incremental_cost,     na.rm = TRUE),
            ann_cost_lo   = quantile(annual_incremental_cost, 0.05, na.rm = TRUE),
            ann_cost_hi   = quantile(annual_incremental_cost, 0.95, na.rm = TRUE),
            cum_cost_med  = median(cumulative_incremental_cost, na.rm = TRUE),
            cum_cost_lo   = quantile(cumulative_incremental_cost, 0.05, na.rm = TRUE),
            cum_cost_hi   = quantile(cumulative_incremental_cost, 0.95, na.rm = TRUE),
            ann_adap      = first(annual_drug_only),          # deterministic
            cum_adap      = first(cumulative_drug_only),      # deterministic
            sav_med       = median(cumulative_incremental_cost, na.rm = TRUE) - first(cumulative_drug_only),
            sav_lo        = quantile(cumulative_incremental_cost, 0.05, na.rm = TRUE)- first(cumulative_drug_only),
            sav_hi        = quantile(cumulative_incremental_cost, 0.95, na.rm = TRUE)- first(cumulative_drug_only),
            .groups = "drop"
        )
    
    # --- assemble display data frame ---
    tbl <- counts %>%
        left_join(active,     by = "year") %>%
        left_join(cost_data,  by = "year") %>%
        mutate(
            Year                = as.character(year),
            `Projected Excess\nNew Incident Cases`       = fmt_ci(exc_new_med, exc_new_lo, exc_new_hi),
            `Excess Newly\nDiagnosed`                    = fmt_ci(exc_dx_med,  exc_dx_lo,  exc_dx_hi),
            `Estimated to Start\nART Immediately`        = fmt_ci(imm_med,     imm_lo,     imm_hi),
            `Estimated to Start\nART After Lag`          = fmt_ci(lag_med,     lag_lo,     lag_hi),
            `Total Estimated\nto Start ART`              = fmt_ci(tot_med,     tot_lo,     tot_hi),
            `Active in ART\n(Excess Diagnoses)`          = fmt_ci(act_med,     act_lo,     act_hi),
            `Annual Cost\nof Care`                       = fmt_ci(ann_cost_med, ann_cost_lo, ann_cost_hi, dollar = TRUE),
            `Cumulative Cost\nof Care`                   = fmt_ci(cum_cost_med, cum_cost_lo, cum_cost_hi, dollar = TRUE),
            `Annual ADAP`                                = fmt_num(ann_adap,    dollar = TRUE),
            `Cumulative ADAP`                            = fmt_num(cum_adap,    dollar = TRUE),
            # Savings stored as raw numbers for conditional coloring
            sav_display = fmt_ci(sav_med, sav_lo, sav_hi, dollar = TRUE),
            sav_is_negative = sav_med > 0
        ) %>%
        select(Year,
               `Projected Excess\nNew Incident Cases`,
               `Excess Newly\nDiagnosed`,
               `Estimated to Start\nART Immediately`,
               `Estimated to Start\nART After Lag`,
               `Total Estimated\nto Start ART`,
               `Active in ART\n(Excess Diagnoses)`,
               `Annual Cost\nof Care`,
               `Cumulative Cost\nof Care`,
               `Annual ADAP`,
               `Cumulative ADAP`,
               sav_display,
               sav_is_negative)
    
    return(tbl)
}


# -------------------------------------------------
# Render flextable with exact docx styling
# -------------------------------------------------
make_flextable <- function(tbl_df) {
    
    # Rename savings for display; keep sav_is_negative as metadata
    neg_rows <- which(tbl_df$sav_is_negative)
    
    sav_col_name <- "Savings\n(Cum Cost \u2212 Cum ADAP)"
    
    display_df <- tbl_df %>%
        select(-sav_is_negative) %>%
        rename_with(~ sav_col_name, .cols = "sav_display")
    
    # Column names for flextable
    col_names <- names(display_df)
    n_cols     <- length(col_names)
    n_rows     <- nrow(display_df)
    
    # Column widths in inches (converted from DXA / 1440)
    # DXA: 855 1050 840 855 855 855 855 885 1065 765 765 1605
    col_widths_dxa <- c(855, 1050, 840, 855, 855, 855, 855, 885, 1065, 765, 765, 1605)
    col_widths_in  <- col_widths_dxa / 1440
    
    # Alternating row fills: odd = white, even = light blue
    row_fills <- ifelse(seq_len(n_rows) %% 2 == 1, "ffffff", "d6e4f0")
    
    # Grey border definition
    grey_border <- fp_border(color = "#aaaaaa", width = 0.5)
    
    ft <- flextable(display_df) %>%
        # --- overall table geometry ---
        set_table_properties(layout = "fixed", width = 1) %>%
        width(j = seq_len(n_cols), width = col_widths_in) %>%
        
        # --- header: dark navy bg, white 7pt bold, centered ---
        bg(part = "header", bg = "#1f4e79") %>%
        color(part = "header", color = "#ffffff") %>%
        bold(part = "header", bold = TRUE) %>%
        fontsize(part = "header", size = 7) %>%
        font(part = "header", fontname = "Arial") %>%
        align(part = "header", align = "center") %>%
        valign(part = "header", valign = "bottom") %>%
        padding(part = "header", padding.top = 4, padding.bottom = 4,
                padding.left = 3, padding.right = 3) %>%
        
        # --- body: alternating row shading ---
        bg(part = "body", bg = "white") %>%    # default
        fontsize(part = "body", size = 7.5) %>%
        font(part = "body", fontname = "Arial") %>%
        align(part = "body", align = "center") %>%
        align(part = "body", j = 1, align = "center") %>%
        padding(part = "body", padding.top = 3, padding.bottom = 3,
                padding.left = 3, padding.right = 3) %>%
        
        # --- borders: light grey throughout ---
        border_outer(part = "all",    border = grey_border) %>%
        border_inner_h(part = "all",  border = grey_border) %>%
        border_inner_v(part = "all",  border = grey_border)
    
    # Apply alternating row fills
    for (i in seq_len(n_rows)) {
        ft <- bg(ft, i = i, bg = paste0("#", row_fills[i]), part = "body")
    }
    
    # Red text for negative savings rows
    if (length(neg_rows) > 0) {
        ft <- color(ft,
                    i = neg_rows,
                    j = n_cols,   # last column = Savings
                    color = "#c00000",
                    part = "body")
    }
    
    # Year column: bold
    ft <- bold(ft, j = 1, bold = TRUE, part = "body")
    
    return(ft)
}


# -------------------------------------------------
# Write to Word (landscape, narrow margins)
# -------------------------------------------------
write_art_word_table <- function(location_filter,
                                 new_excess,
                                 start_paths,
                                 compare_with_rw,
                                 rw_funding,
                                 output_path = "ART_table.docx",
                                 cost_scenario_label = "Median cost") {
    
    tbl_df <- build_art_table(
        location_filter   = location_filter,
        new_excess        = new_excess,
        start_paths       = start_paths,
        compare_with_rw   = compare_with_rw,
        rw_funding        = rw_funding,
        cost_scenario_label = cost_scenario_label
    )
    
    ft <- make_flextable(tbl_df)
    
    # Table title paragraph
    title_style <- fp_text(
        bold      = TRUE,
        font.size = 11,
        font.family = "Arial"
    )
    title_para <- fpar(
        ftext(
            paste0("Table 1: New Initiators to ART after ADAP cut \u2013 ",
                   location_filter),
            title_style
        )
    )
    
    doc <- read_docx() %>%
        # Landscape US Letter, narrow margins (0.5 inch all sides)
        body_set_default_section(
            prop_section(
                page_size = page_size(
                    orient = "landscape",
                    width  = 11,
                    height = 8.5
                ),
                page_margins = page_mar(top = 0.5, bottom = 0.5, left = 0.5, right = 0.5, header = 0.2, footer = 0.2, gutter = 0)
            )
        ) %>%
        body_add_fpar(title_para) %>%
        body_add_par("", style = "Normal") %>%  # small gap
        body_add_flextable(ft)
    
    print(doc, target = output_path)
    message("Saved: ", output_path)
    invisible(output_path)
}


# =============================================================================
# USAGE
# After running the costing model, call:
#
write_art_word_table(
    location_filter   = "FL",
    new_excess        = new_excess,
    start_paths       = start_paths,
    compare_with_rw   = compare_with_rw,
    rw_funding        = rw_funding,
    output_path       = "~/ART_table_FL.docx"
)

#==============================================================================#
# Hellenberg 2011 based mixture model, supplemental figure 2.
#==============================================================================#
# Parameters
ceiling_val <- 0.86   # 0.86 ≈ pi, this is the saturation asymptote
lambda      <- 1.2    # rate (= e exponent coefficient)

tau <- seq(0, 5, length.out = 500)

# Saturating form (decay exponent). 
F_lag <- ceiling_val * (1 - exp(-lambda * tau))

df <- data.frame(tau = tau, F_lag = F_lag)

ggplot(df, aes(x = tau, y = F_lag)) +
    geom_line(color = "#1F4E79", linewidth = 1) +
    geom_hline(yintercept = ceiling_val, linetype = "dashed",
               color = "grey50") +
    annotate("text", x = max(tau), y = ceiling_val,
             label = "asymptote = 0.86", hjust = 1, vjust = -0.5,
             size = 3, color = "grey40") +
    labs(
        x = expression(tau),
        y = expression(F[lag](tau)),
        title = expression(F[lag](tau) == 0.86 ~ (1 - e^{-1.2*tau}))
    ) +
    theme_minimal(base_size = 12)
