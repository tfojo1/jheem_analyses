library(officer)
library(flextable)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(scales)
# =============================================================================
# ADAP Elimination Manuscript — Consolidated Table + Text Script (FIXED)
# =============================================================================

# =============================================================================
# SECTION 0: Formatting helpers (single set, used everywhere)
# =============================================================================

fmt_dollar <- function(x) {
    abs_x <- abs(x)
    core  <- ifelse(abs_x >= 1e9, sprintf("$%.2fB", abs_x / 1e9), sprintf("$%.1fM", abs_x / 1e6))
    out   <- ifelse(x < 0, paste0("\u2212", core), core)
    out[is.na(x)] <- "NA"
    out
}

fmt_ratio <- function(x) {
    s   <- sprintf("%.3f", abs(x))
    out <- ifelse(x < 0, paste0("\u2212", s), s)
    out[is.na(x)] <- "NA"
    out
}

fmt_n <- function(x, comma = TRUE) {
    out <- format(round(x), big.mark = if (comma) "," else "", scientific = FALSE, trim = TRUE)
    out[is.na(x)] <- "NA"
    out
}

# Vectorized: works on a single med/lo/hi triple (Table S6, text generators)
# and on whole columns inside mutate() (Table S5, one call per row of years).
fmt_ci <- function(med, lo, hi, fmt_fn = fmt_n) {
    out <- paste0(fmt_fn(med), " [", fmt_fn(lo), "\u2013", fmt_fn(hi), "]")
    out[is.na(med) | is.na(lo) | is.na(hi)] <- "NA"
    out
}

fmt_dollar_ci  <- function(med, lo, hi) fmt_ci(med, lo, hi, fmt_dollar)
fmt_ratio_ci   <- function(med, lo, hi) fmt_ci(med, lo, hi, fmt_ratio)
fmt_n_ci       <- function(med, lo, hi) fmt_ci(med, lo, hi, fmt_n)

grey_border  <- fp_border(color = "#aaaaaa", width = 0.5)
thick_border <- fp_border(color = "#1f4e79", width = 1.5)


compute_count_metrics <- function(locs, final_year, new_excess, start_paths,
                                  B = 100000, seed = 123) {
    
    if (length(locs) == 1) {
        inf <- new_excess %>%
            filter(location == locs, year <= final_year) %>%
            group_by(sim) %>%
            summarise(cum_excess = sum(excess_incidence, na.rm = TRUE), .groups = "drop")
        
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
            warning("No data for location: ", loc, " \u2014 skipping in bootstrap total")
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

compute_cost_metrics <- function(locs, final_year, compare_with_rw, B = 100000, seed = 123) {
    
    scenarios <- levels(compare_with_rw$cost_scenario)
    
    adap_by_loc <- compare_with_rw %>%
        filter(location %in% locs, year == final_year) %>%
        group_by(location) %>%
        summarise(adap_loc = first(cumulative_drug_only), .groups = "drop")
    
    total_adap <- sum(adap_by_loc$adap_loc, na.rm = TRUE)
    
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
            warning("No cost data for location: ", loc, " \u2014 skipping in bootstrap total")
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

# Annual (non-cumulative) cost, pooled across scenarios, 2.5/97.5 UI.
# Used only by Table S5's "Annual Cost of Care" column — cumulative cost,
# net cost, and ratio in Table S5 come from compute_cost_metrics() directly,
# not from this.
compute_annual_cost_metrics <- function(loc, yr, compare_with_rw) {
    scenarios <- levels(compare_with_rw$cost_scenario)
    vals <- compare_with_rw %>%
        filter(location == loc, year == yr, cost_scenario %in% scenarios) %>%
        pull(annual_incremental_cost)
    
    list(
        med = median(vals, na.rm = TRUE),
        lo  = quantile(vals, 0.025, na.rm = TRUE),
        hi  = quantile(vals, 0.975, na.rm = TRUE)
    )
}

# =============================================================================
# SECTION 2: Table S6 — main manuscript / state summary table at final_year
# =============================================================================

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
        `New HIV Cases\n(Cum. through 2035)` = fmt_n_ci(counts$inf$med, counts$inf$lo, counts$inf$hi),
        `Person-Years\non ART`               = fmt_n_ci(counts$py$med,  counts$py$lo,  counts$py$hi),
        `Cum. ART Care\nCost`                = fmt_dollar_ci(costs$cost$med, costs$cost$lo, costs$cost$hi),
        `Cum. ADAP\nSpending Avoided`        = fmt_dollar(costs$total_adap),
        `Net Cost\n(Care - ADAP)`            = fmt_dollar_ci(costs$net$med, costs$net$lo, costs$net$hi),
        `NCER\n(Net Cost / ADAP Spending)`   = fmt_ratio_ci(costs$ratio$med, costs$ratio$lo, costs$ratio$hi)
    )
}

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

make_state_flextable <- function(tbl_df, total_label = "Total (US)") {
    
    total_row_idx <- which(tbl_df$State == total_label)
    has_total     <- length(total_row_idx) == 1
    
    neg_net_rows   <- which(tbl_df$net_med_raw   < 0)
    neg_ratio_rows <- which(tbl_df$ratio_med_raw < 0)
    
    display_df <- tbl_df %>%
        dplyr::select(-location, -net_med_raw, -ratio_med_raw)
    
    col_names <- names(display_df)
    n_cols    <- length(col_names)
    n_rows    <- nrow(display_df)
    
    stopifnot("Width array must match column count" = n_cols == 7L)
    col_widths_dxa <- c(600, 1950, 1550, 1900, 1900, 1950, 1950)
    col_widths_in  <- col_widths_dxa / 1440
    
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
        padding(part = "header", padding.top = 4, padding.bottom = 4,
                padding.left = 3, padding.right = 3) %>%
        bg(part = "body", bg = "white") %>%
        fontsize(part = "body", size = 7.5) %>%
        font(part = "body", fontname = "Arial") %>%
        align(part = "body", align = "center") %>%
        align(part = "body", j = 1, align = "left") %>%
        padding(part = "body", padding.top = 3, padding.bottom = 3,
                padding.left = 4, padding.right = 3) %>%
        border_outer(part = "all",   border = grey_border) %>%
        border_inner_h(part = "all", border = grey_border) %>%
        border_inner_v(part = "all", border = grey_border)
    
    state_idx <- setdiff(seq_len(n_rows), total_row_idx)
    row_fills <- ifelse(seq_along(state_idx) %% 2 == 1, "ffffff", "d6e4f0")
    for (k in seq_along(state_idx)) {
        ft <- bg(ft, i = state_idx[k], bg = paste0("#", row_fills[k]), part = "body")
    }
    
    if (has_total) {
        ft <- bg(ft, i = total_row_idx, bg = "#d9d9d9", part = "body")
        ft <- bold(ft, i = total_row_idx, j = seq_len(n_cols), bold = TRUE, part = "body")
        ft <- border(ft, i = total_row_idx, border.top = thick_border, part = "body")
    }
    
    net_col <- which(col_names == "Net Cost\n(Care - ADAP)")
    if (length(neg_net_rows) > 0 && length(net_col) > 0) {
        ft <- color(ft, i = neg_net_rows, j = net_col, color = "#c00000", part = "body")
    }
    
    if (length(neg_ratio_rows) > 0) {
        ft <- color(ft, i = neg_ratio_rows, j = n_cols, color = "#c00000", part = "body")
    }
    
    ft <- bold(ft, j = 1, bold = TRUE, part = "body")
    
    ft
}

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
            sprintf("Table S6. State-Level Impact of ADAP Elimination, Cumulative Through %d", final_year),
            title_style
        )
    )
    
    footnote_style <- fp_text(font.size = 7, font.family = "Arial", italic = TRUE)
    footnote_para  <- fpar(
        ftext(paste0(
            "Median [95% uncertainty interval] shown for stochastic quantities. ",
            "Cum. ADAP Spending Avoided is deterministic (point estimate). ",
            "For cost, net cost, and NCER columns, both the median and 95% interval are computed by pooling all ",
            "draws across all three cost scenarios (Low/Median/High) together with all simulations \u2014 cost ",
            "scenario is treated as an additional source of uncertainty, with no scenario-specific central estimate. ",
            "Net Cost = Cumulative ART care cost \u2212 Cumulative ADAP spending avoided; ",
            "NCER = Net Cost / Cumulative ADAP Spending Avoided; ",
            "negative values (red) indicate ADAP savings exceed downstream care costs. ",
            "\"", total_label, "\" sums each quantity across all 30 states, assuming independence across states: ",
            "the national interval is constructed via bootstrap resampling (B = ", B, " draws), independently ",
            "resampling each state's pooled (scenario x sim) value and summing across states. ",
            "Costs in ", final_year, " USD. ",
            "This table and Table S5 (state year-by-year detail) are generated from the same underlying ",
            "compute_cost_metrics() function, so cumulative cost, net cost, and NCER values for any given ",
            "state/year will match exactly between the two tables and the manuscript text."
        ), footnote_style)
    )
    
    doc <- read_docx() %>%
        body_set_default_section(
            prop_section(
                page_size = page_size(orient = "landscape", width = 11, height = 8.5),
                page_margins = page_mar(
                    top = 0.5, bottom = 0.5, left = 0.5, right = 0.5,
                    header = 0.2, footer = 0.2, gutter = 0
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
# SECTION 3: Table S5 — FL/DC year-by-year detail table
# =============================================================================

build_art_table <- function(location_filter,
                            new_excess,
                            start_paths,
                            compare_with_rw,
                            years = 2026:2035) {
    
    # ── counts: new infections, diagnoses, ART starts (cumulative through
    #    each year, 2.5/97.5 UI, via the shared count function) ────────────
    counts_by_year <- purrr::map_dfr(years, function(yr) {
        counts <- compute_count_metrics(location_filter, yr, new_excess, start_paths)
        tibble(
            year        = yr,
            cum_inf_med = counts$inf$med, cum_inf_lo = counts$inf$lo, cum_inf_hi = counts$inf$hi,
            cum_py_med  = counts$py$med,  cum_py_lo  = counts$py$lo,  cum_py_hi  = counts$py$hi
        )
    })
    
    # ── annual (non-cumulative) new infections / starts, pooled scenarios
    #    not applicable here (new_excess/start_paths have no cost_scenario
    #    column) — 2.5/97.5 across sims directly ─────────────────────────
    annual_counts <- new_excess %>%
        filter(location == location_filter, year %in% years) %>%
        left_join(
            start_paths %>%
                dplyr::select(location, sim, year, delayed_starts, total_starts),
            by = c("location", "sim", "year")
        ) %>%
        group_by(year) %>%
        summarise(
            exc_inc_med = median(excess_incidence, na.rm = TRUE),
            exc_inc_lo  = quantile(excess_incidence, 0.025, na.rm = TRUE),
            exc_inc_hi  = quantile(excess_incidence, 0.975, na.rm = TRUE),
            exc_new_med = median(excess_new, na.rm = TRUE),
            exc_new_lo  = quantile(excess_new, 0.025, na.rm = TRUE),
            exc_new_hi  = quantile(excess_new, 0.975, na.rm = TRUE),
            imm_med     = median(immediate_starts, na.rm = TRUE),
            imm_lo      = quantile(immediate_starts, 0.025, na.rm = TRUE),
            imm_hi      = quantile(immediate_starts, 0.975, na.rm = TRUE),
            lag_med     = median(delayed_starts, na.rm = TRUE),
            lag_lo      = quantile(delayed_starts, 0.025, na.rm = TRUE),
            lag_hi      = quantile(delayed_starts, 0.975, na.rm = TRUE),
            tot_med     = median(total_starts, na.rm = TRUE),
            tot_lo      = quantile(total_starts, 0.025, na.rm = TRUE),
            tot_hi      = quantile(total_starts, 0.975, na.rm = TRUE),
            .groups = "drop"
        )
    
    # ── cost columns: annual cost computed locally (pooled scenarios,
    #    2.5/97.5); cumulative cost, net cost, and NCER pulled directly
    #    from compute_cost_metrics() — the single source of truth ─────────
    cost_by_year <- purrr::map_dfr(years, function(yr) {
        ann  <- compute_annual_cost_metrics(location_filter, yr, compare_with_rw)
        cum  <- compute_cost_metrics(location_filter, yr, compare_with_rw)
        tibble(
            year          = yr,
            ann_cost_med  = ann$med, ann_cost_lo = ann$lo, ann_cost_hi = ann$hi,
            cum_cost_med  = cum$cost$med,  cum_cost_lo  = cum$cost$lo,  cum_cost_hi  = cum$cost$hi,
            cum_adap      = cum$total_adap,
            net_med       = cum$net$med,   net_lo       = cum$net$lo,   net_hi       = cum$net$hi,
            ratio_med     = cum$ratio$med, ratio_lo     = cum$ratio$lo, ratio_hi     = cum$ratio$hi
        )
    })
    
    tbl <- counts_by_year %>%
        left_join(annual_counts, by = "year") %>%
        left_join(cost_by_year,  by = "year") %>%
        mutate(
            Year                                    = as.character(year),
            `Excess Incident HIV Infections\n(Annual) [95% UI]` = fmt_ci(exc_inc_med, exc_inc_lo, exc_inc_hi),
            `Excess New Diagnosed Infections\n(Annual) [95% UI]`   = fmt_ci(exc_new_med, exc_new_lo, exc_new_hi),
            `Excess New Diagnosed Infections\n(Cumulative) [95% UI]` = fmt_n_ci(cum_inf_med, cum_inf_lo, cum_inf_hi),
            `Estimated to Start\nART Immediately`        = fmt_ci(imm_med, imm_lo, imm_hi),
            `Estimated to Start\nART After Lag`          = fmt_ci(lag_med, lag_lo, lag_hi),
            `Total Estimated\nto Start ART`              = fmt_ci(tot_med, tot_lo, tot_hi),
            `Person-Years on ART\n(Cumulative) [95% UI]` = fmt_n_ci(cum_py_med, cum_py_lo, cum_py_hi),
            `Annual Excess\nHIV Care Cost`               = fmt_dollar_ci(ann_cost_med, ann_cost_lo, ann_cost_hi),
            `Cumulative Excess\nHIV Care Cost`           = fmt_dollar_ci(cum_cost_med, cum_cost_lo, cum_cost_hi),
            `Cumulative ADAP\nSpending`                  = fmt_dollar(cum_adap),
            `Net Cost\n(Care - ADAP)`                    = fmt_dollar_ci(net_med, net_lo, net_hi),
            `NCER\n(Net Cost / ADAP Spending)`           = fmt_ratio_ci(ratio_med, ratio_lo, ratio_hi),
            net_is_negative   = net_med < 0,
            ratio_is_negative = ratio_med < 0
        ) %>%
        dplyr::select(Year,
               `Excess Incident HIV Infections\n(Annual) [95% UI]`,
               `Excess New Diagnosed Infections\n(Annual) [95% UI]`,
               `Excess New Diagnosed Infections\n(Cumulative) [95% UI]`,
               `Estimated to Start\nART Immediately`,
               `Estimated to Start\nART After Lag`,
               `Total Estimated\nto Start ART`,
               `Person-Years on ART\n(Cumulative) [95% UI]`,
               `Annual Excess\nHIV Care Cost`,
               `Cumulative Excess\nHIV Care Cost`,
               `Cumulative ADAP\nSpending`,
               `Net Cost\n(Care - ADAP)`,
               `NCER\n(Net Cost / ADAP Spending)`,
               net_is_negative, ratio_is_negative)
    
    tbl
}

make_art_flextable <- function(tbl_df) {
    
    neg_net_rows   <- which(tbl_df$net_is_negative)
    neg_ratio_rows <- which(tbl_df$ratio_is_negative)
    
    display_df <- tbl_df %>%
        dplyr::select(-net_is_negative, -ratio_is_negative)
    
    col_names <- names(display_df)
    n_cols    <- length(col_names)
    n_rows    <- nrow(display_df)
    
    row_fills <- ifelse(seq_len(n_rows) %% 2 == 1, "ffffff", "d6e4f0")
    
    ft <- flextable(display_df) %>%
        set_table_properties(layout = "autofit") %>%
        bg(part = "header", bg = "#1f4e79") %>%
        color(part = "header", color = "#ffffff") %>%
        bold(part = "header", bold = TRUE) %>%
        fontsize(part = "header", size = 7) %>%
        font(part = "header", fontname = "Arial") %>%
        align(part = "header", align = "center") %>%
        valign(part = "header", valign = "bottom") %>%
        padding(part = "header", padding.top = 4, padding.bottom = 4,
                padding.left = 3, padding.right = 3) %>%
        bg(part = "body", bg = "white") %>%
        fontsize(part = "body", size = 7.5) %>%
        font(part = "body", fontname = "Arial") %>%
        align(part = "body", align = "center") %>%
        padding(part = "body", padding.top = 3, padding.bottom = 3,
                padding.left = 3, padding.right = 3) %>%
        border_outer(part = "all",   border = grey_border) %>%
        border_inner_h(part = "all", border = grey_border) %>%
        border_inner_v(part = "all", border = grey_border)
    
    for (i in seq_len(n_rows)) {
        ft <- bg(ft, i = i, bg = paste0("#", row_fills[i]), part = "body")
    }
    
    net_col   <- which(col_names == "Net Cost\n(Care - ADAP)")
    ratio_col <- which(col_names == "NCER\n(Net Cost / ADAP Spending)")
    
    if (length(neg_net_rows) > 0 && length(net_col) > 0) {
        ft <- color(ft, i = neg_net_rows, j = net_col, color = "#c00000", part = "body")
    }
    if (length(neg_ratio_rows) > 0 && length(ratio_col) > 0) {
        ft <- color(ft, i = neg_ratio_rows, j = ratio_col, color = "#c00000", part = "body")
    }
    
    ft <- bold(ft, j = 1, bold = TRUE, part = "body")
    
    ft
}

write_art_word_table <- function(location_filter,
                                 new_excess,
                                 start_paths,
                                 compare_with_rw,
                                 years = 2026:2035,
                                 output_path = "ART_table.docx") {
    
    tbl_df <- build_art_table(
        location_filter = location_filter,
        new_excess      = new_excess,
        start_paths     = start_paths,
        compare_with_rw = compare_with_rw,
        years           = years
    )
    
    ft <- make_art_flextable(tbl_df)
    
    title_style <- fp_text(bold = TRUE, font.size = 11, font.family = "Arial")
    title_para <- fpar(
        ftext(paste0("Table S5. Year-by-Year Impact of ADAP Elimination \u2013 ", location_filter), title_style)
    )
    
    footnote_style <- fp_text(font.size = 7, font.family = "Arial", italic = TRUE)
    footnote_para <- fpar(
        ftext(paste0(
            "Cumulative Excess HIV Care Cost, Net Cost, and NCER are computed identically to Table S6 ",
            "(all three cost scenarios pooled, 2.5th/97.5th percentile UI) so values at 2035 in this table ",
            "match Table S6 and the manuscript text exactly for this state."
        ), footnote_style)
    )
    
    doc <- read_docx() %>%
        body_set_default_section(
            prop_section(
                page_size = page_size(orient = "landscape", width = 14, height = 8.5),
                page_margins = page_mar(top = 0.5, bottom = 0.5, left = 0.5, right = 0.5,
                                        header = 0.2, footer = 0.2, gutter = 0)
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
# SECTION 4: Figure 2 state-level association table (supplement)
# =============================================================================

abb_to_name <- setNames(state.name, state.abb)
abb_to_name <- c(abb_to_name, DC = "District of Columbia", PR = "Puerto Rico")

build_fig2_table <- function(plot_df, box_df = NULL, compare_with_rw = NULL) {
    
    fig2_df <- plot_df %>%
        mutate(state_full = dplyr::coalesce(abb_to_name[location], location))
    
    if (!("District of Columbia" %in% fig2_df$state_full) &&
        !any(plot_df$location %in% c("DC"))) {
        
        warning(
            "District of Columbia is missing from plot_df \u2014 it will be ",
            "absent from the Figure 2 supplement table unless box_df and ",
            "compare_with_rw are supplied to backfill it."
        )
        
        if (!is.null(box_df) && !is.null(compare_with_rw) &&
            "District of Columbia" %in% box_df$state_full) {
            
            # Use the SAME compute_cost_metrics() ratio, not box_df's raw
            # per-sim ratio field, so this table's NCER for DC matches
            # Table S5/S6 exactly rather than being a separately-derived
            # median.
            dc_costs <- compute_cost_metrics("DC", 2035, compare_with_rw)
            
            dc_row <- tibble(
                state_full                  = "District of Columbia",
                expansion_label              = "Medicaid expansion", # DC expanded; confirm
                med_ratio                    = dc_costs$ratio$med,
                med_adap_spending             = dc_costs$total_adap,
                avg_prop_suppressed_on_adap  = NA_real_,  # fill in from upstream state-characteristics source
                avg_transmission_rate        = NA_real_,  # fill in from upstream state-characteristics source
                avg_suppression_pct          = NA_real_,  # fill in from upstream state-characteristics source
                urbanicity                    = NA_real_   # fill in from upstream state-characteristics source
            )
            
            fig2_df <- bind_rows(fig2_df, dc_row)
        }
    }
    
    fig2_df %>%
        arrange(desc(med_ratio)) %>%
        transmute(
            `State`                        = state_full,
            `Medicaid Expansion Status`    = expansion_label,
            `NCER\n(Net Cost / ADAP Spending)` = med_ratio,
            `ADAP Spending per\nClient (2026 USD)`               = avg_annual_spending_per_client,
            `Cum. ADAP Spending\nthrough 2035 (2026 USD)`        = med_adap_spending,
            `Proportion of Suppressed\nPWH on ADAP (%)`          = avg_prop_suppressed_on_adap * 100,
            `Avg. Transmission Rate`       = avg_transmission_rate,
            `Diagnosed HIV-weighted\nUrbanicity`                 = urbanicity
        )
}

make_fig2_flextable <- function(fig2_table_df) {
    flextable(fig2_table_df) %>%
        colformat_double(j = "NCER\n(Net Cost / ADAP Spending)", digits = 3) %>%
        colformat_double(j = "ADAP Spending per\nClient (2026 USD)",
                         big.mark = ",", digits = 0, prefix = "$") %>%
        colformat_double(j = "Cum. ADAP Spending\nthrough 2035 (2026 USD)",
                         big.mark = ",", digits = 0, prefix = "$") %>%
        colformat_double(j = "Proportion of Suppressed\nPWH on ADAP (%)", digits = 1) %>%
        colformat_double(j = "Avg. Transmission Rate", digits = 3) %>%
        colformat_double(j = "Diagnosed HIV-weighted\nUrbanicity", digits = 3) %>%
        theme_zebra() %>%
        fontsize(size = 9, part = "all") %>%
        fontsize(size = 9, part = "header") %>%
        bold(part = "header") %>%
        align(align = "center", part = "header") %>%
        align(align = "center", j = 2:8, part = "body") %>%
        set_caption(
            caption = paste0(
                "State-level values underlying Figure 2: NCER at 2035 and associated predictors ",
                "(proportion of suppressed PWH on ADAP, average transmission rate, viral suppression, ",
                "and diagnosed HIV-weighted urbanicity), by Medicaid expansion status."
            )
        ) %>%
        fit_to_width(max_width = 13, unit = "in")
}

write_fig2_table_word <- function(fig2_table_df, output_path = "figure2_state_table.docx") {
    ft_fig2 <- make_fig2_flextable(fig2_table_df)
    
    doc <- read_docx() %>%
        body_add_flextable(ft_fig2) %>%
        body_end_section_landscape()
    
    print(doc, target = output_path)
    message("Saved: ", output_path)
    invisible(output_path)
}

# =============================================================================
# SECTION 5: In-text Results generators
#   Unchanged logic (already called compute_cost_metrics() correctly) —
#   kept in this file so the text, Table S5, and Table S6 are always
#   generated from the same run.
# =============================================================================

generate_adap_text <- function(
        location_filter = "FL",
        final_year       = 2035,
        base_year        = 2026,
        new_excess,
        start_paths,
        compare_with_rw,
        B    = 10000,
        seed = 123
) {
    
    counts <- compute_count_metrics(location_filter, final_year, new_excess, start_paths, B = B, seed = seed)
    cum_infections <- counts$inf
    
    metrics_final <- compute_cost_metrics(location_filter, final_year, compare_with_rw, B = B, seed = seed)
    cost_final  <- metrics_final$cost
    net_final   <- metrics_final$net
    ratio_final <- metrics_final$ratio
    
    metrics_yr1 <- compute_cost_metrics(location_filter, base_year, compare_with_rw, B = B, seed = seed)
    yr1_cost         <- metrics_yr1$cost
    yr1_adap_savings <- metrics_yr1$total_adap
    
    scenarios <- levels(compare_with_rw$cost_scenario)
    
    adap_loc <- compare_with_rw %>%
        filter(location == location_filter, year == final_year) %>%
        summarise(val = first(cumulative_drug_only)) %>%
        pull(val)
    
    crossover_by_draw <- compare_with_rw %>%
        filter(location == location_filter, year <= final_year, cost_scenario %in% scenarios) %>%
        mutate(net = cumulative_incremental_cost - adap_loc) %>%
        group_by(sim, cost_scenario) %>%
        filter(net > 0) %>%
        slice_min(year, n = 1, with_ties = FALSE) %>%
        ungroup()
    
    n_draws_total <- compare_with_rw %>%
        filter(location == location_filter, year == final_year, cost_scenario %in% scenarios) %>%
        nrow()
    
    n_draws_cross <- nrow(crossover_by_draw)
    
    never_crosses  <- n_draws_cross == 0
    always_crosses <- n_draws_cross == n_draws_total &&
        all(crossover_by_draw$year == base_year)
    
    if (!never_crosses) {
        crossover_yr_stats <- crossover_by_draw %>%
            summarise(med = median(year), lo = quantile(year, 0.025), hi = quantile(year, 0.975))
        
        yrs_stats <- crossover_by_draw %>%
            mutate(yrs = year - base_year) %>%
            summarise(med = median(yrs), lo = quantile(yrs, 0.025), hi = quantile(yrs, 0.975))
        
        crossover_year_med <- round(crossover_yr_stats$med)
    }
    
    if (never_crosses) {
        
        txt <- sprintf(
            paste0(
                "In %s, cessation of ADAP spending was projected to generate %s excess ",
                "new HIV infections between %d\u2013%d, leading to %s in cumulative ",
                "downstream ART costs over the 10 years. ",
                "Only %s of these costs were realized in %d, compared to %s in savings ",
                "from the state\u2019s ADAP budget. ",
                "Projected costs grew throughout the modeling horizon but did not surpass ",
                "ADAP savings by %d; the NCER stood at %s by %d, indicating ",
                "that ADAP savings continued to outpace cumulative downstream care costs ",
                "through the end of the projection period."
            ),
            location_filter,
            fmt_n_ci(cum_infections$med, cum_infections$lo, cum_infections$hi),
            base_year, final_year,
            fmt_dollar_ci(cost_final$med, cost_final$lo, cost_final$hi),
            fmt_dollar_ci(yr1_cost$med, yr1_cost$lo, yr1_cost$hi),
            base_year,
            fmt_dollar(yr1_adap_savings),
            final_year,
            fmt_ratio_ci(ratio_final$med, ratio_final$lo, ratio_final$hi),
            final_year
        )
        
    } else if (always_crosses) {
        
        txt <- sprintf(
            paste0(
                "In %s, cessation of ADAP spending was projected to generate %s excess ",
                "new HIV infections between %d\u2013%d, leading to %s in cumulative ",
                "downstream ART costs over the 10 years. ",
                "Cumulative downstream ART costs exceeded ADAP savings from the first year ",
                "of the projection, with %s in costs in %d alone against %s in ADAP savings. ",
                "Net downstream costs reached %s by %d, with an NCER of %s by %d ",
                "(i.e., every $1 saved from ADAP elimination is projected to generate $%.2f ",
                "in additional HIV care costs above ADAP savings by %d)."
            ),
            location_filter,
            fmt_n_ci(cum_infections$med, cum_infections$lo, cum_infections$hi),
            base_year, final_year,
            fmt_dollar_ci(cost_final$med, cost_final$lo, cost_final$hi),
            fmt_dollar_ci(yr1_cost$med, yr1_cost$lo, yr1_cost$hi),
            base_year,
            fmt_dollar(yr1_adap_savings),
            fmt_dollar_ci(net_final$med, net_final$lo, net_final$hi),
            final_year,
            fmt_ratio_ci(ratio_final$med, ratio_final$lo, ratio_final$hi),
            final_year,
            ratio_final$med,
            final_year
        )
        
    } else {
        
        txt <- sprintf(
            paste0(
                "In %s, cessation of ADAP spending was projected to generate %s excess ",
                "new HIV infections between %d\u2013%d, leading to %s in costs over the ",
                "10 years. ",
                "Only %s of these costs were realized in %d, compared to %s in savings ",
                "from the state\u2019s ADAP budget. ",
                "However, the projected costs grew faster than savings, surpassing them ",
                "by %d, and reaching an NCER of %s by %d ",
                "(i.e., every $1 saved from ADAP elimination is projected to generate $%.2f ",
                "in additional HIV care costs above ADAP savings by %d) (Figure 1A)."
            ),
            location_filter,
            fmt_n_ci(cum_infections$med, cum_infections$lo, cum_infections$hi),
            base_year, final_year,
            fmt_dollar_ci(cost_final$med, cost_final$lo, cost_final$hi),
            fmt_dollar_ci(yr1_cost$med, yr1_cost$lo, yr1_cost$hi),
            base_year,
            fmt_dollar(yr1_adap_savings),
            crossover_year_med,
            fmt_ratio_ci(ratio_final$med, ratio_final$lo, ratio_final$hi),
            final_year,
            ratio_final$med,
            final_year
        )
    }
    
    cat(txt, "\n")
    
    invisible(list(
        text             = txt,
        location         = location_filter,
        cum_infections   = cum_infections,
        cost_final       = cost_final,
        net_final        = net_final,
        ratio_final      = ratio_final,
        adap_final       = metrics_final$total_adap,
        yr1_cost         = yr1_cost,
        yr1_adap_savings = yr1_adap_savings,
        crossover_yr_med = if (!never_crosses) crossover_year_med  else NA_real_,
        crossover_yr_lo  = if (!never_crosses) round(yrs_stats$lo) else NA_real_,
        crossover_yr_hi  = if (!never_crosses) round(yrs_stats$hi) else NA_real_,
        never_crosses    = never_crosses,
        always_crosses   = always_crosses
    ))
}

generate_adap_aggregate_text <- function(
        final_year        = 2035,
        base_year         = 2026,
        new_excess,
        start_paths,
        compare_with_rw,
        exclude_locations = c("Total", "total"),
        B    = 10000,
        seed = 123
) {
    
    locations <- compare_with_rw %>%
        filter(year == final_year, !location %in% exclude_locations) %>%
        distinct(location) %>%
        pull(location) %>%
        sort()
    
    metrics_total <- compute_cost_metrics(locations, final_year, compare_with_rw, B = B, seed = seed)
    
    total_adap_spending <- metrics_total$total_adap
    total_costs         <- metrics_total$cost
    aggregate_ratio     <- metrics_total$ratio
    
    scenarios <- levels(compare_with_rw$cost_scenario)
    
    adap_by_loc <- compare_with_rw %>%
        filter(location %in% locations, year == final_year) %>%
        group_by(location) %>%
        summarise(adap_loc = first(cumulative_drug_only), .groups = "drop")
    
    pooled_crossover <- compare_with_rw %>%
        filter(location %in% locations, year <= final_year, cost_scenario %in% scenarios) %>%
        left_join(adap_by_loc, by = "location") %>%
        mutate(net = cumulative_incremental_cost - adap_loc) %>%
        group_by(location, sim, cost_scenario) %>%
        filter(net > 0) %>%
        slice_min(year, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        mutate(yrs_to_crossover = year - base_year) %>%
        summarise(
            med = median(yrs_to_crossover),
            lo  = quantile(yrs_to_crossover, 0.025),
            hi  = quantile(yrs_to_crossover, 0.975)
        )
    
    txt_aggregate <- sprintf(
        paste0(
            "Across the %d states modeled, ADAP elimination was projected to generate ",
            "a cumulative program spending offset of %s over 10 years; however, this was, ",
            "on average, more than offset by projected %s in downstream HIV care costs ",
            "attributable to incident HIV infections, yielding a national NCER of %s by %d ",
            "(assuming independent uncertainty across states)."
        ),
        length(locations),
        fmt_dollar(total_adap_spending),
        fmt_dollar_ci(total_costs$med, total_costs$lo, total_costs$hi),
        fmt_ratio_ci(aggregate_ratio$med, aggregate_ratio$lo, aggregate_ratio$hi),
        final_year
    )
    
    txt_crossover <- sprintf(
        paste0(
            "These findings suggest that the short-term fiscal savings of ADAP elimination ",
            "are transient and will be outweighed within %.0f [%.0f\u2013%.0f] years by the HIV ",
            "care costs associated with incident HIV infections in every modeled state."
        ),
        pooled_crossover$med, pooled_crossover$lo, pooled_crossover$hi
    )
    
    cat(txt_aggregate, "\n\n")
    cat(txt_crossover, "\n")
    
    invisible(list(
        text_aggregate      = txt_aggregate,
        text_crossover      = txt_crossover,
        total_adap_spending = total_adap_spending,
        total_costs         = total_costs,
        aggregate_ratio     = aggregate_ratio,
        pooled_crossover    = pooled_crossover
    ))
}

# =============================================================================
# USAGE 
# =============================================================================

# --- Table S6: main state summary ---
write_state_summary_word(
    final_year      = 2035,
    new_excess      = new_excess,
    start_paths     = start_paths,
    compare_with_rw = compare_with_rw,
    output_path     = "~/ADAP_state_summary_2035.docx"
)

# --- Table S5: FL / DC year-by-year detail ---
write_art_word_table(
    location_filter = "FL",
    new_excess      = new_excess,
    start_paths     = start_paths,
    compare_with_rw = compare_with_rw,
    output_path     = "~/ART_table_FL.docx"
)
# write_art_word_table(
#     location_filter = "DC",
#     new_excess      = new_excess,
#     start_paths     = start_paths,
#     compare_with_rw = compare_with_rw,
#     output_path     = "~/ART_table_DC.docx"
# )

# --- Figure 2 supplement table ---
fig2_table_df <- build_fig2_table(plot_df, box_df = box_df, compare_with_rw = compare_with_rw)
write_fig2_table_word(fig2_table_df, output_path = "~/figure2_state_table.docx")

# --- Results-paragraph text: Florida vignette + US aggregate ---
generate_adap_text(
    location_filter = "FL",
    final_year      = 2035,
    base_year       = 2026,
    new_excess      = new_excess,
    start_paths     = start_paths,
    compare_with_rw = compare_with_rw
)
#
generate_adap_aggregate_text(
    final_year      = 2035,
    base_year       = 2026,
    new_excess      = new_excess,
    start_paths     = start_paths,
    compare_with_rw = compare_with_rw
)

################### added input parameters table for adjusted % intiators

format_pct_interval <- function(x) {
    qs <- quantile(
        x,
        probs = c(0.50, 0.025, 0.975),
        na.rm = TRUE
    )
    
    sprintf(
        "%.1f%% [%.1f–%.1f]",
        100 * qs[[1]],
        100 * qs[[2]],
        100 * qs[[3]]
    )
}

output_table <- adjusted_care_draws %>%
    filter(location != "Total") %>%
    mutate(
        expansion_status = if_else(
            medicaid_expansion == 1,
            "Expansion",
            "Non-expansion"
        )
    ) %>%
    group_by(location, expansion_status) %>%
    summarise(
        `Baseline viral suppression (2025)` =
            format_pct_interval(care_fraction_2025),
        
        `ADAP share: % virally suppressed PWH receiving ADAP services` =
            format_pct_interval(adap_share_suppressed_2025),
        
        `Viral suppression loss: % ADAP-supported PWH expected to lose viral suppression` =
            format_pct_interval(fraction_adap_losing_suppression),
        
        `Immediate ART initiators (at diagnosis), post-ADAP elimination` =
            format_pct_interval(care_fraction_post_adap),
        
        `Adjusted delayed-cohort return threshold, post-ADAP elimination` =
            format_pct_interval(pi_return_post_adap),
        
        .groups = "drop"
    ) %>%
    rename(
        State = location,
        `Expansion Status` = expansion_status
    ) %>%
    arrange(State)

write_csv(
    output_table,
    "Table_S5_state_specific_ART_initiation.csv"
)

# =============================================================================
# FIGURE S3-S4 (2 x 2)
# =============================================================================


BASELINE.YEAR <- "2025"

# --- NCER + cumulative ADAP spending at 2035 --------------------------------
savings_2035 <- compare_with_rw %>%
    filter(
        year          == 2035,
        !location %in% c("Total", "total")
    ) %>%
    group_by(location) %>%
    summarise(
        med_ratio = median(
            (cumulative_incremental_cost - cumulative_drug_only) / cumulative_drug_only,
            na.rm = TRUE
        ),
        med_adap_spending = median(cumulative_drug_only, na.rm = TRUE),
        .groups = "drop"
    )

# --- A. ADAP coverage (% of diagnosed PLWH on ADAP) -------------------------
adap_pct_df <- df %>%
    filter(year == 2025, intervention == "noint",
           outcome %in% c("adap.clients", "diagnosed.prevalence")) %>%
    dplyr::select(location, sim, outcome, value) %>%
    pivot_wider(names_from = outcome, values_from = value) %>%
    mutate(adap_pct = adap.clients / diagnosed.prevalence) %>%
    group_by(location) %>%
    summarise(avg_adap_pct = mean(adap_pct, na.rm = TRUE), .groups = "drop") %>%
    filter(location != "Total")

# --- B. Viral suppression ----------------------------------------------------
suppression_pct_df <- df %>%
    filter(year == 2025, intervention == "noint",
           outcome %in% c("suppression", "diagnosed.prevalence")) %>%
    dplyr::select(location, sim, outcome, value) %>%
    pivot_wider(names_from = outcome, values_from = value) %>%
    mutate(suppression_pct = suppression / diagnosed.prevalence) %>%
    group_by(location) %>%
    summarise(avg_suppression_pct = mean(suppression_pct, na.rm = TRUE),
              .groups = "drop") %>%
    filter(location != "Total")

# --- C. Per-capita diagnosed prevalence --------------------------------------
pc_df <- df %>%
    filter(year == 2025, intervention == "noint",
           outcome %in% c("diagnosed.prevalence", "population")) %>%
    dplyr::select(location, sim, outcome, value) %>%
    pivot_wider(names_from = outcome, values_from = value) %>%
    mutate(diag_prev_pc = diagnosed.prevalence / population) %>%
    group_by(location) %>%
    summarise(avg_diag_prev_pc = mean(diag_prev_pc, na.rm = TRUE), .groups = "drop") %>%
    filter(location != "Total")

# --- D. ADAP <-> suppression overlap (ADAP-direction) -----------------------
adap_supp_df <- df %>%
    filter(year == 2025, intervention == "noint",
           outcome %in% c("adap.suppression", "adap.clients", "suppression")) %>%
    dplyr::select(location, sim, outcome, value) %>%
    pivot_wider(names_from = outcome, values_from = value) %>%
    mutate(
        prop_adap_suppressed = adap.suppression / adap.clients
    ) %>%
    group_by(location) %>%
    summarise(
        avg_prop_adap_suppressed = mean(prop_adap_suppressed, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    filter(location != "Total")

# --- ADAP client count (2025 baseline, noint) -- size var for spending fig -
adap_clients_df <- df %>%
    filter(year == 2025, intervention == "noint", outcome == "adap.clients") %>%
    group_by(location) %>%
    summarise(avg_adap_clients = mean(value, na.rm = TRUE), .groups = "drop") %>%
    filter(location != "Total")

# --- Medicaid expansion ------------------------------------------------------
non_expansion <- c("AL", "FL", "GA", "ID", "KS", "MS", "NC", "SC",
                   "SD", "TN", "TX", "WI", "WY")
medicaid_expansion <- tibble(location = unique(compare_with_rw$location)) %>%
    mutate(expanded = !(location %in% non_expansion),
           expansion_label = if_else(expanded, "Medicaid expansion", "Non-expansion"))

# --- Assemble ----------------------------------------------------------------
plot_df <- savings_2035 %>%
    left_join(adap_pct_df,        by = "location") %>%
    left_join(suppression_pct_df, by = "location") %>%
    left_join(pc_df,              by = "location") %>%
    left_join(adap_supp_df,       by = "location") %>%
    left_join(adap_clients_df,    by = "location") %>%
    left_join(medicaid_expansion, by = "location") %>%
    filter(!location %in% c("Total", "total"))

# =============================================================================
# SHARED AESTHETICS
# =============================================================================
shared_theme <- theme_bw() +
    theme(
        legend.position  = "none",
        axis.title       = element_text(size = 9),
        axis.text        = element_text(size = 8),
        panel.grid.minor = element_blank(),
        plot.tag         = element_text(size = 10, face = "bold")
    )

expansion_colors <- c(
    "Medicaid expansion" = "#2e6b75",
    "Non-expansion"      = "#a8cdd1"
)

# =============================================================================
# CORRELATION LABEL HELPER (Spearman, unadjusted -- same as Figure 2)
# =============================================================================
make_corr_label <- function(x, y, data) {
    ct <- cor.test(data[[x]], data[[y]], method = "spearman", exact = FALSE)
    sprintf("\u03c1 = %.2f", ct$estimate)
}

# =============================================================================
# PANEL FUNCTION
#   size_labels defaults to the Figure 2 dollar-billions formatter; pass a
#   different formatter (e.g. label_comma()) for non-dollar size variables.
# =============================================================================
make_panel <- function(data,
                       x_var,
                       x_lab,
                       corr_label,
                       corr_pos    = "topleft",
                       pct_x       = FALSE,
                       pct_accuracy = 1,
                       x_scale     = 1,
                       x_suffix    = "",
                       size_var    = "med_adap_spending",
                       size_lab    = "Cum. ADAP spending\nthrough 2035 (USD)",
                       size_labels = scales::label_dollar(scale = 1e-9, suffix = "B"),
                       y_var       = "med_ratio",
                       y_lab       = "Net Cost of ADAP Elimination\nto ADAP Expenditure Ratio") {
    
    x_range <- range(data[[x_var]], na.rm = TRUE)
    y_range <- range(data[[y_var]], na.rm = TRUE)
    
    ann_x     <- if (corr_pos == "topleft")
        x_range[1] + 0.02 * diff(x_range)
    else
        x_range[2] - 0.02 * diff(x_range)
    ann_hjust <- if (corr_pos == "topleft") 0 else 1
    ann_y     <- y_range[2]
    
    p <- ggplot(data, aes(
        x    = .data[[x_var]],
        y    = .data[[y_var]],
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
            labels = size_labels
        ) +
        labs(x = x_lab, y = y_lab, fill = NULL) +
        shared_theme
    
    if (pct_x)
        p <- p + scale_x_continuous(labels = scales::percent_format(accuracy = pct_accuracy))
    else if (x_scale != 1 || x_suffix != "")
        p <- p + scale_x_continuous(labels = scales::label_dollar(scale = x_scale, suffix = x_suffix))
    
    p <- p +
        guides(
            fill = guide_legend(
                override.aes = list(size = 5, shape = 22),
                theme = theme(
                    legend.background = element_rect(color = "black", fill = NA, linewidth = 0.4),
                    legend.margin      = margin(5, 8, 5, 8)
                )
            ),
            size = guide_legend(
                override.aes = list(fill = "grey50", color = "grey30", shape = 21),
                theme = theme(
                    legend.background = element_rect(color = "black", fill = NA, linewidth = 0.4),
                    legend.margin      = margin(5, 8, 5, 8)
                )
            )
        )
    
    p
}

# =============================================================================
# CORRELATION LABELS
# =============================================================================
label_adap_pct       <- make_corr_label("avg_adap_pct",            "med_ratio", plot_df)
label_supp_pct       <- make_corr_label("avg_suppression_pct",      "med_ratio", plot_df)
label_diag_pc        <- make_corr_label("avg_diag_prev_pc",         "med_ratio", plot_df)
label_adap_supp      <- make_corr_label("avg_prop_adap_suppressed", "med_ratio", plot_df)
label_spending_total <- make_corr_label("med_adap_spending",        "med_ratio", plot_df)

# =============================================================================
# FIGURE S4 PANELS (2 x 2)
# =============================================================================
p_adap_pct_s4 <- make_panel(
    data       = plot_df,
    x_var      = "avg_adap_pct",
    x_lab      = "ADAP Coverage of Diagnosed PWH, %",
    corr_label = label_adap_pct,
    corr_pos   = "topright",
    pct_x      = TRUE,
    size_var   = "med_adap_spending",
    size_lab   = "Cum. ADAP spending\nthrough 2035 (USD)"
) + labs(tag = "A")

p_supp_pct_s4 <- make_panel(
    data       = plot_df,
    x_var      = "avg_suppression_pct",
    x_lab      = "Viral Suppression, % of Diagnosed",
    corr_label = label_supp_pct,
    corr_pos   = "topright",
    pct_x      = TRUE,
    size_var   = "med_adap_spending",
    size_lab   = "Cum. ADAP spending\nthrough 2035 (USD)"
) + labs(tag = "B")

p_diag_pc_s4 <- make_panel(
    data        = plot_df,
    x_var       = "avg_diag_prev_pc",
    x_lab       = "Diagnosed Prevalence per Capita",
    corr_label  = label_diag_pc,
    corr_pos    = "topright",
    pct_x       = TRUE,
    pct_accuracy = 0.1,
    size_var    = "med_adap_spending",
    size_lab    = "Cum. ADAP spending\nthrough 2035 (USD)"
) + labs(tag = "C")

p_adap_supp_s4 <- make_panel(
    data       = plot_df,
    x_var      = "avg_prop_adap_suppressed",
    x_lab      = "Proportion of ADAP Recipients Suppressed, %",
    corr_label = label_adap_supp,
    corr_pos   = "topright",
    pct_x      = TRUE,
    size_var   = "med_adap_spending",
    size_lab   = "Cum. ADAP spending\nthrough 2035 (USD)"
) + labs(tag = "D")

p_combined_s4 <- (p_adap_pct_s4 | p_supp_pct_s4) /
    (p_diag_pc_s4 | p_adap_supp_s4) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom", legend.box = "horizontal")

print(p_combined_s4)

ggsave("figure_S4.pdf", p_combined_s4, width = 10, height = 9, units = "in")

# =============================================================================
# SEPARATE SINGLE-PANEL FIGURE: cumulative ADAP spending vs NCER
# =============================================================================
p_spending_total <- make_panel(
    data        = plot_df,
    x_var       = "med_adap_spending",
    x_lab       = "Cumulative ADAP Spending\nthrough 2035 (USD)",
    corr_label  = label_spending_total,
    corr_pos    = "topright",
    x_scale     = 1e-6, x_suffix = "M",
    size_var    = "avg_adap_clients",
    size_lab    = "ADAP clients\n(2025 baseline)",
    size_labels = scales::label_comma()
) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom", legend.box = "horizontal")

print(p_spending_total)

ggsave("figure_S3_spending.pdf", p_spending_total, width = 7, height = 6, units = "in")