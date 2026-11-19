# Sensitivity analysis step 2: Top vs. bottom 20% comparison and parameter ranking (v3)
# Pipeline (v3): step1 sort sims -> step2 top/bottom 20% ranking -> step3 PRCC -> step4 tornado plots
#
# Outcomes: % reduction in 2030 incidence, doxy.cov.20 vs. no intervention, for the
# total population, MSM and women (see 00_prcc_settings.R).
# For each MSA x outcome x parameter: the outcome in sims with the lowest vs. highest
# TOP_BOTTOM_FRAC of that parameter's values.
# Baseline projection = primary-analysis estimate = median across all sims in the MSA
# (same statistic as analysis/intervention/03_manuscript_main.R).
# Ranking: largest deviation from baseline, max(|median top - baseline|, |median bottom - baseline|).

cat("\n==== PRCC step 2/4: top vs. bottom 20% comparison and parameter ranking (started ", format(Sys.time(), "%H:%M:%S"), ") ====\n", sep = "")

# SETTINGS ----
source('../jheem_analyses/applications/SHIELD/analysis/sensitivity_analyses/prcc/00_prcc_settings.R')

# LOAD ----
param_matrices     <- get(load(paste0(OUT.PATH, "param_matrices.Rdata")))
sorted_arrs        <- get(load(paste0(OUT.PATH, "sorted_arrs.Rdata")))
total_calc_results <- get(load(paste0(BASE.PATH, "total_calc_results.Rdata")))
sex_calc_results   <- get(load(paste0(BASE.PATH, "sex_calc_results.Rdata")))

# TOP / BOTTOM SUMMARIES ----
tail_stats <- function(v, group) {
    v <- v[!is.na(v)]
    tibble(topOrBottom = group,
           `Min.`    = min(v),
           `1st Qu.` = quantile(v, probs = 0.25, names = FALSE),
           Median    = median(v),
           `3rd Qu.` = quantile(v, probs = 0.75, names = FALSE),
           `Max.`    = max(v))
}

# Parameters with no variation across sims are skipped.
# sorted_arrs (step 1): each column = sim numbers ordered by that parameter's value (lowest first)
summarize_tails <- function(outcome, city) {
    y       <- get_outcome(outcome, city, total_calc_results, sex_calc_results)
    pm      <- param_matrices[[city]]
    ord_all <- sorted_arrs[[city]]
    stopifnot(nrow(ord_all) == length(y))

    params <- colnames(pm)[apply(pm, 2, var) > 0]
    n_sim  <- nrow(ord_all)
    n_tail <- round(n_sim * TOP_BOTTOM_FRAC)

    map_dfr(params, function(p) {
        ord <- ord_all[, p]
        bind_rows(tail_stats(y[ord[1:n_tail]], "bottom"),
                  tail_stats(y[ord[(n_sim - n_tail + 1):n_sim]], "top")) %>%
            mutate(parameter = p)
    }) %>%
        mutate(location = city,
               outcome  = outcome,
               baseline = median(y, na.rm = TRUE))
}

tails <- map_dfr(names(OUTCOMES), function(o) {
    map_dfr(locations, function(city) summarize_tails(o, city))
})

# RANKING ----
rank_df <- tails %>%
    select(location, outcome, parameter, baseline, topOrBottom, Median) %>%
    pivot_wider(names_from = topOrBottom, values_from = Median, names_prefix = "median_") %>%
    mutate(top_minus_baseline    = median_top - baseline,
           bottom_minus_baseline = median_bottom - baseline,
           max_abs_dev_baseline  = pmax(abs(top_minus_baseline), abs(bottom_minus_baseline)),
           top_minus_bottom      = median_top - median_bottom) %>%
    group_by(location, outcome) %>%
    arrange(desc(max_abs_dev_baseline), .by_group = TRUE) %>%
    mutate(rank = row_number()) %>%
    ungroup() %>%
    relocate(rank, .after = parameter)

# Save ----
save(tails, rank_df, file = paste0(OUT.PATH, "top_bottom_ranking.Rdata"))
