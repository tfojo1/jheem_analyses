# Sensitivity analysis step 3: Calculate PRCC values (v3)
# Pipeline (v3): step1 sort sims -> step2 top/bottom 20% ranking -> step3 PRCC -> step4 tornado plots
#
# PRCC is calculated with ALL parameters in the model (a partial correlation must
# adjust for every other parameter); it is then added to the step 2 results.
# PRCC is not used to select or rank parameters.


cat("\n==== PRCC step 3/4: PRCC for all parameters, added to the ranking (started ", format(Sys.time(), "%H:%M:%S"), ") ====\n", sep = "")

# SETTINGS ----
source('../jheem_analyses/applications/SHIELD/analysis/sensitivity_analyses/prcc/00_prcc_settings.R')

# LOAD ----
param_matrices     <- get(load(paste0(OUT.PATH, "param_matrices.Rdata")))
load(paste0(OUT.PATH, "top_bottom_ranking.Rdata"))    # loads tails and rank_df (step 2)
total_calc_results <- get(load(paste0(BASE.PATH, "total_calc_results.Rdata")))
sex_calc_results   <- get(load(paste0(BASE.PATH, "sex_calc_results.Rdata")))

# Calculate PRCC ----
# For each MSA: drop parameters with no variation and sims with NA outcome,
# run PRCC (estimate and 95% CI; no p-values)
calc_prcc <- function(outcome, city) {
    all_params <- as.data.frame(param_matrices[[city]])
    all_params <- all_params[, apply(all_params, 2, var) > 0, drop = FALSE]

    y <- get_outcome(outcome, city, total_calc_results, sex_calc_results)
    stopifnot(length(y) == nrow(all_params))

    df <- cbind(all_params, data.frame(outcome = y))
    n_dropped <- sum(is.na(df$outcome))
    if (n_dropped > 0) cat(city, outcome, ": dropped", n_dropped, "sims with NA outcome\n")
    df <- df[!is.na(df$outcome), ]

    calc_prcc_base(df, conf.level = 0.95) %>%
        arrange(desc(abs(est)))
}

prccs <- list()
for (outcome in names(OUTCOMES)) {
    print(paste0("Calculating PRCC for outcome ", outcome, " at ", Sys.time()))
    prccs[[outcome]] <- lapply(locations, function(city) calc_prcc(outcome, city))
}

prcc_long <- map_dfr(names(prccs), function(o) {
    bind_rows(prccs[[o]], .id = "location") %>% mutate(outcome = o, .after = location)
})

# Add PRCC to the step 2 ranking ----
rank_df <- rank_df %>%
    left_join(select(prcc_long, location, outcome, parameter = var, prcc = est,
                     prcc_lower = lower, prcc_upper = upper),
              by = c("location", "outcome", "parameter")) %>%
    mutate(direction_agrees_prcc = sign(top_minus_bottom) == sign(prcc),
           large_prcc            = abs(prcc) >= MIN_ABS_PRCC,
           stands_out            = large_std_diff & large_prcc)      # both criteria (00_prcc_settings.R)

# Save ----
save(prccs,   file = paste0(OUT.PATH, "prccs.Rdata"))
save(rank_df, file = paste0(OUT.PATH, "rank_df_with_prcc.Rdata"))

# Parameters that stand out (std_diff >= MIN_STD_DIFF and |PRCC| >= MIN_ABS_PRCC) ----
# 1. How many per MSA x outcome
stand_out_counts <- rank_df %>%
    group_by(location, outcome) %>%
    summarize(n_params_tested    = n(),
              n_large_std_diff   = sum(large_std_diff, na.rm = TRUE),    # first criterion only
              n_stand_out        = sum(stands_out, na.rm = TRUE),        # both criteria
              max_std_diff    = round(max(std_diff, na.rm = TRUE), 2),
              .groups = "drop")

cat("\nParameters that stand out (standardized difference >= ", MIN_STD_DIFF, " SD and |PRCC| >= ", MIN_ABS_PRCC,
    "), by MSA and outcome:\n", sep = "")
print(as.data.frame(pivot_wider(select(stand_out_counts, location, outcome, n_stand_out),
                                names_from = outcome, values_from = n_stand_out)), row.names = FALSE)

# 2. For each parameter: in how many MSAs it stands out, and in which direction (PRCC sign).
#    Summaries are over the MSAs where it stands out. Readable names for the manuscript table.
titles <- get_param_titles()
param_label <- function(p) ifelse(is.na(titles[p]), p, gsub("\\s*\n\\s*", " ", titles[p]))
med_range   <- function(x, d) {
    x <- x[!is.na(x)]
    if (length(x) == 1) return(formatC(x, format = "f", digits = d))
    paste0(formatC(median(x), format = "f", digits = d), " (",
           formatC(min(x), format = "f", digits = d), " to ", formatC(max(x), format = "f", digits = d), ")")
}

stand_out_by_param <- rank_df %>%
    filter(stands_out %in% TRUE) %>%
    group_by(outcome, parameter) %>%
    summarize(n_msas      = n(),
              n_positive  = sum(prcc > 0),
              n_negative  = sum(prcc < 0),
              prcc_txt    = med_range(prcc, 2),
              std_txt     = med_range(std_diff, 1),
              median_std  = median(std_diff),
              msa_names   = paste(sort(MSA.LABELS[location]), collapse = ", "),
              .groups = "drop") %>%
    arrange(factor(outcome, levels = names(OUTCOMES)), desc(n_msas), desc(median_std)) %>%
    transmute(Outcome                                            = OUTCOME.LABELS[outcome],
              Parameter                                          = param_label(parameter),
              !!paste0("MSAs (of ", length(locations), ")")      := n_msas,
              `Positive PRCC (MSAs)`                             = n_positive,
              `Negative PRCC (MSAs)`                             = n_negative,
              `PRCC, median (range)`                             = prcc_txt,
              `Standardized difference (SD), median (range)`     = std_txt,
              `MSAs where influential`                           = msa_names,
              `Parameter code`                                   = parameter)

save(stand_out_counts, stand_out_by_param, file = paste0(OUT.PATH, "stand_out_summary.Rdata"))

write_csv_with_notes(
    prcc_long,
    paste0(OUT.PATH, "prcc_all_params.csv"),
    c(paste0("PRCC of each calibrated parameter vs. each outcome in ", YEAR, " (", INTERVENTION, " vs. no intervention, same simulation): ",
             "total/msm/female = % reduction in incidence; rir = women % reduction / MSM % reduction (NA when MSM incidence did not fall); ",
             "women_minus_msm = women - MSM % reduction (percentage points). One row per MSA x outcome x parameter."),
      "est = PRCC (-1 to 1; positive = higher parameter value -> higher outcome value); lower/upper = 95% CI (Fisher z). Rows sorted by |PRCC| within MSA x outcome.")
)

write_csv_with_notes(
    rank_df,
    paste0(OUT.PATH, "parameter_ranking_top_bottom.csv"),
    c(paste0("Outcomes in ", YEAR, " (", INTERVENTION, " vs. no intervention, same simulation): total/msm/female = % reduction in incidence; ",
             "rir = women/MSM ratio of % reductions (NA when MSM incidence did not fall); women_minus_msm = percentage points. ",
             "baseline = primary-analysis median across all sims in the MSA; median_top/median_bottom = median outcome in sims with the highest/lowest ",
             round(TOP_BOTTOM_FRAC * 100), "% of the parameter value."),
      paste0("rank = order of std_diff within MSA x outcome (1 = largest); max_abs_dev_baseline = max(|top - baseline|, |bottom - baseline|); ",
             "differences in the outcome's units; std_diff = |top - bottom| / outcome_sd (SD across all sims); large_std_diff = std_diff >= ", MIN_STD_DIFF,
             "; prcc = PRCC from the full-parameter model; large_prcc = |prcc| >= ", MIN_ABS_PRCC, "; stands_out = large_std_diff and large_prcc; direction_agrees_prcc = sign(top - bottom) matches sign(PRCC)."))
)

write_csv_with_notes(
    mutate(stand_out_counts, MSA = MSA.LABELS[location], .before = location),
    paste0(OUT.PATH, "stand_out_counts.csv"),
    c(paste0("Number of parameters that stand out per MSA x outcome: standardized quintile difference |median top 20% - median bottom 20%| / SD of the outcome >= ",
             MIN_STD_DIFF, " AND |PRCC| >= ", MIN_ABS_PRCC, " (MIN_STD_DIFF, MIN_ABS_PRCC in 00_prcc_settings.R)."),
      "n_params_tested = parameters with any variation; n_large_std_diff = meeting the standardized-difference criterion only; n_stand_out = meeting both; max_std_diff = largest standardized difference (in SDs).")
)

write_csv_with_notes(
    stand_out_by_param,
    paste0(OUT.PATH, "stand_out_by_parameter.csv"),
    c(paste0("Influential parameters by outcome: parameters with a standardized difference >= ", MIN_STD_DIFF,
             " SD (|median outcome, top 20% of the parameter's values - bottom 20%| / SD of the outcome in the MSA) and |PRCC| >= ",
             MIN_ABS_PRCC, " in at least one MSA. Outcomes in ", YEAR, ", doxy-PEP at ", sub("doxy.cov.", "", INTERVENTION, fixed = TRUE),
             "% coverage vs. no doxy-PEP."),
      paste0("MSAs = number of MSAs where the parameter is influential; Positive/Negative PRCC = of those, MSAs where higher values increase/decrease the outcome; ",
             "PRCC and standardized difference = median (range) across those MSAs. Rows sorted by outcome, then number of MSAs."))
)

# ORIGINAL COMMENTED-OUT CODE (kept as is) ----

# # Save ----
# save(prccs_outcome1,
#     file = paste0(BASE.PATH, "prccs_outcome1.Rdata")
# )
# save(prccs_outcome2,
#     file = paste0(BASE.PATH, "prccs_outcome2.Rdata")
# )
# # 
# # prccs_outcome1 <- get(load(file = paste0(BASE.PATH, "prccs_outcome1.Rdata")))
# # prccs_outcome2 <- get(load(file = paste0(BASE.PATH, "prccs_outcome2.Rdata")))
# 
# ## TO FIND MOST SIGNIFICANT PER CITY
# p_ranked_1 <- lapply(prccs_outcome1, function(df) {
#     head(arrange(df, p.value), n=10) %>%
#         select(var, est, p.value)
# })
# 
# 
# # # Gets the ones that are most frequently in cities' top 15
# # sort(table(unlist(sapply(p_ranked_1, function(x) {unique(x$var)}))), decreasing = T)[1:10]
# 
# ## TO FIND MOST SIGNIFICANT PER CITY
# p_ranked_2 <- lapply(prccs_outcome2, function(df) {
#     head(arrange(df, p.value), n=15) %>%
#         select(var, est, p.value)
# })
# 
# # # Gets the ones that are most frequently in cities' top 15
# # sort(table(unlist(sapply(p_ranked_2, function(x) {unique(x$var)}))), decreasing = T)[1:10]
# 
# 
# unique(c(unlist(lapply(p_ranked_1, function(x) {x$var})),
#          unlist(lapply(p_ranked_2, function(x) {x$var}))))

# save(p_ranked_1,
#      file = paste0(BASE.PATH, "p_ranked_1.Rdata")
# )
# save(p_ranked_2,
#      file = paste0(BASE.PATH, "p_ranked_2.Rdata")
# )
# 
# params_ranked <- list(
#     "1" = p_ranked_1,
#     "2" = p_ranked_2
# )
# save(params_ranked,
#      file = paste0(BASE.PATH, "params_ranked.Rdata"))
