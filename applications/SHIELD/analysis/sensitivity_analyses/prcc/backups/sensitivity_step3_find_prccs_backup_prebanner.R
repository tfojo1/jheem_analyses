# Sensitivity analysis step 3: Calculate PRCC values (v3)
# Pipeline (v3): step1 sort sims -> step2 top/bottom 20% ranking -> step3 PRCC -> step4 tornado plots
#
# PRCC is calculated with ALL parameters in the model (a partial correlation must
# adjust for every other parameter); it is then reported for the top-ranked
# parameters from step 2. PRCC is not used to select or rank parameters.

# library(epiR)    # replaced by calc_prcc_base() in 00_prcc_settings.R

# SETTINGS ----
source('../jheem_analyses/applications/SHIELD/analysis/sensitivity_analyses/prcc/00_prcc_settings.R')

# LOAD ----
param_matrices     <- get(load(paste0(OUT.PATH, "param_matrices.Rdata")))
load(paste0(OUT.PATH, "top_bottom_ranking.Rdata"))    # loads tails and rank_df (step 2)
total_calc_results <- get(load(paste0(BASE.PATH, "total_calc_results.Rdata")))
sex_calc_results   <- get(load(paste0(BASE.PATH, "sex_calc_results.Rdata")))

# Calculate PRCC ----
# For each MSA: drop parameters with no variation and sims with NA outcome,
# run PRCC, then adjust p-values for the number of parameters tested
calc_prcc <- function(outcome, city) {
    all_params <- as.data.frame(param_matrices[[city]])
    all_params <- all_params[, apply(all_params, 2, var) > 0, drop = FALSE]

    y <- get_outcome(outcome, city, total_calc_results, sex_calc_results)
    stopifnot(length(y) == nrow(all_params))

    df <- cbind(all_params, data.frame(outcome = y))
    n_dropped <- sum(is.na(df$outcome))
    if (n_dropped > 0) cat(city, outcome, ": dropped", n_dropped, "sims with NA outcome\n")
    df <- df[!is.na(df$outcome), ]

    # epi.prcc(df, sided.test = 2, conf.level = 0.95) %>%
    calc_prcc_base(df, conf.level = 0.95) %>%
        mutate(p.adj = p.adjust(p.value, method = P.ADJUST.METHOD)) %>%
        arrange(p.value)
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
                     prcc_lower = lower, prcc_upper = upper, p.value, p.adj),
              by = c("location", "outcome", "parameter")) %>%
    mutate(direction_agrees_prcc = sign(top_minus_bottom) == sign(prcc))

# Save ----
save(prccs,   file = paste0(OUT.PATH, "prccs.Rdata"))
save(rank_df, file = paste0(OUT.PATH, "rank_df_with_prcc.Rdata"))

write_csv_with_notes(
    prcc_long,
    paste0(OUT.PATH, "prcc_all_params.csv"),
    c(paste0("PRCC of each calibrated parameter vs. % reduction in ", YEAR, " incidence (", INTERVENTION,
             " vs. no intervention, same simulation); one row per MSA x outcome (total, msm, female) x parameter."),
      paste0("est = PRCC (-1 to 1; positive = higher parameter value -> larger % reduction); lower/upper = 95% CI; ",
             "p.adj = ", P.ADJUST.METHOD, "-adjusted p within MSA x outcome (reported only, not used for selection)."))
)

write_csv_with_notes(
    rank_df,
    paste0(OUT.PATH, "parameter_ranking_top_bottom.csv"),
    c(paste0("Outcome = % reduction in ", YEAR, " incidence (", INTERVENTION, " vs. no intervention, same simulation). ",
             "baseline = primary-analysis median across all sims in the MSA; median_top/median_bottom = median outcome in sims with the highest/lowest ",
             round(TOP_BOTTOM_FRAC * 100), "% of the parameter value."),
      paste0("rank = order of max_abs_dev_baseline = max(|top - baseline|, |bottom - baseline|) within MSA x outcome (1 = largest); ",
             "differences in percentage points; prcc = PRCC from the full-parameter model; direction_agrees_prcc = sign(top - bottom) matches sign(PRCC)."))
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
