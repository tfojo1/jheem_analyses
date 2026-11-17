# Sensitivity analysis step 1: Calculate PRCC values (v2, see CHANGES.md)

library(epiR)

# SETTINGS ----
source('../jheem_analyses/applications/SHIELD/analysis/sensitivity_v2/00_sensitivity_settings.R')

# LOAD ----
calib.simsets      <- load.calib.simsets(SHIELD.TEN.MSAS, CALIB.CODE, 1000)
total_calc_results <- get(load(paste0(BASE.PATH, "total_calc_results.Rdata")))
sex_calc_results   <- get(load(paste0(BASE.PATH, "sex_calc_results.Rdata")))

# Parameter matrices (n.sim x n.params), one per MSA ----
# Saved so step 2 does not need calib.simsets in memory
param_matrices <- lapply(locations, function(city) {
    simset <- extract.calib.simsets(calib.simsets, city, CALIB.CODE, exact = T)[[1]]$full_simset
    all_params <- t(simset$parameters)
    colnames(all_params) <- simset$parameter.names
    all_params
})

# Calculate PRCC ----
# For each MSA: drop parameters with no variation and sims with NA outcome,
# run PRCC, then adjust p-values for the number of parameters tested
calc_prcc <- function(outcome, city) {
    all_params <- as.data.frame(param_matrices[[city]])
    all_params <- all_params[, apply(all_params, 2, var) > 0, drop = FALSE]

    df <- cbind(all_params,
                data.frame(outcome = get_outcome(outcome, city, total_calc_results, sex_calc_results)))
    n_dropped <- sum(is.na(df$outcome))
    if (n_dropped > 0) cat(city, "outcome", outcome, ": dropped", n_dropped, "sims with NA outcome\n")
    df <- df[!is.na(df$outcome), ]

    epi.prcc(df, sided.test = 2, conf.level = 0.95) %>%
        mutate(p.adj = p.adjust(p.value, method = P.ADJUST.METHOD)) %>%
        arrange(p.value)
}

prccs <- list()
for (outcome in c("1", "2")) {
    print(paste0("Calculating PRCC for outcome ", outcome, " at ", Sys.time()))
    prccs[[outcome]] <- lapply(locations, function(city) calc_prcc(as.numeric(outcome), city))
}

# Save ----
save(prccs,          file = paste0(OUT.PATH, "prccs.Rdata"))
save(param_matrices, file = paste0(OUT.PATH, "param_matrices.Rdata"))

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
