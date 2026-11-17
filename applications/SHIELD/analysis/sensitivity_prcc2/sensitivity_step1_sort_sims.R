# Sensitivity analysis step 1: Build parameter matrices and sort simulations by parameter value (v3)
# Pipeline (v3): step1 sort sims -> step2 top/bottom 20% ranking -> step3 PRCC -> step4 tornado plots
# v2 versions: sensitivity_step1_find_prccs_backup.R, sensitivity_step2_sort_sims_backup.R

# SETTINGS ----
source('../jheem_analyses/applications/SHIELD/analysis/sensitivity_prcc2/00_sensitivity_settings.R')

# Parameter matrices (n.sim x n.params), one per MSA ----
# Reuses the v2 file if it exists, so calib.simsets does not have to be loaded again
pm_file <- paste0(V2.PATH, "param_matrices.Rdata")
if (file.exists(pm_file)) {
    cat("Reusing", pm_file, "\n")
    param_matrices <- get(load(pm_file))
} else {
    calib.simsets  <- load.calib.simsets(SHIELD.TEN.MSAS, CALIB.CODE, 1000)
    param_matrices <- lapply(locations, function(city) {
        simset <- extract.calib.simsets(calib.simsets, city, CALIB.CODE, exact = T)[[1]]$full_simset
        all_params <- t(simset$parameters)
        colnames(all_params) <- simset$parameter.names
        all_params
    })
    rm(calib.simsets)
}

# Sort simulations by parameter value ----
# In each matrix, the last row holds the number of the sim with the largest
# value for that parameter (e.g., "357" means sim #357 has the largest value)
sorted_arrs <- lapply(param_matrices, function(x) {
    rv <- apply(x, 2, order)
    colnames(rv) <- colnames(x)
    rv
})

# Save ----
save(param_matrices, file = paste0(OUT.PATH, "param_matrices.Rdata"))
save(sorted_arrs,    file = paste0(OUT.PATH, "sorted_arrs.Rdata"))

## OLD VERSION THAT AGGREGATED THE TEN CITIES FOR ONE PLOT----
# # Outcome 1: Percent Reduction in Incidence in 2030 versus No Intervention
# # FORMULA: 100 * (noint's 2030 incidence - doxy.cov.30's 2030 incidence) / noint's 2030 incidence
# 
# total_raw_results <- get(load(paste0(BASE.PATH, "total_raw_results.Rdata")))
# 
# # Get each component for each city, then create 10 versions with sims sorted
# # according to each parameter's sim order
# 
# # Doing this in 2 lines so that it works even if location ever has length 1 and would have gotten dropped
# sub <- total_raw_results["2030", , c("noint", "doxy.cov.30"), , "incidence", drop = FALSE]
# sub <- abind::adrop(sub, drop = c(1, 5))   # drop the year and outcome dims (positions 1 and 5)
# # sub now has dims: sim x intervention x location
# 
# params <- names(PLOT_1_PARAMS)
# 
# result <- array(
#     NA_real_, # could be just "NA", but we may as well do this if we already know it will get coerced to a number
#     dim = c(sim = unname(dim(sub)[1]), intervention = 2, location = length(locations), parameter = length(params)),
#     dimnames = list(sim = NULL,
#                     intervention = c("noint", "doxy.cov.30"),
#                     location = locations,
#                     parameter = params)
# )
# 
# for (loc in locations) {
#     for (p in params) {
#         ord <- sorted_arrs[[loc]][, p]        # sim order for this location/parameter
#         result[, , loc, p] <- sub[ord, , loc]
#     }
# }
# 
# # Verify ordering
# all_good <- TRUE
# for (loc in locations) {
#     for (p in params) {
#         ord <- sorted_arrs[[loc]][, p]
#         expected <- unname(sub[ord, , loc])
#         actual   <- unname(result[, , loc, p])
#         if (!identical(expected, actual)) {
#             all_good <- FALSE
#             cat("Mismatch at", loc, p, "\n")
#         }
#     }
# }
# all_good
# 
# # Great, now I have "result" array with sim, both interventions, location, and the 10 parameters.
# 
# # Here's the final array with the aggregated values for the plot
# outcome1_agg <- 100 * (apply(result[,"noint",,], c("sim", "parameter"), sum) -
#                            apply(result[,"doxy.cov.30",,], c("sim", "parameter"), sum)) /
#     apply(result[,"noint",,], c("sim", "parameter"), sum)
# 
# # Outcome 2: Ratio of percent reduction in women vs. MSM
# 
# sex_raw_results <-  get(load(paste0(BASE.PATH, "sex_raw_results.Rdata")))
# 
# sub <- sex_raw_results["2030", c("female", "msm"), , "incidence", c("noint", "doxy.cov.30"), , drop = FALSE]
# sub <- abind::adrop(sub, drop = c(1, 4))   # drop the year and outcome dims (positions 1 and 5)
# # sub now has dims: sim x intervention x location
# 
# params <- names(PLOT_2_PARAMS)
# 
# result <- array(
#     NA_real_, # could be just "NA", but we may as well do this if we already know it will get coerced to a number
#     dim = c(sex = 2, sim = unname(dim(sub)["sim"]), intervention = 2, location = length(locations), parameter = length(params)),
#     dimnames = list(sex = c("female", "msm"),
#                     sim = NULL,
#                     intervention = c("noint", "doxy.cov.30"),
#                     location = locations,
#                     parameter = params)
# )
# 
# for (loc in locations) {
#     for (p in params) {
#         ord <- sorted_arrs[[loc]][, p]       # sim order for this location/parameter
#         result[, , , loc, p] <- sub[, ord, , loc]
#     }
# }
# 
# # Verify ordering
# all_good <- TRUE
# for (loc in locations) {
#     for (p in params) {
#         ord <- sorted_arrs[[loc]][, p]
#         expected <- unname(sub[, ord, , loc])
#         actual   <- unname(result[, , , loc, p])
#         if (!identical(expected, actual)) {
#             all_good <- FALSE
#             cat("Mismatch at", loc, p, "\n")
#         }
#     }
# }
# all_good
# 
# # Here's the final array with the aggregated values for the plot
# outcome2_agg <-((apply(result["female",,"noint",,], c("sim", "parameter"), sum) -
#                      apply(result["female",,"doxy.cov.30",,], c("sim", "parameter"), sum)) /
#                     apply(result["female",,"noint",,], c("sim", "parameter"), sum)) /
#     ((apply(result["msm",,"noint",,], c("sim", "parameter"), sum) -
#           apply(result["msm",,"doxy.cov.30",,], c("sim", "parameter"), sum)) /
#          apply(result["msm",,"noint",,], c("sim", "parameter"), sum))
# 
# # Save ---
# 
# save(outcome1_agg,
#      file = paste0(BASE.PATH, "outcome1_agg.Rdata"))
# save(outcome2_agg,
#      file = paste0(BASE.PATH, "outcome2_agg.Rdata"))
