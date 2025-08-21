# Full Pipeline

# With fresh calibration, run "process_calibration.R" to produce
# "total_results.Rdata",
# "age_results.Rdata",
# "_sex_results.Rdata",
# "race_sex_results.Rdata",
# which are all arrays with multiple outcomes and locations.

# Then produce the 65+ estimates and median age estimates with
# "generate_65_plus_estimates.R" and
# "generate_median_age_estimates.R".
# source("../jheem_analyses/applications/age_analysis/generate_sixty_five_plus_estimates.R")

# Figure out the order of the states according to total prevalence.
# state_order <- c(names(sort(apply(total_results['2025',,"diagnosed.prevalence",,], "location", mean), decreasing = T)), "total")
# state_order_names <- c(get.location.name(state_order[1:(length(state_order)-1)]), total="Total")
# save(state_order, file="../jheem_analyses/applications/age_analysis/Rdata Objects/state_order.Rdata")
# save(state_order_names, file="../jheem_analyses/applications/age_analysis/Rdata Objects/state_order_names.Rdata")

# Make the median age estimates
# source("../jheem_analyses/applications/age_analysis/generate_median_age_estimates.R")