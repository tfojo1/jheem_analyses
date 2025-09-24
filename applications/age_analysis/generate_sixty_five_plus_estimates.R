# Generate 65+ estimates from the results.

source("../jheem_analyses/applications/age_analysis/helpers.R")

load("../jheem_analyses/applications/age_analysis/Rdata Objects/age_results.Rdata")
my_states <- dimnames(age_results)$location

# # This creates an array where the first dimension is the subset - "under_65" and "over_65"
# sixty_five_plus_estimates <- get_65_estimates(age_results[c("2025", "2040"),,,"diagnosed.prevalence",,],
#                                               c("year", "location"),
#                                               top.age=100)
# save(sixty_five_plus_estimates, file = "../jheem_analyses/applications/age_analysis/Rdata Objects/sixty_five_plus_estimates.Rdata")

# Consolidate all threads
all_threads <- lapply(1:25, function(i) {
    filename = paste0("../jheem_analyses/applications/age_analysis/Rdata Objects/sixty_five_plus_estimates/thread_",
                      i, ".Rdata")
    arr_with_sim_last <- get(load(file=filename))
    arr_with_loc_last <- apply(arr_with_sim_last, c("", "year", "sim", "location"), function(x) {x})
})

new_dimnames <- dimnames(all_threads[[1]])
new_dimnames$location <- c(my_states, "total")

sixty_five_plus_estimates <- array(Reduce(`c`, all_threads),
                                   sapply(new_dimnames, length),
                                   new_dimnames)
save(sixty_five_plus_estimates, file="../jheem_analyses/applications/age_analysis/Rdata Objects/sixty_five_plus_estimates.Rdata")
