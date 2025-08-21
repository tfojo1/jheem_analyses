# Generate 65+ estimates from the results.

source("../jheem_analyses/applications/age_analysis/helpers.R")

load("../jheem_analyses/applications/age_analysis/Rdata Objects/age_results.Rdata")

# This creates an array where the first dimension is the subset - "under_65" and "over_65"
sixty_five_plus_estimates <- get_65_estimates(age_results[c("2025", "2040"),,,"diagnosed.prevalence",,],
                                              c("year", "location"),
                                              top.age=100)
save(sixty_five_plus_estimates, file = "../jheem_analyses/applications/age_analysis/Rdata Objects/sixty_five_plus_estimates.Rdata")
