# Generate Urbanicity
all_states <- c("AL", "AR", "AZ", "CA", "CO", "FL",
                "GA", "IL", "KY", "LA", "MD", "MI",
                "MO", "MS", "NC", "NY", "OH", "OK",
                "SC", "TN", "TX", "VA", "WA", "WI")
source("../jheem_analyses/source_code.R")
source("../jheem_analyses/applications/cdc_testing/urbanicity_calculations.R")
AGING_ANALYSIS_URBANICITY <- get.urbanicity.metric(locations=all_states,
                                                   years=2021,
                                                   outcome="diagnosed.prevalence")
save(AGING_ANALYSIS_URBANICITY,
     file = "../jheem_analyses/applications/age_analysis/Rdata Objects/urbanicity.RData")
