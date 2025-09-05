source("../jheem_analyses/applications/ehe/ehe_specification.R")

all_states <- c("AL", "AR", "AZ", "CA", "CO", "FL",
                "GA", "IL", "KY", "LA", "MD", "MI",
                "MO", "MS", "NC", "NY", "OH", "OK",
                "SC", "TN", "TX", "VA", "WA", "WI")

cat("Creating simset collection...")
simset_collection <- create.simset.collection('ehe', 'final.ehe.state', all_states, interventions = 'noint', n.sim = 1000)

cat("Getting total-level results...")
total_results = simset_collection$get(outcomes = c('incidence', 'diagnosed.prevalence', 'new',
                                                   'suppression', 'population',
                                                   'sexual.transmission.rates','prep.uptake','testing',
                                                   'infected', 'incidence', 'awareness',
                                                   'immigration', 'emigration'),
                                      dimension.values=list(year=2010:2040),
                                      keep.dimensions=c('year'),
                                      verbose = T)
cat("Getting age-stratified results...")
age_results <- simset_collection$get(outcomes = c('incidence', 'diagnosed.prevalence', 'new',
                                                  'suppression', 'population',
                                                  'sexual.transmission.rates','prep.uptake','testing',
                                                  'infected', 'incidence', 'awareness',
                                                  'immigration', 'emigration'),
                                     dimension.values=list(year=2010:2040),
                                     keep.dimensions=c('year', 'age'),
                                     verbose = T)
cat("Done getting results!")

# SAVE THESE:
total_results_to_save <- apply(total_results, c("year", "outcome", "location"), mean)
age_results_to_save <- apply(age_results, c("year", "age", "outcome", "location"), mean)