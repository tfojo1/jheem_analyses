# Purpose: To process the calibrated simsets for each city by
# loading them from the Q drive, running the intervention,
# and getting results from them.
source("../jheem_analyses/applications/ehe/ehe_specification.R")
source("../jheem_analyses/applications/age_analysis/helpers.R")

all_states <- c("AL", "AR", "AZ", "CA", "CO", "FL",
                "GA", "IL", "KY", "LA", "MD", "MI",
                "MO", "MS", "NC", "NY", "OH", "OK",
                "SC", "TN", "TX", "VA", "WA", "WI")

years <- c("2025", "2040")

cat("Creating simset collection...")
simset_collection <- create.simset.collection('ehe', 'final.ehe.state', all_states, interventions = 'noint', n.sim = 1000)
cat("Running simset collection to 2040...")
simset_collection$run(start.year = 2025, end.year = 2040, verbose=T)
cat("Getting total-level results...")
total_results = simset_collection$get(outcomes = c('incidence', 'diagnosed.prevalence', 'new',
                                                   'suppression', 'population',
                                                   'sexual.transmission.rates','prep.uptake','testing'),
                                      dimension.values=list(year=2010:2040),
                                      keep.dimensions=c('year'),
                                      verbose = T)
save(total_results, file="../jheem_analyses/applications/age_analysis/Rdata Objects/total_results.Rdata")
cat("Getting age-stratified results...")
age_results <- simset_collection$get(outcomes = c('incidence', 'diagnosed.prevalence', 'new',
                                                  'suppression', 'population',
                                                  'sexual.transmission.rates','prep.uptake','testing'),
                                     dimension.values=list(year=2010:2040),
                                     keep.dimensions=c('year', 'age'),
                                     verbose = T)
save(age_results, file="../jheem_analyses/applications/age_analysis/Rdata Objects/age_results.Rdata")
cat("Getting age-race-stratified results...")
race_results <- simset_collection$get(outcomes = c('incidence', 'diagnosed.prevalence', 'new',
                                                   'suppression', 'population', 'infected',
                                                   'sexual.transmission.rates','prep.uptake','testing'),
                                      dimension.values=list(year=2010:2040),
                                      keep.dimensions=c('year', 'age', 'race'),
                                      verbose = T)
save(race_results, file="../jheem_analyses/applications/age_analysis/Rdata Objects/race_results.Rdata")
cat("Getting age-race-sex-stratified results...")
race_sex_results <- simset_collection$get(outcomes = c('incidence', 'diagnosed.prevalence', 'new',
                                                       'suppression', 'population', 'infected',
                                                       'sexual.transmission.rates','prep.uptake','testing'),
                                          dimension.values=list(year=2010:2040),
                                          keep.dimensions=c('year', 'age', 'race', 'sex'),
                                          verbose = T)
save(race_sex_results, file="../jheem_analyses/applications/age_analysis/Rdata Objects/race_sex_results.Rdata")