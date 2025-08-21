
# my_states <- c("AL", "CA", "FL", "GA", "IL", "LA", "MO", "MS", "NY", "TX", "WI")
new_states <- c("AR", "AZ", "CO", "IN", "KY", "MA", "MD", "MI", "NC", "NJ", "NV", "OH", "OK", "PA", "SC", "TN", "VA", "WA")
# NOT: IN, MA
# my_states <- new_states[1:3]

if (1==2) {
    source("../jheem_analyses/applications/EHE/ehe_specification.R")
    simset_list <- lapply(my_states, function(state) {
        cat("Retrieving a simset...")
        tryCatch({retrieve.simulation.set('ehe', state, 'final.ehe.state', n.sim=1000)},
                 error=function(e) {NULL})
    })
    names(simset_list) <- my_states
    cat("Finished!")
    
    cat("Running simsets further...")
    noint <- get.null.intervention()
    noint_list <- lapply(simset_list, function(simset) {
        tryCatch({noint$run(simset, start.year=2025, end.year=2040, verbose=T)},
                 error=function(e) {NULL})
    })
    names(noint_list) <- my_states
    cat("Finished!")
    
    save(noint_list, file=paste0("../jheem_analyses/applications/age_analysis/Rdata Objects/new_state_simset_list_", i, ".Rdata"))
}

noint_list <- get(load("../jheem_analyses/applications/age_analysis/Rdata Objects/state_simset_list.Rdata"))

years=c('2025', '2040')
source("../jheem_analyses/applications/age_analysis/helpers.R")

# Make a vector per state
csv_single_rows <- t(matrix(unlist(lapply(my_states, function(state) {
    cat(state)
    prev_data <- noint_list[[state]]$get(outcomes='diagnosed.prevalence',
                                         keep.dimensions=c('year', 'age'),
                                         dimension.values=list(year=years))
    inc_data <- noint_list[[state]]$get(outcomes='new',
                                        keep.dimensions=c('year', 'age'),
                                        dimension.values=list(year=years))
    
    # Median Age
    prev_med_age <- get_stats(get_med_age(prev_data))
    inc_med_age <- get_stats(get_med_age(inc_data))
    
    # Number Over 55
    prev_num_over_55 <- get_stats(get_num_over_55(prev_data))
    inc_num_over_55 <- get_stats(get_num_over_55(inc_data))
    
    # Proportion Over 55
    prev_prop_over_55 <- get_stats(get_prop_over_55(prev_data))
    inc_prop_over_55 <- get_stats(get_prop_over_55(inc_data))
    
    # One row, before being converted to TWO per state
    this_row <- c(format_med_and_interval(prev_med_age),
                  format_med_and_interval(inc_med_age),
                  format_med_and_interval(prev_num_over_55),
                  format_med_and_interval(inc_num_over_55),
                  format_med_and_interval(prev_prop_over_55, is.percentage = T),
                  format_med_and_interval(inc_prop_over_55, is.percentage = T))
    prettyNum(this_row, big.mark=",", preserve.width = "none")
})), ncol=length(my_states)))

# Reorder from most prevalent cases in 2025 to least
# Make sure to change if the 9th column isn't the right one anymore
csv_single_rows <- csv_single_rows[order(as.numeric(str_remove(csv_single_rows[,9], ",")), decreasing = T),]

# Prepare for table formatting where median and CI are stacked vertically
csv_double_rows <- convert_to_double_rows(csv_single_rows)

# Save to CSV
write.table(csv_double_rows, file="../jheem_analyses/applications/age_analysis/prelim_table.csv", sep=",", row.names=F, col.names=F)

simset_collection <- create.simset.collection('ehe', 'final.ehe.state', my_states, interventions = 'noint', n.sim = 1000)


simset_collection$run(start.year = 2025, end.year = 2040, verbose=T)

total_results = simset_collection$get(outcomes = c('incidence', 'diagnosed.prevalence', 'new',
                                                'suppression', 'population',
                                                'sexual.transmission.rates','prep.uptake','testing'),
                                   dimension.values=list(year=2010:2040),
                                   keep.dimensions=c('year'),
                                   verbose = T)
age_results <- simset_collection$get(outcomes = c('incidence', 'diagnosed.prevalence', 'new',
                                                  'suppression', 'population',
                                                  'sexual.transmission.rates','prep.uptake','testing'),
                                     dimension.values=list(year=2010:2040),
                                     keep.dimensions=c('year', 'age'),
                                     verbose = T)
race_results <- simset_collection$get(outcomes = c('incidence', 'diagnosed.prevalence', 'new',
                                                        'suppression', 'population', 'infected',
                                                        'sexual.transmission.rates','prep.uptake','testing'),
                                           dimension.values=list(year=2010:2040),
                                           keep.dimensions=c('year', 'age', 'race'),
                                           verbose = T)
# Actually risk is IDU, so we don't care here. Wanted sex to get msm
race_sex_results <- simset_collection$get(outcomes = c('incidence', 'diagnosed.prevalence', 'new',
                                                        'suppression', 'population', 'infected',
                                                        'sexual.transmission.rates','prep.uptake','testing'),
                                           dimension.values=list(year=2010:2040),
                                           keep.dimensions=c('year', 'age', 'race', 'sex'),
                                           verbose = T)
race_sex_results <- 
save(total_results, file="../jheem_analyses/applications/age_analysis/total_results.Rdata")
save(age_results, file="../jheem_analyses/applications/age_analysis/age_results.Rdata")
save(race_results, file="../jheem_analyses/applications/age_analysis/race_results.Rdata")
save(race_risk_results, file="../jheem_analyses/applications/age_analysis/race_risk_results.Rdata")
save(race_sex_results, file="../jheem_analyses/applications/age_analysis/race_sex_results.Rdata")

total_results <- get(load("../jheem_analyses/applications/age_analysis/Rdata Objects/total_results.Rdata"))
age_results <- get(load("../jheem_analyses/applications/age_analysis/Rdata Objects/age_results.Rdata"))
