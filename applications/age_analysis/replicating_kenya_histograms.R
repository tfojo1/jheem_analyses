# Replicating the Figure 3 of Melissa's Kenya paper
# Using EHE state-level simsets for Georgia
# Andrew 5/22/25

# library(ggplot2)
source("../jheem_analyses/applications/age_analysis/helpers.R")
source("../jheem_analyses/applications/EHE/ehe_specification.R")

#----
# if (!exists("sim")) {
#     sim <- retrieve.simulation.set('ehe', 'GA', 'final.ehe.state', n.sim=100)
# }
# last_sim <- sim$last.sim()
# arr <- last_sim$get(outcomes = 'diagnosed.prevalence',
#                     keep.dimensions = c('year', 'age'),
#                     dimension.values = list(year=c('2025')))
# 
# df <- reshape2::melt(arr)
# 
# # I'll plot thousands of people
# df$value <- df$value / 1000
# 
# # Histogram by age
# age_hist <- ggplot(data=df) + geom_bar(mapping=aes(x=age, y=value), stat='identity')
# 
# # Prettify
# age_hist <- age_hist +
#     ggtitle("Diagnosed Prevalence in 2025 (Georgia)") +
#     ylab("Persons (in thousands)") +
#     xlab("Age Group")
#----
my_states <- c('AL', 'WI', 'FL', 'NY', 'MS', 'LA', 'TX', 'CA', 'MO', 'IL', 'GA')
# my_states <- 'GA'
# cat("Retrieving simsets...")
# simset_list <- lapply(my_states, function(state) {
#     cat("Retrieving a simset...")
#     tryCatch({retrieve.simulation.set('ehe', state, 'final.ehe.state', n.sim=100)},
#              error=function(e) {NULL})
# })
# names(simset_list) <- my_states
# cat("Finished!")
# 
# cat("Running simsets further...")
# noint <- get.null.intervention()
# noint_list <- lapply(simset_list, function(simset) {
#     tryCatch({noint$run(simset, start.year=2025, end.year=2040, verbose=T)},
#              error=function(e) {NULL})
# })
# names(noint_list) <- my_states
# cat("Finished!")
# 
# save(noint_list, file="../jheem_analyses/applications/age_analysis/state_simset_list.Rdata")
noint_list <- get(load("../jheem_analyses/applications/age_analysis/state_simset_list.Rdata"))

all_years=2025:2040
years=c('2025', '2040')
stats_dimnames <- list(metric=c('lower', 'median', 'upper'),year=years)


# Make a vector per state
csv_single_rows <- t(matrix(unlist(lapply(my_states, function(state) {
    prev_data <- noint_list[[state]]$get(outcomes='diagnosed.prevalence',
                                         keep.dimensions=c('year', 'age'),
                                         dimension.values=list(year=years))
    inc_data <- noint_list[[state]]$get(outcomes='incidence',
                                        keep.dimensions=c('year', 'age'),
                                        dimension.values=list(year=years))
    
    # Median Age
    prev_med_age <- get_stats(get_med_age(prev_data))
    inc_med_age <- get_stats(get_med_age(inc_data))
    
    # Number Over 55
    prev_num_over_55 <- get_stats(get_num_over_55(prev_data))
    inc_num_over_55 <- get_stats(get_num_over_55(inc_data))
    
    # Proportion Over 55
    prev_prop_over_55 <- get_stats(get_prop_over_55(prev_data), digits=2)
    inc_prop_over_55 <- get_stats(get_prop_over_55(inc_data), digits=2)
    
    # One row, before being converted to TWO per state
    c(format_med_and_interval(prev_num_over_55),
      format_med_and_interval(inc_num_over_55),
      format_med_and_interval(prev_prop_over_55, is.percentage = T),
      format_med_and_interval(inc_prop_over_55, is.percentage = T),
      format_med_and_interval(prev_med_age),
      format_med_and_interval(inc_med_age))
})), ncol=length(my_states)))

# Prepare for table formatting where median and CI are stacked vertically
csv_double_rows <- convert_to_double_rows(csv_single_rows)

# Save to CSV
write.table(csv_double_rows, file="../jheem_analyses/applications/age_analysis/prelim_table.csv", sep=",", row.names=F, col.names=F)
          