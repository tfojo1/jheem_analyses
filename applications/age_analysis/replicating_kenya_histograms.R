# Replicating the Figure 3 of Melissa's Kenya paper
# Using EHE state-level simsets for Georgia
# Andrew 5/22/25

# library(ggplot2)
# library(tidyverse)
# source("../jheem_analyses/applications/age_analysis/helpers.R")
# source("../jheem_analyses/applications/EHE/ehe_specification.R")

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
heavy_states <- c('CA', 'FL', 'GA', 'NY', 'TX')
light_states <- c('AL', 'IL', 'LA', 'MO', 'MS', 'WI')
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
#----
# noint_list <- get(load("../jheem_analyses/applications/age_analysis/state_simset_list.Rdata"))

all_years=2025:2040
years=c('2025', '2040')
# stats_dimnames <- list(metric=c('lower', 'median', 'upper'),year=years)


# Make a vector per state
csv_single_rows <- t(matrix(unlist(lapply(my_states, function(state) {
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
    prev_prop_over_55 <- get_stats(get_prop_over_55(prev_data), digits=2)
    inc_prop_over_55 <- get_stats(get_prop_over_55(inc_data), digits=2)
    
    # One row, before being converted to TWO per state
    this_row <- c(format_med_and_interval(prev_num_over_55),
                  format_med_and_interval(inc_num_over_55),
                  format_med_and_interval(prev_prop_over_55, is.percentage = T),
                  format_med_and_interval(inc_prop_over_55, is.percentage = T),
                  format_med_and_interval(prev_med_age),
                  format_med_and_interval(inc_med_age))
    prettyNum(this_row, big.mark=",", preserve.width = "none")
})), ncol=length(my_states)))

# Prepare for table formatting where median and CI are stacked vertically
csv_double_rows <- convert_to_double_rows(csv_single_rows)

# Save to CSV
write.table(csv_double_rows, file="../jheem_analyses/applications/age_analysis/prelim_table.csv", sep=",", row.names=F, col.names=F)
          
#----
# for GA, sim #36 has oldest median age (54) and #61 has youngest (35).
ga_oldest <- ga_noint$subset(54)
ga_youngest <- ga_noint$subset(61)

# These plots are difficult to interpret...
# To Do:
# add better legend
# label which is new and old
# fix numbers to show commas in table?

# 6/5/2025 The inter-birthday Day

# Make bar plot with error bars of WI ages.

all_data <- Reduce(rbind, lapply(my_states, function(state) {
    arr <- get_stats(noint_list[[state]]$get(outcomes='diagnosed.prevalence',
                                             keep.dimensions=c('year', 'age'),
                                             dimension.values=list(year=years)),
                     keep.dimensions=c('year', 'age'))
    df <- reshape2::melt(arr) %>%
        bind_cols(state=state) %>%
        pivot_wider(names_from="metric")
}))

wi_plot <- ggplot(data=filter(all_data, state=="WI"), aes(x=age, fill=factor(year))) +
    geom_bar(mapping=aes(y=median), position='dodge2', stat='identity') +
    geom_errorbar(mapping = aes(ymin=lower, ymax=upper), position='dodge2') +
    ggtitle("Prevalence in Wisconsin")
ca_plot <- ggplot(data=filter(all_data, state=="CA"), aes(x=age, fill=factor(year))) +
    geom_bar(mapping=aes(y=median), position='dodge2', stat='identity') +
    geom_errorbar(mapping = aes(ymin=lower, ymax=upper), position='dodge2') +
    ggtitle("Prevalence in California")

# What else?
# Think about calibrations modifications?
# transmission rate spline points to keep it going so high in the future?
# likelihood to limit growth of incidence?

# write.shaded.table(tab=csv_double_rows, )

# I'll make a stacked bar plot with x as year (continuous) and y as prev, age stacked.



wi_stack_plot <- ggplot(data=filter(data_all_years, state=="WI"), aes(x=year, fill=age)) +
    geom_bar(mapping=aes(y=median), stat='identity')
make_stack_plot <- function(state_name) {
    ggplot(data=filter(data_all_years, state==state_name), aes(x=year, fill=age)) +
        geom_bar(mapping=aes(y=median), stat='identity') +
        ggtitle(paste0("Prevalence in ", state_name))
}
ca_stack_plot <- make_stack_plot("CA")
stack_plots <- setNames(lapply(my_states, function(state_name) {make_stack_plot(state_name)}),
                        my_states)

heavy_stack_plots <- make_all_stack_plots(heavy_states)
light_stack_plots <- make_all_stack_plots(light_states)

percent_prev <- data_all_years