
data_all_years <- Reduce(rbind, lapply(my_states, function(state) {
    arr <- get_stats(noint_list[[state]]$get(outcomes='diagnosed.prevalence',
                                             keep.dimensions=c('year', 'age'),
                                             dimension.values=list(year=2025:2040)),
                     keep.dimensions=c('year', 'age'))
    df <- reshape2::melt(arr) %>%
        bind_cols(state=state) %>%
        pivot_wider(names_from="metric") %>%
        group_by(year) %>%
        mutate(percentage=100*median/sum(median)) %>%
        ungroup()
        
}))

make_all_stack_plots <- function(states) {
    ggplot(data=filter(data_all_years, state%in%states), aes(x=year, fill=age)) +
        geom_bar(mapping=aes(y=median), stat='identity', position=position_stack(reverse = T)) +
        facet_wrap(vars(state)) +
        ggtitle(paste0("Prevalence"))
}

make_all_perc_stack_plots <- function(states) {
    ggplot(data=filter(data_all_years, state%in%states), aes(x=year, fill=age)) +
        geom_bar(mapping=aes(y=percentage), stat='identity', position=position_stack(reverse = T)) +
        facet_wrap(vars(state)) +
        ggtitle(paste0("Prevalence as Percentage"))
}

data_with_race <- Reduce(rbind, lapply(my_states, function(state) {
    arr <- get_stats(noint_list[[state]]$get(outcomes='diagnosed.prevalence',
                                             keep.dimensions=c('year', 'age', 'race'),
                                             dimension.values=list(year=2025:2040)),
                     keep.dimensions=c('year', 'age', 'race'))
    df <- reshape2::melt(arr) %>%
        bind_cols(state=state) %>%
        separate(age, c("shortAge", "discard"), sep=" ", remove = F) %>%
        pivot_wider(names_from="metric") %>%
        group_by(year, race) %>%
        mutate(percentage=100*median/sum(median))
}))

make_stack_race_two_years <- function(states) {
    ggplot(data=filter(data_with_race, year%in%c(2025,2040), state%in%states), aes(x=shortAge, fill=race)) +
        geom_bar(mapping=aes(y=median), position='dodge2', stat='identity') +
        geom_errorbar(mapping = aes(ymin=lower, ymax=upper), position='dodge2') +
        facet_wrap(vars(state, year)) +
        ggtitle("Prevalence")
}

# How about the plot with the three superimposed distributions?
# That will involve making a smooth line, I guess?
# It'll be faceted by race and have x=age, y=prev.
# The curve data will be the 100 simulations ranked by median age, I guess?

restratified_age_data <- Reduce(rbind, lapply(my_states, function(state) {
    arr <- noint_list[[state]]$get(outcomes='diagnosed.prevalence',
                                             keep.dimensions=c('year', 'age'),
                                             dimension.values=list(year=c(2025,2030, 2035, 2040)))
    restratified_arr <- get_stats(get_restratified_ages(arr), keep.dimensions = c('year', 'age'))
    df <- reshape2::melt(restratified_arr) %>%
        bind_cols(state=state) %>%
        separate(age, c("shortAge", "discard"), sep=" ", remove = F) %>%
        mutate(age=as.numeric(shortAge)) %>%
        select(-shortAge, -discard) %>%
        mutate(year=as.factor(year)) %>%
        pivot_wider(names_from="metric") %>%
        group_by(year) %>%
        mutate(percentage=100*median/sum(median))
}))

make_age_curve <- function(states) {
    ggplot(data=filter(restratified_age_data, state%in%states), aes(x=age, y=percentage)) +
        geom_line(aes(color=year), linewidth=1) +
        facet_wrap(vars(state)) +
        ggtitle("Prevalence")
}

# I've re-ordered the stack plots with "position=position_stack(reverse=T)"
# so that oldest age is on the top like people apparently expect it to be.

# Doing Chi-Square on age/race data.
# I'll take the age/race data and first stat summarize it (get medians).
age_race_arrs <- setNames(lapply(my_states, function(state) {
    get_stats(noint_list[[state]]$get(outcomes='diagnosed.prevalence',
                                      keep.dimensions=c('year', 'age', 'race'),
                                      dimension.values=list(year=2025:2040)),
              keep.dimensions=c('year', 'age', 'race'))
}), my_states)
# Let me try Chi-Square...
# Looks like I want my data in a "table" with just age/race for a year.
# aa <- filter(data_with_race, year==2025, state=="AL") %>% ungroup() %>% select(age, race, median, -year, -state) %>%
#     pivot_longer
aa <- age_race_arrs[[1]]['median','2025',,]

# Method 1: Chi-Sq on medians
sapply(my_states, function(state) {
    aa <- age_race_arrs[[state]]['median', '2040',,]
    tryCatch({chisq.test(aa)$p.value}, warning=function(w) {browser()})
    # MS gets warning... population too small???
})
# The p-values are EXTREMELY small... no point in comparing states

# Method 2: 

# Okay, for now, let me just put the double y-axis on the absolute value prev plots.
# I think this just means a different y-axis for the upper and lower facets
# For facet_grid, I think I'll need to introduce a variable for the rows and cols.
make_stack_multiple_y_axis <- function(states) {
    ggplot(data=filter(data_all_years, state%in%states), aes(x=year, fill=age)) +
        geom_bar(mapping=aes(y=median), stat='identity', position=position_stack(reverse = T)) +
        facet_wrap(vars(state), scales="free_y") +
        ggtitle(paste0("Prevalence"))
}

# Picking two or three most trustworthy states. Do this based on incidence projections.


