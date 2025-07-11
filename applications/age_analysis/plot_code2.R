library("scales")
library(locations)
library(tidyverse)

my_palette = ggsci::pal_jama()
data_just_age <- Reduce(rbind, lapply(final_states, function(state) {
    arr <- get_stats(noint_list[[state]]$get(outcomes='diagnosed.prevalence',
                                             keep.dimensions=c('year', 'age'),
                                             dimension.values=list(year=2025:2040)),
                     keep.dimensions=c('year', 'age'))
    reshape2::melt(arr) %>%
        bind_cols(state=state) %>%
        pivot_wider(names_from="metric") %>%
        group_by(year) %>%
        mutate(percentage=100*median/sum(median)) %>%
        ungroup() %>%
        mutate(stateName = get.location.name(state))
    
}))

data_age_race <- Reduce(rbind, lapply(final_states, function(state) {
    arr <- get_stats(noint_list[[state]]$get(outcomes='diagnosed.prevalence',
                                             keep.dimensions=c('year', 'age', 'race'),
                                             dimension.values=list(year=2025:2040)),
                     keep.dimensions=c('year', 'age', 'race'))
    reshape2::melt(arr) %>%
        bind_cols(state=state) %>%
        pivot_wider(names_from="metric") %>%
        group_by(year, race) %>%
        mutate(percentage=100*median/sum(median)) %>%
        ungroup() %>%
        mutate(stateName = get.location.name(state))
}))

make_race_prev_plot <- function(states) {
    ggplot(data=filter(data_age_race, state%in%states), aes(x=year, fill=age)) +
        geom_bar(mapping=aes(y=median), stat='identity', position=position_stack(reverse = T)) +
        facet_wrap(vars(race, stateName)) +
        ggtitle(paste0("Diagnosed Prevalence")) +
        scale_fill_manual(values=my_palette(length(unique(data_just_age$age)))) +
        scale_y_continuous(labels=comma) +
        guides(fill = guide_legend(reverse=T)) +
        labs(y = "cases", x = "Year", fill="Age") +
        theme_bw()
}
make_race_prev_plot(final_states)

make_race_prop_plot <- function(states) {
    ggplot(data=filter(data_age_race, state%in%states), aes(x=year, fill=age)) +
        geom_bar(mapping=aes(y=percentage), stat='identity', position=position_stack(reverse = T)) +
        facet_wrap(vars(race, stateName)) +
        ggtitle(paste0("Diagnosed Prevalence")) +
        scale_fill_manual(values=my_palette(length(unique(data_just_age$age)))) +
        scale_y_continuous(labels=comma) +
        guides(fill = guide_legend(reverse=T)) +
        labs(y = "cases", x = "Year", fill="Age") +
        theme_bw()
}
make_race_prop_plot(final_states)

median_age_data <- Reduce(rbind, lapply(final_states, function(state) {
    arr <- noint_list[[state]]$get(outcomes='diagnosed.prevalence',
                                   keep.dimensions=c('year', 'age'),
                                   dimension.values=list(year=2025:2040))
    med_age_arr <- get_stats(get_med_age(arr))
    df <- reshape2::melt(med_age_arr) %>%
        bind_cols(state=state) %>%
        mutate(year=as.factor(year)) %>%
        pivot_wider(names_from="metric") %>%
        mutate(stateName = get.location.name(state))
}))

median_age_population_data <- Reduce(rbind, lapply(final_states, function(state) {
    arr <- noint_list[[state]]$get(outcomes='population',
                                   keep.dimensions=c('year', 'age'),
                                   dimension.values=list(year=2025:2040))
    med_age_arr <- get_stats(get_med_age(arr))
    df <- reshape2::melt(med_age_arr) %>%
        bind_cols(state=state) %>%
        mutate(year=as.factor(year)) %>%
        pivot_wider(names_from="metric") %>%
        mutate(stateName = get.location.name(state))
}))

median_age_race_data <- Reduce(rbind, lapply(final_states, function(state) {
    arr <- noint_list[[state]]$get(outcomes='diagnosed.prevalence',
                                   keep.dimensions=c('year', 'age', 'race'),
                                   dimension.values=list(year=2025:2040))
    med_age_arr <- get_stats(get_med_age(arr, keep.dimensions = c('year', 'race')), keep.dimensions = c('year', 'race'))
    df <- reshape2::melt(med_age_arr) %>%
        bind_cols(state=state) %>%
        mutate(year=as.factor(year)) %>%
        pivot_wider(names_from="metric") %>%
        mutate(stateName = get.location.name(state))
}))

make_prev_plot <- function(states) {
    ggplot(data=filter(data_just_age, state%in%states), aes(x=year, fill=age)) +
        geom_bar(mapping=aes(y=median), stat='identity', position=position_stack(reverse = T)) +
        facet_wrap(vars(stateName)) +
        ggtitle(paste0("Diagnosed Prevalence")) +
        scale_fill_manual(values=my_palette(length(unique(data_just_age$age)))) +
        scale_y_continuous(labels=comma) +
        guides(fill = guide_legend(reverse=T)) +
        labs(y = "cases", x = "Year", fill="Age") +
        theme_bw()
}

make_prev_plot(final_states)

make_med_age_population_timeline <- function(states) {
    ggplot(data=filter(median_age_population_data, state%in%states), aes(x=year)) +
        geom_point(mapping=aes(y=median)) +
        facet_wrap(vars(stateName)) +
        geom_errorbar(mapping=aes(ymin=lower, ymax=upper, width = 0.5)) +
        ggtitle(paste0("Median Age")) +
        labs(y="Age", x= "Year") +
        theme_bw() +
        scale_x_discrete(breaks=seq(2025, 2040, by=5)) +
        scale_y_continuous(limits=c(0, NA))
}
make_med_age_population_timeline(final_states)

# To improve appearance:
# - Change color palette -> DONE
# - Format y axis numbers to have commas for thousand -> DONE
# - reverse age group order in legend -> DONE
# - change y axis title to "cases" -> DONE

# Next, make a plot showing median age over time.

make_med_age_timeline <- function(states) {
    ggplot(data=filter(median_age_data, state%in%states), aes(x=year)) +
        geom_point(mapping=aes(y=median)) +
        facet_wrap(vars(stateName)) +
        geom_errorbar(mapping=aes(ymin=lower, ymax=upper, width = 0.5)) +
        ggtitle(paste0("Median Age")) +
        labs(y="Age", x= "Year") +
        theme_bw() +
        scale_x_discrete(breaks=seq(2025, 2040, by=5)) +
        scale_y_continuous(limits=c(0, NA))
}
make_med_age_timeline(final_states)

make_med_age_race_timeline <- function(states) {
    ggplot(data=filter(median_age_race_data, state%in%states), aes(x=year, color=race)) +
        geom_point(mapping=aes(y=median)) +
        facet_wrap(vars(race, stateName)) +
        geom_errorbar(mapping=aes(ymin=lower, ymax=upper, width = 0.5)) +
        ggtitle(paste0("Median Age")) +
        labs(y="Age", x= "Year") +
        theme_bw() +
        scale_x_discrete(breaks=seq(2025, 2040, by=5)) +
        scale_y_continuous(limits=c(0, NA)) +
        scale_color_manual(values=my_palette(length(unique(median_age_race_data$race))))
}
make_med_age_race_timeline(final_states)

ss <- do.call(ggpubr::ggarrange, lapply(final_states, function(state) {
    rv <- simplot(noint_list[[state]], outcomes=c('diagnosed.prevalence', 'new'), dimension.values = list(year=2015:2040)) +
        guides(linetype = "none") + guides(fill="none") + guides(shape="none") +
        theme_bw()
}))

ss_age <- do.call(ggpubr::ggarrange, c(lapply(final_states, function(state) {
    rv <- simplot(noint_list[[state]], outcomes=c('diagnosed.prevalence', 'new'), dimension.values = list(year=2015:2040), split.by='age',
                  summary.type = 'median.and.interval') +
        guides(linetype = "none") + guides(fill="none") + guides(shape="none") +
        theme_bw()
}), list(common.legend=T)))
