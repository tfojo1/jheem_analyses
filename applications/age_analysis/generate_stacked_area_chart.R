# The stacked area chart shows diagnosed prevalence by age group,
# faceted by state.

library(tidyverse)
library("scales")
library(locations)

# Source necessary files
source("../jheem_analyses/applications/age_analysis/helpers.R")

# Source necessary objects ----
load("../jheem_analyses/applications/age_analysis/Rdata Objects/total_results.Rdata")
load("../jheem_analyses/applications/age_analysis/Rdata Objects/age_results.Rdata")

load(file="../jheem_analyses/applications/age_analysis/Rdata Objects/state_order.Rdata")
load(file="../jheem_analyses/applications/age_analysis/Rdata Objects/state_order_names.Rdata")

# Stacked Area Plots----
stacked_area_total_arr <- apply(age_results[as.character(2025:2040),,,"diagnosed.prevalence",,], c("year", "age", "sim"), sum)
stacked_area_raw_arr <- array(c(age_results[as.character(2025:2040),,,"diagnosed.prevalence",,],
                                stacked_area_total_arr),
                              dim=c(year=length(2025:2040), age=5, sim=1000, location=length(state_order)),
                              dimnames=list(year=2025:2040, age=dimnames(age_results)[["age"]], sim=1:1000, location=c(my_states, "total")))

my_states <- setdiff(sort(state_order), "total")

stacked_area_plot_data <- reshape2::melt(get_stats(stacked_area_raw_arr,
                                                   keep.dimensions = c('year', 'age', 'location'))) %>%
    pivot_wider(names_from="metric") %>%
    group_by(year, location) %>%
    mutate(percentage=100*median/sum(median)) %>%
    ungroup() %>%
    mutate(stateName = factor(get.location.name(location), levels=c(state_order_names[my_states], total="Total")))
stacked_area_plot_data[stacked_area_plot_data$location == "total", "stateName"] <- "Total"

# Use "absolute", but have option to show by percentage instead.
my_palette <- ggsci::pal_jama()
make_stacked_area_plots <- function(states, type=c("absolute", "percentage")[1]) {
    y_param <- if (type=="absolute") "median" else "percentage"
    ggplot(data=filter(stacked_area_plot_data, location%in%states), aes(x=year, fill=age)) +
        geom_bar(mapping=aes(y=!!(sym(y_param))), stat='identity', position=position_stack(reverse = T)) +
        facet_wrap(vars(stateName), scales="free_y") +
        # ggtitle(paste0("Diagnosed Prevalence")) +
        scale_fill_manual(values=my_palette(length(unique(stacked_area_plot_data$age)))) +
        scale_y_continuous(labels=comma) +
        guides(fill = guide_legend(reverse=T)) +
        labs(y = "Cases", x = "Year", fill="Age") +
        theme_bw() +
        theme(legend.position="bottom")
}

plot <- make_stacked_area_plots(c(my_states, "total"))
ggsave(filename = "../jheem_analyses/applications/age_analysis/stacked_area_chart.png",
       plot=plot,width = 10, height=10)

# Do I want this saved in a particular size? Number of pixels? ?