library(tidyverse)
library(locations)

# Source necessary files
source("../jheem_analyses/applications/age_analysis/helpers.R")

# Source necessary objects ----
load(file="../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_timeline_arr.Rdata")
load(file="../jheem_analyses/applications/age_analysis/Rdata Objects/state_order_names.Rdata")

# Find quantiles
med_age_data <- reshape2::melt(get_stats(med_age_timeline_arr[c("2025", "2040"),,],
                                         keep.dimensions = c('year', 'location'),
                                         include.quartiles = T)) %>%
    pivot_wider(names_from = metric, values_from = value) %>%
    mutate(year = factor(year))

# New idea 1: Boxplot of delta median ages, and change axis labels to have 2025 median age.
med_age_delta_data <- reshape2::melt(get_stats(med_age_timeline_arr["2040",,,drop=F] -
                                                   med_age_timeline_arr["2025",,,drop=F],
                                               keep.dimensions = c("year", "location"),
                                               include.quartiles = T)) %>%
    pivot_wider(names_from = metric, values_from = value) %>%
    mutate(year = factor(year)) %>%
    select(-year) %>%
    mutate(isTotal = location=="total")

# # Order by delta median age
# location_ordering <- (med_age_delta_data %>%
#                           arrange(mean))$location
# Order by median age in 2025
location_ordering <- (med_age_data %>%
                          filter(year=="2025") %>%
                          arrange(mean))$location
# location_ordering <- (med_age_data %>%
#                           select(year, location, mean) %>%
#                           pivot_wider(names_from = year, values_from = mean) %>%
#                           mutate(delta = `2040` - `2025`) %>%
#                           arrange(delta))$location
med_age_data <- mutate(med_age_data,
                       location = factor(location,
                                         levels = c(setdiff(location_ordering, "total"),
                                                          "total")))
med_age_delta_data <- mutate(med_age_delta_data,
                             location = factor(location,
                                               levels = c(setdiff(location_ordering, "total"),
                                                          "total")))

formatted_labels <- med_age_data %>%
    filter(year=="2025") %>%
    select(location, mean) %>%
    merge(data.frame(location = names(state_order_names),
                     fullname = state_order_names)) %>%
    mutate(formatted = paste0(fullname, " (", mean, ")"))
formatted_labels <- setNames(formatted_labels$formatted,
                             formatted_labels$location)
# formatted_labels["total"] <- "Total (Median Age, 2025: 51)"
formatted_labels["total"] <- "Total (51)"

# Plot 
plot <- ggplot(med_age_delta_data, mapping=aes(y = location)) +
    geom_boxplot(aes(xmin = lower,
                     xlower = lowermid,
                     xmiddle = median, # actually got some deltaMedAge means outside the box!
                     xupper = uppermid,
                     xmax = upper,
                     fill = isTotal),
                 width = 0.5,
                 linewidth = 0.2,
                 stat="identity",
                 position="dodge") +
                 # fill = "#2171b5") +
    # geom_point(aes(x = mean),
    #            color = "#fd8d3c",
    #            size = 2) +
    scale_fill_manual(values=c("#2171b5","#DF8F44FF"), guide="none") +
    scale_y_discrete(labels = formatted_labels) +
    labs(x = "Change in Median Age Between 2025 and 2040 (years)", y  = element_blank()) +
    theme_bw() + 
    theme(legend.position = "bottom",
          axis.text=element_text(size=7),
          axis.title.x = element_text(size=7, hjust = - 0.1)) +
    geom_vline(xintercept = 0, linetype="dashed")

# Plot 
# plot <- ggplot(med_age_data, mapping=aes(y = location)) +
#     geom_boxplot(aes(xmin = lower,
#                  xlower = lowermid,
#                  xmiddle = median, # actually got some deltaMedAge means outside the box!
#                  xupper = uppermid,
#                  xmax = upper),
#              stat="identity",
#              position="dodge",
#              fill = "#2171b5") +
#     # geom_point(aes(x = mean),
#     #            color = "#fd8d3c",
#     #            size = 2) +
#     scale_y_discrete(labels = state_order_names) +
#     labs(x = "Change in Median Age Between 2025 and 2040 (years)", y  = element_blank()) +
#     theme_bw() + 
#     theme(legend.position = "bottom") +
#     geom_vline(xintercept = 0, linetype="dashed")

ggsave(paste0("../jheem_analyses/applications/age_analysis/abstract_figure.png"),
       plot = plot, width = 4, height = 3, dpi=600)

