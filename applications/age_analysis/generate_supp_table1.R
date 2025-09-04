# Supp Table 1 all in one!
# This table has columns:
# Total prevalence, num 55+, and num 65+

library(tidyverse)

# Source necessary files
source("../jheem_analyses/applications/age_analysis/helpers.R")
source("../jheem_analyses/presentation/make_pretty_table.R")

# Source necessary objects ----
load("../jheem_analyses/applications/age_analysis/Rdata Objects/total_results.Rdata")
load("../jheem_analyses/applications/age_analysis/Rdata Objects/age_results.Rdata")
load("../jheem_analyses/applications/age_analysis/Rdata Objects/sixty_five_plus_estimates.Rdata")
sixty_five_plus_estimates <- apply(sixty_five_plus_estimates, c("", "year", "sim", "location"), function(x) {x})

load(file="../jheem_analyses/applications/age_analysis/Rdata Objects/state_order.Rdata")
load(file="../jheem_analyses/applications/age_analysis/Rdata Objects/state_order_names.Rdata")

do_conversion_operations <- function(df, is.percentage=F) {
    rv <-  df %>%
        mutate(stats=paste(mean, lower, upper)) %>%
        select(year, location, stats) %>%
        pivot_wider(names_from=year, values_from = stats) %>%
        separate_wider_delim(`2025`, delim=" ", names=c("mean25", "lo25", "up25")) %>%
        separate_wider_delim(`2040`, delim=" ", names=c("mean40", "lo40", "up40"))
    if (is.percentage) {
        rv <- rv %>%
            mutate(up25 = paste0(up25, "%")) %>%
            mutate(up40 = paste0(up40, "%")) %>%
            mutate(mean25 = paste0(mean25, "%")) %>%
            mutate(mean40 = paste0(mean40, "%"))
    }
    rv <- rv[match(state_order, rv$location),] %>%
        mutate(int25 = paste0("[", lo25, " to ", up25, "]")) %>%
        mutate(int40 = paste0("[", lo40, " to ", up40, "]")) %>%
        select(mean25, int25, mean40, int40) %>%
        mutate(across(everything(), ~prettyNum(., big.mark=",", preserve.width = "none")))
}
do_delta_conversion_operations <- function(df, is.percentage=F) {
    rv <- df
    if (is.percentage) rv <- rv %>% mutate(upper = paste0(upper, "%"), mean = paste0(mean, "%"))
    rv <- rv[match(state_order, rv$location),] %>%
        mutate(interval = paste0("[", lower, " to ", upper, "]")) %>%
        select(mean, interval) %>%
        mutate(across(everything(), ~prettyNum(., big.mark=",", preserve.width = "none")))
}

do_total_conversion_operations <- function(df) {
    # Assume we're already in the correct order in this case
    rv <- df %>%
        mutate(ci = paste0("[", lower, " to ", upper, "]")) %>%
        select(mean, ci) %>%
        mutate(across(everything(), ~prettyNum(., big.mark=",", preserve.width = "none")))
}

# Total prev ----
total_prev_by_loc <- get_stats(total_results["2025",,"diagnosed.prevalence",,], c("location"))
total_prev_by_loc <- total_prev_by_loc[,names(sort(total_prev_by_loc["mean",], decreasing=T))]
# Must keep year in to avoid annoying issues
total_total_prev <- get_stats(apply(total_results["2025",,"diagnosed.prevalence",,,drop=F], c("year", "sim"), sum))
total_prev_df <- reshape2::melt(total_prev_by_loc) %>%
    rbind(reshape2::melt(total_total_prev) %>%
              mutate(location="Total") %>%
              select(-year)) %>%
    pivot_wider(names_from="metric")
total_prev_column <- do_total_conversion_operations(total_prev_df)

# Num 55+ ----

num_totals <- apply(age_results[c("2025", "2040"),,,"diagnosed.prevalence",,], c("year", "age", "sim"), sum)
num_raw_arr <- array(c(age_results[c("2025", "2040"),,,"diagnosed.prevalence",,],
                       num_totals),
                     dim=c(year=2, age=5, sim=1000, location=length(state_order_names)),
                     dimnames=list(year=c("2025", "2040"), age=dimnames(age_results)[["age"]], sim=1:1000, location=c(my_states, "total")))
num_data <- get_num_over_55(num_raw_arr,
                            keep.dimensions=c("year", "location"))
num_delta <- num_data["2040",,] - num_data["2025",,]

table_num_over_55 <- cbind(do_conversion_operations(reshape2::melt(get_stats(num_data,
                                                                             keep.dimensions=c("year", "location"))) %>%
                                                        pivot_wider(names_from="metric") %>%
                                                        mutate(stateName=get.location.name(location))),
                           do_delta_conversion_operations(reshape2::melt(get_stats(num_delta,
                                                                                   keep.dimensions="location")) %>%
                                                              pivot_wider(names_from="metric") %>%
                                                              mutate(stateName=get.location.name(location))))

# Num 65+ ----

over_65_totals <- apply(sixty_five_plus_estimates["65_plus",,,], c("year", "sim"), sum)
under_65_totals <- apply(sixty_five_plus_estimates["under_65",,,], c("year", "sim"), sum)

num_65_raw_arr <- array(c(sixty_five_plus_estimates["65_plus",,,],
                          over_65_totals),
                        dim=c(year=2, sim=1000, location=length(state_order_names)), # WRONG ORDER!
                        dimnames=list(year=c("2025", "2040"), sim=1:1000, location=c(my_states, "total")))
num_65_delta <- num_65_raw_arr["2040",,] - num_65_raw_arr["2025",,]

table_num_over_65 <- cbind(do_conversion_operations(reshape2::melt(get_stats(num_65_raw_arr,
                                                                             keep.dimensions=c("year", "location"))) %>%
                                                        pivot_wider(names_from="metric") %>%
                                                        mutate(stateName=get.location.name(location))),
                           do_delta_conversion_operations(reshape2::melt(get_stats(num_65_delta,
                                                                                   keep.dimensions="location")) %>%
                                                              pivot_wider(names_from="metric") %>%
                                                              mutate(stateName=get.location.name(location))))

# Combine to make full table ----
csv_double_rows <- convert_to_double_rows(as.matrix(cbind(total_prev_column, table_num_over_55, table_num_over_65)))

write.table(csv_double_rows, file="../jheem_analyses/applications/age_analysis/supp_table1.csv", sep=",", row.names=F, col.names=F)
# save(csv_double_rows, file="../jheem_analyses/applications/age_analysis/table1.Rdata")