library(tidyverse)
load("../jheem_analyses/applications/age_analysis/Rdata Objects/age_results.Rdata")

my_states <- dimnames(age_results)$location

med_age_timeline_total_arr <- apply(age_results[as.character(2025:2040),,,"diagnosed.prevalence",,], c("year", "age", "sim"), sum)
med_age_timeline_raw_arr <- array(c(age_results[as.character(2025:2040),,,"diagnosed.prevalence",,],
                                    med_age_timeline_total_arr),
                                  dim=c(year=length(2025:2040), age=5, sim=1000, location=length(state_order)),
                                  dimnames=list(year=2025:2040, age=dimnames(age_results)[["age"]], sim=1:1000, location=c(my_states, "total")))
cat("Estimating median ages...")
med_age_timeline_arr <- get_med_age(med_age_timeline_raw_arr,
                                    keep.dimensions=c("year", "location"))
cat("Done!")
save(med_age_timeline_arr, file="../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_timeline_arr.Rdata")
med_age_delta_arr <- med_age_timeline_arr["2040",,] - med_age_timeline_arr["2025",,]
med_age_delta_data <- reshape2::melt(get_stats(med_age_delta_arr,
                                               keep.dimensions=c("location"),
                                               include.mean=T,
                                               floor = T)) %>%
    pivot_wider(names_from="metric") %>%
    mutate(stateName = factor(get.location.name(location), levels=state_order_names))
save(med_age_delta_data, file="../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_delta_data.Rdata")
med_age_timeline_data <- reshape2::melt(get_stats(med_age_timeline_arr,
                                                  keep.dimensions=c("year", "location"),
                                                  include.mean=T, include.quartiles=T,
                                                  floor=T)) %>%
    mutate(year=as.factor(year)) %>%
    pivot_wider(names_from="metric") %>%
    mutate(stateName = factor(get.location.name(location), levels=c(state_order_names)))
med_age_timeline_data[med_age_timeline_data$location=="total", "stateName"] <- "Total"
save(med_age_timeline_data, file="../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_timeline_data.Rdata")
