# Multi-threaded generate sixty-five plus estimates

# DEFINE THREAD NUMBER IN CONSOLE
# THREAD_NUMBER <- 1

print(paste0("Starting thread ", THREAD_NUMBER, " at ", Sys.time()))

library(tidyverse)
source("../jheem_analyses/applications/age_analysis/helpers.R")
source('../jheem2/R/tests/source_jheem2_package.R')
load("../jheem_analyses/applications/age_analysis/Rdata Objects/age_results.Rdata")
load("../jheem_analyses/applications/age_analysis/Rdata Objects/state_order.Rdata")

my_states <- dimnames(age_results)$location

med_age_timeline_total_arr <- apply(age_results[as.character(2025:2040),,,"diagnosed.prevalence",,], c("year", "age", "sim"), sum)
med_age_timeline_raw_arr <- array(c(age_results[as.character(2025:2040),,,"diagnosed.prevalence",,],
                                    med_age_timeline_total_arr),
                                  dim=c(year=length(2025:2040), age=5, sim=1000, location=length(state_order)),
                                  dimnames=list(year=2025:2040, age=dimnames(age_results)[["age"]], sim=1:1000, location=c(my_states, "total")))

# One location per thread. Will need 25... estimated 30 minutes or so each?
this_thread_input <- med_age_timeline_raw_arr[,,,THREAD_NUMBER,drop=F]
print(paste0("Beginning sixty-five plus estimation at ", Sys.time()))
# This creates an array where the first dimension is the subset - "under_65" and "over_65"
this_thread_output <- get_65_estimates(age_results[c("2025", "2040"),,,"diagnosed.prevalence",,],
                                              c("year", "location"),
                                              top.age=100,
                                              report.duration=T)
save(this_thread_output,
     file = paste0("../jheem_analyses/applications/age_analysis/Rdata Objects/sixty_five_plus_estimates/thread_",
                   THREAD_NUMBER, ".Rdata"))
print(paste0("Done with thread ", THREAD_NUMBER, " at ", Sys.time()))

