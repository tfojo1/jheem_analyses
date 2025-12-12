#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
THREAD_NUMBER <- as.numeric(args[1])

# Multi-threaded generate median age estimates for race
# Difference from original: We'll still stratify by location so that we can make a big 'ol table for supplement.

print(paste0("Starting thread ", THREAD_NUMBER, " at ", Sys.time()))

library(tidyverse)
setwd("C:/Users/azalesa1/Documents/JHEEM/code/jheem_analyses")

source("../jheem_analyses/applications/age_analysis/helpers.R")
source('../jheem2/R/tests/source_jheem2_package.R')
load("../jheem_analyses/applications/age_analysis/Rdata Objects/race_sex_results.Rdata")

med_age_timeline_race_arr <- apply(race_sex_results[as.character(2025:2040),,,,,"diagnosed.prevalence",,],
                                   c("year", "age", "race", "sim", "location"),
                                   sum)

# One year per thread. Will need 16.
# First do Black
this_thread_input <- med_age_timeline_race_arr[,,"black",,][THREAD_NUMBER,,,,drop=F]
print(paste0("Beginning median age estimation for Black at ", Sys.time()))
this_thread_BLACK_output <- get_med_age(this_thread_input,
                                        keep.dimensions = c("year", "location"),
                                        report.duration=T)
save(this_thread_BLACK_output,
     file = paste0("../jheem_analyses/applications/age_analysis/Rdata Objects/median_age_estimates/race/loc_Black_thread_",
                   THREAD_NUMBER, ".Rdata"))
print(paste0("Done with Black for thread ", THREAD_NUMBER, " at ", Sys.time()))

# Next do hispanic
this_thread_input <- med_age_timeline_race_arr[,,"hispanic",,][THREAD_NUMBER,,,,drop=F]
print(paste0("Beginning median age estimation for Hispanic at ", Sys.time()))
this_thread_HISP_output <- get_med_age(this_thread_input,
                                       keep.dimensions = c("year", "location"),
                                       report.duration=T)
save(this_thread_HISP_output,
     file = paste0("../jheem_analyses/applications/age_analysis/Rdata Objects/median_age_estimates/race/loc_HISP_thread_",
                   THREAD_NUMBER, ".Rdata"))
print(paste0("Done with Hispanic for thread ", THREAD_NUMBER, " at ", Sys.time()))

# Finally do Other
this_thread_input <- med_age_timeline_race_arr[,,"other",,][THREAD_NUMBER,,,,drop=F]
print(paste0("Beginning median age estimation for Other at ", Sys.time()))
this_thread_OTHER_output <- get_med_age(this_thread_input,
                                        keep.dimensions = c("year", "location"),
                                        report.duration=T)
save(this_thread_OTHER_output,
     file = paste0("../jheem_analyses/applications/age_analysis/Rdata Objects/median_age_estimates/race/loc_OTHER_thread_",
                   THREAD_NUMBER, ".Rdata"))
print(paste0("Done with thread ", THREAD_NUMBER, " at ", Sys.time()))