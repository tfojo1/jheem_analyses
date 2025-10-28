#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
THREAD_NUMBER <- as.numeric(args[1])

# Multi-threaded generate median age estimates for sex (MSM vs. HET)

print(paste0("Starting thread ", THREAD_NUMBER, " at ", Sys.time()))

library(tidyverse)
setwd("C:/Users/azalesa1/Documents/JHEEM/code/jheem_analyses")

source("../jheem_analyses/applications/age_analysis/helpers.R")
source('../jheem2/R/tests/source_jheem2_package.R')
load("../jheem_analyses/applications/age_analysis/Rdata Objects/race_sex_results.Rdata")

med_age_timeline_sex_arr <- apply(race_sex_results[as.character(2025:2040),,,,,"diagnosed.prevalence",,],
                                  c("year", "age", "sex", "sim"),
                                  sum)

# One year per thread. Will need 16.
# First do MSM
this_thread_input <- med_age_timeline_sex_arr[,,"msm",][THREAD_NUMBER,,,drop=F]
print(paste0("Beginning median age estimation for MSM at ", Sys.time()))
this_thread_MSM_output <- get_med_age(this_thread_input,
                                      keep.dimensions = c("year"),
                                      report.duration=T)
save(this_thread_MSM_output,
     file = paste0("../jheem_analyses/applications/age_analysis/Rdata Objects/median_age_estimates/sex/MSM_thread_",
                   THREAD_NUMBER, ".Rdata"))
print(paste0("Done with MSM for thread ", THREAD_NUMBER, " at ", Sys.time()))

# Then do heterosexual
this_thread_input <- apply(med_age_timeline_sex_arr[,,c("heterosexual_male", "female"),],
                           c("year", "age", "sim"),
                           sum)[THREAD_NUMBER,,,drop=F]
print(paste0("Beginning median age estimation for heterosexual at ", Sys.time()))
this_thread_HET_output <- get_med_age(this_thread_input,
                                      keep.dimensions = c("year"),
                                      report.duration=T)
save(this_thread_HET_output,
     file = paste0("../jheem_analyses/applications/age_analysis/Rdata Objects/median_age_estimates/sex/HET_thread_",
                   THREAD_NUMBER, ".Rdata"))
print(paste0("Done with thread ", THREAD_NUMBER, " at ", Sys.time()))