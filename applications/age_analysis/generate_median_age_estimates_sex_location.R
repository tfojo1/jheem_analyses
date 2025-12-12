library(tidyverse)
library(locations)
source("../jheem_analyses/applications/age_analysis/helpers.R")
load("../jheem_analyses/applications/age_analysis/Rdata Objects/race_sex_results.Rdata")

model_arr <- race_sex_results[as.character(2025:2040),,,,,"diagnosed.prevalence",,]

# Consolidate all threads
all_MSM_threads <- lapply(1:16, function(i) {
    filename = paste0("../jheem_analyses/applications/age_analysis/Rdata Objects/median_age_estimates/sex/loc_MSM_thread_",
                      i, ".Rdata")
    arr_with_year_first <- get(load(file=filename))
    arr_with_loc_first <- apply(arr_with_year_first, c("location", "sim", "year"), function(x) {x})
})
all_HET_threads <- lapply(1:16, function(i) {
    filename = paste0("../jheem_analyses/applications/age_analysis/Rdata Objects/median_age_estimates/sex/loc_HET_thread_",
                      i, ".Rdata")
    arr_with_year_first <- get(load(file=filename))
    arr_with_loc_first <- apply(arr_with_year_first, c("location", "sim", "year"), function(x) {x})
})

med_age_timeline_MSM_arr <- apply(array(Reduce(`c`, all_MSM_threads),
                                        dim(model_arr)[c("location", "sim", "year")],
                                        dimnames(model_arr)[c("location", "sim", "year")]),
                                  c("year", "location", "sim"),
                                  function(x) {x})
med_age_timeline_HET_arr <- apply(array(Reduce(`c`, all_HET_threads),
                                        dim(model_arr)[c("location", "sim", "year")],
                                        dimnames(model_arr)[c("location", "sim", "year")]),
                                  c("year", "location", "sim"),
                                  function(x) {x})
med_age_timeline_sex_arr <- array(c(med_age_timeline_MSM_arr, med_age_timeline_HET_arr),
                                  c(dim(model_arr)[c("year", "location", "sim")], sex = 2),
                                  c(dimnames(model_arr)[c("year", "location", "sim")], list(sex = c("msm", "heterosexual"))))
save(med_age_timeline_sex_arr, file="../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_timeline_sex_loc_arr.Rdata")

med_age_delta_sex_arr <- med_age_timeline_sex_arr["2040",,,] - med_age_timeline_sex_arr["2025",,,]
med_age_delta_sex_data <- reshape2::melt(get_stats(med_age_delta_sex_arr,
                                                   keep.dimensions=c("location", "sex"),
                                                   include.mean=T,
                                                   floor = T)) %>%
    pivot_wider(names_from="metric")
save(med_age_delta_sex_data, file="../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_delta_sex_loc_data.Rdata")
med_age_timeline_sex_data <- reshape2::melt(get_stats(med_age_timeline_sex_arr,
                                                      keep.dimensions=c("year", "location", "sex"),
                                                      include.mean=T, include.quartiles=T,
                                                      floor=T)) %>%
    mutate(year=as.factor(year)) %>%
    pivot_wider(names_from="metric")
save(med_age_timeline_sex_data, file="../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_timeline_sex_loc_data.Rdata")
