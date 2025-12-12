library(tidyverse)
library(locations)
source("../jheem_analyses/applications/age_analysis/helpers.R")
load("../jheem_analyses/applications/age_analysis/Rdata Objects/race_sex_results.Rdata")

model_arr <- race_sex_results[as.character(2025:2040),,,,,"diagnosed.prevalence",,]

# Consolidate all threads
all_BLACK_threads <- lapply(1:16, function(i) {
    filename = paste0("../jheem_analyses/applications/age_analysis/Rdata Objects/median_age_estimates/race/loc_BLACK_thread_",
                      i, ".Rdata")
    arr_with_year_first <- get(load(file=filename))
    arr_with_loc_first <- apply(arr_with_year_first, c("location", "sim", "year"), function(x) {x})
})
all_HISP_threads <- lapply(1:16, function(i) {
    filename = paste0("../jheem_analyses/applications/age_analysis/Rdata Objects/median_age_estimates/race/loc_HISP_thread_",
                      i, ".Rdata")
    arr_with_year_first <- get(load(file=filename))
    arr_with_loc_first <- apply(arr_with_year_first, c("location", "sim", "year"), function(x) {x})
})
all_OTHER_threads <- lapply(1:16, function(i) {
    filename = paste0("../jheem_analyses/applications/age_analysis/Rdata Objects/median_age_estimates/race/loc_OTHER_thread_",
                      i, ".Rdata")
    arr_with_year_first <- get(load(file=filename))
    arr_with_loc_first <- apply(arr_with_year_first, c("location", "sim", "year"), function(x) {x})
})
stop()
med_age_timeline_BLACK_arr <- apply(array(Reduce(`c`, all_BLACK_threads),
                                          dim(model_arr)[c("location", "sim", "year")],
                                          dimnames(model_arr)[c("location", "sim", "year")]),
                                    c("year", "location", "sim"),
                                    function(x) {x})
med_age_timeline_HISP_arr <- apply(array(Reduce(`c`, all_HISP_threads),
                                         dim(model_arr)[c("location", "sim", "year")],
                                         dimnames(model_arr)[c("location", "sim", "year")]),
                                   c("year", "location", "sim"),
                                   function(x) {x})
med_age_timeline_OTHER_arr <- apply(array(Reduce(`c`, all_OTHER_threads),
                                          dim(model_arr)[c("location", "sim", "year")],
                                          dimnames(model_arr)[c("location", "sim", "year")]),
                                    c("year", "location", "sim"),
                                    function(x) {x})
med_age_timeline_race_arr <- array(c(med_age_timeline_BLACK_arr, med_age_timeline_HISP_arr, med_age_timeline_OTHER_arr),
                                   c(dim(model_arr)[c("year", "location", "sim")], race = 3),
                                   c(dimnames(model_arr)[c("year", "location", "sim")], list(race = c("black", "hispanic", "other"))))
save(med_age_timeline_race_arr, file="../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_timeline_race_loc_arr.Rdata")

med_age_delta_race_arr <- med_age_timeline_race_arr["2040",,,] - med_age_timeline_race_arr["2025",,,]
med_age_delta_race_data <- reshape2::melt(get_stats(med_age_delta_race_arr,
                                                    keep.dimensions=c("location", "race"),
                                                    include.mean=T,
                                                    floor = T)) %>%
    pivot_wider(names_from="metric")
save(med_age_delta_race_data, file="../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_delta_race_loc_data.Rdata")
med_age_timeline_race_data <- reshape2::melt(get_stats(med_age_timeline_race_arr,
                                                       keep.dimensions=c("year", "location", "race"),
                                                       include.mean=T, include.quartiles=T,
                                                       floor=T)) %>%
    mutate(year=as.factor(year)) %>%
    pivot_wider(names_from="metric")
save(med_age_timeline_race_data, file="../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_timeline_race_loc_data.Rdata")
