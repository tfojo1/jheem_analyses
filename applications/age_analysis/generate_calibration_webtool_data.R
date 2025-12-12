library(tidyverse)
source("../jheem_analyses/applications/age_analysis/helpers.R")
load("../jheem_analyses/applications/age_analysis/Rdata Objects/age_results.Rdata")
load("../jheem_analyses/applications/age_analysis/Rdata Objects/age_results_baseline.Rdata")


# Stitch together baseline and intervention simset data from "age_results_baseline" and "age_results"----
temp1 <- age_results_baseline[as.character(2010:2024),,,c("diagnosed.prevalence", "new"),,]
temp2 <- age_results[as.character(2025:2040),,,c("diagnosed.prevalence", "new"),,]
temp_dimnames <- dimnames(temp1)
temp_dimnames$year <- as.character(2010:2040)

# Flip year to the back, concatenate, then flip year to the front again
# Take ~10 seconds because it has over 7 million data points!
stitched_sim <- apply(array(c(apply(temp1, c("age", "sim", "location", "outcome", "year"), function(x) {x}),
                              apply(temp2, c("age", "sim", "location", "outcome", "year"), function(x) {x})),
                            sapply(temp_dimnames[c("age", "sim", "location", "outcome", "year")], length),
                            temp_dimnames[c("age", "sim", "location", "outcome", "year")]),
                      c("year", "age", "sim", "location", "outcome"), function(x) {x})

# Get diagnosed prevalence simulation and calibration data----
prev_sim <- stitched_sim[,,,,"diagnosed.prevalence"]
my_states <- dimnames(prev_sim)$location
sim_dimnames <- dimnames(prev_sim)[c("year", "age", "location")]

prev_data <- SURVEILLANCE.MANAGER$pull(outcome = "diagnosed.prevalence",
                                       keep.dimensions = c("year", "location", "age"),
                                       dimension.values = list(year=as.character(2010:2025),
                                                               location=dimnames(prev_sim)$location),
                                       target.ontology = as.ontology(sim_dimnames,
                                                                     incomplete.dimensions = c("year", "location")))
prev_data_df <- reshape2::melt(prev_data)

total_prev_sim <- apply(stitched_sim[,,,,"diagnosed.prevalence"], c("year", "sim", "location"), sum)

total_prev_data <- SURVEILLANCE.MANAGER$pull(outcome = "diagnosed.prevalence",
                                              keep.dimensions = c("year", "location"),
                                              dimension.values = list(year=as.character(2010:2025),
                                                                      location=dimnames(prev_sim)$location),
                                              target.ontology = as.ontology(sim_dimnames,
                                                                            incomplete.dimensions = c("year", "location")))
total_prev_data_df <- reshape2::melt(total_prev_data)

# Get new diagnoses simulation and calibration data----
new_sim <- stitched_sim[,,,,"new"]

new_data <- SURVEILLANCE.MANAGER$pull(outcome = "diagnoses",
                                      keep.dimensions = c("year", "location", "age"),
                                      dimension.values = list(year=as.character(2010:2025),
                                                              location=dimnames(new_sim)$location),
                                      target.ontology = as.ontology(sim_dimnames,
                                                                    incomplete.dimensions = c("year", "location")))
new_data_df <- reshape2::melt(new_data)

total_new_sim <- apply(stitched_sim[,,,,"new"], c("year", "sim", "location"), sum)
total_new_data <- SURVEILLANCE.MANAGER$pull(outcome = "diagnoses",
                                             keep.dimensions = c("year", "location"),
                                             dimension.values = list(year=as.character(2010:2025),
                                                                     location=dimnames(new_sim)$location),
                                             target.ontology = as.ontology(sim_dimnames,
                                                                           incomplete.dimensions = c("year", "location")))
total_new_data_df <- reshape2::melt(total_new_data)

# Get statistics----
prev_sim_stats <- reshape2::melt(get_stats(prev_sim, c("year", "age", "location"),include.mean = T,include.quartiles = T)) %>%
    pivot_wider(names_from = metric, values_from = value)
new_sim_stats <- reshape2::melt(get_stats(new_sim, c("year", "age", "location"),include.mean = T,include.quartiles = T)) %>%
    pivot_wider(names_from = metric, values_from = value)
total_prev_sim_stats <- reshape2::melt(get_stats(total_prev_sim, c("year", "location"),include.mean = T,include.quartiles = T)) %>%
    pivot_wider(names_from = metric, values_from = value)
total_new_sim_stats <- reshape2::melt(get_stats(total_new_sim, c("year", "location"),include.mean = T,include.quartiles = T)) %>%
    pivot_wider(names_from = metric, values_from = value)

# Plot for Sanity Check----
test_state <- c("CA")

make_calib_plot <- function(sim.df, truth.df, is.age) {
    plot <- ggplot() +
        geom_ribbon(data=sim.df,
                    aes(x=year,
                        y=mean,
                        ymin = lower,
                        ymax = upper),
                    outline.type = 'full',
                    alpha = 0.5,
                    fill = "#DF8F44FF") +
        geom_line(data=sim.df,
                  aes(x=year,
                      y=mean)) +
        geom_point(data=truth.df,
                   aes(x=year,
                       y=value),
                   fill="#79AF97FF",
                   shape = 21)
    if (is.age)
        plot <- plot + facet_wrap(vars(age), scale="free_y")
    plot
}
aa=make_calib_plot(filter(prev_sim_stats, location == "KY"),
                filter(prev_data_df, location == "KY"),
                is.age=T)
bb = make_calib_plot(filter(total_new_sim_stats, location == "KY"),
                     filter(total_new_data_df, location == "KY"),
                     is.age=F)

# Save dataframes----
if (1==2) {
    save(prev_sim_stats, file = "../jheem_analyses/applications/age_analysis/Rdata Objects/Webtool/prevalence_sim_by_age.Rdata")
    save(new_sim_stats, file = "../jheem_analyses/applications/age_analysis/Rdata Objects/Webtool/diagnoses_sim_by_age.Rdata")
    save(total_prev_sim_stats, file = "../jheem_analyses/applications/age_analysis/Rdata Objects/Webtool/prevalence_sim_by_total.Rdata")
    save(total_new_sim_stats, file = "../jheem_analyses/applications/age_analysis/Rdata Objects/Webtool/diagnoses_sim_by_total.Rdata")
    save(prev_data_df, file = "../jheem_analyses/applications/age_analysis/Rdata Objects/Webtool/prevalence_data_by_age.Rdata")
    save(total_prev_data_df, file = "../jheem_analyses/applications/age_analysis/Rdata Objects/Webtool/diagnoses_data_by_age.Rdata")
    save(new_data_df, file = "../jheem_analyses/applications/age_analysis/Rdata Objects/Webtool/prevalence_data_by_total.Rdata")
    save(total_new_data_df, file = "../jheem_analyses/applications/age_analysis/Rdata Objects/Webtool/diagnoses_data_by_total.Rdata")
}
