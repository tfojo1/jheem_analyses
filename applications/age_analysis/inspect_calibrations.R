# check calibrations
library(tidyverse)

# I'll need the baseline simset data for this.
# Surveillance manager for data
source("../jheem_analyses/source_code.R")

load("../jheem_analyses/applications/age_analysis/Rdata Objects/age_results_baseline.Rdata")
prev_sim <- age_results_baseline[,,,"diagnosed.prevalence",,]
sim_dimnames <- dimnames(apply(prev_sim, c("year", "age", "location"), sum))

prev_data <- SURVEILLANCE.MANAGER$pull(outcome = "diagnosed.prevalence",
                                       keep.dimensions = c("year", "location", "age"),
                                       dimension.values = list(year=as.character(2010:2025),
                                                               location=dimnames(prev_sim)$location),
                                       target.ontology = as.ontology(sim_dimnames,
                                                                     incomplete.dimensions = c("year", "location")))
prev_data_df <- reshape2::melt(prev_data)

my_states <- dimnames(prev_sim)$location

new_sim <- age_results_baseline[,,,"new",,]

new_data <- SURVEILLANCE.MANAGER$pull(outcome = "diagnoses",
                                       keep.dimensions = c("year", "location", "age"),
                                       dimension.values = list(year=as.character(2010:2025),
                                                               location=dimnames(new_sim)$location),
                                       target.ontology = as.ontology(sim_dimnames,
                                                                     incomplete.dimensions = c("year", "location")))
new_data_df <- reshape2::melt(new_data)


# Find mean and interval for sim
prev_sim_summary <- apply(prev_sim, setdiff(names(dim(prev_sim)), "sim"), function(x) {
    c(mean(x, na.rm=T), quantile(x, probs=c(0.025, 1-0.025), na.rm=T))
})
new_dimnames <- c(list(metric = c('mean', 'lower', 'upper')), dimnames(prev_sim_summary)[-1])
dimnames(prev_sim_summary) = new_dimnames
prev_sim_df <- reshape2::melt(prev_sim_summary)
prev_sim_df <- reshape(prev_sim_df, direction='wide', idvar=names(prev_sim_df)[!(names(prev_sim_df) %in% c('metric', 'value'))], timevar='metric')

new_sim_summary <- apply(new_sim, setdiff(names(dim(new_sim)), "sim"), function(x) {
    c(mean(x, na.rm=T), quantile(x, probs=c(0.025, 1-0.025), na.rm=T))
})
new_dimnames <- c(list(metric = c('mean', 'lower', 'upper')), dimnames(new_sim_summary)[-1])
dimnames(new_sim_summary) = new_dimnames
new_sim_df <- reshape2::melt(new_sim_summary)
new_sim_df <- reshape(new_sim_df, direction='wide', idvar=names(new_sim_df)[!(names(new_sim_df) %in% c('metric', 'value'))], timevar='metric')

# Let's check out some states!
# states_to_try <- c("TX", "CA", "GA", "MS", "AL", "TN")
show_calibration <- function(state, outcome) {
    sim_df <- switch(
        outcome,
        "diagnosed.prevalence" = filter(prev_sim_df, location == state),
        "new" = filter(new_sim_df, location == state)
    )
    data_df <- switch(
        outcome,
        "diagnosed.prevalence" = filter(prev_data_df, location == state),
        "new" = filter(new_data_df, location == state)
    )
    title <- paste(state, outcome)
    ggplot() +
        geom_ribbon(data=sim_df,
                    aes(x=year,
                        y=value.mean,
                        ymin = value.lower,
                        ymax = value.upper),
                    outline.type = 'full',
                    alpha = 0.5,
                    fill = "#DF8F44FF") +
        geom_line(data=sim_df,
                  aes(x=year,
                      y=value.mean)) +
        geom_point(data=data_df,
                   aes(x=year,
                       y=value),
                   fill="#79AF97FF",
                   shape = 21) +
        facet_wrap(vars(age), scale="free_y") +
        ggtitle(title)
        
}

for (state in my_states) {
    prev_plot <- show_calibration(state, "diagnosed.prevalence")
    new_plot <- show_calibration(state, "new")
    ggsave(paste0("../jheem_analyses/applications/age_analysis/Calibration Plots/November/", state, "_prev.png"), plot=prev_plot)
    ggsave(paste0("../jheem_analyses/applications/age_analysis/Calibration Plots/November/", state, "_new.png"), plot=new_plot)
}

    