library(ggplot2)
library("scales")

source("../jheem2/R/PLOTS_simplot.R")

# # Get data manager
# source("../jheem_analyses/source_code.R")
 
state_code <- "TX"
intervention_filename <- paste0("simset_2025-08-26_", state_code, ".Rdata")
baseline_filename <- paste0("simset_baseline_2025-08-27_", state_code,".Rdata")

intervention_simset <- get(load(paste0("../jheem_analyses/applications/age_analysis/Rdata Objects/eleven but again/", intervention_filename)))
baseline_simset <- get(load(paste0("../jheem_analyses/applications/age_analysis/Rdata Objects/eleven but again/", baseline_filename)))

# See all facets at once for inspection,
# but figure will use each individually

# simplot(intervention_simset, baseline_simset,
#         outcomes=c("diagnosed.prevalence", "new"),
#         facet.by = "age",
#         dimension.values = list(year=1990:2040),
#         summary.type = "mean.and.interval")

# Doesn't work because the two simsets overlap, among other things.
# What I'll need to do instead is take the output from the first half of simplot
# and make my own figures with it, which is probably good for formatting
# reasons, too.

outcomes <- c("diagnosed.prevalence", "new")

# Determine colors, shapes, etc.
style_manager <- get.default.style.manager()

# As offered by style_manager$get.data.colors(5) or the sim analog.
color_options <- c(grayish = "#374E55FF",
                   orange = "#DF8F44FF",
                   blue = "#00A1D5FF",
                   red = "#B24745FF",
                   green = "#79AF97FF")

ribbon_color <- color_options["orange"]
data_color <- color_options["green"]
data_shape <- style_manager$get.shapes(1)
data_size <- 2

make_calibration_plot <- function(sim.df, data.df, this.outcome, age.group.num=NULL) {
    
    sim.df <- filter(sim.df, outcome == this.outcome)
    data.df <- filter(data.df, outcome == this.outcome)
    
    if (!is.null(age.group.num)) {
        sim.df <- filter(sim.df, facet.by1 == age_groups[age.group.num])
        data.df <- filter(data.df, facet.by1 == age_groups[age.group.num])
    }
    
    ggplot(mapping = aes(x = year,
                         y = value)) +
        geom_line(data = sim.df) +
        geom_ribbon(data = sim.df,
                    aes(ymin = value.lower,
                        ymax = value.upper,
                        alpha = alpha,
                        linewidth = linewidth),
                    fill = ribbon_color,
                    outline.type = "full") +
        geom_point(data = data.df,
                   shape = data_shape,
                   size = data_size,
                   fill = data_color) +
        theme_bw() +
        guides(alpha = "none",
               linewidth = "none") +
        theme(axis.title.y = element_blank(),
              axis.title.x = element_blank()) +
        scale_y_continuous(labels=comma)
    
}

# Totals ----
prepared_total_plot_data <- prepare.plot(list(baseline_simset = baseline_simset,
                                              intervention_simset = intervention_simset),
                                         outcomes,,
                                         dimension.values = list(year=2010:2040),
                                         summary.type = "mean.and.interval")
total_sim_df <- prepared_total_plot_data$df.sim
total_data_df <- prepared_total_plot_data$df.truth

overlapping_years_total <- intersect(unique(filter(total_sim_df, simset == "baseline_simset")$year),
                                     unique(filter(total_sim_df, simset == "intervention_simset")$year))
total_sim_df <- filter(total_sim_df,
                       !(year %in% overlapping_years_total) | simset == "intervention_simset")

for (outcome in outcomes) {
    plot <- make_calibration_plot(total_sim_df, total_data_df, outcome)
    ggsave(paste0("../jheem_analyses/applications/age_analysis/Calibration Plots/",
                  gsub("\\.", "_", outcome), "_total.png"),
           plot = plot,width = 2, height = 1.2, dpi = 300)
}

# Age stratified ----

age_groups <- c(age1="13-24 years",
                age2="25-34 years",
                age3="35-44 years",
                age4="45-54 years",
                age5="55+ years")


# I can do "prepare.plot()" with a named list of the two simsets,
# along with a vector of the two outcomes.
prepared_plot_data <- prepare.plot(list(baseline_simset = baseline_simset,
                                        intervention_simset = intervention_simset),
                                   outcomes,
                                   facet.by = "age",
                                   dimension.values = list(year=2010:2040),
                                   summary.type = "mean.and.interval")

sim_df <- prepared_plot_data$df.sim
data_df <- prepared_plot_data$df.truth

# Filter out the baseline simset on years where we have both
overlapping_years <- intersect(unique(filter(sim_df, simset == "baseline_simset")$year),
                               unique(filter(sim_df, simset == "intervention_simset")$year))
sim_df <- filter(sim_df,
                 !(year %in% overlapping_years) | simset == "intervention_simset")

for (outcome in outcomes) {
    for (i in seq_along(age_groups)) {
        plot <- make_calibration_plot(sim_df, data_df, outcome, i)
        ggsave(paste0("../jheem_analyses/applications/age_analysis/Calibration Plots/",
                      gsub("\\.", "_", outcome), "_", i, ".png"),
               plot = plot,width = 2, height = 1.2, dpi = 300)
        # Will I paste them into Word at 60%?
    }
}