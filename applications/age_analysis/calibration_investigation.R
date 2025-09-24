library(ggplot2)
library("scales")
# 
# source("../jheem2/R/PLOTS_simplot.R")

# Get data manager
source("../jheem_analyses/source_code.R")

intervention_simset <- get(load("../jheem_analyses/applications/age_analysis/Rdata Objects/eleven but again/simset_2025-08-26_CA.Rdata"))
baseline_simset <- get(load("../jheem_analyses/applications/age_analysis/Rdata Objects/eleven but again/simset_baseline_2025-08-27_CA.Rdata"))

# outcomes <- c("diagnosed.prevalence", "new", "prep.uptake", "suppression")
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

make_calibration_plot <- function(sim.df, data.df, use.outcome="new") {
    # sim.df <- filter(sim.df, outcome==use.outcome)
    # data.df <- filter(data.df, outcome==use.outcome)
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
        facet_wrap(vars(facet.by1, outcome), scale="free_y") +
        # facet_wrap(vars(facet.by1)) +
        # facet_grid(rows=vars(facet.by2), cols=vars(facet.by1), scale="free_y") +
        theme_bw() +
        guides(alpha = "none",
               linewidth = "none") +
        theme(axis.title.y = element_blank(),
              axis.title.x = element_blank()) +
        scale_y_continuous(labels=comma)
}

# Race stratified ----
# I can do "prepare.plot()" with a named list of the two simsets,
# along with a vector of the two outcomes.
prepared_plot_data <- prepare.plot(list(baseline_simset = baseline_simset,
                                        intervention_simset = intervention_simset),
                                   outcomes,
                                   facet.by = c("age"),
                                   dimension.values = list(year=2010:2040),
                                   summary.type = "mean.and.interval")

sim_df <- prepared_plot_data$df.sim
data_df <- prepared_plot_data$df.truth

# Filter out the baseline simset on years where we have both
overlapping_years <- intersect(unique(filter(sim_df, simset == "baseline_simset")$year),
                               unique(filter(sim_df, simset == "intervention_simset")$year))
sim_df <- filter(sim_df,
                 !(year %in% overlapping_years) | simset == "intervention_simset")

# sim_df <- filter(sim_df, outcome=="new")
# data_df <- filter(data_df, outcome=="new")
make_calibration_plot(sim_df, data_df, "diagnosed.prevalence")
make_calibration_plot(sim_df, data_df, "prep.uptake.proportion")
make_calibration_plot(sim_df, data_df, "suppression")
make_calibration_plot(sim_df, data_df, "new")
