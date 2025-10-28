library(tidyverse)
library(locations)
library(smplot2)

# Source necessary files
source("../jheem_analyses/applications/age_analysis/helpers.R")

# Source necessary objects ----
load(file="../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_timeline_arr.Rdata")
load(file="../jheem_analyses/applications/age_analysis/Rdata Objects/state_order_names.Rdata")

load("../jheem_analyses/applications/age_analysis/Rdata Objects/total_results.Rdata")
load("../jheem_analyses/applications/age_analysis/Rdata Objects/age_results.Rdata")
load("../jheem_analyses/applications/age_analysis/Rdata Objects/urbanicity.Rdata")
load("../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_delta_data.Rdata")
load("../jheem_analyses/applications/age_analysis/Rdata Objects/prop_delta_data.Rdata")

original_eleven <- c("AL", "CA", "FL", "GA", "IL", "LA", "MO", "MS", "NY", "TX", "WI")
# ONLY_ELEVEN <- F
ONLY_ELEVEN <- T

make_scatterplot <- function(x.outcome, x.lab,
                             y.param="deltaMedAge",
                             show.size.guide = F,
                             show.color.guide = F) {
    plot <- ggplot(data=filter(scatterplot_df, outcome==x.outcome),
                   mapping=aes(y=!!sym(y.param), x=value, label = locationLabel, group = location)) +
        labs(y=element_blank(), x=x.lab) +
        theme_bw() +
        geom_point(aes(size = sqrtPrev25,
                       color = urbanicity)) +
        ggrepel::geom_label_repel() +
        theme(legend.position = "bottom",
              legend.key.spacing.x = unit(0.1, units = "mm"),
              legend.text = element_text(margin = margin(l = 0.1, unit = "mm")),
              )
    if (show.size.guide) {
        plot <- plot + scale_size_continuous(name = "Diagnosed Prevalence in 2025",
                                             breaks = c(1,2,3),
                                             labels = scales::comma(c(1e4, 4e4, 9e4)))
    } else {
        plot <- plot + scale_size_continuous(guide = "none")
    }
    low_color = "yellow"
    high_color = "red"
    if (show.color.guide) {
        plot <- plot + scale_color_gradient(low = low_color,
                                            high = high_color,
                                            name = "Urbanicity Score")
    } else {
        plot <- plot + scale_color_gradient(low = low_color,
                                            high = high_color,
                                            guide = "none")
    }
    plot
}

# delta Prop 55----
if (1==2) {
    prop_data <- get_prop_over_55(age_results[c("2025", "2040"),,,"diagnosed.prevalence",,],
                                  keep.dimensions=c("year", "location"))
    prop_delta <- prop_data["2040",,] - prop_data["2025",,]
    prop_delta_data <- reshape2::melt(get_stats(prop_delta, keep.dimensions = c("location"))) %>%
        filter(metric=="mean") %>%
        select(location, value)
    save(prop_delta_data, file="../jheem_analyses/applications/age_analysis/Rdata Objects/prop_delta_data.Rdata")
}

# Create data frame and generate plots----
deltaProp <- prop_delta_data$value
prev25 <- apply(total_results["2025",,"diagnosed.prevalence",,], "location", mean)
newPerPrev25 <- apply(total_results["2025",,"new",,]/
                          total_results["2025",,"diagnosed.prevalence",,],
                      "location", mean)
prevPerPop25 <- apply(total_results["2025",,"diagnosed.prevalence",,]/
                          total_results["2025",,"population",,],
                      "location", mean)
deltaMedAge <- filter(med_age_delta_data, location!="total")$mean
medAge25 <- filter(med_age_data, location!="total", year=="2025")$mean
medAgeNew <- get(load("../jheem_analyses/applications/age_analysis/Rdata Objects/median_age_estimates/new/med_age_timeline_data.Rdata")) %>%
    filter(location!="total", year=="2025") %>%
    select(mean)

if (ONLY_ELEVEN) {
    location_labels_to_show <- original_eleven
} else {
    location_labels_to_show <- c("CA", "FL", "AL", "OK", "TX")
}

# I did the pivot_longer thing originally because of doing facet_wrap, though it's no longer needed
scatterplot_df <- data.frame(deltaMedAge = deltaMedAge,
                             deltaProp = deltaProp,
                             medAge25 = medAge25,
                             newPerPrev25 = 100 * newPerPrev25,
                             prevPerPop25 = 100 * prevPerPop25,
                             prev25 = prev25,
                             medAgeNew = medAgeNew$mean,
                             urbanicity = AGING_ANALYSIS_URBANICITY,
                             location = factor(names(AGING_ANALYSIS_URBANICITY)),
                             row.names = NULL) %>%
    mutate(sqrtPrev25 = sqrt(prev25) / 100) %>%
    pivot_longer(cols = c("medAge25", "newPerPrev25", "prevPerPop25", "medAgeNew"),
                 names_to = "outcome") %>%
    mutate(locationLabel = ifelse(location %in% location_labels_to_show,
                                  as.character(location), "")) %>%
    # arrange(locationLabel)
    arrange(sqrtPrev25)

if (ONLY_ELEVEN) {
    scatterplot_df <- scatterplot_df %>%
        filter(location %in% original_eleven)
}

plot_metadata <- c(medAge25 = "Median Age Among Adult PWDH in 2025",
                   newPerPrev25 = "New Diagnoses/Diagnosed Prevalence in 2025 (%)",
                   prevPerPop25 = "Diagnosed Prevalence/General Population in 2025 (%)",
                   medAgeNew = "Median Age Among New Diagnoses in 2025")

for (y_var in c("deltaMedAge", "deltaProp")) {
    for (i in seq_along(plot_metadata)) {
        # show_color_guide = names(plot_metadata)[i] == "medAgeNew"
        # show_size_guide = names(plot_metadata)[i] == "prevPerPop25"
        plot <- make_scatterplot(names(plot_metadata)[i],
                                 plot_metadata[i],
                                 y.param = y_var)
                                 # show.size.guide = show_size_guide,
                                 # show.color.guide = show_color_guide)
        # Note: Using width 4, height 3, dpi 300, I can put 2 shrunk to 75% side-by-side in Word
        if (ONLY_ELEVEN) {
            ggsave(paste0("../jheem_analyses/applications/age_analysis/Figures/HIV & Aging/Scatterplot/",y_var,"_",names(plot_metadata)[i],".png"),
                   plot = plot, width = 4, height = 3, dpi=300)
        } else {
            ggsave(paste0("../jheem_analyses/applications/age_analysis/Figures/Manuscript/Scatterplot/",y_var,"_",names(plot_metadata)[i],".png"),
                   plot = plot, width = 4, height = 3, dpi=300)
        }
    }
}

# Correlations----

correlations <- setNames(sapply(names(plot_metadata), function(this_outcome) {
    cor(filter(scatterplot_df, outcome==this_outcome)$deltaMedAge,
        filter(scatterplot_df, outcome==this_outcome)$value,
        method = "spearman")
}),
names(plot_metadata))

sort(correlations**2, decreasing = T)

propCorrelations <- setNames(sapply(names(plot_metadata), function(this_outcome) {
    cor(filter(scatterplot_df, outcome==this_outcome)$deltaProp,
        filter(scatterplot_df, outcome==this_outcome)$value)
}),
names(plot_metadata))

sort(propCorrelations**2, decreasing = T)

# For manual legends----
summary(scatterplot_df$urbanicity) # ranges from 0.5431 to 0.9639
summary((100 * scatterplot_df$sqrtPrev25)^2) # ranges from 7052 to 140514
# I think I have to make a fake plot, then save it and copy it in, shrunk to the proportional size
# Have to save this one wider (5 vs 4) since so made characters
# In paint, I crop it to a height of 150px (from top) and width of 1350px (from left).
size_legend <- make_scatterplot("medAgeNew", "TEST", show.size.guide = T)
if (ONLY_ELEVEN) {
    ggsave("../jheem_analyses/applications/age_analysis/Figures/HIV & Aging/Scatterplot/size_legend.png",
           plot = size_legend, width = 5, height = 3, dpi=300)
} else {
    ggsave("../jheem_analyses/applications/age_analysis/Figures/Manuscript/Scatterplot/size_legend.png",
           plot = size_legend, width = 5, height = 3, dpi=300)
}

color_legend <- make_scatterplot("medAgeNew", "TEST", show.color.guide = T)
# This one I cropped to 200px (from top)
# Actually, I cropped it to 200px (from top), then to 175px (from bottom), and to 1300 (from left)
# And THEN to 1100 (from right)
if (ONLY_ELEVEN) {
    ggsave("../jheem_analyses/applications/age_analysis/Figures/HIV & Aging/Scatterplot/color_legend.png",
           plot = color_legend, width = 5, height = 3, dpi=300)
} else {
    ggsave("../jheem_analyses/applications/age_analysis/Figures/Manuscript/Scatterplot/color_legend.png",
           plot = color_legend, width = 5, height = 3, dpi=300)
}

# And of course, both shrunk to 75% (actually becomes 76% apparently) to match plots.
