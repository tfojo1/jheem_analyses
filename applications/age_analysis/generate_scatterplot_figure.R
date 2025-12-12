library(tidyverse)
library(locations)
library(smplot2)
library(ggpubr)

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
load("../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_timeline_data.Rdata")

original_eleven <- c("AL", "CA", "FL", "GA", "IL", "LA", "MO", "MS", "NY", "TX", "WI")
ONLY_ELEVEN <- F
# ONLY_ELEVEN <- T

# old way----
# make_scatterplot <- function(x.outcome, x.lab, tag.letter="TEST",
#                              y.param="deltaMedAge",
#                              show.size.guide = F,
#                              show.color.guide = F) {
#     plot <- ggplot(data=filter(scatterplot_df, outcome==x.outcome),
#                    mapping=aes(y=!!sym(y.param), x=value, label = locationLabel, group = location)) +
#         labs(y=element_blank(), x=x.lab, tag=tag.letter) +
#         theme_bw() +
#         geom_point(aes(size = sqrtPrev25,
#                        fill = urbanicity),
#                    shape = 21,
#                    color = "black",) +
#         ggrepel::geom_label_repel() +
#         theme(legend.position = "bottom",
#               legend.key.spacing.x = unit(0.1, units = "mm"),
#               legend.text = element_text(margin = margin(l = 0.1, unit = "mm")),
#               axis.title.x = element_text(size = 6.5))
#     if (show.size.guide) {
#         plot <- plot + scale_size_continuous(name = "Diagnosed Prevalence in 2025",
#                                              breaks = c(1,2,3),
#                                              labels = scales::comma(c(1e4, 4e4, 9e4)))
#     } else {
#         plot <- plot + scale_size_continuous(guide = "none")
#     }
#     low_color = "yellow"
#     high_color = "red"
#     if (show.color.guide) {
#         plot <- plot + scale_fill_gradient(low = low_color,
#                                             high = high_color,
#                                             name = "Urbanicity Score")
#     } else {
#         plot <- plot + scale_fill_gradient(low = low_color,
#                                             high = high_color,
#                                             guide = "none")
#     }
#     plot
# }

# new way----
make_scatterplot <- function(x.outcome, x.lab, tag.letter="TEST",
                             y.param="deltaMedAge",
                             show.size.guide = F,
                             show.color.guide = F) {
    
    used_ldf <- if (y.param == "deltaMedAge") deltaMedAge_ldf else (deltaProp_ldf)
    
    coordinate_colnames <- names(used_ldf)[sapply(strsplit(names(deltaMedAge_ldf), "_"), function(x) {x.outcome %in% x})]
    ldf <- used_ldf[c("state", coordinate_colnames)]
    colnames(ldf) <- c("state", "x", "y")
    # axis_hjust <- if (x.outcome == "medAge25") 0.8 else 0
    
    rdf <- if (y.param == "deltaMedAge") filter(deltaMedAge_rdf, plot == x.outcome) else filter(deltaProp_rdf, plot == x.outcome)
    
    plot <- ggplot(data=filter(scatterplot_df, outcome==x.outcome),
                   mapping=aes(y=!!sym(y.param), x=value)) +
        labs(y=element_blank(), x=x.lab, tag=tag.letter) +
        theme_bw() +
        geom_point(aes(size = sqrtPrev25,
                       fill = urbanicity),
                   shape = 21,
                   color = "black",) +
        geom_text(aes(label = state,
                      x = x,
                      y = y),
                  data = ldf) +
        geom_label(aes(label = value,
                       x = x,
                       y = y),
                   data = rdf) +
        # ggrepel::geom_label_repel() +
        theme(legend.position = "bottom",
              legend.key.spacing.x = unit(0.1, units = "mm"),
              legend.title = element_text(size = 10),
              legend.text = element_text(margin = margin(l = 0.1, unit = "mm"), size = 8))
    
    # if (x.outcome == "medAge25") {
    #     plot <- plot + theme(axis.title.x = element_text(size = 8))} # no longer doing hjust=0.8 because wrapping
    # else {
    #     plot <- plot + theme(axis.title.x = element_text(size = 8))
    # }
        
    
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
        plot <- plot + scale_fill_gradient(low = low_color,
                                           high = high_color,
                                           name = "Urbanicity Score")
    } else {
        plot <- plot + scale_fill_gradient(low = low_color,
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
medAge25 <- filter(med_age_timeline_data, location!="total", year=="2025")$mean
medAgeNew <- get(load("../jheem_analyses/applications/age_analysis/Rdata Objects/median_age_estimates/new/med_age_timeline_data.Rdata")) %>%
    filter(location!="total", year=="2025") %>%
    select(mean)

if (ONLY_ELEVEN) {
    location_labels_to_show <- original_eleven
} else {
    location_labels_to_show <- c("CA", "FL", "AL", "OK", "TX")
}

deltaMedAge_ldf <- data.frame(state = c("CA", "FL", "AL", "OK", "TX"),
                              medAgeNew_x = c(34.7, 34.9, 32.5, 32.5, 30.2),
                              medAgeNew_y = c(17, 12, -3, -8, 3),
                              medAge25_x = c(54.5, 54, 45, 47, 47),
                              medAge25_y = c(17, 9.5, -3, -8, 5),
                              newPerPrev25_x = c(2.6, 2.25, 3.6, 4.35, 3.75),
                              newPerPrev25_y = c(17, 10, -5, -9, 7),
                              prevPerPop25_x = c(0.49, 0.55, 0.4, 0.18, 0.47),
                              prevPerPop25_y = c(17, 13, -5, -7, 3))

deltaProp_ldf <- data.frame(state = c("CA", "FL", "AL", "OK", "TX"),
                            medAgeNew_x = c(34.8, 34.8, 32.8, 32.8, 30.5),
                            medAgeNew_y = c(17, 13, 1, -2, 13),
                            medAge25_x = c(54.5, 55.4, 44, 47.5, 44.5),
                            medAge25_y = c(17, 14, 2, -1.5, 11),
                            newPerPrev25_x = c(2.6, 2.4, 3.52, 4.4, 3.75),
                            newPerPrev25_y = c(17, 11, -1, -0.6, 12),
                            prevPerPop25_x = c(0.48, 0.68, 0.4, 0.2, 0.48),
                            prevPerPop25_y = c(16, 13, -1, 0, 10))

deltaMedAge_rdf <- data.frame(plot = c("medAgeNew", "medAge25", "newPerPrev25", "prevPerPop25"),
                              value = c("R = 0.59", "R = 0.64", "R = -0.73", "R = 0.59"),
                              x = c(34.8, 55, 2, 0.6),
                              y = c(-5, -5, -5, -5))

deltaProp_rdf <- data.frame(plot = c("medAgeNew", "medAge25", "newPerPrev25", "prevPerPop25"),
                              value = c("R = 0.55", "R = 0.42", "R = -0.66", "R = 0.50"),
                              x = c(34.8, 55, 2, 0.6),
                              y = c(1.6, 1.6, 1.6, 1.6))

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
    arrange(desc(sqrtPrev25))
    # mutate(location = factor(location, levels = rev(arrange(scatterplot_df %>% filter(outcome=="medAge25"), sqrtPrev25)$location)))
    # filter(location == "OK")

if (ONLY_ELEVEN) {
    scatterplot_df <- scatterplot_df %>%
        filter(location %in% original_eleven)
}

plot_metadata <- c(newPerPrev25 = "Ratio of New Diagnoses to\nPrevalence in 2025 (%)",
                   medAge25 = "Median Age Among Adults Living with\nDiagnosed HIV in 2025",
                   medAgeNew = "Median Age Among New Diagnoses\nof HIV in 2025",
                   prevPerPop25 = "HIV Prevalence per\nPopulation in 2025 (%)")

for (y_var in c("deltaMedAge", "deltaProp")) {
# for (y_var in c("deltaMedAge")) {
    for (i in seq_along(plot_metadata)) {
        # show_color_guide = names(plot_metadata)[i] == "medAgeNew"
        # show_size_guide = names(plot_metadata)[i] == "prevPerPop25"
        plot <- make_scatterplot(names(plot_metadata)[i],
                                 plot_metadata[i],
                                 y.param = y_var,
                                 tag.letter = LETTERS[i])
                                 # show.size.guide = show_size_guide,
                                 # show.color.guide = show_color_guide)
        # Note: Using width 4, height 3, dpi 300, I can put 2 shrunk to 75% side-by-side in Word
        if (ONLY_ELEVEN) {
            ggsave(paste0("../jheem_analyses/applications/age_analysis/Figures/HIV & Aging/Scatterplot/Revised Dec 2025/",y_var,"_",names(plot_metadata)[i],".png"),
                   plot = plot, width = 3, height = 2.25, dpi=300)
        } else {
            ggsave(paste0("../jheem_analyses/applications/age_analysis/Figures/Manuscript/Scatterplot/Revised Dec 2025/",y_var,"_",names(plot_metadata)[i],".png"),
                   plot = plot, width = 3, height = 2.25, dpi=300)
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

sort(correlations, decreasing = T)

propCorrelations <- setNames(sapply(names(plot_metadata), function(this_outcome) {
    cor(filter(scatterplot_df, outcome==this_outcome)$deltaProp,
        filter(scatterplot_df, outcome==this_outcome)$value,
        method = "spearman")
}),
names(plot_metadata))

sort(propCorrelations, decreasing = T)

# For manual legends----
summary(scatterplot_df$urbanicity) # ranges from 0.5431 to 0.9639
summary((100 * scatterplot_df$sqrtPrev25)^2) # ranges from 7052 to 140514
# I think I have to make a fake plot, then save it and copy it in, shrunk to the proportional size
# Have to save this one wider (5 vs 4) since so made characters
# In paint, I crop it to a height of 150px (from top) and width of 1350px (from left).
size_legend <- as_ggplot(get_legend(make_scatterplot("medAgeNew", "TEST", show.size.guide = T)))
if (ONLY_ELEVEN) {
    ggsave("../jheem_analyses/applications/age_analysis/Figures/HIV & Aging/Scatterplot/Revised Dec 2025/size_legend.png",
           plot = size_legend, width = 5, height = 3, dpi=300)
} else {
    ggsave("../jheem_analyses/applications/age_analysis/Figures/Manuscript/Scatterplot/Revised Dec 2025/size_legend.png",
           plot = size_legend, width = 7, height = 0.3, dpi=300)
}

color_legend <- as_ggplot(get_legend(make_scatterplot("medAgeNew", "TEST", show.color.guide = T)))
# This one I cropped to 200px (from top)
# Actually, I cropped it to 200px (from top), then to 175px (from bottom), and to 1300 (from left)
# And THEN to 1100 (from right)
if (ONLY_ELEVEN) {
    ggsave("../jheem_analyses/applications/age_analysis/Figures/HIV & Aging/Scatterplot/Revised Dec 2025/color_legend.png",
           plot = color_legend, width = 5, height = 3, dpi=300)
} else {
    ggsave("../jheem_analyses/applications/age_analysis/Figures/Manuscript/Scatterplot/Revised Dec 2025/color_legend.png",
           # plot = color_legend, width = 5, height = 3, dpi=300)
           plot = color_legend, width = 2.5, height = 0.32, dpi=300)
}

# And of course, both shrunk to 75% (actually becomes 76% apparently) to match plots.
