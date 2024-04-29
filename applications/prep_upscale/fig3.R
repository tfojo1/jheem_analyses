source("systematic_interventions.R")

results_df <- results_merge
library(ggplot2)
library(dplyr)

loc_colors <- c(
  "Houston" = "#6BAED6",
  "Miami" = "#FF7F0E",
  "Chicago" = "#9467BD"
)


ggplot(results_df) +
  geom_line(aes(x = year, y = bh_bl_ir, group = loc, color = "Baseline"),
            linewidth = 3.5) +
  geom_point(aes(x = year, y = bh_bl_ir, group = loc, color = 'Baseline'),
             size = 7) +
  geom_line(aes(x = year, y = bh_ir, color = loc, group = loc),
            linewidth = 3.5) +
  geom_point(aes(x = year, y = bh_ir, color = loc, group = loc),
             size = 7) +
  facet_wrap(~loc, dir = "v")+
  labs(
    title = "Incidence Rate among Black/Hispanic MSM",
    subtitle = "40% Additional PrEP Uptake, 80% PrEP Persistence",
    x = "Year",
    y = "Incidence Rate (per 100,000 population)",
    color = "Location"
    # caption = "The black line refers to the baseline incidence rate."
  ) +
  scale_color_manual(values = c("Baseline" = "grey4", loc_colors),
                     name = "Location") +
  theme_minimal() +
  guides(color = "none") +
  # increase font size
  theme(text = element_text(size = 22),
        # increase facet font size 
        strip.text = element_text(size = 28)) -> p1
#   
# 
# ggplot(results_df) +
#   geom_line(aes(x = year, y = bh_bl_ir, group = loc, color = "Baseline")) +
#   geom_point(aes(x = year, y = bh_bl_ir, group = loc, color = 'Baseline')) +
#   geom_line(aes(x = year, y = bh_ir, color = "Max Intervention", group = loc)) +
#   geom_point(aes(x = year, y = bh_ir, color = "Max Intervention", group = loc)) +
#   geom_area(aes(x = year, y = upper_bh_ir, group = loc, fill = "95% Confidence Interval"), alpha = 0.2) +
#   geom_area(aes(x = year, y = lower_bh_ir, group = loc), alpha = 0.2, fill = "grey") +
#   facet_wrap(~loc, dir = "v") +
#   labs(
#     title = "Incidence Rate among Black/Hispanic MSM by Location",
#     subtitle = "40% Additional PrEP Uptake, 80% PrEP Persistence",
#     x = "Year",
#     y = "Incidence Rate (per 100,000 population)",
#     color = "Location"
#   ) +
#   scale_color_manual(values = c("Baseline" = "grey4", "Max Intervention" = "red3"),
#                      name = "Type") +
#   scale_fill_manual(values = c("95% Confidence Interval"="grey")) +
#   theme_minimal()




ggplot(results_df) +
  geom_line(aes(x = year, y = bl_irr, group = loc, color = "Baseline"),
            linewidth = 3.5) +
  geom_point(aes(x = year, y = bl_irr, group = loc, color = "Baseline"),
             size = 7) +
  geom_line(aes(x = year, y = irr.x, color = loc, group = loc),
            linewidth = 3.5) +
  geom_point(aes(x = year, y = irr.x, color = loc, group = loc),
             size = 7) +
  facet_wrap(~loc, dir = "v") +
  labs(
    title = "Incidence Rate Ratio",
    subtitle = "40% Additional PrEP Uptake, 80% PrEP Persistence",
    x = "Year",
    y = "Incidence Rate Ratio",
    color = "Location"
    # caption = "Black line refers to the baseline IRR."
  ) +
  scale_color_manual(values = c("Baseline" = "grey4", loc_colors),
                     name = "Location") +
  theme_minimal() +
  guides(color = "none") +
  # increase font size
  theme(text = element_text(size = 22),
        # increase facet font size 
        strip.text = element_text(size = 28)) -> p2

tgt <- cowplot::plot_grid(p1, p2, nrow = 1)

pdf("Fig3_UPDATED.pdf", width = 16.1, height = 12.89)
tgt
dev.off()

