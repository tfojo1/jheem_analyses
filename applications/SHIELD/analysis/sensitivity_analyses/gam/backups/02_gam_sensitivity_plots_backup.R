# GAM-based sensitivity analysis: heatmap of variance shares
# Run 01_gam_sensitivity.R first. Run from the jheem_analyses folder.

library(tidyverse)

source('../jheem_analyses/commoncode/locations_of_interest.R')            # SHIELD.TEN.MSAS
source('../jheem_analyses/applications/SHIELD/shield_specification.R')    # ROOT.DIR

# SETTINGS ----
CALIB.CODE  <- "calib.10.20.stage4"
FILE.SUFFIX <- "_test"          # "_test" to plot the one-MSA test results
BASE.PATH   <- paste0(ROOT.DIR, "/shield/outputs/", CALIB.CODE, "/")
OUT.PATH    <- paste0(BASE.PATH, "sensitivity_analysis_GAM/")
FILL.MAX    <- 50          # color scale runs 0 to FILL.MAX; higher values get the darkest color

results <- get(load(paste0(OUT.PATH, "gam_variance_shares", FILE.SUFFIX, ".Rdata")))

# Order groups by their median share across MSAs and outcomes (largest on top);
# the "All parameters" reference row goes at the bottom
group_order <- results %>%
    filter(group != "All parameters") %>%
    group_by(group) %>%
    summarize(m = median(share_above_null_pct)) %>%
    arrange(m) %>%
    pull(group)

plot_df <- results %>%
    mutate(group   = factor(group, levels = c("All parameters", group_order)),
           outcome = factor(outcome, levels = c("Total", "MSM", "Women", "Women minus MSM")),
           msa     = setNames(names(SHIELD.TEN.MSAS), SHIELD.TEN.MSAS)[msa])

plot <- ggplot(plot_df, aes(x = msa, y = group, fill = share_above_null_pct)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = round(share_above_null_pct)), size = 2.5,
              color = ifelse(plot_df$share_above_null_pct > FILL.MAX / 2, "white", "black")) +
    scale_fill_gradient(low = "#F1F6FB", high = "#08306B", limits = c(0, FILL.MAX),
                        breaks = seq(0, FILL.MAX, 10),
                        labels = c(seq(0, FILL.MAX - 10, 10), paste0(FILL.MAX, "+")),
                        oob = scales::squish,      # values above FILL.MAX get the darkest color
                        name = "% of outcome\nvariance explained") +
    facet_wrap(~ outcome, nrow = 1) +
    labs(x = NULL, y = NULL,
         title = "Parameter groups explaining uncertainty in the 2030 % reduction in incidence",
         subtitle = "First-order variance shares (above chance level); shares overlap and do not sum to 100%") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_blank(),
          legend.position = "right")

ggsave(plot = plot, filename = paste0(OUT.PATH, "gam_variance_shares_heatmap", FILE.SUFFIX, ".png"),
       dpi = 300, width = 18, height = 7)
