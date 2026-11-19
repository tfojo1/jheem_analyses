# GAM sensitivity analysis step 3: heatmaps of variance shares (parameter groups; single parameters)
# Pipeline: step1 parameter correlations -> step2 variance shares -> step3 heatmap
# Run step 2 first. Run from the jheem_analyses folder.

# Settings and paths (test runs: OUT.PATH/test/, from TEST.MSA) come from 00_gam_settings.R
cat("\n==== GAM step 3/3: heatmap of variance shares (started ", format(Sys.time(), "%H:%M:%S"), ") ====\n", sep = "")

source('../jheem_analyses/applications/SHIELD/analysis/sensitivity_analyses/gam/00_gam_settings.R')
# library(tidyverse)
#
# source('../jheem_analyses/commoncode/locations_of_interest.R')            # SHIELD.TEN.MSAS
# source('../jheem_analyses/applications/SHIELD/shield_specification.R')    # ROOT.DIR

# SETTINGS ----
# CALIB.CODE  <- "calib.10.20.stage4"
# FILE.SUFFIX <- "_test"          # "_test" to plot the one-MSA test results
# BASE.PATH   <- paste0(ROOT.DIR, "/shield/outputs/", CALIB.CODE, "/")
# OUT.PATH    <- paste0(BASE.PATH, "sensitivity_analysis_GAM/")
FILL.MAX    <- 50          # color scale runs 0 to FILL.MAX; higher values get the darkest color

results <- get(load(paste0(OUT.PATH, "gam_variance_shares.Rdata")))

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
           outcome = factor(outcome, levels = GAM_OUTCOMES),
           # outcome = factor(outcome, levels = c("Total", "MSM", "Women", "Women minus MSM")),
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

ggsave(plot = plot, filename = paste0(OUT.PATH, "gam_variance_shares_heatmap.png"),
       dpi = 300, width = 22, height = 7)
#        dpi = 300, width = 18, height = 7)

# SINGLE-PARAMETER FIGURE ----
# Top TOP_N_SINGLE parameters per outcome (ranked by median share across the MSAs analysed),
# share of outcome variance explained by each parameter on its own. Single shares overlap
# when parameters are correlated, so they cannot be added; chance level is about 1%.
single_file <- paste0(OUT.PATH, "gam_single_param_shares.Rdata")
if (!file.exists(single_file)) {
    cat("No single-parameter results (RUN.SINGLE.PARAMS was FALSE in step 2): figure skipped\n")
} else {
    single <- get(load(single_file))

    top_single <- single %>%
        group_by(outcome, parameter, group) %>%
        summarize(m = median(share_pct), .groups = "drop") %>%
        group_by(outcome) %>%
        slice_max(m, n = TOP_N_SINGLE, with_ties = FALSE) %>%
        arrange(outcome, m) %>%                                   # largest on top of each panel
        mutate(row_id = paste(outcome, parameter, sep = "___")) %>%
        ungroup()

    single_df <- single %>%
        inner_join(select(top_single, outcome, parameter, row_id), by = c("outcome", "parameter")) %>%
        mutate(row_id  = factor(row_id, levels = top_single$row_id),
               outcome = factor(outcome, levels = GAM_OUTCOMES),
               msa     = setNames(names(SHIELD.TEN.MSAS), SHIELD.TEN.MSAS)[msa])

    plot_single <- ggplot(single_df, aes(x = msa, y = row_id, fill = share_pct)) +
        geom_tile(color = "white", linewidth = 0.5) +
        geom_text(aes(label = round(share_pct)), size = 2.5,
                  color = ifelse(single_df$share_pct > FILL.MAX / 2, "white", "black")) +
        scale_y_discrete(labels = function(x) sub("^.*___", "", x)) +     # show the parameter name only
        scale_fill_gradient(low = "#F1F6FB", high = "#08306B", limits = c(0, FILL.MAX),
                            breaks = seq(0, FILL.MAX, 10),
                            labels = c(seq(0, FILL.MAX - 10, 10), paste0(FILL.MAX, "+")),
                            oob = scales::squish,
                            name = "% of outcome\nvariance explained") +
        facet_wrap(~ outcome, ncol = 3, scales = "free_y") +
        labs(x = NULL, y = NULL,
             title = paste0("Top ", TOP_N_SINGLE, " single parameters explaining uncertainty in the 2030 doxy-PEP effect"),
             subtitle = "Share of variance explained by each parameter alone; shares overlap for correlated parameters and do not sum to 100% (chance level about 1%)") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              panel.grid = element_blank(),
              legend.position = "right")

    ggsave(plot = plot_single, filename = paste0(OUT.PATH, "gam_single_param_shares_heatmap.png"),
           dpi = 300, width = 22, height = 2 + 2 * (1 + TOP_N_SINGLE * 0.35))
}
