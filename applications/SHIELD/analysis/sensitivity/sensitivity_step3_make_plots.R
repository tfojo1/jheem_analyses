# Sensitivity analysis step 3: generate plots


library(tidyverse)
library(abind)

# SETTINGS ----
CALIB.CODE = "calib.10.20.stage4"
locations <- setNames(SHIELD.TEN.MSAS, SHIELD.TEN.MSAS)

BASE.PATH <- paste0(ROOT.DIR,"/shield/outputs/", CALIB.CODE, "/")

# what fraction of all the simsets will count as "top __" or bottom __" set, each?
TOP_BOTTOM_FRAC <- 0.2
N_SIM = 1000
MIN_N_PARAMS = 5
P.VALUE.THRESHOLD = 0.05


# LOAD OBJECTS ----
prccs <- get(load(paste0(BASE.PATH, "prccs.Rdata")))
sorted_arrs <- get(load(paste0(BASE.PATH, "sorted_arrs.Rdata")))
ALL_PARAMS_TITLES <- get(load(paste0(BASE.PATH, "ALL_PARAM_TITLES.Rdata")))

# prccs_outcome1 <- get(load(paste0(BASE.PATH, "prccs_outcome1.Rdata")))
# prccs_outcome2 <- get(load(paste0(BASE.PATH, "prccs_outcome2.Rdata")))

# Make a data frame with the following columns:
# `Min.`, `1st Qu.`, Median, `3rd Qu.`, `Max.`
# topOrBottom, which determines whether top or bottom 20% by param sort

# And 3 rows per parameter: one for the "top" sim set, another for the "bottom"
# sim set, and a third with NAs to add spacing between boxplots

# NOTE:
# For the dotted vertical ine on the plot, which is going to be the average
# value of the outcome across all the [800] composite simulations, there is no
# single value because for the region, we had to add sets of 10 simulations
# together from each location to get one simulation for the region. Each of the
# 800 used in the plot was made that way. However, if you changed which
# simulations were combined with which other ones, you'd get different regional
# values. On this plot, the sims were combined in a different way for each
# parameter because we had the sim with the lowest value of the parameter in
# each location combine with the same for the other locations. A parameter-
# agnotistic way to combine them would be to stick with the original order from
# each simset. 1st goes with 1st, 800th goes with 800th. But if we round, all
# these permutations would probably get the mean value anyways. For simplicity,
# I'll just take the average of the averages from each of the 10
# simsets we've made for the plot.

make_prcc_plot <- function(outcome, location) {
    # browser()
    prccs_filtered <- filter(prccs[[outcome]][[location]], p.value <= P.VALUE.THRESHOLD)
    params <- prccs_filtered$var
    ests <- setNames(prccs_filtered$est, params)
    
    sim_values <- apply(sorted_arrs[[location]][,params], 2, function(sim_order) {
        if (outcome == 1) {
            # Multiply by negative once since is a "reduction"
            -1 * total_calc_results["2030", sim_order, "doxy.cov.20", location, "pct_change_incidence_vs_noint"]
        } else {
            rv <-
                sex_calc_results["2030", "female", sim_order, "doxy.cov.20", location, "pct_change_incidence_vs_noint"] /
                sex_calc_results["2030", "msm", sim_order, "doxy.cov.20", location, "pct_change_incidence_vs_noint"]
            # Correct NAs to zero, I guess?
            rv[is.na(rv)] <- 0
            rv
        }
    })
    
    bottom_df <- reshape2::melt(sim_values[1:(N_SIM*TOP_BOTTOM_FRAC), params]) %>%
        group_by(parameter) %>%
        summarize(`Min.` = min(value),
                  `1st Qu.` = quantile(value, probs=0.25),
                  Median = median(value),
                  Mean = mean(value),
                  `3rd Qu.` = quantile(value, probs=0.75),
                  `Max.` = max(value)) %>%
        mutate(topOrBottom = "bottom")
    
    top_df <- reshape2::melt(sim_values[(N_SIM*(1-TOP_BOTTOM_FRAC) + 1): N_SIM, params]) %>%
        group_by(parameter) %>%
        summarize(`Min.` = min(value),
                  `1st Qu.` = quantile(value, probs=0.25),
                  Median = median(value),
                  Mean = mean(value),
                  `3rd Qu.` = quantile(value, probs=0.75),
                  `Max.` = max(value)) %>%
        mutate(topOrBottom = "top")
    
    
    # Now prune to only those with a difference large enough
    diff_df <- merge(select(bottom_df, parameter, Median),
                     select(top_df, parameter, Median),
                     by = "parameter") %>%
        mutate(diff_Median = abs(Median.x - Median.y)) %>%
        arrange(desc(diff_Median))
    
    THRESHOLD <- if (outcome == 1) 5 else 0.05
    
    if (sum(diff_df$diff_Median >= THRESHOLD) >= MIN_N_PARAMS) {
        diff_df <- filter(diff_df, diff_Median >= THRESHOLD)
    } else {
        diff_df <- diff_df[1:MIN_N_PARAMS,]
    }
    
    # Now we reverse the sort because the factor needs to be reversed
    # to have the widest gap at the top
    diff_df <- arrange(diff_df, diff_Median)
    params <- as.vector(diff_df$parameter)
    params_formatted <- setNames(paste0(ALL_PARAM_TITLES[params],
                                        " (", round(ests[params], digits=3),
                                        ")"),
                                 params)
    num_params <- length(params)
    
    boxplot_df <- rbind(
        filter(bottom_df, parameter %in% params),
        filter(top_df, parameter %in% params),
        tibble(parameter = params,
               `Min.` = rep(NA, num_params),
               `1st Qu.` = rep(NA, num_params),
               Median = rep(NA, num_params),
               Mean = rep(NA, num_params),
               `3rd Qu.` = rep(NA, num_params),
               `Max.` = rep(NA, num_params),
               topOrBottom = "ZZZ")
    ) %>%
        mutate(
            parameter = factor(params_formatted[as.character(parameter)],
                               levels = params_formatted)
        )
    
    x_title <- c(
        "1" = "Reduction in 2030 Incidence vs. No Intervention (%)",
        "2" = "Women-to-MSM Relative Impact Ratio"
    )[outcome]
    
    plot <- ggplot(boxplot_df) +
        geom_boxplot(aes(xmin = `Min.`,
                         xlower = `1st Qu.`,
                         xmiddle = Median,
                         xupper = `3rd Qu.`,
                         xmax = `Max.`,
                         y = parameter,
                         fill = topOrBottom),
                     stat="identity",
                     position="dodge") +
        labs(x = x_title, y  = NULL) +
        ggtitle(setNames(names(SHIELD.TEN.MSAS), SHIELD.TEN.MSAS)[location]) +
        scale_fill_manual(name = NULL,
                          labels = c(top = paste0("Simulations with the highest ",
                                                  round(TOP_BOTTOM_FRAC*100),"% of parameter values"),
                                     bottom = paste0("Simulations with the lowest ",
                                                     round(TOP_BOTTOM_FRAC*100),"% of parameter values")),
                          values = c(top = "#00A1D5FF",
                                     bottom = "#B24745FF")) +
        theme_bw() + 
        theme(legend.position = "bottom",
              legend.text = element_text(size=10)) +
        guides(fill = guide_legend(ncol = 1)) +
        geom_vline(xintercept = median(sim_values[,1]), linetype = "dashed")
    
    output <- list(
        plot = plot,
        num_params = num_params
    )
    
}

for (city in SHIELD.TEN.MSAS) {
    cat(paste0("City: ", city, "\n"))
    for (outcome in 1:2) {
        tmp <- make_prcc_plot(outcome, city)
        plot <- tmp$plot
        num_params <- tmp$num_params
        ggsave(plot = plot,
               filename = paste0(BASE.PATH, "figures/PRCC plots/", city, "__outcome", outcome, ".png"),
               dpi = 300, width = 6.5, height = 1 + num_params / 2)
    }
}


# OLD VERSION THAT AGGREGATED ALL THE CITIES ----
# 
# outcome1_agg <- get(load(paste0(BASE.PATH, "outcome1_agg.Rdata")))
# outcome1_agg <- sorted_arrs[[1]][,names(PLOT_1_PARAMS)] #Atlanta
# outcome2_agg <- get(load(paste0(BASE.PATH, "outcome2_agg.Rdata")))

# # For each of the above parameters, find an AVERAGE PRCC value
# avg_est_all_params_1 <- prccs_outcome1 %>%
#     bind_rows(.id = "location") %>%
#     group_by(var) %>%
#     summarise(mean_est = mean(est, na.rm = TRUE),
#               .groups = "drop") %>%
#     deframe()
# avg_est_all_params_2 <- prccs_outcome2 %>%
#     bind_rows(.id = "location") %>%
#     group_by(var) %>%
#     summarise(mean_est = mean(est, na.rm = TRUE),
#               .groups = "drop") %>%
#     deframe()
# 
# boxplot_df_1 <- rbind(
#     reshape2::melt(outcome1_agg[1:(400*0.2),]) %>%
#         group_by(parameter) %>%
#         summarize(`Min.` = min(value),
#                   `1st Qu.` = quantile(value, probs=0.25),
#                   Median = median(value),
#                   Mean = mean(value),
#                   `3rd Qu.` = quantile(value, probs=0.75),
#                   `Max.` = max(value)) %>%
#         mutate(topOrBottom = "bottom"),
#     reshape2::melt(outcome1_agg[(400*0.8 + 1):400,]) %>%
#         group_by(parameter) %>%
#         summarize(`Min.` = min(value),
#                   `1st Qu.` = quantile(value, probs=0.25),
#                   Median = median(value),
#                   Mean = mean(value),
#                   `3rd Qu.` = quantile(value, probs=0.75),
#                   `Max.` = max(value)) %>%
#         mutate(topOrBottom = "top"),
#     tibble(parameter = PLOT_1_PARAMS,
#                `Min.` = rep(NA, 10),
#                `1st Qu.` = rep(NA, 10),
#                Median = rep(NA, 10),
#                Mean = rep(NA, 10),
#                `3rd Qu.` = rep(NA, 10),
#                `Max.` = rep(NA, 10),
#                topOrBottom = "ZZZ")
# )
# boxplot_df_2 <- rbind(
#     reshape2::melt(outcome2_agg[1:(400*0.2),]) %>%
#         group_by(parameter) %>%
#         summarize(`Min.` = min(value),
#                   `1st Qu.` = quantile(value, probs=0.25),
#                   Median = median(value),
#                   Mean = mean(value),
#                   `3rd Qu.` = quantile(value, probs=0.75),
#                   `Max.` = max(value)) %>%
#         mutate(topOrBottom = "bottom"),
#     reshape2::melt(outcome2_agg[(400*0.8 + 1):400,]) %>%
#         group_by(parameter) %>%
#         summarize(`Min.` = min(value),
#                   `1st Qu.` = quantile(value, probs=0.25),
#                   Median = median(value),
#                   Mean = mean(value),
#                   `3rd Qu.` = quantile(value, probs=0.75),
#                   `Max.` = max(value)) %>%
#         mutate(topOrBottom = "top"),
#     tibble(parameter = PLOT_2_PARAMS,
#            `Min.` = rep(NA, 10),
#            `1st Qu.` = rep(NA, 10),
#            Median = rep(NA, 10),
#            Mean = rep(NA, 10),
#            `3rd Qu.` = rep(NA, 10),
#            `Max.` = rep(NA, 10),
#            topOrBottom = "ZZZ")
# )
# # Note: might need to add a "check.names" column
# 
# # This step should have the one I want on top be LAST, because the y-axis will treat the factors like numbers with higher on top.
# regional_top_sims_df_1 <- filter(boxplot_df_1, topOrBottom == "top")
# regional_bottom_sims_df_1 <- filter(boxplot_df_1, topOrBottom == "bottom")
# param_order_for_boxplot_1 <- order(abs(regional_top_sims_df_1$Median - regional_bottom_sims_df_1$Median), decreasing=F)
# params_formatted_1 <- setNames(paste0(PLOT_1_PARAMS, " (", round(avg_est_all_params_1[names(PLOT_1_PARAMS)], digits=3), ")")[param_order_for_boxplot_1],
#                              names(PLOT_1_PARAMS)[param_order_for_boxplot_1])
# 
# # I have to wrap it in as.character because the "param" column is already a factor... thanks to reshape2::melt.
# boxplot_df_1 <- rbind(regional_top_sims_df_1, regional_bottom_sims_df_1) %>%
#     mutate(parameter = factor(params_formatted_1[as.character(parameter)],
#                           levels = params_formatted_1))
# 
# # This step should have the one I want on top be LAST, because the y-axis will treat the factors like numbers with higher on top.
# regional_top_sims_df_2 <- filter(boxplot_df_2, topOrBottom == "top")
# regional_bottom_sims_df_2 <- filter(boxplot_df_2, topOrBottom == "bottom")
# param_order_for_boxplot_2 <- order(abs(regional_top_sims_df_2$Median - regional_bottom_sims_df_2$Median), decreasing=F)
# params_formatted_2 <- setNames(paste0(PLOT_2_PARAMS, " (", round(avg_est_all_params_2[names(PLOT_2_PARAMS)], digits=3), ")")[param_order_for_boxplot_2],
#                                names(PLOT_2_PARAMS)[param_order_for_boxplot_2])
# 
# # I have to wrap it in as.character because the "param" column is already a factor... thanks to reshape2::melt.
# boxplot_df_2 <- rbind(regional_top_sims_df_2, regional_bottom_sims_df_2) %>%
#     mutate(parameter = factor(params_formatted_2[as.character(parameter)],
#                               levels = params_formatted_2))
# 
# plot1 <- ggplot(boxplot_df_1) +
#     geom_boxplot(aes(xmin = `Min.`,
#                      xlower = `1st Qu.`,
#                      xmiddle = Median,
#                      xupper = `3rd Qu.`,
#                      xmax = `Max.`,
#                      y = parameter,
#                      fill = topOrBottom),
#                  stat="identity",
#                  position="dodge") +
#     labs(x = "Reduction in 2030 Incidence vs. No Intervention (%)", y  = NULL) +
#     scale_fill_manual(name = NULL,
#                       labels = c(top = "Simulations with the highest 20% of parameter values",
#                                  bottom = "Simulations with the lowest 20% of parameter values"),
#                       values = c(top = "#00A1D5FF",
#                                  bottom = "#B24745FF")) +
#     theme_bw() + 
#     theme(legend.position = "bottom",
#           legend.text = element_text(size=10)) +
#     guides(fill = guide_legend(ncol = 1)) +
#     geom_vline(xintercept = vert1, linetype = "dashed")
# 
# plot2 <- ggplot(boxplot_df_2) +
#     geom_boxplot(aes(xmin = `Min.`,
#                      xlower = `1st Qu.`,
#                      xmiddle = Median,
#                      xupper = `3rd Qu.`,
#                      xmax = `Max.`,
#                      y = parameter,
#                      fill = topOrBottom),
#                  stat="identity",
#                  position="dodge") +
#     labs(x = "Women-to-MSM Relative Impact Ratio", y  = NULL) +
#     scale_fill_manual(name = NULL,
#                       labels = c(top = "Simulations with the highest 20% of parameter values",
#                                  bottom = "Simulations with the lowest 20% of parameter values"),
#                       values = c(top = "#00A1D5FF",
#                                  bottom = "#B24745FF")) +
#     theme_bw() + 
#     theme(legend.position = "bottom",
#           legend.text = element_text(size=10)) +
#     guides(fill = guide_legend(ncol = 1)) +
#     geom_vline(xintercept = vert2, linetype = "dashed")

# Save ---

# ggsave(filename = file.path(BASE.PATH, "figures/Fig6_prcc_outcome1.png"),
#        plot = plot1, dpi=300, width = 6, height = 5)
# ggsave(filename = file.path(BASE.PATH, "figures/Fig6_prcc_outcome2.png"),
#        plot = plot2, dpi=300, width = 6, height = 5)
# 
# save(plot1,
#      file = paste0(BASE.PATH, "outcome1_agg.Rdata"))
# save(outcome2_agg,
#      file = paste0(BASE.PATH, "outcome2_agg.Rdata"))