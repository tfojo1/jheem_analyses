# Sensitivity analysis step 4: Tornado plots (v3)
# Pipeline (v3): step1 sort sims -> step2 top/bottom 20% ranking -> step3 PRCC -> step4 tornado plots
#
# One figure per MSA with three panels (total, MSM, women): the top TOP_N_PARAMS
# parameters from step 2, as paired boxplots of the outcome in sims with the lowest
# vs. highest TOP_BOTTOM_FRAC of each parameter's values, labelled with the PRCC (step 3).
# Dashed vertical line = baseline projection (median across all sims in the MSA).

# SETTINGS ----
source('../jheem_analyses/applications/SHIELD/analysis/sensitivity_prcc2/00_sensitivity_settings.R')

# LOAD OBJECTS ----
load(paste0(OUT.PATH, "top_bottom_ranking.Rdata"))          # tails (and rank_df without PRCC)
rank_df          <- get(load(paste0(OUT.PATH, "rank_df_with_prcc.Rdata")))
titles_file      <- paste0(OUT.PATH, "ALL_PARAM_TITLES.Rdata")
# if (!file.exists(titles_file)) titles_file <- paste0(V2.PATH, "ALL_PARAM_TITLES.Rdata")
if (!file.exists(titles_file))    # create it: all_param_titles.R defines ALL_PARAM_TITLES and saves it to OUT.PATH
    source('../jheem_analyses/applications/SHIELD/analysis/sensitivity_prcc2/all_param_titles.R')
ALL_PARAM_TITLES <- get(load(titles_file))

# TORNADO PLOT ----
make_tornado <- function(city) {
    keys <- rank_df %>%
        filter(location == city, rank <= TOP_N_PARAMS) %>%
        mutate(title = ifelse(is.na(ALL_PARAM_TITLES[parameter]), parameter, ALL_PARAM_TITLES[parameter]),
               label = paste0(title, " (PRCC = ", sprintf("%.2f", prcc), ")"),
               key   = paste0(outcome, "___", label)) %>%                    # unique per panel
        arrange(factor(outcome, levels = names(OUTCOMES)), desc(rank))     # rank 1 at the top

    plot_df <- bind_rows(
        semi_join(filter(tails, location == city), keys, by = c("outcome", "parameter")),
        tibble(outcome = keys$outcome, parameter = keys$parameter, topOrBottom = "ZZZ")   # NA rows add spacing
    ) %>%
        left_join(select(keys, outcome, parameter, key), by = c("outcome", "parameter")) %>%
        mutate(key           = factor(key, levels = unique(keys$key)),
               outcome_label = factor(OUTCOMES[outcome], levels = OUTCOMES))

    baseline_df <- keys %>%
        distinct(outcome, baseline) %>%
        mutate(outcome_label = factor(OUTCOMES[outcome], levels = OUTCOMES))

    ggplot(plot_df) +
        geom_boxplot(aes(xmin = `Min.`,
                         xlower = `1st Qu.`,
                         xmiddle = Median,
                         xupper = `3rd Qu.`,
                         xmax = `Max.`,
                         y = key,
                         fill = topOrBottom),
                     stat = "identity",
                     position = "dodge",
                     na.rm = TRUE) +
        geom_vline(data = baseline_df, aes(xintercept = baseline), linetype = "dashed") +
        facet_wrap(~ outcome_label, nrow = 1, scales = "free") +
        scale_y_discrete(labels = function(x) sub("^[a-z]+___", "", x)) +
        labs(x = paste0("Reduction in ", YEAR, " Incidence vs. No Intervention (%)"), y = NULL,
             caption = "Dashed line: baseline projection (median across all simulations).") +
        ggtitle(setNames(names(SHIELD.TEN.MSAS), SHIELD.TEN.MSAS)[city]) +
        scale_fill_manual(name = NULL,
                          breaks = c("top", "bottom"),
                          labels = c(top = paste0("Simulations with the highest ",
                                                  round(TOP_BOTTOM_FRAC*100), "% of parameter values"),
                                     bottom = paste0("Simulations with the lowest ",
                                                     round(TOP_BOTTOM_FRAC*100), "% of parameter values")),
                          values = c(top = "#00A1D5FF",
                                     bottom = "#B24745FF",
                                     ZZZ = NA)) +
        theme_bw() +
        theme(legend.position = "bottom",
              legend.text = element_text(size = 10),
              strip.text = element_text(size = 11, face = "bold")) +
        guides(fill = guide_legend(ncol = 1))
}

for (city in locations) {
    cat(paste0("City: ", city, "\n"))
    ggsave(plot = make_tornado(city),
           filename = paste0(OUT.PATH, "figures/tornado/", city, "__tornado.png"),
           dpi = 300, width = FIG_WIDTH, height = FIG_HEIGHT)
}

# V2 PLOTTING CODE (one plot per MSA x outcome; kept for reference) ----
# # For each parameter with a significant PRCC (adjusted p), compare the outcome
# # in sims with the lowest vs. highest TOP_BOTTOM_FRAC of that parameter's values.
# # Dashed vertical line = median outcome across all sims for that MSA.
# 
# summarize_group <- function(sim_values, rows, group) {
#     reshape2::melt(sim_values[rows, , drop = FALSE], varnames = c("sim", "parameter")) %>%
#         filter(!is.na(value)) %>%
#         group_by(parameter) %>%
#         summarize(`Min.`    = min(value),
#                   `1st Qu.` = quantile(value, probs = 0.25),
#                   Median    = median(value),
#                   `3rd Qu.` = quantile(value, probs = 0.75),
#                   `Max.`    = max(value)) %>%
#         mutate(parameter = as.character(parameter), topOrBottom = group)
# }
# 
# make_prcc_plot <- function(outcome, location) {
#     prccs_filtered <- filter(prccs[[outcome]][[location]], p.adj <= P.VALUE.THRESHOLD)
#     if (nrow(prccs_filtered) == 0) {
#         cat("  No significant parameters for outcome", outcome, "- no plot\n")
#         return(NULL)
#     }
#     params <- prccs_filtered$var
#     ests   <- setNames(prccs_filtered$est, params)
# 
#     # Outcome per sim (original order), then re-ordered by each parameter's values
#     outcome_values <- get_outcome(as.numeric(outcome), location, total_calc_results, sex_calc_results)
#     sim_order  <- sorted_arrs[[location]][, params, drop = FALSE]
#     sim_values <- matrix(outcome_values[sim_order], nrow = nrow(sim_order),
#                          dimnames = list(NULL, params))
# 
#     n_sim  <- nrow(sim_values)
#     n_tail <- round(n_sim * TOP_BOTTOM_FRAC)
#     bottom_df <- summarize_group(sim_values, 1:n_tail, "bottom")
#     top_df    <- summarize_group(sim_values, (n_sim - n_tail + 1):n_sim, "top")
# 
#     # Keep parameters with a large enough gap in medians (at least MIN_N_PARAMS, if available)
#     diff_df <- inner_join(select(bottom_df, parameter, Median),
#                           select(top_df, parameter, Median),
#                           by = "parameter") %>%
#         mutate(diff_Median = abs(Median.x - Median.y)) %>%
#         arrange(desc(diff_Median))
# 
#     n_keep  <- max(sum(diff_df$diff_Median >= MEDIAN_GAP[outcome]), min(MIN_N_PARAMS, nrow(diff_df)))
#     diff_df <- diff_df[1:n_keep, ]
# 
#     # Reverse the sort so the widest gap is at the top of the plot
#     params <- rev(diff_df$parameter)
#     titles <- ifelse(is.na(ALL_PARAM_TITLES[params]), params, ALL_PARAM_TITLES[params])
#     params_formatted <- setNames(paste0(titles, " (", round(ests[params], digits = 3), ")"), params)
#     num_params <- length(params)
# 
#     boxplot_df <- bind_rows(
#         filter(bottom_df, parameter %in% params),
#         filter(top_df, parameter %in% params),
#         tibble(parameter = params, topOrBottom = "ZZZ")    # NA rows add spacing between boxplots
#     ) %>%
#         mutate(parameter = factor(params_formatted[parameter], levels = params_formatted))
# 
#     x_title <- c(
#         "1" = "Reduction in 2030 Incidence vs. No Intervention (%)",
#         "2" = "Women-to-MSM Relative Impact Ratio"
#     )[outcome]
# 
#     plot <- ggplot(boxplot_df) +
#         geom_boxplot(aes(xmin = `Min.`,
#                          xlower = `1st Qu.`,
#                          xmiddle = Median,
#                          xupper = `3rd Qu.`,
#                          xmax = `Max.`,
#                          y = parameter,
#                          fill = topOrBottom),
#                      stat="identity",
#                      position="dodge") +
#         labs(x = x_title, y  = NULL) +
#         ggtitle(setNames(names(SHIELD.TEN.MSAS), SHIELD.TEN.MSAS)[location]) +
#         scale_fill_manual(name = NULL,
#                           breaks = c("top", "bottom"),
#                           labels = c(top = paste0("Simulations with the highest ",
#                                                   round(TOP_BOTTOM_FRAC*100),"% of parameter values"),
#                                      bottom = paste0("Simulations with the lowest ",
#                                                      round(TOP_BOTTOM_FRAC*100),"% of parameter values")),
#                           values = c(top = "#00A1D5FF",
#                                      bottom = "#B24745FF",
#                                      ZZZ = NA)) +
#         theme_bw() +
#         theme(legend.position = "bottom",
#               legend.text = element_text(size=10)) +
#         guides(fill = guide_legend(ncol = 1)) +
#         geom_vline(xintercept = median(outcome_values, na.rm = TRUE), linetype = "dashed")
# 
#     list(plot = plot, num_params = num_params)
# }
# 
# for (city in SHIELD.TEN.MSAS) {
#     cat(paste0("City: ", city, "\n"))
#     for (outcome in c("1", "2")) {
#         tmp <- make_prcc_plot(outcome, city)
#         if (is.null(tmp)) next
#         ggsave(plot = tmp$plot,
#                filename = paste0(OUT.PATH, "figures/PRCC plots/", city, "__outcome", outcome, ".png"),
#                dpi = 300, width = 6.5, height = 1 + tmp$num_params / 2)
#     }
# }

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