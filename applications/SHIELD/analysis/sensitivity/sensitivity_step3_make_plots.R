# Sensitivity analysis step 3: generate plots


library(tidyverse)
library(abind)

# SETTINGS ----
CALIB.CODE = "calib.10.20.stage3"
locations <- setNames(SHIELD.TEN.MSAS, SHIELD.TEN.MSAS)

BASE.PATH <- paste0(ROOT.DIR,"/shield/outputs/", CALIB.CODE, "/")

# what fraction of all the simsets will count as "top __" or bottom __" set, each?
TOP_BOTTOM_FRAC <- 0.2
N_SIM = 800
N_PARAMS = 5

ALL_PARAM_TITLES <- c(
    "age14.black.aging.rate.multiplier.1"                     = "Age-Out Rate Multiplier\nBlack Age 14, 2010",
    "age14.black.aging.rate.multiplier.2"                     = "Age-Out Rate Multiplier\nBlack Age 14, 2020",
    "age14.hispanic.aging.rate.multiplier.1"                  = "Age-Out Rate Multiplier\nHispanic Age 14, 2010",
    "age14.hispanic.aging.rate.multiplier.2"                  = "Age-Out Rate Multiplier\nHispanic Age 14, 2020",
    "age15.19.fertility.rate.multiplier"                      = "Fertility Rate Multiplier\nAges 15-19",
    "age19.black.aging.rate.multiplier.1"                     = "Age-Out Rate Multiplier\nBlack Age 19, 2010",
    "age19.black.aging.rate.multiplier.2"                     = "Age-Out Rate Multiplier\nBlack Age 19, 2020",
    "age19.hispanic.aging.rate.multiplier.2"                  = "Age-Out Rate Multiplier\nHispanic Age 19, 2020",
    "age19.other.aging.rate.multiplier.1"                     = "Age-Out Rate Multiplier\nOther Race Age 19, 2010",
    "age20.24.fertility.rate.multiplier"                      = "Fertility Rate Multiplier\nAges 20-24",
    "age24.black.aging.rate.multiplier.1"                     = "Age-Out Rate Multiplier\nBlack Age 24, 2010",
    "age24.black.aging.rate.multiplier.2"                     = "Age-Out Rate Multiplier\nBlack Age 24, 2020",
    "age24.other.aging.rate.multiplier.1"                     = "Age-Out Rate Multiplier\nOther Race Age 24, 2010",
    "age29.hispanic.aging.rate.multiplier.1"                  = "Age-Out Rate Multiplier\nHispanic Age 29, 2010",
    "age34.hispanic.aging.rate.multiplier.2"                  = "Age-Out Rate Multiplier\nHispanic Age 34, 2020",
    "age34.other.aging.rate.multiplier.1"                     = "Age-Out Rate Multiplier\nOther Race Age 34, 2010",
    "age35.39.fertility.rate.multiplier"                      = "Fertility Rate Multiplier\nAges 35-39",
    "age39.black.aging.rate.multiplier.1"                     = "Age-Out Rate Multiplier\nBlack Age 39, 2010",
    "age39.hispanic.aging.rate.multiplier.1"                  = "Age-Out Rate Multiplier\nHispanic Age 39, 2010",
    "age44.black.aging.rate.multiplier.1"                     = "Age-Out Rate Multiplier\nBlack Age 44, 2010",
    "age44.black.aging.rate.multiplier.2"                     = "Age-Out Rate Multiplier\nBlack Age 44, 2020",
    "age44.other.aging.rate.multiplier.1"                     = "Age-Out Rate Multiplier\nOther Race Age 44, 2010",
    "age44.other.aging.rate.multiplier.2"                     = "Age-Out Rate Multiplier\nOther Race Age 44, 2020",
    "age49.black.aging.rate.multiplier.2"                     = "Age-Out Rate Multiplier\nBlack Age 49, 2020",
    "age49.hispanic.aging.rate.multiplier.2"                  = "Age-Out Rate Multiplier\nHispanic Age 49, 2020",
    "age49.other.aging.rate.multiplier.2"                     = "Age-Out Rate Multiplier\nOther Race Age 49, 2020",
    "age54.black.aging.rate.multiplier.1"                     = "Age-Out Rate Multiplier\nBlack Age 54, 2010",
    "age54.hispanic.aging.rate.multiplier.1"                  = "Age-Out Rate Multiplier\nHispanic Age 54, 2010",
    "age54.hispanic.aging.rate.multiplier.2"                  = "Age-Out Rate Multiplier\nHispanic Age 54, 2020",
    "age54.other.aging.rate.multiplier.1"                     = "Age-Out Rate Multiplier\nOther Race Age 54, 2010",
    "age64.black.aging.rate.multiplier.1"                     = "Age-Out Rate Multiplier\nBlack Age 64, 2010",
    "age64.hispanic.aging.rate.multiplier.1"                  = "Age-Out Rate Multiplier\nHispanic Age 64, 2010",
    "age64.other.aging.rate.multiplier.1"                     = "Age-Out Rate Multiplier\nOther Race Age 64, 2010",
    "black.black.sexual.multi"                                = "Sexual Mixing Multiplier\nBlack-Black",
    "black.general.mortality.rate.multiplier"                 = "General Mortality Rate\nMultiplier Black",
    "black.sti.screening.covid.multiplier"                    = "STI Screening COVID-Year\nMultiplier Black",
    "el.rel.secondary.transmissibility"                       = "Early Latent-to-Secondary\nRelative Transmissibility",
    "female.general.mortality.rate.multiplier"                = "General Mortality Rate\nMultiplier Female",
    "first.trimester.slope.mult"                              = "First Trimester\nSlope Multiplier",
    "fraction.el.misclassified.ll"                            = "Fraction Early Latent\nMisclassified as Late Latent",
    "fraction.msm.pairings.with.female"                       = "Fraction of MSM\nPairings with Females",
    "global.transmission.rate.het"                            = "Global Transmission Rate\nHeterosexual",
    "global.transmission.rate.msm"                            = "Global Transmission Rate\nMSM",
    "hispanic.general.mortality.rate.multiplier"              = "General Mortality Rate\nMultiplier Hispanic",
    "hispanic.hispanic.sexual.multi"                          = "Sexual Mixing Multiplier\nHispanic-Hispanic",
    "hispanic.immigration.rate.multiplier.1"                  = "Immigration Rate Multiplier\nHispanic, 2010",
    "oe.female.pairings.with.msm"                             = "Observed vs. Expected\nFemale Pairings with MSM",
    "or.careseeking.symptomatic.ps.black"                     = "Odds Ratio of Careseeking,\nSymptomatic P/S Black",
    "or.careseeking.symptomatic.ps.female"                    = "Odds Ratio of Careseeking,\nSymptomatic P/S Female",
    "or.careseeking.symptomatic.ps.heterosexual_male"         = "Odds Ratio of Careseeking,\nSymptomatic P/S Heterosexual Male",
    "or.careseeking.symptomatic.ps.hispanic"                  = "Odds Ratio of Careseeking,\nSymptomatic P/S Hispanic",
    "or.careseeking.symptomatic.ps.msm"                       = "Odds Ratio of Careseeking,\nSymptomatic P/S MSM",
    "or.slope.careseeking.symptomatic.ps"                     = "Odds Ratio Slope of Careseeking,\nSymptomatic P/S",
    "or.slope.syphilis.to.hiv.testing"                        = "Odds Ratio Slope of\nSyphilis-to-HIV Testing",
    "other.fertility.rate.multiplier"                         = "Fertility Rate Multiplier\nOther Race",
    "other.general.mortality.rate.multiplier"                 = "General Mortality Rate\nMultiplier Other Race",
    "other.other.sexual.multi"                                = "Sexual Mixing Multiplier\nOther-Other",
    "other.proportion.msm.of.male.mult"                       = "Proportion MSM of Male\nMultiplier, Other Race",
    "other.sti.screening.covid.multiplier"                    = "STI Screening COVID-Year\nMultiplier Other Race",
    "prop.early.latent.to.secondary"                          = "Proportion Early Latent\nRelapsing to Secondary",
    "prp.infections.among.msm.1970"                           = "Proportion of Infections\namong MSM in 1970",
    "prp.symptomatic.secondary"                               = "Proportion Symptomatic\nin Secondary Stage",
    "ratio.of.undiagnosed.to.diagnosed.1970"                  = "Ratio of Undiagnosed\nto Diagnosed in 1970",
    "rr.prp.symptomatic.primary.female"                       = "RR for Proportion Symptomatic in\nPrimary Stage, Female",
    "rr.prp.symptomatic.primary.heterosexual_male"            = "RR for Proportion Symptomatic in\nPrimary Stage, Heterosexual Male",
    "screening.rate.future.change.mult"                       = "Screening Rate Future\nChange Multiplier",
    "screening.rate.multiplier.female.2000"                   = "Screening Rate Multiplier\nFemale 2000 Spline Value",
    "screening.rate.multiplier.female.2010"                   = "Screening Rate Multiplier\nFemale 2010 Spline Value",
    "screening.rate.multiplier.female.2020"                   = "Screening Rate Multiplier\nFemale 2020 Spline Value",
    "screening.rate.multiplier.heterosexual_male.rel.female"  = "Screening Rate Multiplier,\nHeterosexual Male Relative to Female",
    "screening.rate.multiplier.hispanic"                      = "Screening Rate Multiplier\nHispanic",
    "screening.rate.multiplier.msm.1990"                      = "Screening Rate Multiplier\nMSM 1990 Spline Value",
    "screening.rate.multiplier.msm.2010"                      = "Screening Rate Multiplier\nMSM 2010 Spline Value",
    "screening.rate.multiplier.msm.2020"                      = "Screening Rate Multiplier\nMSM 2020 Spline Value",
    "transmission.rate.future.change.mult"                    = "Transmission Rate Future\nChange Multiplier",
    "transmission.rate.multiplier.age14.msm"                  = "Transmission Rate Multiplier\nAge 14 MSM",
    "transmission.rate.multiplier.age19.heterosexual"         = "Transmission Rate Multiplier\nAge 19 Heterosexual",
    "transmission.rate.multiplier.age19.msm"                  = "Transmission Rate Multiplier\nAge 19 MSM",
    "transmission.rate.multiplier.age24.heterosexual"         = "Transmission Rate Multiplier\nAge 24 Heterosexual",
    "transmission.rate.multiplier.age29.heterosexual"         = "Transmission Rate Multiplier\nAge 29 Heterosexual",
    "transmission.rate.multiplier.age29.msm"                  = "Transmission Rate Multiplier\nAge 29 MSM",
    "transmission.rate.multiplier.age34.heterosexual"         = "Transmission Rate Multiplier\nAge 34 Heterosexual",
    "transmission.rate.multiplier.age34.msm"                  = "Transmission Rate Multiplier\nAge 34 MSM",
    "transmission.rate.multiplier.age39.heterosexual"         = "Transmission Rate Multiplier\nAge 39 Heterosexual",
    "transmission.rate.multiplier.age44.heterosexual"         = "Transmission Rate Multiplier\nAge 44 Heterosexual",
    "transmission.rate.multiplier.age44.msm"                  = "Transmission Rate Multiplier\nAge 44 MSM",
    "transmission.rate.multiplier.age49.msm"                  = "Transmission Rate Multiplier\nAge 49 MSM",
    "transmission.rate.multiplier.age54.heterosexual"         = "Transmission Rate Multiplier\nAge 54 Heterosexual",
    "transmission.rate.multiplier.age64.heterosexual"         = "Transmission Rate Multiplier\nAge 64 Heterosexual",
    "transmission.rate.multiplier.age65.msm"                  = "Transmission Rate Multiplier\nAge 65+ MSM",
    "transmission.rate.multiplier.heterosexual1990"           = "Transmission Rate Multiplier\nHeterosexual 1990 Spline Value",
    "transmission.rate.multiplier.heterosexual1995"           = "Transmission Rate Multiplier\nHeterosexual 1995 Spline Value",
    "transmission.rate.multiplier.heterosexual2000"           = "Transmission Rate Multiplier\nHeterosexual 2000 Spline Value",
    "transmission.rate.multiplier.heterosexual2010"           = "Transmission Rate Multiplier\nHeterosexual 2010 Spline Value",
    "transmission.rate.multiplier.heterosexual2022"           = "Transmission Rate Multiplier\nHeterosexual 2022 Spline Value",
    "transmission.rate.multiplier.hispanic"                   = "Transmission Rate Multiplier\nHispanic",
    "transmission.rate.multiplier.msm1970"                    = "Transmission Rate Multiplier\nMSM 1970 Spline Value",
    "transmission.rate.multiplier.msm2000"                    = "Transmission Rate Multiplier\nMSM 2000 Spline Value",
    "transmission.rate.multiplier.msm2010"                    = "Transmission Rate Multiplier\nMSM 2010 Spline Value",
    "transmission.rate.multiplier.msm2022"                    = "Transmission Rate Multiplier\nMSM 2022 Spline Value",
    "transmission.rate.multiplier.other"                      = "Transmission Rate Multiplier\nOther Race"
)

# LOAD OBJECTS ----
params_ranked <- get(load(paste0(BASE.PATH, "params_ranked.Rdata")))
sorted_arrs <- get(load(paste0(BASE.PATH, "sorted_arrs.Rdata")))

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
    params <- params_ranked[[outcome]][[location]][["var"]][1:N_PARAMS]
    ests <- setNames(params_ranked[[outcome]][[location]][["est"]][1:N_PARAMS],
                     params)
    
    sim_values <- apply(sorted_arrs[[location]][,params], 2, function(sim_order) {
        if (outcome == 1) {
            # Multiply by negative once since is a "reduction"
            -1 * total_calc_results["2030", sim_order, "doxy.cov.20", location, "pct_change_incidence_vs_noint"]
        } else {
            sex_calc_results["2030", "female", sim_order, "doxy.cov.20", location, "pct_change_incidence_vs_noint"] /
                sex_calc_results["2030", "msm", sim_order, "doxy.cov.20", location, "pct_change_incidence_vs_noint"]
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
    
    param_order <- order(abs(top_df$Median - bottom_df$Median), decreasing=F)
    params_formatted <- setNames(paste0(ALL_PARAM_TITLES[params],
                                        " (", round(ests[params], digits=3),
                                        ")")[param_order],
                                   params[param_order])
    
    boxplot_df <- rbind(
        bottom_df,
        top_df,
        tibble(parameter = params,
               `Min.` = rep(NA, N_PARAMS),
               `1st Qu.` = rep(NA, N_PARAMS),
               Median = rep(NA, N_PARAMS),
               Mean = rep(NA, N_PARAMS),
               `3rd Qu.` = rep(NA, N_PARAMS),
               `Max.` = rep(NA, N_PARAMS),
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
    
}

for (city in SHIELD.TEN.MSAS) {
    cat(paste0("City: ", city, "\n"))
    for (outcome in 1:2) {
        plot <- make_prcc_plot(outcome, city)
        ggsave(plot = plot,
               filename = paste0(BASE.PATH, "figures/PRCC plots/", city, "__outcome", outcome, ".png"),
               dpi = 300, width = 6.5, height = 4)
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