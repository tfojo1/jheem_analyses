# Sensitivity analysis step 3: generate plots


library(tidyverse)
library(abind)

# SETTINGS ----
CALIB.CODE = "calib.8.21.stage3.az"
locations = setNames(SHIELD.TEN.MSAS[c(1:8,10)], SHIELD.TEN.MSAS[c(1:8,10)])

BASE.PATH <- paste0(ROOT.DIR,"/shield/outputs/", CALIB.CODE)

# what fraction of all the simsets will count as "top __" or bottom __" set, each?
TOP_BOTTOM_FRAC <- 0.2

# MAKE SURE THIS MATCHES STEP 2'S PARAMS!
# These were just from Atlanta
PLOT_PARAMS <- c(
    "transmission.rate.future.change.mult",
    "hispanic.proportion.msm.of.male.mult",
    "screening.rate.multiplier.black",
    "hispanic.hispanic.sexual.multi",
    "screening.rate.future.change.mult",
    "transmission.rate.multiplier.age65.heterosexual",
    "screening.rate.multiplier.female",
    "other.other.sexual.multi",
    "or.careseeking.symptomatic.ps.other",
    "age29.hispanic.aging.rate.multiplier.2"
)

# LOAD OBJECTS ----
prccs_outcome1 <- get(load(paste0(BASE.PATH, "prccs_outcome1.Rdata")))
prccs_outcome2 <- get(load(paste0(BASE.PATH, "prccs_outcome2.Rdata")))
outcome1_agg <- get(load(paste0(BASE.PATH, "outcome1_agg.Rdata")))
outcome2_agg <- get(load(paste0(BASE.PATH, "outcome2_agg.Rdata")))

# Make a data frame with the following columns:
# `Min.`, `1st Qu.`, Median, `3rd Qu.`, `Max.`
# topOrBottom, which determines whether top or bottom 20% by param sort

# And 3 rows per parameter: one for the "top" sim set, another for the "bottom"
# sim set, and a third with NAs to add spacing between boxplots

boxplot_df <- rbind(
    reshape2::melt(outcome1_agg[1:(400*0.2),]) %>%
        group_by(parameter) %>%
        summarize(`Min.` = min(value),
                  `1st Qu.` = quantile(value, probs=0.25),
                  Median = median(value),
                  Mean = mean(value),
                  `3rd Qu.` = quantile(value, probs=0.75),
                  `Max.` = max(value)) %>%
        mutate(topOrBottom = "bottom"),
    reshape2::melt(outcome1_agg[(400*0.8 + 1):400,]) %>%
        group_by(parameter) %>%
        summarize(`Min.` = min(value),
                  `1st Qu.` = quantile(value, probs=0.25),
                  Median = median(value),
                  Mean = mean(value),
                  `3rd Qu.` = quantile(value, probs=0.75),
                  `Max.` = max(value)) %>%
        mutate(topOrBottom = "top"),
    tibble(parameter = PLOT_PARAMS,
               `Min.` = rep(NA, 10),
               `1st Qu.` = rep(NA, 10),
               Median = rep(NA, 10),
               Mean = rep(NA, 10),
               `3rd Qu.` = rep(NA, 10),
               `Max.` = rep(NA, 10),
               topOrBottom = "ZZZ")
)
# Note: might need to add a "check.names" column

# This step should have the one I want on top be LAST, because the y-axis will treat the factors like numbers with higher on top.
param_order_for_boxplot <- order(abs(regional_top_sims_df$Median - regional_bottom_sims_df$Median), decreasing=F)
most_significant_params_formatted <- setNames(paste0(param_full_names[common_significant_params], " (", round(common_sig_param_estimates, digits=3), ")")[param_order_for_boxplot],
                                              common_significant_params[param_order_for_boxplot])

# I have to wrap it in as.character because the "param" column is already a factor... thanks to reshape2::melt.
boxplot_df <- rbind(regional_top_sims_df, regional_bottom_sims_df) %>%
    mutate(param = factor(most_significant_params_formatted[as.character(param)],
                          levels = most_significant_params_formatted))

# # For vertical dashed line of regional delta prop 55+.
# vline_value <- filter(med_age_delta_data, location=="total")[["mean"]] # 11

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
    labs(x = "Reduction in 2030 Incidence vs. No Intervention (%)", y  = element_blank()) +
    scale_fill_manual(name = element_blank(),
                      labels = c(top = "Simulations with the highest 20% of parameter values",
                                 bottom = "Simulations with the lowest 20% of parameter values"),
                      values = c(top = "#00A1D5FF",
                                 bottom = "#B24745FF")) +
    theme_bw() + 
    theme(legend.position = "bottom",
          legend.text = element_text(size=10))
    # geom_vline(xintercept = vline_value, linetype="dashed")
