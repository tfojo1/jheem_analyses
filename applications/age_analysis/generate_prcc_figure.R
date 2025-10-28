# Generate the PRCC figure using the new outcome of interest, delta median age.
# 9-24-2025
# redone 10-28-2025

library(epiR)
library(tidyverse)

# To use epiR::epi.prcc, put all parameters in a data frame.
# 1000 rows (simulations of GA)
# 300 columns (parameters) + 1 column (delta proportion 55+)

# Source necessary files
source("../jheem_analyses/applications/age_analysis/helpers.R")
source("../jheem_analyses/source_code.R")

# Source necessary objects ----
load(file="../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_timeline_arr.Rdata")
load(file="../jheem_analyses/applications/age_analysis/Rdata Objects/state_order.Rdata")
load(file="../jheem_analyses/applications/age_analysis/Rdata Objects/state_order_names.Rdata")
load(file="../jheem_analyses/applications/age_analysis/Rdata Objects/params_24.Rdata")
load(file="../jheem_analyses/applications/age_analysis/Rdata Objects/med_age_delta_data.Rdata")

my_states <- state_order[1:24] # to exclude "total"

if (1==2) {
    # Data frame with one column per parameter (and cols for simulation, location, and value)
    raw_params_df <- reshape2::melt(params) %>%
        select(-intervention) %>%
        pivot_wider(names_from = parameter)
    
    # # First, testing all states individually to see if the most significant parameters
    # # are the same across the states.
    # 
    all_params_df <- lapply(my_states, function(state_code) {
        delta_med_age_col <- med_age_timeline_arr["2040",,state_code] -
            med_age_timeline_arr["2025",,state_code]
        ## WARNING! raw_params_df has parameter dimension with "simulation" and "location" as the first two values.
        tryCatch({df <- filter(raw_params_df, location==state_code) %>%
            select(-simulation, -location) %>%
            cbind(delta_med_age_col)},
            error=function(e) {browser()})
    })
    names(all_params_df) <- my_states
    
    # EACH OF THE 24 PRCC OUTPUTS IS 388 ROWS (by parameter) AND 7 COLS:
    # 'var', 'est', 'lower', 'upper', 'test.statistic', 'df', 'p.value'
    all_prccs <- lapply(all_params_df, function(df) {
        epi.prcc(df, sided.test=2, conf.level=0.95)
    })
    names(all_prccs) <- my_states
    save(all_prccs, file = "../jheem_analyses/applications/age_analysis/Rdata Objects/all_prccs.Rdata")
}
if (!(exists("all_prccs")))
    load(file = "../jheem_analyses/applications/age_analysis/Rdata Objects/all_prccs.Rdata")

# See what the top 20 most significant params are (highest p.values) for each
# and see if we have 10 that are common to all.
all_most_significant_params <- lapply(all_prccs, function(state_prcc) {
    (arrange(state_prcc, p.value))[1:20, "var"]
})
common_significant_params <- names(sort(table(unlist(all_most_significant_params)),decreasing = T)[1:10])

# # Now write their full names.
# param_full_names <- c(age3.msm.hiv.aging.multiplier.2 = "Aging rate among age 35-44\nMSM PWDH in 2020",
#                       black.hiv.aging.multiplier.2 = "Aging rate among Black PWDH\nin 2020",
#                       age3.msm.hiv.aging.multiplier.1 = "Aging rate among age 35-44\nMSM PWDH in 2010",
#                       black.hiv.aging.multiplier.1 = "Aging rate among Black PWDH\nin 2010",
#                       future.testing.slope = "Future change in testing rates",
#                       hispanic.hiv.aging.multiplier.2 = "Aging rate among Hispanic PWDH\nin 2020",
#                       age3.heterosexual.hiv.aging.multiplier.2 = "Aging rate among age 35-44\nHeterosexual PWDH in 2020",
#                       black.msm.ratio.trate.change.after.t2 = "Future change in transmission rates\namong Black MSM individuals",
#                       age4.msm.hiv.aging.multiplier.2 = "Aging rate among age 45-54 MSM PWDH\nin 2020",
#                       future.suppression.slope = "Future change in suppression rates")

# Now write their full names.
param_full_names <- c(age2.msm.hiv.aging.multiplier.2 = "Proportion of 25-34 year old MSM PWH\nwho are 34 years old in 2020",
                      black.hiv.aging.multiplier.2 = "Proportion of black PWH who are in final year\nof their age bracket in 2020",
                      black.hiv.aging.multiplier.1 = "Proportion of black PWH who are in final year\nof their age bracket in 2010",
                      age4.heterosexual.hiv.aging.multiplier.2 = "Proportion of 45-54 year old MSM PWH\nwho are 54 years old in 2020",
                      future.testing.slope = "Yearly rise in testing rates",
                      age2.msm.hiv.aging.multiplier.1 = "Proportion of 25-34 year old MSM PWH\nwho are 34 years old in 2010",
                      age3.msm.hiv.aging.multiplier.1 = "Proportion of 35-44 year old MSM PWH\nwho are 44 years old in 2010",
                      age3.msm.hiv.aging.multiplier.2 = "Proportion of 35-44 year old MSM PWH\nwho are 44 years old in 2020",
                      black.msm.ratio.trate.change.after.t2 = "Change in transmission rate among black PWH\nfrom 2020 to 2030",
                      hispanic.hiv.aging.multiplier.2 = "Proportion of hispanic PWH who are in final year\nof their age bracket in 2020")

if (!setequal(common_significant_params, names(param_full_names)))
    stop("Oops! You might need to update the significant parameters if they've changed.")

# The estimates
common_sig_param_estimates <- setNames(sapply(common_significant_params, function(param) {
    # Find which number that parameter is out of the ~388. See the warning above about why subtract 3.
    param_number <- which(names(raw_params_df) == param) - 3
    # Take the average of the estimates of the parameter's prcc across the states
    Reduce(`+`, sapply(all_prccs, function(state_prccs) {state_prccs[[param_number, "est"]]})) /
        length(my_states)}
), common_significant_params)

# Now, I find the simulations with the top and bottom values of each of
# those ten parameters, for each state.
# Then I add the top 20% of each state, ordered, to the top 20% of each
# other state. As in, CA's 2nd highest combined with FL's 2nd highest.

all_sorted_sims <- lapply(my_states, function(state_code) {
    setNames(lapply(common_significant_params, function(param) {
        names(sort(params[param,,state_code,]))
    }),
    common_significant_params)
})
names(all_sorted_sims) <- my_states

# Now, collect only the top and bottom set.
all_top_sims <- lapply(my_states, function(state_code) {
    setNames(lapply(all_sorted_sims[[state_code]], function(sims_for_param) {
        sims_for_param[801:1000]
    }),
    common_significant_params)
})
all_bottom_sims <- lapply(my_states, function(state_code) {
    setNames(lapply(all_sorted_sims[[state_code]], function(sims_for_param) {
        sims_for_param[1:200]
    }),
    common_significant_params)
})
names(all_bottom_sims) <- names(all_top_sims) <- my_states

# Now get their values for the outcome of interest.
# UPDATE: I cannot use the proportion 55+ used above, because it won't aggregate
# properly. I need to collect the age results for these states, then calculate
# the proportion 55+ on the aggregate data.

# UPPER UPDATE: With median age, I can follow most of the same steps, where we
# add every kth simulation of each state to each other, to end with 200 top (or
# bottom) simulations. Then, having the age-stratified prevalence summed, we can
# calculate median age for each.


all_top_sims_prev_values <- lapply(common_significant_params, function(param) {
    Reduce(`+`, lapply(my_states, function(state_code) {
        prev <- age_results[c("2025", "2040"),, as.numeric(all_top_sims[[state_code]][[param]]), "diagnosed.prevalence", state_code,]
    }))
})
all_bottom_sims_prev_values <- lapply(common_significant_params, function(param) {
    Reduce(`+`, lapply(my_states, function(state_code) {
        prev <- age_results[c("2025", "2040"),, as.numeric(all_bottom_sims[[state_code]][[param]]), "diagnosed.prevalence", state_code,]
    }))
})
names(all_top_sims_prev_values) <- names(all_bottom_sims_prev_values) <- common_significant_params

# # Now calculate the proportion.
# regional_top_sims_values <- lapply(common_significant_params, function(param) {
#     prop55 <- get_prop_over_55(all_top_sims_prev_values[[param]], digits=4)
#     delta <- prop55["2040",] - prop55["2025",]
# })
# regional_bottom_sims_values <- lapply(common_significant_params, function(param) {
#     prop55 <- get_prop_over_55(all_bottom_sims_prev_values[[param]], digits=4)
#     delta <- prop55["2040",] - prop55["2025",]
# })
# names(regional_top_sims_values) <- names(regional_bottom_sims_values) <- common_significant_params

# Now calculate the median age
if (1==2) {
    regional_top_sims_values <- lapply(common_significant_params, function(param) {
        medAge <- get_med_age(all_top_sims_prev_values[[param]])
        delta <- medAge["2040",] - medAge["2025",]
    })
    regional_bottom_sims_values <- lapply(common_significant_params, function(param) {
        medAge <- get_med_age(all_bottom_sims_prev_values[[param]])
        delta <- medAge["2040",] - medAge["2025",]
    })
    names(regional_top_sims_values) <- names(regional_bottom_sims_values) <- common_significant_params
    save(regional_top_sims_values, file = "../jheem_analyses/applications/age_analysis/Rdata Objects/reg_top_sims_values.Rdata") 
    save(regional_bottom_sims_values, file = "../jheem_analyses/applications/age_analysis/Rdata Objects/reg_bottom_sims_values.Rdata")
}

if (!exists(regional_top_sims_values))
    load(file = "../jheem_analyses/applications/age_analysis/Rdata Objects/reg_top_sims_values.Rdata")
if (!exists(regional_bottom_sims_values))
    load(file = "../jheem_analyses/applications/age_analysis/Rdata Objects/reg_bottom_sims_values.Rdata")


# Convert to data frame
regional_top_df <- Reduce(cbind, regional_top_sims_values)
regional_bottom_df <- Reduce(cbind, regional_bottom_sims_values)
colnames(regional_top_df) <- colnames(regional_bottom_df) <- common_significant_params

# Get five quartile values.
regional_top_sims_summary <- apply(regional_top_df, 2, function(col) {summary(col)})
regional_bottom_sims_summary <- apply(regional_bottom_df, 2, function(col) {summary(col)})

regional_top_sims_df <- reshape2::melt(regional_top_sims_summary) %>%
    rename(quartile=Var1, param=Var2) %>%
    pivot_wider(names_from = quartile) %>%
    mutate(topOrBottom = "top")
regional_bottom_sims_df <- reshape2::melt(regional_bottom_sims_summary) %>%
    rename(quartile=Var1, param=Var2) %>%
    pivot_wider(names_from = quartile) %>%
    mutate(topOrBottom = "bottom")

# This step should have the one I want on top be LAST, because the y-axis will treat the factors like numbers with higher on top.
param_order_for_boxplot <- order(abs(regional_top_sims_df$Median - regional_bottom_sims_df$Median), decreasing=F)
most_significant_params_formatted <- setNames(paste0(param_full_names[common_significant_params], " (", round(common_sig_param_estimates, digits=3), ")")[param_order_for_boxplot], 
                                              common_significant_params[param_order_for_boxplot])

# I have to wrap it in as.character because the "param" column is already a factor... thanks to reshape2::melt.
boxplot_df <- rbind(regional_top_sims_df, regional_bottom_sims_df) %>%
    mutate(param = factor(most_significant_params_formatted[as.character(param)],
                          levels = most_significant_params_formatted))

# For vertical dashed line of regional delta prop 55+.
vline_value <- filter(med_age_delta_data, location=="total")[["mean"]] # 11

plot <- ggplot(boxplot_df) +
    geom_boxplot(aes(xmin = `Min.`,
                     xlower = `1st Qu.`,
                     xmiddle = Median,
                     xupper = `3rd Qu.`,
                     xmax = `Max.`,
                     y = param,
                     fill = topOrBottom),
                 stat="identity",
                 position="dodge") +
    labs(x = "Change in Median Age (years)", y  = element_blank()) +
    scale_fill_discrete(name = element_blank(),
                        labels = c(top = "Simulations with the highest 20% of parameter values",
                                   bottom = "Simulations with the lowest 20% of parameter values")) +
    theme_bw() + 
    theme(legend.position = "bottom") +
    geom_vline(xintercept = vline_value, linetype="dashed")
ggsave(paste0("../jheem_analyses/applications/age_analysis/Figures/Manuscript/PRCC_figure.png"),
       plot = plot, width = 10, height = 7, dpi=300)
