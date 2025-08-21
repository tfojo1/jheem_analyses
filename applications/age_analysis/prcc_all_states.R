# PRCC for all states
# 8-19-2025

library(epiR)

# To use epiR::epi.prcc, put all parameters in a data frame.
# 1000 rows (simulations of GA)
# 300 columns (parameters) + 1 column (delta proportion 55+)

# load(file="../jheem_analyses/applications/age_analysis/Rdata Objects/my_params.Rdata")
# raw_param_df <- reshape2::melt(my_params) %>%
#     pivot_wider(names_from = parameter)
# 
# my_states <- state_order[1:24] # to exclude "total"
# 
# # First, testing all states individually to see if the most significant parameters
# # are the same across the states.
# 
all_params_df <- lapply(my_states, function(state_code) {
    prop_arr <- get_prop_over_55(age_results[c("2025", "2040"),,,"diagnosed.prevalence", state_code,])
    deltaProp_col <- reshape2::melt(prop_arr["2040",,drop=F]-prop_arr["2025",,drop=F]) %>%
        select(value)

    ## WARNING! raw_param_df has parameter dimension with "simulation", "location", "intervention" as the first three values.
    df <- filter(raw_param_df, location==state_code) %>%
        select(-simulation, -location, -intervention) %>%
        cbind(deltaProp_col)
})
names(all_params_df) <- my_states
# 
# all_prccs <- lapply(all_params_df, function(df) {
#     epi.prcc(df, sided.test=2, conf.level=0.95)
# })
# names(all_prccs) <- my_states

# save(all_prccs, file="../jheem_analyses/applications/age_analysis/Rdata Objects/all_prccs.Rdata")
load("../jheem_analyses/applications/age_analysis/Rdata Objects/all_prccs.Rdata")

num_significant <- 10

# Returns list of data frames.
all_most_significant_params <- lapply(all_prccs, function(prcc) {
    (prcc %>% arrange(-desc(p.value)) %>% head(num_significant))$var
})
names(all_most_significant_params) <- my_states

# They mostly seem to have the same top ones; I'll only keep the ten most common.
common_significant_params <- names(sort(table(unlist(all_most_significant_params)),decreasing = T)[1:10])

# Now write their full names.
param_full_names <- c(age3.msm.hiv.aging.multiplier.2 = "Aging rate among age 35-44\nMSM PWDH in 2020",
                      black.hiv.aging.multiplier.2 = "Aging rate among Black PWDH\nin 2020",
                      age3.msm.hiv.aging.multiplier.1 = "Aging rate among age 35-44\nMSM PWDH in 2010",
                      black.hiv.aging.multiplier.1 = "Aging rate among Black PWDH\nin 2010",
                      future.testing.slope = "Future change in testing rates",
                      hispanic.hiv.aging.multiplier.2 = "Aging rate among Hispanic PWDH\nin 2020",
                      age3.heterosexual.hiv.aging.multiplier.2 = "Aging rate among age 34-44\nHeterosexual PWDH in 2020",
                      black.msm.ratio.trate.change.after.t2 = "Future change in transmission rates\namong Black MSM individuals",
                      age4.msm.hiv.aging.multiplier.2 = "Aging rate among age 45-54 MSM PWDH\nin 2020",
                      future.suppression.slope = "Future change in suppression rates")
if (!setequal(common_significant_params, names(param_full_names)))
    stop("Oops! You might need to update the significant parameters if they've changed.")

# The estimates
common_sig_param_estimates <- setNames(sapply(common_significant_params, function(param) {
    # Find which number that parameter is out of the ~388. See the warning above about why subtract 3.
    param_number <- which(names(raw_param_df) == param) - 3
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
        names(sort(my_params[param,,state_code,]))
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

# Now calculate the proportion.
regional_top_sims_values <- lapply(common_significant_params, function(param) {
    prop55 <- get_prop_over_55(all_top_sims_prev_values[[param]], digits=4)
    delta <- prop55["2040",] - prop55["2025",]
})
regional_bottom_sims_values <- lapply(common_significant_params, function(param) {
    prop55 <- get_prop_over_55(all_bottom_sims_prev_values[[param]], digits=4)
    delta <- prop55["2040",] - prop55["2025",]
})
names(regional_top_sims_values) <- names(regional_bottom_sims_values) <- common_significant_params

# Convert to data frame
regional_top_df <- Reduce(cbind, regional_top_sims_values)
regional_bottom_df <- Reduce(cbind, regional_bottom_sims_values)
colnames(regional_top_df) <- colnames(regional_bottom_df) <- common_significant_params

# Take get five quartile values.
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
vline_value <- mean(prop_delta["total",]) # 9.76

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
    labs(x = "Change in Proportion Age 55+ (%)", y  = element_blank()) +
    scale_fill_discrete(name = element_blank(),
                        labels = c(top = "Simulations with the highest 20% of parameter values",
                                   bottom = "Simulations with the lowest 20% of parameter values")) +
    theme_bw() + 
    theme(legend.position = "bottom") +
    geom_vline(xintercept = vline_value, linetype="dashed")
ggsave(paste0("../jheem_analyses/applications/age_analysis/PRCC_figure.png"),
       plot = plot, width = 10, height = 7, dpi=300)
