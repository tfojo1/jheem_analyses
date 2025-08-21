# Partial Rank Correlation Coefficient
# July 2nd, 2025

library(epiR)

# To use epiR::epi.prcc, put all parameters in a data frame.
# 1000 rows (simulations of GA)
# 300 columns (parameters) + 1 column (delta proportion 55+)

my_params <- simset_collection$get.parameters(verbose = T)
# save(my_params, file="../jheem_analyses/applications/age_analysis/my_params.Rdata")
load(file="../jheem_analyses/applications/age_analysis/my_params.Rdata")

params_df <- reshape2::melt(my_params) %>%
    pivot_wider(names_from = parameter) %>%
    filter(location=="GA") %>%
    select(-simulation, -location, -intervention)

prop_arr <- get_prop_over_55(age_results[c("2025", "2040"),,,"diagnosed.prevalence","GA",])

deltaProp_col <- reshape2::melt(prop_arr["2040",,drop=F]-prop_arr["2025",,drop=F]) %>%
    select(value)

params_df <- cbind(params_df, deltaProp_col)

prcc <- epi.prcc(params_df, sided.test=2, conf.level=0.95)

# Find top 50 significant p-value parameters
# Then find simulations with top 20% and bottom 20% value of that parameter
# I'm a bit confused that the "est" being positive doesn't always appear to
# make those with the highest value of the parameter have greater deltaProp55+
# than those with the lowest value.

num_significant <- 10
most_significant_params <- (prcc %>% arrange(-desc(p.value)) %>% head(num_significant))$var
most_sig_param_est <- setNames((prcc %>% arrange(-desc(p.value)) %>% head(num_significant))$est,
                               most_significant_params)
sorted_sims <- setNames(lapply(most_significant_params, function(param) {names(sort(my_params[param,,"GA",]))}), most_significant_params)

top_sims <- lapply(sorted_sims, function(x) {x[801:1000]})
bottom_sims <- lapply(sorted_sims, function(x) {x[1:200]})
top_sims_values <- Reduce(cbind, lapply(top_sims, function(x) {
    data.frame(value=deltaProp_col$value[as.numeric(x)])
}))
bottom_sims_values <- Reduce(cbind, lapply(bottom_sims, function(x) {
    data.frame(value=deltaProp_col$value[as.numeric(x)])
}))
colnames(top_sims_values) <- colnames(bottom_sims_values) <- most_significant_params

# Now we want to get the quartiles (five values) for the top and bottom each param
top_sims_summary <- apply(top_sims_values, 2, function(col) {
    summary(col)
})
bottom_sims_summary <- apply(bottom_sims_values, 2, function(col) {
    summary(col)
})

top_sims_df <- reshape2::melt(top_sims_summary) %>%
    rename(quartile=Var1, param=Var2) %>%
    pivot_wider(names_from = quartile) %>%
    mutate(topOrBottom = "top")
bottom_sims_df <- reshape2::melt(bottom_sims_summary) %>%
    rename(quartile=Var1, param=Var2) %>%
    pivot_wider(names_from = quartile) %>%
    mutate(topOrBottom = "bottom")

# This step should have the one I want on top be LAST, because the y-axis will treat the factors like numbers with higher on top.
param_order_for_boxplot <- order(abs(top_sims_df$Median - bottom_sims_df$Median), decreasing=F)
most_significant_params_formatted <- setNames(paste0(most_significant_params, " (", round(most_sig_param_est, digits=3), ")")[param_order_for_boxplot],
                                              most_significant_params[param_order_for_boxplot])

# I have to wrap it in as.character because the "param" column is already a factor... thanks to reshape2::melt.
boxplot_df <- rbind(top_sims_df, bottom_sims_df) %>%
    mutate(param = factor(most_significant_params_formatted[as.character(param)],
                          levels = most_significant_params_formatted))

# ISSUE: Plot order incorrect?

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
    theme(legend.position = "bottom")
ggsave(paste0("../jheem_analyses/applications/age_analysis/PRCC_figure.png"),
       plot = plot, width = 10, height = 7, dpi=300)

# This indicates that this parameter has a NEGATIVE correlation,
# though the PRCC said it was positive 0.53
# rbind(cbind(x = params_df$black.hiv.aging.multiplier.2, y = params_df$value) %>% 
#     as.data.frame() %>% 
#     top_n(200, x),
#     cbind(x = params_df$black.hiv.aging.multiplier.2, y = params_df$value) %>% 
#         as.data.frame() %>% 
#         top_n(200, -x)
# ) %>% as.data.frame() %>%
#     ggplot(aes(x=x, y = y)) +
#     geom_point() +
#     sm_statCorr()


# prcc, pcc, and delta blue vs. red, ordered by difference
# for CA and WI, perhaps
# add vline for mean from main analysis
