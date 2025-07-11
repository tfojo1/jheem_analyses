# Partial Rank Correlation Coefficient
# July 2nd, 2025

library(epiR)

# To use epiR::epi.prcc, put all parameters in a data frame.
# 1000 rows (simulations of GA)
# 300 columns (parameters) + 1 column (delta proportion 55+)

my_params <- simset_collection$get.parameters(verbose = T)
save(my_params, file="../jheem_analyses/applications/age_analysis/my_params.Rdata")

params_df <- reshape2::melt(my_params) %>%
    pivot_wider(names_from = parameter) %>%
    filter(location=="GA") %>%
    select(-simulation, -location, -intervention)

prop_arr <- get_prop_over_55(age_results[c("2025", "2040"),,,"diagnosed.prevalence","GA",])

deltaProp_col <- reshape2::melt(prop_arr["2040",,drop=F]-prop_arr["2025",,drop=F]) %>%
    select(value)

params_df <- cbind(params_df, deltaProp_col)

prcc <- epi.prcc(params_df, sided.test=2, conf.level=0.95)

# Find top 10 significant p-value parameters
# Then find simulations with top 20% and bottom 20% value of that parameter
# I'm a bit confused that the "est" being positive doesn't always appear to
# make those with the highest value of the parameter have greater deltaProp55+
# than those with the lowest value.

most_significant_params <- (prcc %>% arrange(-desc(p.value)) %>% head(10))$var
most_sig_param_est <- (prcc %>% arrange(-desc(p.value)) %>% head(10))$est
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

prcc_df <- rbind(top_sims_values %>% mutate(quantile="top", sim=1:200),
                 bottom_sims_values %>% mutate(quantile="bottom", sim=801:1000))%>%
    pivot_longer(names_to = "param", cols = -c(quantile, sim))

ggplot(prcc_df) + geom_boxplot(aes(x=value, y=param, fill=quantile),
                               position="dodge")


# This indicates that this parameter has a NEGATIVE correlation,
# though the PRCC said it was positive 0.53
rbind(cbind(x = params_df$black.hiv.aging.multiplier.2, y = params_df$value) %>% 
    as.data.frame() %>% 
    top_n(200, x),
    cbind(x = params_df$black.hiv.aging.multiplier.2, y = params_df$value) %>% 
        as.data.frame() %>% 
        top_n(200, -x)
) %>% as.data.frame() %>%
    ggplot(aes(x=x, y = y)) +
    geom_point() +
    sm_statCorr()

# prcc, pcc, and delta blue vs. red, ordered by difference
# for CA and WI, perhaps
# add vline for mean from main analysis