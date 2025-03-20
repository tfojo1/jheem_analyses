
# Define years and locations
YEARS.TO.CONSIDER = as.character(2025:2030)
LOCATIONS = dimnames(full.results.sub)$location  # Extract all MSAs

# Extract relevant dimensions
years = dimnames(full.results.sub)$year
locations = dimnames(full.results.sub)$location
interventions = dimnames(full.results.sub)$intervention

# Subset only "loseRW" intervention
intervention_idx = which(interventions == "loseRW")

# Extract incidence data 
incidence_int = apply(full.results.sub[YEARS.TO.CONSIDER, , , , , , "incidence", , intervention_idx, drop = F], c('sim', 'location'), sum, na.rm=T)
incidence_noint = apply(full.results.sub[YEARS.TO.CONSIDER, , , , , , "incidence", , "noint", drop = F], c('sim', 'location'), sum, na.rm=T)

# Compute relative incidence per location
relative_incidence = ifelse(incidence_noint == 0, 0, (incidence_int - incidence_noint) / incidence_noint) # Same array shape

dimnames(relative_incidence)
    
loseRW_parameters = all.parameters[,,,intervention_idx, drop = T]
Noint_parameters = all.parameters[,,,"noint", drop = T]
Parameter_shift = (loseRW_parameters %>% replace(is.na(.), 0)) - 
  (Noint_parameters %>% replace(is.na(.), 0))

# Extract relevant dimension names
locations = dimnames(relative_incidence)$location
parameters = dimnames(Parameter_shift)$parameter

# Initialize a results storage data frame
prcc_results = data.frame(parameter = character(), location = character(), PRCC = numeric())

# Compute PRCC per location
for (loc_idx in seq_along(locations)) {
  loc_name = locations[loc_idx]
  
  # Extract relative incidence for this location
  rel_inc_vec = relative_incidence[, loc_idx]  # Vector of 100 simulations
  
  # Skip if all values are NA or constant
  if (all(is.na(rel_inc_vec)) || length(unique(rel_inc_vec)) < 2) next
  
  # Residualize relative incidence
  res_incidence = residuals(lm(rel_inc_vec ~ 1, na.action = na.exclude))
  
  for (param_idx in seq_along(parameters)) {
    param_name = parameters[param_idx]
    
    # Extract the parameter values for this location across simulations
    param_vec = loseRW_parameters[param_idx, , loc_idx]  # Vector of 100 simulations
    
    # Skip if all values are NA or constant
    if (all(is.na(param_vec)) || length(unique(param_vec)) < 2) next
    
    # Residualize the parameter
    res_param = residuals(lm(param_vec ~ 1, na.action = na.exclude))
    
    # Compute Spearman correlation (PRCC)
    prcc_value = cor(res_param, res_incidence, method = "spearman", use = "complete.obs")
    
    # Store result
    prcc_results = rbind(prcc_results, data.frame(parameter = param_name, location = loc_name, PRCC = prcc_value))
  }
}



# Defined the parameter list from RYAN.WHITE.PARAMETERS.PRIOR
rw_parameters_list <- c(
  "non.adap.or", "non.adap.msm.or", "non.adap.msm.idu.or", "non.adap.idu.male.or",
  "non.adap.idu.female.or", "non.adap.heterosexual.male.or", "non.adap.heterosexual.female.or",
  "non.adap.black.or", "non.adap.hispanic.or", "non.adap.other.or",
  "non.adap.age1.or", "non.adap.age2.or", "non.adap.age3.or", "non.adap.age4.or", "non.adap.age5.or",
  "oahs.or", "oahs.msm.or", "oahs.msm.idu.or", "oahs.idu.male.or", "oahs.idu.female.or",
  "oahs.heterosexual.male.or", "oahs.heterosexual.female.or",
  "oahs.black.or", "oahs.hispanic.or", "oahs.other.or",
  "oahs.age1.or", "oahs.age2.or", "oahs.age3.or", "oahs.age4.or", "oahs.age5.or",
  "adap.or", "adap.msm.or", "adap.msm.idu.or", "adap.idu.male.or", "adap.idu.female.or",
  "adap.heterosexual.male.or", "adap.heterosexual.female.or",
  "adap.black.or", "adap.hispanic.or", "adap.other.or",
  "adap.age1.or", "adap.age2.or", "adap.age3.or", "adap.age4.or", "adap.age5.or",
  "proportion.adap.without.non.adap.rw",
  "rw.suppression.or", "rw.suppression.slope.or",
  "rw.suppression.msm.or", "rw.suppression.msm.idu.or", "rw.suppression.idu.male.or",
  "rw.suppression.idu.female.or", "rw.suppression.heterosexual.male.or", "rw.suppression.heterosexual.female.or",
  "rw.suppression.black.or", "rw.suppression.hispanic.or", "rw.suppression.other.or",
  "rw.suppression.age1.or", "rw.suppression.age2.or", "rw.suppression.age3.or",
  "rw.suppression.age4.or", "rw.suppression.age5.or",
  "rw.suppression.black.slope.or", "rw.suppression.hispanic.slope.or", "rw.suppression.other.slope.or",
  "adap.vs.oahs.suppression.or", "non.oahs.vs.oahs.suppression.or", "lose.adap.effect",
  "lose.oahs.effect","lose.rw.support.effect" 
)

# Compute median PRCC per parameter across all locations
top_10_params = prcc_results %>%
  filter(parameter %in% rw_parameters_list) %>%
  group_by(parameter) %>%
  summarize(median_PRCC = median(abs(PRCC), na.rm = TRUE)) %>%
  arrange(desc(median_PRCC)) %>%
  slice(1:10) %>%
  pull(parameter)  # Extract top 10 parameter names

# Filter results to only keep the selected top 10 parameters
filtered_prcc_results = prcc_results %>%
  filter(parameter %in% top_10_params) %>%
  mutate(parameter = as.character(parameter))


# Plot results using ggplot2
PRCC = ggplot(filtered_prcc_results, aes(x = PRCC, y = reorder(parameter, PRCC))) +
  geom_boxplot( fill = "#619CFF") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1) +  # Dashed line at 0
  theme_minimal() +
  labs(
    x = "Partial Rank Correlation Coefficient (PRCC)",
    y = "Parameter",
  )

PRCC
################################
# Define influential parameters
selected_parameters = filtered_prcc_results %>%
  group_by(parameter) %>%
  summarize(median_PRCC = median(PRCC, na.rm = TRUE)) %>%
  arrange(median_PRCC) %>%
  pull(parameter) 

# Initialize results storage
comparison_results <- data.frame(parameter = character(), 
                                 location = character(), 
                                 group = character(), 
                                 median_incidence = numeric(), 
                                 stringsAsFactors = FALSE)



# Iterate over each parameter and location
for (param in selected_parameters) {      
  for (loc in dimnames(relative_incidence)$location) {  
    
    # Extract values for the parameter and corresponding HIV incidence
    param_values <- Parameter_shift[param, , loc]  # Parameter values across simulations
    inc_values <- relative_incidence[, loc]  # Corresponding HIV incidence across simulations
    
    # Ensure there are no NA values before ranking
    ranked_idx <- order(param_values, decreasing = TRUE, na.last = NA)  # Descending order
    num_simulations <- length(ranked_idx)
    
    # Ensure we have enough simulations to select a top/bottom 20%
    top_20_idx <- ranked_idx[1:floor(num_simulations * 0.2)]  # Highest parameter values
    bottom_20_idx <- ranked_idx[(num_simulations - floor(num_simulations * 0.2) + 1):num_simulations]  # Lowest parameter values
      
      # Compute median of top 20% incidence values
    median_top_20 <- median(inc_values[top_20_idx], na.rm = TRUE)
    median_bottom_20 <- median(inc_values[bottom_20_idx], na.rm = TRUE)
      
      # Store results
    comparison_results <- rbind(comparison_results, 
                                  data.frame(parameter = param, location = loc, group = "Top 20%", median_incidence = median_top_20),
                                  data.frame(parameter = param, location = loc, group = "Bottom 20%", median_incidence = median_bottom_20))
    }
  }

prcc_plot_build <- ggplot_build(PRCC)
extracted_order <- prcc_plot_build$layout$panel_params[[1]]$y$get_labels()

comparison_results <- comparison_results %>%
  mutate(parameter = factor(parameter, levels = extracted_order))

# Create box plot
difference <- ggplot(comparison_results, aes(x = median_incidence*100, y = parameter, fill = group)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +  # Boxplot without outliers
  geom_vline(xintercept = 50, linetype = "dashed", color = "black", size = 1) +
  scale_fill_manual(values = c("Top 20%" = "blue", "Bottom 20%" = "orange")) +
  theme_minimal() +
  labs(
    x = "Median % Relative HIV Incidence ",
    y = "Parameter",
    fill = "Group"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
difference

ggpubr::ggarrange(PRCC, difference,
                  ncol = 1, nrow = 2, 
                  common.legend = F)


##### update in-text paragraph 7

# === Identify the Three Most Influential Parameters === #
top_3_params <- prcc_results %>%
  filter(parameter %in% rw_parameters_list) %>%
  group_by(parameter) %>%
  summarize(median_PRCC = median(abs(PRCC), na.rm = TRUE)) %>%
  arrange(desc(median_PRCC)) %>%
  slice(1:3) %>%
  pull(parameter)

# Extract the corresponding median PRCC values
top_3_prcc_values <- prcc_results %>%
  filter(parameter %in% top_3_params) %>%
  group_by(parameter) %>%
  summarize(median_PRCC = median(abs(PRCC), na.rm = TRUE)) %>%
  pull(median_PRCC)

# === Compute Relative Increase in Infections for ADAP Suppression Loss === #
# Filter for ADAP-related suppression loss parameter
adap_suppression_results <- comparison_results %>%
  filter(grepl("adap", parameter, ignore.case = TRUE))

# Compute the mean relative increase in infections for top & bottom 20%
top_20_mean_incidence <- adap_suppression_results %>%
  filter(group == "Top 20%") %>%
  summarize(mean_incidence = mean(median_incidence, na.rm = TRUE)) %>%
  pull(mean_incidence) * 100  # Convert to percentage

bottom_20_mean_incidence <- adap_suppression_results %>%
  filter(group == "Bottom 20%") %>%
  summarize(mean_incidence = mean(median_incidence, na.rm = TRUE)) %>%
  pull(mean_incidence) * 100  # Convert to percentage

# === Format Output for Report === #
cat(sprintf(
  "\nIn our probabilistic sensitivity analyses, the three most influential parameters were %s, %s, and %s, 
  with median PRCCs by location of %.3f, %.3f, and %.3f, respectively. 
  In the 20%% of simulations with the highest values of %s, the by-location mean relative increase in infections for indefinite cessation of 
  Ryan White scenario as compared to continuation was %.0f%%; in the 20%% of simulations with the lowest values of %s, 
  the relative increase in infections was %.0f%%.\n",
  top_3_params[1], top_3_params[2], top_3_params[3],
  top_3_prcc_values[1], top_3_prcc_values[2], top_3_prcc_values[3],
  top_3_params[1], top_20_mean_incidence,
  top_3_params[1], bottom_20_mean_incidence
))



