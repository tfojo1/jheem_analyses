# Install necessary libraries if they are not already installed
packages = c("ggplot2", "tidyverse", "broom", "ks", "ggpubr", 
              "gridExtra", "gt", "sf", "usmap", "flextable", "officer",
              "scales", "sensitivity", "ppcor")

# Check if each package is installed, and install missing ones
install_if_missing = function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Apply the function to each package
sapply(packages, install_if_missing)

# Load necessary libraries
library(ggplot2)
library(tidyverse)
library(broom)
library(ks)       
library(ggpubr)   
library(gridExtra)
library(gt)
library(sf)
library(usmap)
library(scales)
library(flextable)
library(officer)
library(sensitivity)
library(ppcor)


# Suppress warnings and messages globally
options(warn = -1)  # Suppress warnings
suppressMessages({
  suppressWarnings({

################################################################################
################################################################################
################################################################################
################################################################################
#==========================Survey Analysis=====================================#
################################################################################
################################################################################
################################################################################
################################################################################


# Load data
rw_survey = read.csv("../jheem_analyses/applications/ryan_white/rw_survey.csv")
file_path = "../jheem_analyses/applications/ryan_white/rw_survey_174.csv"
df = read.csv(file_path, stringsAsFactors = FALSE)
colnames(df) = colnames(rw_survey) 

# Identify columns that contain "Checked"/"Unchecked" values
binary_cols = sapply(df, function(col) all(col %in% c("Checked", "Unchecked", NA)))

# Convert "Checked" to 1 and "Unchecked" to 0
df[binary_cols] = lapply(df[binary_cols], function(col) ifelse(col == "Checked", 1, 0))

# Save the cleaned dataset
write.csv(df, "../jheem_analyses/applications/ryan_white/rw_survey.csv", row.names = FALSE)

colnames(rw_survey) = gsub("^X\\.|\\.$", "", colnames(rw_survey))  # Remove unwanted prefixes/suffixes
colnames(rw_survey) = gsub("\\.+", "_", colnames(rw_survey))  # Replace multiple dots with underscores
#=========== Plot original distribution =======================================#

# Reshape the data from wide to long format
rw_survey_long = rw_survey %>%
  dplyr::select(q1_adap_loss, q2_oahs_loss, q3_support_loss) %>%  # Select columns to plot
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Plot density curves
ggplot(rw_survey_long, aes(x = value, fill = variable, color = variable)) +
  geom_density(alpha = 0.5) +  # Semi-transparent overlapping densities
  scale_fill_manual(values = c("blue", "green", "red")) +  # Custom fill colors
  scale_color_manual(values = c("blue", "green", "red")) +  # Match line colors
  labs(title = "Density Plot of Loss Variables",
       x = "% Loss",
       y = "Density",
       fill = "Variable") +
  theme_minimal()
#=========== Transformation divergence computed ===============================#

compute_kl_divergence = function(P_samples, Q_samples) {
  # Fit KDE with added regularization
  H_P = Hpi(x = P_samples) + diag(ncol(P_samples)) * 1e-6
  H_Q = Hpi(x = Q_samples) + diag(ncol(Q_samples)) * 1e-6
  
  P_kde = kde(x = P_samples, H = H_P)
  Q_kde = kde(x = Q_samples, H = H_Q)
  
  # Evaluate densities at P sample points
  P_density = predict(P_kde, x = P_samples)
  Q_density = predict(Q_kde, x = P_samples)
  
  # Avoid log(0) and numerical instability
  epsilon = 1e-10
  P_density = pmax(P_density, epsilon)
  Q_density = pmax(Q_density, epsilon)
  
  # Compute KL divergence
  KL_div = sum(P_density * log(P_density / Q_density)) / length(P_density)
  
  # Ensure KL is non-negative
  return(max(KL_div, 0))
}

compare_kl_divergence = function(n, rw_survey) {
  # Extract relevant columns and remove rows with NA values
  X = rw_survey[, c("q1_adap_loss", "q2_oahs_loss", "q3_support_loss")]
  X = X[complete.cases(X), ] / 100
  X = as.matrix(X)
  
  # Ensure values are strictly within (0,1)
  epsilon = 1e-6
  X = pmax(pmin(X, 1 - epsilon), epsilon)
  
  # Get number of rows in X
  num_rows = nrow(X)
  
  # --- 1. Apply Transformations ---
  arcsin_X = asin(sqrt(X))
  logit_X  = log(X / (1 - X))
  probit_X = qnorm(X)
  
  # --- 2. Fit KDE in Transformed Space ---
  kde_arcsin = kde(x = arcsin_X)
  kde_logit  = kde(x = logit_X)
  kde_probit = kde(x = probit_X)
  kde_original = kde(x = X)
  
  # --- 3. Sample 'n' Points from KDE ---
  sampled_arcsin = rkde(n, kde_arcsin)
  sampled_logit  = rkde(n, kde_logit)
  sampled_probit = rkde(n, kde_probit)
  
  # --- 4. Back-Transform to Original Scale ---
  sampled_arcsin = (sin(sampled_arcsin))^2
  sampled_logit  = exp(sampled_logit) / (1 + exp(sampled_logit))
  sampled_probit = pnorm(sampled_probit)
  
  # --- 5. Bootstrap Sampling (FIXED) ---
  sampled_bootstrap = X[sample(1:num_rows, size = n, replace = TRUE), ]
  
  # --- 6. Compute KL Divergence ---
  kl_arcsin = compute_kl_divergence(X, sampled_arcsin)
  kl_logit  = compute_kl_divergence(X, sampled_logit)
  kl_probit = compute_kl_divergence(X, sampled_probit)
  kl_bootstrap = compute_kl_divergence(X, sampled_bootstrap)  # Corrected baseline comparison
  
  # Return KL divergence values
  return(data.frame(
    Transformation = c("Arcsin-Sqrt", "Logit", "Probit", "Bootstrap"),
    KL_Divergence = c(kl_arcsin, kl_logit, kl_probit, kl_bootstrap)
  ))
}


# Function to apply transformations, fit KDE, sample, and plot pairwise comparisons
plot_pairwise_transformations_vs_original = function(n, rw_survey) {
  # Extract relevant columns and remove rows with NA values
  X = rw_survey[, c("q1_adap_loss", "q2_oahs_loss", "q3_support_loss")]
  X = X[complete.cases(X), ] / 100  # Convert percentages to proportions
  X = as.matrix(X)
  
  # Ensure values are strictly within (0,1)
  epsilon = 1e-6
  X = pmax(pmin(X, 1 - epsilon), epsilon)
  
  # --- 1. Apply Transformations ---
  arcsin_X = asin(sqrt(X))   # Arcsin-Sqrt transformation
  logit_X  = log(X / (1 - X)) # Logit transformation
  probit_X = qnorm(X)         # Inverse CDF (Probit)
  
  # --- 2. Fit KDE in Transformed Space ---
  kde_arcsin = kde(x = arcsin_X)
  kde_logit  = kde(x = logit_X)
  kde_probit = kde(x = probit_X)
  
  # --- 3. Sample 'n' Points from KDE ---
  sampled_arcsin = rkde(n, kde_arcsin)
  sampled_logit  = rkde(n, kde_logit)
  sampled_probit = rkde(n, kde_probit)
  
  # --- 4. Back-Transform to Original Scale ---
  sampled_arcsin = (sin(sampled_arcsin))^2
  sampled_logit  = exp(sampled_logit) / (1 + exp(sampled_logit))
  sampled_probit = pnorm(sampled_probit)
  
  # --- 5. Bootstrap Sampling ---
  sampled_bootstrap = X[sample(1:nrow(X), size = n, replace = TRUE), ]
  
  # --- 6. Store Transformations in a List ---
  transformations = list(
    "Arcsin-Sqrt" = sampled_arcsin,
    "Logit" = sampled_logit,
    "Probit" = sampled_probit,
    "Bootstrap" = sampled_bootstrap
  )
  
  variables = c("q1_adap_loss", "q2_oahs_loss", "q3_support_loss")
  
  # --- 7. Generate KDE Plots for Each Transformation and Each Variable ---
  plot_list = list()
  for (trans_name in names(transformations)) {
    sampled_data = transformations[[trans_name]]
    
    for (i in 1:3) {
      # Extract variable data
      original_var = X[, i]
      transformed_var = sampled_data[, i]
      
      # Convert to long format for ggplot
      original_df = data.frame(Value = original_var, Type = "Original")
      transformed_df = data.frame(Value = transformed_var, Type = trans_name)
      combined_df = rbind(original_df, transformed_df)
      
      # Generate KDE plot
      p = ggplot(combined_df, aes(x = Value, fill = Type)) +
        geom_density(alpha = 0.5) +
        theme_minimal() +
        labs(title = paste("Original vs.", trans_name, "\nVariable:", variables[i]),
             x = "Value", y = "Density") +
        theme(legend.position = "right")
      
      # Store plot in list
      plot_list[[paste(trans_name, variables[i], sep = "_")]] = p
    }
  }
  
  # --- 8. Arrange all plots in a grid ---
  combined_plot = ggarrange(plotlist = plot_list, ncol = 3, nrow = 4)
  
  return(combined_plot)
}

# Run function and plot
compare_plot = plot_pairwise_transformations_vs_original(1000, rw_survey)
compare_kl_divergence(1000, rw_survey)
print(compare_plot)

#=========== Logistic regression of RW support loss ==========================#

# Define Medicaid expansion status and Census region for each state
state_info = tribble(
  ~state,              ~medicaid_expansion, ~census_region,
  "Alabama",           "No",                "South",
  "Alaska",            "Yes",               "West",
  "Arizona",           "Yes",               "West",
  "Arkansas",          "Yes",               "South",
  "California",        "Yes",               "West",
  "Colorado",          "Yes",               "West",
  "Connecticut",       "Yes",               "Northeast",
  "Delaware",          "Yes",               "South",
  "District_of_Columbia", "Yes",           "South",
  "Florida",           "No",                "South",
  "Georgia",           "No",                "South",
  "Hawaii",            "Yes",               "West",
  "Idaho",             "Yes",               "West",
  "Illinois",          "Yes",               "Midwest",
  "Indiana",           "Yes",               "Midwest",
  "Iowa",              "Yes",               "Midwest",
  "Kansas",            "No",                "Midwest",
  "Kentucky",          "Yes",               "South",
  "Louisiana",         "Yes",               "South",
  "Maine",             "Yes",               "Northeast",
  "Maryland",          "Yes",               "South",
  "Massachusetts",     "Yes",               "Northeast",
  "Michigan",          "Yes",               "Midwest",
  "Minnesota",         "Yes",               "Midwest",
  "Mississippi",       "No",                "South",
  "Missouri",          "Yes",               "Midwest",
  "Montana",           "Yes",               "West",
  "Nebraska",          "Yes",               "Midwest",
  "Nevada",            "Yes",               "West",
  "New_Hampshire",     "Yes",               "Northeast",
  "New_Jersey",        "Yes",               "Northeast",
  "New_Mexico",        "Yes",               "West",
  "New_York",          "Yes",               "Northeast",
  "North_Carolina",    "Yes",               "South",
  "North_Dakota",      "Yes",               "Midwest",
  "Ohio",              "Yes",               "Midwest",
  "Oklahoma",          "Yes",               "South",
  "Oregon",            "Yes",               "West",
  "Pennsylvania",      "Yes",               "Northeast",
  "Rhode_Island",      "Yes",               "Northeast",
  "South_Carolina",    "No",                "South",
  "South_Dakota",      "Yes",               "Midwest",
  "Tennessee",         "No",                "South",
  "Texas",             "No",                "South",
  "Utah",              "Yes",               "West",
  "Vermont",           "Yes",               "Northeast",
  "Virginia",          "Yes",               "South",
  "Washington",        "Yes",               "West",
  "West_Virginia",     "Yes",               "South",
  "Wisconsin",         "No",                "Midwest",
  "Wyoming",           "No",                "West"
)

rw_survey_clean = rw_survey %>%
  rename_with(~ gsub("^X\\.choice\\.|\\.$", "", .)) %>% # Clean state column names
  dplyr::select(-c("choice_I_dont_know","choice_I_dont_know_1", "choice_I_dont_know_2","choice_I_dont_know_3"))

# Convert from wide to long format for state choices
rw_survey_long = rw_survey_clean %>%
  pivot_longer(cols = starts_with("choice_"), names_to = "state", values_to = "selected") %>%
  filter(selected == 1) %>%  # Keep only selected states
  dplyr::select(-selected) %>%  # Remove redundant column
  mutate(state = gsub("^choice_", "", state)) 

# Merge with Medicaid expansion and Census region data
rw_survey_final = rw_survey_long %>%
  left_join(state_info, by = "state")

rw_survey_final = rw_survey_final %>%
  mutate(
    medicaid_expansion = as.factor(medicaid_expansion),  # Yes/No
    q4_rural_percentage = q4_rural_percentage / 100, 
    rural_region = ifelse(q4_rural_percentage > 0.50, 1, 0),  # Define rural region
    rural_region = as.factor(rural_region),  
    q1_adap_loss = as.numeric(q1_adap_loss),  # Ensure predictors are numeric
    q2_oahs_loss = as.numeric(q2_oahs_loss),
    q3_support_loss = as.numeric(q3_support_loss),
    # Create separate binary indicators for each Census Region
    census_Midwest = ifelse(census_region == "Midwest", 1, 0),
    census_Northeast = ifelse(census_region == "Northeast", 1, 0),
    census_South = ifelse(census_region == "South", 1, 0),
    census_West = ifelse(census_region == "West", 1, 0)
  )




# Logistic regression for Medicaid expansion (Yes/No)
medicaid_model = glm(medicaid_expansion ~ q1_adap_loss + q2_oahs_loss + q3_support_loss,
                      data = rw_survey_final, family = binomial)
summary(medicaid_model)

# Logistic regression for each Census Region separately
midwest_model = glm(census_Midwest ~ q1_adap_loss + q2_oahs_loss + q3_support_loss, 
                     data = rw_survey_final, family = binomial)

northeast_model = glm(census_Northeast ~ q1_adap_loss + q2_oahs_loss + q3_support_loss, 
                       data = rw_survey_final, family = binomial)

south_model = glm(census_South ~ q1_adap_loss + q2_oahs_loss + q3_support_loss, 
                   data = rw_survey_final, family = binomial)

west_model = glm(census_West ~ q1_adap_loss + q2_oahs_loss + q3_support_loss, 
                  data = rw_survey_final, family = binomial)

# Logistic regression for Rural region
rural_model = glm(rural_region ~ q1_adap_loss + q2_oahs_loss + q3_support_loss,
                   data = rw_survey_final, family = binomial)
summary(rural_model)


# Function to extract OR, CIs, and p-values
extract_model_data = function(model, outcome_name) {
  tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(outcome = outcome_name)
}

# Extract results for each model
medicaid_results = extract_model_data(medicaid_model, "Medicaid Expansion")
midwest_results = extract_model_data(midwest_model, "Midwest")
northeast_results = extract_model_data(northeast_model, "Northeast")
south_results = extract_model_data(south_model, "South")
west_results = extract_model_data(west_model, "West")
rural_results = extract_model_data(rural_model, "Rural Region") 

# Combine results
model_results = bind_rows(medicaid_results, rural_results, midwest_results, northeast_results, south_results, west_results)

# Clean variable names for plotting
model_results = model_results %>%
  rename(Odds_Ratio = estimate, Lower_CI = conf.low, Upper_CI = conf.high) %>%
  filter(term != "(Intercept)")

model_results$term =  gsub("_"," ", gsub("q[0-9]","%", model_results$term))

# Forest plot using ggplot2
forest = ggplot(model_results, aes(x = term, y = Odds_Ratio, ymin = Lower_CI, ymax = Upper_CI, color = outcome)) +
  geom_pointrange(position = position_dodge(width = 0.5)) +  # Points & CIs
  geom_hline(yintercept = 1, linetype = "dashed") +  # Reference line at OR = 1
  coord_flip() +  # Flip coordinates for better readability
  labs(
    x = "Predictor Variables",
    y = "Odds Ratio (95% CI)"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())


#============================Figure 2==========================================#

plot_arcsin_sqrt_vs_original = function(n, rw_survey) {
  # Extract relevant columns and remove rows with NA values
  X = rw_survey[, c("q1_adap_loss", "q2_oahs_loss", "q3_support_loss")]
  X = X[complete.cases(X), ] / 100  # Convert percentages to proportions
  X = as.matrix(X)
  
  # Ensure values are strictly within (0,1)
  epsilon = 1e-6
  X = pmax(pmin(X, 1 - epsilon), epsilon)
  
  # --- 1. Apply Arcsin-Sqrt Transformation ---
  arcsin_X = asin(sqrt(X))
  
  # --- 2. Fit KDE in Transformed Space ---
  kde_arcsin = kde(x = arcsin_X)
  
  # --- 3. Sample 'n' Points from KDE ---
  sampled_arcsin = rkde(n, kde_arcsin)
  
  # --- 4. Back-Transform to Original Scale ---
  sampled_arcsin = ((sin(sampled_arcsin))^2)
  
  # --- 5. Generate Density Plots for Each Variable ---
  variables = c("q1_adap_loss", "q2_oahs_loss", "q3_support_loss")
  plot_list = list()
  
  for (i in 1:3) {
    original_var = X[, i]*100
    transformed_var = sampled_arcsin[, i]*100
    
    # Convert to long format for ggplot
    original_df = data.frame(Value = original_var, Type = "Original")
    transformed_df = data.frame(Value = transformed_var, Type = "Arcsin-Sqrt")
    combined_df = rbind(original_df, transformed_df)
    
    # Clean variable name: remove "q#", replace underscores with spaces     
    clean_variable =  gsub("_", " ", gsub("q[0-9]", "", variables[i]))  
    
    # Generate the histogram for the original and density for the transformed     
    p = ggplot() +       
      geom_histogram(data = original_df, aes(x = Value, fill = Type),                       
                     bins = 30, alpha = 0.5, position = "identity") +       
      geom_density(data = transformed_df, aes(x = Value, color = Type, y = ..count..),                     
                   size = 1.2, alpha = 0.8) +  # Scale density to match counts       
      theme_minimal() +       
      labs(x = paste("%", clean_variable), y = "Count") +       
      theme(legend.position = "right") +       
      scale_fill_manual(values = c("Original" = "blue")) +  # Customize fill color       
      scale_color_manual(values = c("Arcsin-Sqrt" = "red"))  # Customize line color          
    
    # Store each plot in list
    plot_list[[variables[i]]] = p  
  }
  
  return(plot_list)
}
# Run function
figures_1_3 = plot_arcsin_sqrt_vs_original(1000, rw_survey)

######################
rw_survey_long = rw_survey %>%
  dplyr::select(q4_rural_percentage) %>%  # Select columns to plot
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Plot density curves
rural = ggplot(rw_survey_long, aes(x = value, fill = variable)) +
  geom_histogram() +
  scale_fill_manual(values = c( "q4_rural_percentage"= "#00BFC4")) +
  labs(
    x = "% Rural Treated",
    y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

rural

#######################
# First, arrange the top 4 plots in a 2x2 grid
top_grid = ggarrange(
  figures_1_3[["q1_adap_loss"]], 
  figures_1_3[["q2_oahs_loss"]], 
  figures_1_3[["q3_support_loss"]], 
  rural, 
  ncol = 2, nrow = 2,
  labels = c("A","B","C","D")
)

# Now, center the forest plot beneath the top grid
figure_2 = ggarrange(
  top_grid, 
  forest, 
  ncol = 1, 
  heights = c(2, 1),  
  widths = c(0.8),
  labels = c("","E")
)


figure_2

ggsave("prelim_results/figure_2_rw.png", plot = figure_2, width = 10, height = 10, dpi = 300)

#=======================Supp. Analyses=========================================#

# Categorize the data and count occurrences
rw_survey_pie = rw_survey %>%
  mutate(category = ifelse(q4_rural_percentage > 50, "Above 50%", "Below 50%")) %>%
  count(category) %>%
  mutate(label = paste0(n))  # Create labels for counts

# Plot pie chart with labels
ggplot(rw_survey_pie, aes(x = "", y = n, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("Above 50%" = "#00BFC4", "Below 50%" = "#F8766D")) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 5) +  # Add labels
  labs(fill = "Category", title = "Distribution of % Rural Above and Below 50%") +
  theme_void()



# Categorize the data and count occurrences
rw_survey_pie = rw_survey_final %>%
  mutate(category = ifelse(medicaid_expansion == "Yes", "Medicaid expansion", "Non medicaid expansion")) %>%
  count(category) %>%
  mutate(label = paste0(n))  # Create labels for counts

# Plot pie chart with labels
ggplot(rw_survey_pie, aes(x = "", y = n, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("Medicaid expansion" = "#00BFC4", "Non medicaid expansion" = "#F8766D")) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 5) +  # Add labels
  labs(fill = "Category", title = "Distribution of responses by ") +
  theme_void()
#######################

library(gt)

# Compute mean for Q1-Q3 in different groups
summary_table = rw_survey_final %>%
  summarize(
    Group = "Everyone",
    Q1 = median(q1_adap_loss, na.rm = TRUE),
    Q2 = median(q2_oahs_loss, na.rm = TRUE),
    Q3 = median(q3_support_loss, na.rm = TRUE)
  ) 
mediciad_summary = rw_survey_final %>%
  mutate(Group = ifelse(medicaid_expansion == "Yes", "Medicaid expansion", "No medicaid expansion")) %>%
  group_by(Group) %>%
  summarize(
    Q1 = median(q1_adap_loss, na.rm = TRUE),
    Q2 = median(q2_oahs_loss, na.rm = TRUE),
    Q3 = median(q3_support_loss, na.rm = TRUE)
  ) 
rural_summary = rw_survey %>%
  filter(!is.na(q4_rural_percentage)) %>%  # Remove NA values first
  mutate(Group = ifelse(q4_rural_percentage >= 50, "Rural ≥ 50%", "Rural < 50%")) %>%
  group_by(Group) %>%
  summarize(
    Q1 = median(q1_adap_loss, na.rm = TRUE),
    Q2 = median(q2_oahs_loss, na.rm = TRUE),
    Q3 = median(q3_support_loss, na.rm = TRUE)
  )
census_region_summary =  rw_survey_final %>%
  group_by(census_region) %>%
  summarize(
    Q1 = median(q1_adap_loss, na.rm = TRUE),
    Q2 = median(q2_oahs_loss, na.rm = TRUE),
    Q3 = median(q3_support_loss, na.rm = TRUE)
  ) %>% rename(Group = census_region) 

summary_table = rbind(summary_table, mediciad_summary,rural_summary,census_region_summary)
# Create a nicely formatted table with gt
summary_table %>%
  gt() %>%
  tab_header(
    title = "Summary Table of Mean Loss for Q1-Q3",
    subtitle = "Comparing Groups: Medicaid Expansion, Rural, and Census Regions"
  ) %>%
  cols_label(
    Group = "Group",
    Q1 = "Median ADAP Loss",
    Q2 = "Median OAHS Loss",
    Q3 = "Median \n Other RWAP Support Loss"
  ) %>%
  fmt_number(
    columns = c(Q1, Q2, Q3),
    decimals = 2
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(12)
  )



#=======================Supp. Figure 2=========================================#
library(sf)
library(usmap)

# Aggregate the number of respondents per state
state_counts = rw_survey_final %>%
  count(state, name = "respondents")

# Get US states shapefile
us_states = us_map("states")

# Merge state respondent counts with shapefile
us_states = us_states %>%
  left_join(state_counts, by = c("full" = "state"))

# Replace NA with 0 for states with no respondents
us_states$respondents[is.na(us_states$respondents)] = 0

# Plot the USA choropleth map
usa_map = plot_usmap(data = state_counts, values = "respondents", regions = "states") +
  scale_fill_gradient(low = "#d1e5f0", high = "#2166ac", na.value = "gray90", name = "Respondents") +
  theme_void() +
  labs(title = "Survey Respondents per State", subtitle = "Ryan White Clinic Survey Data") +
  theme(legend.position = "right")


# Aggregate the number of respondents per state
state_counts = rw_survey_final %>%
  count(state, name = "respondents") %>%
  arrange(desc(respondents))  # Sort states by respondent count

# Create a bar plot
ranking = ggplot(state_counts, aes(x = reorder(state, respondents), y = respondents, fill = respondents)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_c(option = "plasma", name = "Respondents") +
  coord_flip() +  # Flip for better readability
  theme_minimal() +
  labs(
    x = "State",
    y = "Number of Respondents"
  ) +
  theme(legend.position = "none")

combined_plot = ggarrange(usa_map, ranking, ncol = 1, widths = c(1.5, 1))

# Display the combined plot
print(combined_plot)


#=======================In-text paragraph 1====================================#

# Compute IQR as a range (Q1 - Q3)
q1_rural = quantile(rw_survey_final$q4_rural_percentage, probs = 0.25, na.rm = TRUE)
q3_rural = quantile(rw_survey_final$q4_rural_percentage, probs = 0.75, na.rm = TRUE)

q1_adap_loss = quantile(rw_survey_final$q1_adap_loss, probs = 0.25, na.rm = TRUE)
q3_adap_loss = quantile(rw_survey_final$q1_adap_loss, probs = 0.75, na.rm = TRUE)

q1_oahs_loss = quantile(rw_survey_final$q2_oahs_loss, probs = 0.25, na.rm = TRUE)
q3_oahs_loss = quantile(rw_survey_final$q2_oahs_loss, probs = 0.75, na.rm = TRUE)

q1_support_loss = quantile(rw_survey_final$q3_support_loss, probs = 0.25, na.rm = TRUE)
q3_support_loss = quantile(rw_survey_final$q3_support_loss, probs = 0.75, na.rm = TRUE)


# Count unique Medicaid expansion and non-expansion states
medicaid_expansion_states = rw_survey_final %>%
  filter(medicaid_expansion == "Yes") %>%
  distinct(state) %>%
  nrow()

non_medicaid_expansion_states = rw_survey_final %>%
  filter(medicaid_expansion == "No") %>%
  distinct(state) %>%
  nrow()

# Extract values dynamically from the dataset
num_respondents = length(unique(rw_survey_final$Record_ID))
num_states = length(unique(rw_survey_final$state))
medicaid_expansion_states = rw_survey_final %>%
  filter(medicaid_expansion == "Yes") %>%
  distinct(state) %>%
  nrow()
non_medicaid_expansion_states = rw_survey_final %>%
  filter(medicaid_expansion == "No") %>%
  distinct(state) %>%
  nrow()
# Rural proportion
median_rural = median(rw_survey_final$q4_rural_percentage, na.rm = TRUE)
iqr_rural = quantile(rw_survey_final$q4_rural_percentage, probs = c(0.25, 0.75), na.rm = TRUE)

# Expected losses in viral suppression
median_adap_loss = median(rw_survey_final$q1_adap_loss, na.rm = TRUE)
iqr_adap_loss = quantile(rw_survey_final$q1_adap_loss, probs = c(0.25, 0.75), na.rm = TRUE)

median_oahs_loss = median(rw_survey_final$q2_oahs_loss, na.rm = TRUE)
iqr_oahs_loss = quantile(rw_survey_final$q2_oahs_loss, probs = c(0.25, 0.75), na.rm = TRUE)

median_support_loss = median(rw_survey_final$q3_support_loss, na.rm = TRUE)
iqr_support_loss = quantile(rw_survey_final$q3_support_loss, probs = c(0.25, 0.75), na.rm = TRUE)


output_file = "prelim_results/results_filled_in.txt"

# Open the file for appending
sink(output_file, append = F)
cat("\n")

# Print formatted summary
cat(sprintf("\n%d Ryan White clinic directors, administrators, and health officials familiar with Ryan White clinical programs completed our survey. Respondents cared for patients across %d states within all four census regions, for which %d states are Medicaid expansion states (Supplemental Figure S2).\n",
            num_respondents, num_states, medicaid_expansion_states))

cat(sprintf("\nThe median proportion of patients from rural areas was %.0f%% (interquartile range [IQR] %.0f–%.0f%%).\n",
            median_rural, iqr_rural[1], iqr_rural[2]))

cat(sprintf("\nThe median expected losses in viral suppression were %.0f%% (IQR %.0f–%.0f%%) among recipients of any AIDS drug assistance, %.0f%% (IQR %.0f–%.0f%%) among outpatient ambulatory health services recipients who were not receiving AIDS drug assistance, and %.2f%% (IQR %.0f–%.1f%%) among Ryan White clients receiving neither AIDS drug assistance nor outpatient ambulatory health services (Figure 2 A-D).\n",
            median_adap_loss, iqr_adap_loss[1], iqr_adap_loss[2],
            median_oahs_loss, iqr_oahs_loss[1], iqr_oahs_loss[2],
            median_support_loss, iqr_support_loss[1], iqr_support_loss[2]))
cat("\n")
sink()


################################################################################
################################################################################
################################################################################
################################################################################
#==========================Simulation Analysis=================================#
################################################################################
################################################################################
################################################################################
################################################################################


#========================== Figure 3 ==========================================#

simset.baseline = load.simulation.set("Q:/simulations/rw/full.with.covid2-100/C.26420/rw_full.with.covid2-100_C.26420_baseline.Rdata")
simset.noint = load.simulation.set("Q:/simulations/rw/full.with.covid2-100/C.26420/rw_full.with.covid2-100_C.26420_noint.Rdata")
simset.lose = load.simulation.set("Q:/simulations/rw/full.with.covid2-100/C.26420/rw_full.with.covid2-100_C.26420_loseRW.Rdata")
simset.templose = load.simulation.set("Q:/simulations/rw/full.with.covid2-100/C.26420/rw_full.with.covid2-100_C.26420_temploseRW.Rdata")


#simset.baseline = load.simulation.set("/Users/ryanforster/Downloads/simsets_test/rw_full.with.covid2-100_C.26420_baseline.Rdata")
#simset.noint = load.simulation.set("/Users/ryanforster/Downloads/simsets_test/rw_full.with.covid2-100_C.26420_noint.Rdata")
#simset.lose = load.simulation.set("/Users/ryanforster/Downloads/simsets_test/rw_full.with.covid2-100_C.26420_loseRW.Rdata")
#simset.templose = load.simulation.set("/Users/ryanforster/Downloads/simsets_test/rw_full.with.covid2-100_C.26420_temploseRW.Rdata")


PLOT.YEARS.INT = 2015:2035


new_legend_labels = c("Continuation", "Cessation or Interruption")
# Generate the plots
cessation_incidence = simplot(simset.noint, simset.lose, c('incidence'), 
                               dimension.values = list(year = PLOT.YEARS.INT), 
                               summary.type = 'mean.and.interval') +   
  ggplot2::theme_bw() + 
  ggtitle(NULL) +   
  theme(axis.title.x = element_blank()) +  # Keep x-axis label for bottom row
  scale_linetype_manual(name = "Intervention Type", 
                        values = c("solid", "dashed"), 
                        labels = new_legend_labels)

cessation_new = simplot(simset.noint, simset.lose, c('new'), 
                         dimension.values = list(year = PLOT.YEARS.INT), 
                         summary.type = 'mean.and.interval') +   
  ggplot2::theme_bw() + 
  ggtitle(NULL) +   
  theme(axis.title.x = element_blank()) +  
  scale_linetype_manual(name = "Intervention Type", 
                        values = c("solid", "dashed"), 
                        labels = new_legend_labels)

interruption_incidence = simplot(simset.noint, simset.templose, c('incidence'), 
                                  dimension.values = list(year = PLOT.YEARS.INT), 
                                  summary.type = 'mean.and.interval') +   
  ggplot2::theme_bw() + 
  ggtitle(NULL) +  # Keep x-axis label for bottom row
  scale_linetype_manual(name = "Intervention Type", 
                        values = c("solid", "dashed"), 
                        labels = new_legend_labels)

interruption_new = simplot(simset.noint, simset.templose, c('new'), 
                            dimension.values = list(year = PLOT.YEARS.INT), 
                            summary.type = 'mean.and.interval') +   
  ggplot2::theme_bw() + 
  ggtitle(NULL) +  # Keep x-axis label for bottom row
  scale_linetype_manual(name = "Intervention Type", 
                        values = c("solid", "dashed"), 
                        labels = new_legend_labels)


# Arrange plots with panel labels and section labels
p = ggarrange(cessation_incidence, cessation_new, 
               interruption_incidence, interruption_new,
               ncol = 2, nrow = 2, 
               align = "hv",
               common.legend = TRUE,
               labels = c("A", "B", "C", "D"),  # Panel labels
               label.x = 0.05, label.y = 1.02)  # Adjust position of labels

# Add scenario labels for Cessation and Interruption
final_plot = p
  
  
# Print final figure
print(final_plot)


ggsave("prelim_results/figure_3_rw.png", plot = final_plot, width = 10, height = 10, dpi = 300)

#========================== In-text paragraphs 2-6 ============================#


####### Paragraphs 2 -> 4 in results
# Extract indices for intervention scenarios
intervention_idx_noint = which(dimnames(full.results)$intervention == "noint")        # RW services continue
intervention_idx_loseRW = which(dimnames(full.results)$intervention == "loseRW")      # RW services stop
intervention_idx_tempRW = which(dimnames(full.results)$intervention == "temploseRW")  # RW resumes in 2029

# === INCIDENCE: Compute Total Incident Cases for Each Scenario === #
inc_noint_2025_2030 = apply(full.results[as.character(2025:2030), , , , , , "incidence", , intervention_idx_noint, drop = FALSE], c(6, 8), sum, na.rm = TRUE)
inc_noint_2025_2035 = apply(full.results[as.character(2025:2035), , , , , , "incidence", , intervention_idx_noint, drop = FALSE], c(6, 8), sum, na.rm = TRUE)

inc_loseRW_2025_2030 = apply(full.results[as.character(2025:2030), , , , , , "incidence", , intervention_idx_loseRW, drop = FALSE], c(6, 8), sum, na.rm = TRUE)
inc_loseRW_2025_2035 = apply(full.results[as.character(2025:2035), , , , , , "incidence", , intervention_idx_loseRW, drop = FALSE], c(6, 8), sum, na.rm = TRUE)

inc_tempRW_2025_2030 = apply(full.results[as.character(2025:2030), , , , , , "incidence", , intervention_idx_tempRW, drop = FALSE], c(6, 8), sum, na.rm = TRUE)
inc_tempRW_2025_2035 = apply(full.results[as.character(2025:2035), , , , , , "incidence", , intervention_idx_tempRW, drop = FALSE], c(6, 8), sum, na.rm = TRUE)

# Compute medians & credible intervals
XX_inc_noint_2025_2030 = median(inc_noint_2025_2030, na.rm = TRUE)
CI_inc_noint_2025_2030 = quantile(inc_noint_2025_2030, probs = c(0.025, 0.975), na.rm = TRUE)

XX_inc_noint_2025_2035 = median(inc_noint_2025_2035, na.rm = TRUE)
CI_inc_noint_2025_2035 = quantile(inc_noint_2025_2035, probs = c(0.025, 0.975), na.rm = TRUE)

XX_inc_loseRW_2025_2030 = median(inc_loseRW_2025_2030, na.rm = TRUE)
CI_inc_loseRW_2025_2030 = quantile(inc_loseRW_2025_2030, probs = c(0.025, 0.975), na.rm = TRUE)

XX_inc_loseRW_2025_2035 = median(inc_loseRW_2025_2035, na.rm = TRUE)
CI_inc_loseRW_2025_2035 = quantile(inc_loseRW_2025_2035, probs = c(0.025, 0.975), na.rm = TRUE)

XX_inc_tempRW_2025_2030 = median(inc_tempRW_2025_2030, na.rm = TRUE)
CI_inc_tempRW_2025_2030 = quantile(inc_tempRW_2025_2030, probs = c(0.025, 0.975), na.rm = TRUE)

XX_inc_tempRW_2025_2035 = median(inc_tempRW_2025_2035, na.rm = TRUE)
CI_inc_tempRW_2025_2035 = quantile(inc_tempRW_2025_2035, probs = c(0.025, 0.975), na.rm = TRUE)

# Compute % increase in incidence between scenarios (correct denominators)
inc_increase_RW_2025_2030 = (inc_loseRW_2025_2030 - inc_noint_2025_2030) / inc_noint_2025_2030 * 100
XX_inc_increase_RW_2025_2030 = median(inc_increase_RW_2025_2030, na.rm = TRUE)
CI_inc_increase_RW_2025_2030 = quantile(inc_increase_RW_2025_2030, probs = c(0.025, 0.975), na.rm = TRUE)

inc_increase_RW_2025_2035 = (inc_loseRW_2025_2035 - inc_noint_2025_2035) / inc_noint_2025_2035 * 100
XX_inc_increase_RW_2025_2035 = median(inc_increase_RW_2025_2035, na.rm = TRUE)
CI_inc_increase_RW_2025_2035 = quantile(inc_increase_RW_2025_2035, probs = c(0.025, 0.975), na.rm = TRUE)

inc_increase_tempRW_2025_2030 = (inc_tempRW_2025_2030 - inc_noint_2025_2030) / inc_noint_2025_2030 * 100
XX_inc_increase_tempRW_2025_2030 = median(inc_increase_tempRW_2025_2030, na.rm = TRUE)
CI_inc_increase_tempRW_2025_2030 = quantile(inc_increase_tempRW_2025_2030, probs = c(0.025, 0.975), na.rm = TRUE)

inc_increase_tempRW_2025_2035 = (inc_tempRW_2025_2035 - inc_noint_2025_2035) / inc_noint_2025_2035 * 100
XX_inc_increase_tempRW_2025_2035 = median(inc_increase_tempRW_2025_2035, na.rm = TRUE)
CI_inc_increase_tempRW_2025_2035 = quantile(inc_increase_tempRW_2025_2035, probs = c(0.025, 0.975), na.rm = TRUE)

# === PRINT RESULTS === #
sink(output_file, append = TRUE)
cat("\n")

cat(sprintf("\nIf Ryan White programs continue uninterrupted, we project %.0f incident HIV infections from 2025 through 2030 (95%% credible interval %.0f to %.0f) across all 31 cities (Table 1). Through 2035, we project %.0f (%.0f to %.0f) incident infections across all cities.\n",
            XX_inc_noint_2025_2030, CI_inc_noint_2025_2030[1], CI_inc_noint_2025_2030[2], 
            XX_inc_noint_2025_2035, CI_inc_noint_2025_2035[1], CI_inc_noint_2025_2035[2]))

cat(sprintf("\nIf Ryan White services cease in July 2025 and do not resume, we project the number of incident infections across all 31 cities will rise to %.0f (%.0f - %.0f) – an increase of %.1f%% (%.1f to %.1f%%) over the infections projected in the RWHAP continuation scenario. Incident infections across all cities through 2035 would increase by %.1f%% (%.1f to %.1f%%).\n",
            XX_inc_loseRW_2025_2030, CI_inc_loseRW_2025_2030[1], CI_inc_loseRW_2025_2030[2], 
            XX_inc_increase_RW_2025_2030, CI_inc_increase_RW_2025_2030[1], CI_inc_increase_RW_2025_2030[2], 
            XX_inc_increase_RW_2025_2035, CI_inc_increase_RW_2025_2035[1], CI_inc_increase_RW_2025_2035[2]))

cat(sprintf("\nIf Ryan White services cease in July 2025 and resume in January 2029, we project %.0f incident infections (%.0f to %.0f) across all 31 cities – an increase of %.1f%% (%.1f to %.1f%%) vs. continuing the RWHAP. Through 2035, incident infections across all cities would increase by %.1f%% (%.1f to %.1f%%).\n",
            XX_inc_tempRW_2025_2030, CI_inc_tempRW_2025_2030[1], CI_inc_tempRW_2025_2030[2], 
            XX_inc_increase_tempRW_2025_2030, CI_inc_increase_tempRW_2025_2030[1], CI_inc_increase_tempRW_2025_2030[2], 
            XX_inc_increase_tempRW_2025_2035, CI_inc_increase_tempRW_2025_2035[1], CI_inc_increase_tempRW_2025_2035[2]))

cat("\n")
sink()

####### Paragraphs 5 & 6 in results
# Subset full.results to 2025-2030 for incidence calculations
full.results.sub = full.results[as.character(2025:2030), , , , , , , , ]

# Define relevant indices
age_under_35 = c("13-24 years", "25-34 years")  # Young adults
race_groups = c("black", "hispanic")            # Black & Hispanic populations
interventions = dimnames(full.results)$intervention
locations = dimnames(full.results)$location

# Extract indices for intervention scenarios
intervention_idx_noint = which(interventions == "noint")        # RW services continue
intervention_idx_loseRW = which(interventions == "loseRW")      # RW services stop
intervention_idx_tempRW = which(interventions == "temploseRW")  # RW resumes in 2029

# === INCIDENCE: Compute % Increase Between Scenarios === #
# Extract total incidence per scenario (Summing over age groups & race)
inc_noint_2025_2030 = apply(full.results.sub[, , , , , , "incidence", , intervention_idx_noint, drop = FALSE], c(6, 8), sum, na.rm = TRUE)
inc_loseRW_2025_2030 = apply(full.results.sub[, , , , , , "incidence", , intervention_idx_loseRW, drop = FALSE], c(6, 8), sum, na.rm = TRUE)
inc_tempRW_2025_2030 = apply(full.results.sub[, , , , , , "incidence", , intervention_idx_tempRW, drop = FALSE], c(6, 8), sum, na.rm = TRUE)

# Extract total incidence through 2035 for full comparison
inc_noint_2025_2035 = apply(full.results[as.character(2025:2035), , , , , , "incidence", , intervention_idx_noint, drop = FALSE], c(6, 8), sum, na.rm = TRUE)
inc_loseRW_2025_2035 = apply(full.results[as.character(2025:2035), , , , , , "incidence", , intervention_idx_loseRW, drop = FALSE], c(6, 8), sum, na.rm = TRUE)

# Compute % increase in incidence compared to 'noint' scenario
inc_increase_RW = (inc_loseRW_2025_2030 - inc_noint_2025_2030) / inc_noint_2025_2030 * 100
XX_inc = median(inc_increase_RW, na.rm = TRUE)
CI_inc = quantile(inc_increase_RW, probs = c(0.025, 0.975), na.rm = TRUE)

# Compute % increase through 2035
inc_increase_RW_2035 = (inc_loseRW_2025_2035 - inc_noint_2025_2035) / inc_noint_2025_2035 * 100
XX_inc_2035 = median(inc_increase_RW_2035, na.rm = TRUE)
CI_inc_2035 = quantile(inc_increase_RW_2035, probs = c(0.025, 0.975), na.rm = TRUE)

# Compute % increase separately for Black and Hispanic populations
inc_black_noint = apply(full.results.sub[, , "black", , , , "incidence", , intervention_idx_noint, drop = FALSE], c(6, 8), sum, na.rm = TRUE)
inc_black_loseRW = apply(full.results.sub[, , "black", , , , "incidence", , intervention_idx_loseRW, drop = FALSE], c(6, 8), sum, na.rm = TRUE)

black_increase = (inc_black_loseRW - inc_black_noint) / inc_black_noint * 100
XX_black = median(black_increase, na.rm = TRUE)
CI_black = quantile(black_increase, probs = c(0.025, 0.975), na.rm = TRUE)

inc_hisp_noint = apply(full.results.sub[, , "hispanic", , , , "incidence", , intervention_idx_noint, drop = FALSE], c(6, 8), sum, na.rm = TRUE)
inc_hisp_loseRW = apply(full.results.sub[, , "hispanic", , , , "incidence", , intervention_idx_loseRW, drop = FALSE], c(6, 8), sum, na.rm = TRUE)

hisp_increase = (inc_hisp_loseRW - inc_hisp_noint) / inc_hisp_noint * 100
XX_hisp = median(hisp_increase, na.rm = TRUE)
CI_hisp = quantile(hisp_increase, probs = c(0.025, 0.975), na.rm = TRUE)

# === HIV DIAGNOSES: Compute Total New Diagnoses for Each Scenario === #

# Extract diagnoses for the full period (2025-2035)
diag_noint_2025_2035 = apply(full.results[as.character(2025:2035), , , , , , "new", , intervention_idx_noint, drop = FALSE], c(6, 8), sum, na.rm = TRUE)
diag_loseRW_2025_2035 = apply(full.results[as.character(2025:2035), , , , , , "new", , intervention_idx_loseRW, drop = FALSE], c(6, 8), sum, na.rm = TRUE)

# Extract diagnoses for the period 2025-2030 (used for temporary cessation scenario)
diag_noint_2025_2030 = apply(full.results[as.character(2025:2030), , , , , , "new", , intervention_idx_noint, drop = FALSE], c(6, 8), sum, na.rm = TRUE)
diag_tempRW_2025_2030 = apply(full.results[as.character(2025:2030), , , , , , "new", , intervention_idx_tempRW, drop = FALSE], c(6, 8), sum, na.rm = TRUE)

# Compute total diagnoses & 95% CI for full period
XX_diag_noint = median(diag_noint_2025_2035, na.rm = TRUE)
CI_diag_noint = quantile(diag_noint_2025_2035, probs = c(0.025, 0.975), na.rm = TRUE)

XX_diag_loseRW = median(diag_loseRW_2025_2035, na.rm = TRUE)
CI_diag_loseRW = quantile(diag_loseRW_2025_2035, probs = c(0.025, 0.975), na.rm = TRUE)

# Compute total diagnoses & 95% CI for tempRW scenario (2025-2030)
XX_diag_tempRW = median(diag_tempRW_2025_2030, na.rm = TRUE)
CI_diag_tempRW = quantile(diag_tempRW_2025_2030, probs = c(0.025, 0.975), na.rm = TRUE)

# Compute % increase in diagnoses between scenarios
diag_increase_RW = (diag_loseRW_2025_2035 - diag_noint_2025_2035) / diag_noint_2025_2035 * 100
XX_diag_increase_RW = median(diag_increase_RW, na.rm = TRUE)
CI_diag_increase_RW = quantile(diag_increase_RW, probs = c(0.025, 0.975), na.rm = TRUE)

# Corrected % increase for tempRW scenario (ensuring same years in numerator and denominator)
diag_increase_tempRW = (diag_tempRW_2025_2030 - diag_noint_2025_2030) / diag_noint_2025_2030 * 100
XX_diag_increase_tempRW = median(diag_increase_tempRW, na.rm = TRUE)
CI_diag_increase_tempRW = quantile(diag_increase_tempRW, probs = c(0.025, 0.975), na.rm = TRUE)

# === PRINT RESULTS === #

# Open the file for appending
sink(output_file, append = T)
cat("\n")

cat(sprintf("\nOur projections show that the increased infections from 2025 through 2030 fell principally on adults under 35 years old – an %.1f%% (%.1f to %.1f%%) increase across all 31 cities – and on Black and Hispanic populations – an increase of %.1f%% (%.1f to %.1f%%) among Black and %.1f%% (%.1f to %.1f%%) among Hispanic city residents.\n",
            XX_inc, CI_inc[1], CI_inc[2], XX_black, CI_black[1], CI_black[2], XX_hisp, CI_hisp[1], CI_hisp[2]))

cat(sprintf("\nIncreases in HIV diagnoses lagged behind incidence. If Ryan White services continue uninterrupted, we project %.0f new diagnoses from 2025 through 2035 across all 31 cities (95%% credible interval %.0f to %.0f). If Ryan White services stop indefinitely in July 2025, this would rise to %.0f diagnoses (%.0f to %.0f) – an increase of %.1f%% (%.1f to %.1f%%). If Ryan White services stop in July 2025 and resume in January 2029, we project %.0f diagnoses from 2025 through 2030 (%.0f to %.0f) – an increase of %.1f%% (%.1f to %.1f%%).\n",
            XX_diag_noint, CI_diag_noint[1], CI_diag_noint[2], XX_diag_loseRW, CI_diag_loseRW[1], CI_diag_loseRW[2], XX_diag_increase_RW, CI_diag_increase_RW[1], CI_diag_increase_RW[2],
            XX_diag_tempRW, CI_diag_tempRW[1], CI_diag_tempRW[2], XX_diag_increase_tempRW, CI_diag_increase_tempRW[1], CI_diag_increase_tempRW[2]))

cat("\n")
sink()
#========================== Figure 4 ==========================================#

abs.total.infections.averted.loseRW.by.city = apply(total.incidence[YEARS.TO.CONSIDER,,,'loseRW',drop=F] - total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)
rel.total.infections.averted.loseRW.by.city = abs.total.infections.averted.loseRW.by.city  / apply(total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)

abs.total.infections.averted.temploseRW.by.city = apply(total.incidence[YEARS.TO.CONSIDER,,,'temploseRW',drop=F] - total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)
rel.total.infections.averted.temploseRW.by.city = abs.total.infections.averted.temploseRW.by.city  / apply(total.incidence[YEARS.TO.CONSIDER,,,'noint',drop=F], c('sim','location'), sum, na.rm=T)

rel.total.infections.averted.loseRW.by.city
dimnames(rel.total.infections.averted.loseRW.by.city)[[2]] = get.location.name(dimnames(rel.total.infections.averted.loseRW.by.city)[[2]])
rel.total.infections.averted.temploseRW.by.city
dimnames(rel.total.infections.averted.temploseRW.by.city)[[2]] = get.location.name(dimnames(rel.total.infections.averted.temploseRW.by.city)[[2]])

# Convert both arrays to data frames
df_indefinite = as.data.frame(as.table(rel.total.infections.averted.loseRW.by.city)) 
df_temporary = as.data.frame(as.table(rel.total.infections.averted.temploseRW.by.city))

# Rename columns
colnames(df_indefinite) = c("Simulation", "MSA", "Relative_Increase")
colnames(df_temporary) = c("Simulation", "MSA", "Relative_Increase")

# Add a column indicating cessation type
df_indefinite$Cessation_Type = "Cessation"
df_temporary$Cessation_Type = "Interruption"

# Combine the two datasets
df_combined = bind_rows(df_indefinite, df_temporary)

blank_MSAs = c(
  "Memphis, TN-MS-AR",
  "Washington-Arlington-Alexandria, DC-VA-MD-WV"
)

# Filter out the blank MSAs
df_combined = df_combined %>%
  filter(!(MSA %in% blank_MSAs))

# Compute median increase for sorting
median_increase = df_combined %>%
  group_by(MSA) %>%
  summarise(Median_Increase = median(Relative_Increase, na.rm = TRUE)) %>%
  arrange(Median_Increase)

# Convert City to a factor to ensure correct ordering in the plot
df_combined$MSA = factor(df_combined$MSA, levels = median_increase$MSA)

# Compute national medians for each cessation type
national_medians = df_combined %>%
  group_by(Cessation_Type) %>%
  summarise(National_Median = median(Relative_Increase, na.rm = TRUE) * 100,
            MAD = mad(Relative_Increase, na.rm = TRUE) * 100)

# Merge shading data back into the main dataframe
df_combined = left_join(df_combined, national_medians, by = "Cessation_Type")

df_national = df_combined %>%
  group_by(Cessation_Type) %>%
  summarise(Relative_Increase = list(Relative_Increase)) %>%
  unnest(cols = Relative_Increase) %>%
  mutate(MSA = "National Trend")  # Label for national boxplot only

figure_4 = ggplot(df_combined, aes(x = Relative_Increase * 100, y = MSA, fill = Cessation_Type)) +
  # Add boxplots for MSA-level data
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  # Add separate boxplot for national trend
  geom_boxplot(data = df_national, aes(x = Relative_Increase * 100, y = "National Trend", fill = Cessation_Type),
               outlier.shape = NA, alpha = 0.7) +
  # Define colors for boxplots
  scale_fill_manual(values = c("Cessation" = "#E69F00", "Interruption" = "#56B4E9")) +
  theme_minimal() +
  labs(
    x = "% Relative Increase in Cases",
    y = "Metropolitan Statistical Areas (MSAs)",
    fill = "Scenario"
  ) +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size = 10)
  )


ggsave("prelim_results/figure_4_rw.png", plot = figure_4, width = 10, height = 10, dpi = 300)

#=========================Table 1==============================================#

# Define relevant years and interventions
YEARS = as.character(2025:2030)
LOCATIONS = dimnames(total.incidence)$location
INTERVENTIONS = dimnames(full.results)$intervention

# Identify intervention indices
intervention_noint = which(INTERVENTIONS == "noint")         # Continuation of RWHAP
intervention_loseRW = which(INTERVENTIONS == "loseRW")       # Cessation of RWHAP
intervention_tempRW = which(INTERVENTIONS == "temploseRW")   # Interruption of RWHAP

# Aggregate incidence across selected years (sum over years), keeping (location, sim)
inc_noint = apply(total.incidence[YEARS, , , intervention_noint, drop = FALSE], 
                   MARGIN = c('sim','location'), # (location, sim)
                   FUN = sum, na.rm = TRUE)

averted_loseRW = apply(total.incidence[YEARS, , , intervention_loseRW, drop = FALSE] - total.incidence[YEARS, , , intervention_noint, drop = FALSE], 
                    MARGIN = c('sim','location'), 
                    FUN = sum, na.rm = TRUE)

averted_tempRW = apply(total.incidence[YEARS, , , intervention_tempRW, drop = FALSE] - total.incidence[YEARS, , , intervention_noint, drop = FALSE], 
                    MARGIN = c('sim','location'), 
                    FUN = sum, na.rm = TRUE)


# Compute relative percent reduction
rel_averted_loseRW = (averted_loseRW / inc_noint) * 100
rel_averted_tempRW = (averted_tempRW / inc_noint) * 100

# Function to compute median and credible intervals (2.5%, 97.5%)
compute_ci = function(data) { 
  apply(data, 2, function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)) 
}

# Compute median values (across sims)
med_noint = apply(inc_noint, 2, median, na.rm = TRUE)

med_averted_loseRW = apply(averted_loseRW, 2, median, na.rm = TRUE)
med_averted_tempRW = apply(averted_tempRW, 2, median, na.rm = TRUE)

med_rel_averted_loseRW = apply(rel_averted_loseRW, 2, median, na.rm = TRUE)
med_rel_averted_tempRW = apply(rel_averted_tempRW, 2, median, na.rm = TRUE)

# Compute 95% credible intervals (across sims)
ci_noint = compute_ci(inc_noint)

ci_averted_loseRW = compute_ci(averted_loseRW)
ci_averted_tempRW = compute_ci(averted_tempRW)

ci_rel_averted_loseRW = compute_ci(rel_averted_loseRW)
ci_rel_averted_tempRW = compute_ci(rel_averted_tempRW)

# Function to format credible intervals into strings
format_ci = function(median_vals, ci_vals) { 
  sprintf("%.0f (%.0f - %.0f)", median_vals, ci_vals[1, ], ci_vals[2, ]) 
}

# Format data for table
table_data = data.frame(
  Location = dimnames(inc_noint)$location,
  `Incident Infections (95% CI) - Continuation` = format_ci(med_noint, ci_noint),
  `Infections Averted (95% CI) - Cessation` = format_ci(med_averted_loseRW, ci_averted_loseRW),
  `% Relative Infections Averted (95% CI) - Cessation` = format_ci(med_rel_averted_loseRW, ci_rel_averted_loseRW),
  `Infections Averted (95% CI) - Interruption` = format_ci(med_averted_tempRW, ci_averted_tempRW),
  `% Relative Infections Averted (95% CI) - Interruption` = format_ci(med_rel_averted_tempRW, ci_rel_averted_tempRW)
)

# Check final dimensions
dim(table_data)  # Should be (31, 6)

# Ensure column names are formatted properly by replacing dots with spaces
colnames(table_data) = gsub("\\s+", "_", gsub("^X\\s*", "", gsub("\\.+", " ", colnames(table_data))))

# Apply extraction to numeric column for coloring
table_data$Relative_Infections_Averted_Num = table_data$Relative_Infections_Averted_Num = as.numeric(gsub("\\s*\\(.*?\\)", "", table_data$Relative_Infections_Averted_95_CI_Cessation))

# Normalize the values between 0 and 1 for coloring
table_data$Color_Scale = rescale(table_data$Relative_Infections_Averted_Num, to = c(0, 1))

# Order by num column
table_data = table_data %>% arrange(desc(!is.na(Relative_Infections_Averted_Num)), desc(Relative_Infections_Averted_Num))


# Define color scale function
color_map = function(values) {
  sapply(values, function(value) {
    if (is.na(value) | is.nan(value) | is.infinite(value)) {
      return("#FFFF99")  # Bright Yellow for NA values
    } else {
      return(col_numeric(c("green", "red"), domain = c(0, 1))(value))
    }
  })
}

# Generate the flextable with color formatting
ft_table = flextable(table_data %>% select(-Relative_Infections_Averted_Num, -Color_Scale)) %>%
  set_header_labels(
    Location = "Location",
    Incident_Infections_95_CI_Continuation = "Incident Infections (95% CI)",
    Infections_Averted_95_CI_Cessation = "Infections Averted (95% CI)",
    Relative_Infections_Averted_95_CI_Cessation = "% Relative Infections Averted (95% CI)",
    Infections_Averted_95_CI_Interruption = "Infections Averted (95% CI)",
    Relative_Infections_Averted_95_CI_Interruption = "% Relative Infections Averted (95% CI)"
  ) %>%
  add_header(
    Incident_Infections_95_CI_Continuation = "Continuation",
    Infections_Averted_95_CI_Cessation = "Cessation",
    Relative_Infections_Averted_95_CI_Cessation = "Cessation",
    Infections_Averted_95_CI_Interruption = "Interruption",
    Relative_Infections_Averted_95_CI_Interruption = "Interruption",
    top = TRUE
  ) %>%
  merge_h(part = "header") %>%
  merge_v(part = "header") %>%
  align(align = "center", part = "header") %>%   # Center column groups
  bold(part = "header") %>%                     # Make headers bold
  hline_top(part = "header", border = fp_border()) %>%  # Add top border
  bg(j = "Incident_Infections_95_CI_Continuation", bg = color_map(table_data$Color_Scale)) %>%
  bg(j = "Infections_Averted_95_CI_Cessation", bg = color_map(table_data$Color_Scale)) %>%
  bg(j = "Relative_Infections_Averted_95_CI_Cessation", bg = color_map(table_data$Color_Scale)) %>%
  bg(j = "Infections_Averted_95_CI_Interruption", bg = color_map(table_data$Color_Scale)) %>%
  bg(j = "Relative_Infections_Averted_95_CI_Interruption", bg = color_map(table_data$Color_Scale)) %>%
  theme_vanilla() %>%
  autofit()

# Save as PNG
save_as_image(ft_table, path = "prelim_results/table_1.png")
################################################################################
################################################################################
################################################################################
################################################################################
#==========================Sensitivity Analysis================================#
################################################################################
################################################################################
################################################################################
################################################################################

#=======================Partial Rank Correlation===============================#
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

# Extract relevant parameters
loseRW_parameters = all.parameters[,,,intervention_idx, drop = T]
Noint_parameters = all.parameters[,,,"noint", drop = T]
Parameter_shift = (loseRW_parameters %>% replace(is.na(.), 0)) - 
  (Noint_parameters %>% replace(is.na(.), 0))

# Extract dimension names
locations = dimnames(relative_incidence)$location
parameters = dimnames(Parameter_shift)$parameter 

# Defined the parameter list from RYAN.WHITE.PARAMETERS.PRIOR
rw_parameters_list = c(
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

# Initialize a results storage data frame
prcc_results = data.frame(parameter = character(), location = character(), PRCC = numeric())

# Compute PRCC per location using pcor
for (loc_idx in seq_along(locations)) {
  loc_name = locations[loc_idx]
  
  # Extract relative incidence for this location
  rel_inc_vec = relative_incidence[, loc_idx]  # Vector of 100 simulations
  
  # Skip if all values are NA or constant
  if (all(is.na(rel_inc_vec)) || length(unique(rel_inc_vec)) < 2) next
  
  # Create a dataframe of parameter values for this location
  param_df = as.data.frame(t(loseRW_parameters[, , loc_idx]))  # Convert 2D matrix to data frame
  
  # Filter to only include parameters in the filtered list
  param_df = param_df[, intersect(parameters, rw_parameters_list), drop = FALSE]
  
  # Remove fully missing columns
  param_df = param_df[, colSums(!is.na(param_df)) > 0, drop = FALSE]
  
  # Ensure at least two predictors for valid partial correlation analysis
  if (ncol(param_df) > 1) {
    # Combine rel_inc_vec with parameter data
    prcc_input = cbind(rel_inc_vec, param_df)
    
    # Compute PRCC using pcor
    prcc_matrix = pcor(prcc_input, method = "spearman")$estimate
    
    # Extract PRCC values (first row, excluding first column)
    prcc_values = prcc_matrix[1, -1]  
    param_names = colnames(prcc_input)[-1]  # Exclude first column (rel_inc_vec)
    
    # Store results in one step
    prcc_results = rbind(prcc_results, data.frame(
      parameter = param_names, 
      location = loc_name, 
      PRCC = prcc_values, 
      stringsAsFactors = FALSE
    ))
  }
}

# Compute median PRCC per parameter across all locations
top_10_params = prcc_results %>%
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
  geom_boxplot(fill = "#619CFF") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1) +  # Dashed line at 0
  theme_minimal() +
  labs(
    x = "Partial Rank Correlation Coefficient (PRCC)",
    y = "Parameter"
  )

PRCC
#=======================Top/Bottom 20% ========================================#
# Define influential parameters
selected_parameters = filtered_prcc_results %>%
  group_by(parameter) %>%
  summarize(median_PRCC = median(PRCC, na.rm = TRUE)) %>%
  arrange(median_PRCC) %>%
  pull(parameter) 

# Initialize results storage
comparison_results = data.frame(parameter = character(), 
                                 location = character(), 
                                 group = character(), 
                                 median_incidence = numeric(), 
                                 stringsAsFactors = FALSE)



# Iterate over each parameter and location
for (param in selected_parameters) {      
  for (loc in dimnames(relative_incidence)$location) {  
    
    # Extract values for the parameter and corresponding HIV incidence
    param_values = Parameter_shift[param, , loc]  # Parameter values across simulations
    inc_values = relative_incidence[, loc]  # Corresponding HIV incidence across simulations
    
    # Ensure there are no NA values before ranking
    ranked_idx = order(param_values, decreasing = TRUE, na.last = NA)  # Descending order
    num_simulations = length(ranked_idx)
    
    # Ensure we have enough simulations to select a top/bottom 20%
    top_20_idx = ranked_idx[1:floor(num_simulations * 0.2)]  # Highest parameter values
    bottom_20_idx = ranked_idx[(num_simulations - floor(num_simulations * 0.2) + 1):num_simulations]  # Lowest parameter values
    
    # Compute median of top 20% incidence values
    median_top_20 = median(inc_values[top_20_idx], na.rm = TRUE)
    median_bottom_20 = median(inc_values[bottom_20_idx], na.rm = TRUE)
    
    # Store results
    comparison_results = rbind(comparison_results, 
                                data.frame(parameter = param, location = loc, group = "Top 20%", median_incidence = median_top_20),
                                data.frame(parameter = param, location = loc, group = "Bottom 20%", median_incidence = median_bottom_20))
  }
}

prcc_plot_build = ggplot_build(PRCC)
extracted_order = prcc_plot_build$layout$panel_params[[1]]$y$get_labels()

comparison_results = comparison_results %>%
  mutate(parameter = factor(parameter, levels = extracted_order))

# Create box plot
difference = ggplot(comparison_results, aes(x = median_incidence*100, y = parameter, fill = group)) +
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

figure_supp = ggpubr::ggarrange(PRCC, difference,
                  ncol = 1, nrow = 2,labels = c("A", "B"), 
                  common.legend = F)

ggsave("prelim_results/figure_5_old_rw.png", plot = figure_supp , width = 10, height = 10, dpi = 300)


#=======================In-text paragraph 7====================================#

# === Identify the Three Most Influential Parameters === #
top_3_params = prcc_results %>%
  filter(parameter %in% rw_parameters_list) %>%
  group_by(parameter) %>%
  summarize(median_PRCC = median(abs(PRCC), na.rm = TRUE)) %>%
  arrange(desc(median_PRCC)) %>%
  slice(1:3) %>%
  pull(parameter)

# Extract the corresponding median PRCC values
top_3_prcc_values = prcc_results %>%
  filter(parameter %in% top_3_params) %>%
  group_by(parameter) %>%
  summarize(median_PRCC = median(abs(PRCC), na.rm = TRUE)) %>%
  pull(median_PRCC)

# === Compute Relative Increase in Infections for ADAP Suppression Loss === #
# Filter for ADAP-related suppression loss parameter
adap_suppression_results = comparison_results %>%
  filter(grepl("adap", parameter, ignore.case = TRUE))

# Compute the mean relative increase in infections for top & bottom 20%
top_20_mean_incidence = adap_suppression_results %>%
  filter(group == "Top 20%") %>%
  summarize(mean_incidence = mean(median_incidence, na.rm = TRUE)) %>%
  pull(mean_incidence) * 100  # Convert to percentage

bottom_20_mean_incidence = adap_suppression_results %>%
  filter(group == "Bottom 20%") %>%
  summarize(mean_incidence = mean(median_incidence, na.rm = TRUE)) %>%
  pull(mean_incidence) * 100  # Convert to percentage

# === Format Output for Report === #

# Open the file for appending
sink(output_file, append = T)
cat("\n")
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

cat("\n")
sink()

 


################################################################################
################################################################################
################################################################################
################################################################################
#==========================Covariate Analysis==================================#
################################################################################
################################################################################
################################################################################
################################################################################

#=======================New figure 5 ==========================================#

# Load required libraries
library(ggplot2)

# Define years for the analysis
years_to_consider = as.character(2025:2030)

# Extract intervention indices
intervention_noint = which(dimnames(full.results)$intervention == "noint")  # Continuation scenario
intervention_loseRW = which(dimnames(full.results)$intervention == "loseRW")  # Cessation scenario

# **Step 1: Compute relative increase in infections per city**
# Sum over years, then take mean across simulations
incidence_noint = apply(
  apply(full.results[years_to_consider, , , , , , "incidence", , intervention_noint, drop = FALSE],
        c("sim", "location"), sum, na.rm = TRUE),
  "location", mean, na.rm = TRUE
)

incidence_loseRW = apply(
  apply(full.results[years_to_consider, , , , , , "incidence", , intervention_loseRW, drop = FALSE],
        c("sim", "location"), sum, na.rm = TRUE),
  "location", mean, na.rm = TRUE
)

# Compute rel_increase correctly: sum over years, mean across simulations
incidence_noint_summed = apply(
  full.results[years_to_consider, , , , , , "incidence", , intervention_noint, drop = FALSE],
  c("sim", "location"), sum, na.rm = TRUE
)

incidence_loseRW_summed = apply(
  full.results[years_to_consider, , , , , , "incidence", , intervention_loseRW, drop = FALSE],
  c("sim", "location"), sum, na.rm = TRUE
)

rel_increase = apply(
  (incidence_loseRW_summed - incidence_noint_summed) / incidence_noint_summed,
  "location", mean, na.rm = TRUE
)


names(incidence_loseRW) = get.location.name(names(incidence_loseRW))
names(incidence_noint) = get.location.name(names(incidence_noint))
names(rel_increase) = get.location.name(names(rel_increase))



# **Step 2: Compute PWH service fractions**
# ADAP: sum first, then divide, then average across sims
frac_ADAP = apply(
  apply(full.results[years_to_consider, , , , , , "adap.clients", , intervention_loseRW, drop = FALSE], 
        c("sim", "location"), sum, na.rm = TRUE) /
    apply(full.results[years_to_consider, , , , , , "diagnosed.prevalence", , intervention_loseRW, drop = FALSE], 
          c("sim", "location"), sum, na.rm = TRUE),
  "location", mean, na.rm = TRUE
)

# OAHS: sum first, then divide, then average across sims
frac_OAHS = apply(
  apply(full.results[years_to_consider, , , , , , "oahs.clients", , intervention_loseRW, drop = FALSE], 
        c("sim", "location"), sum, na.rm = TRUE) /
    apply(full.results[years_to_consider, , , , , , "diagnosed.prevalence", , intervention_loseRW, drop = FALSE], 
          c("sim", "location"), sum, na.rm = TRUE),
  "location", mean, na.rm = TRUE
)

# non-ADAP: sum first, then divide, then average across sims
frac_non_ADAP = apply(
  apply(full.results[years_to_consider, , , , , , "non.adap.clients", , intervention_loseRW, drop = FALSE], 
        c("sim", "location"), sum, na.rm = TRUE) /
    apply(full.results[years_to_consider, , , , , , "diagnosed.prevalence", , intervention_loseRW, drop = FALSE], 
          c("sim", "location"), sum, na.rm = TRUE),
  "location", mean, na.rm = TRUE
)

frac_suppressed = apply(
  apply(
    full.results[years_to_consider, , , , , , "adap.suppression", , intervention_loseRW, drop = FALSE] +
      full.results[years_to_consider, , , , , , "oahs.suppression", , intervention_loseRW, drop = FALSE],
    c("sim", "location"), sum, na.rm = TRUE
  ) /
    apply(
      full.results[years_to_consider, , , , , , "diagnosed.prevalence", , intervention_loseRW, drop = FALSE],
      c("sim", "location"), sum, na.rm = TRUE
    ),
  "location", mean, na.rm = TRUE
)


frac_suppressed_int = apply(
  (full.results["2025", , , , , , "adap.suppression", , intervention_loseRW, drop = FALSE] +
     full.results["2025", , , , , , "oahs.suppression", , intervention_loseRW, drop = FALSE]) /
    full.results["2025", , , , , , "diagnosed.prevalence", , intervention_loseRW, drop = FALSE],
  "location", mean, na.rm = TRUE
)


# **Step 4: Load Medicaid expansion status**
# Assuming a preloaded named vector `medicaid_expansion` where city names match `dimnames(full.results)$location`

# Define Medicaid Expansion (2/3 Rule)
medicaid_expansion_lookup = c(
  "New York-Newark-Jersey City, NY-NJ-PA" = 1,  
  "Miami-Fort Lauderdale-Pompano Beach, FL" = 0,  
  "Los Angeles-Long Beach-Anaheim, CA" = 1,
  "Atlanta-Sandy Springs-Alpharetta, GA" = 0,
  "Houston-The Woodlands-Sugar Land, TX" = 0,
  "Dallas-Fort Worth-Arlington, TX" = 0,
  "Chicago-Naperville-Elgin, IL-IN-WI" = 1,  # 2/3 expanded
  "Washington-Arlington-Alexandria, DC-VA-MD-WV" = 1,
  "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD" = 1,
  "Orlando-Kissimmee-Sanford, FL" = 0,
  "San Francisco-Oakland-Berkeley, CA" = 1,
  "Phoenix-Mesa-Chandler, AZ" = 0,
  "Tampa-St. Petersburg-Clearwater, FL" = 0,
  "Riverside-San Bernardino-Ontario, CA" = 1,
  "Detroit-Warren-Dearborn, MI" = 1,
  "Baltimore-Columbia-Towson, MD" = 1,
  "Las Vegas-Henderson-Paradise, NV" = 1,
  "Boston-Cambridge-Newton, MA-NH" = 1,
  "San Diego-Chula Vista-Carlsbad, CA" = 1,
  "Charlotte-Concord-Gastonia, NC-SC" = 0,
  "San Antonio-New Braunfels, TX" = 0,
  "Jacksonville, FL" = 0,
  "New Orleans-Metairie, LA" = 1,
  "Memphis, TN-MS-AR" = 0,  # 1/3 expanded
  "Seattle-Tacoma-Bellevue, WA" = 1,
  "Austin-Round Rock-Georgetown, TX" = 0,
  "Indianapolis-Carmel-Anderson, IN" = 1,
  "Columbus, OH" = 1,
  "Baton Rouge, LA" = 1,
  "Sacramento-Roseville-Folsom, CA" = 1,
  "Cleveland-Elyria, OH" = 1
)

medicaid_expansion_status = sapply(names(rel_increase), function(city) {
  if (city %in% names(medicaid_expansion_lookup)) {
    return(medicaid_expansion_lookup[city])
  } else {
    return(NA)
  }
})

# **Step 5: Compute PRCCs using pcor**
param_df = data.frame(
  frac_ADAP = frac_ADAP,
  frac_OAHS = frac_OAHS,
  frac_non_ADAP = frac_non_ADAP,
  frac_suppressed = frac_suppressed_int,
  medicaid = as.numeric(medicaid_expansion_status),
  incidence  = incidence_noint  
)

input_df = cbind(rel_increase, param_df)
input_df = na.omit(input_df)


###########

corrplot(cor(input_df,method ="spearman"), type = "lower", diag = F)
corrplot(pcor(input_df,method ="spearman")$estimate,  type = "lower", diag = F)

X = input_df[, c("frac_ADAP", "frac_OAHS", "frac_non_ADAP", "frac_suppressed", "medicaid")]
Y = input_df$rel_increase

prcc_results = pcc(X = X, y = Y, rank = TRUE, nboot = 1000)

print(prcc_results$PRCC)

# Extract PRCC values and convert to data frame
# Create summary table
prcc_table = data.frame(
  Variable = rownames(prcc_results$PRCC),
  PRCC = round(prcc_results$PRCC[, "original"], 3),
  CI = paste0("[", round(prcc_results$PRCC[, "min. c.i."], 2), ", ",
              round(prcc_results$PRCC[, "max. c.i."], 2), "]")
)

# Optional: Add significance stars based on CI excluding 0
prcc_table$Significance = ifelse(
  prcc_results$PRCC[, "min. c.i."] > 0 | prcc_results$PRCC[, "max. c.i."] < 0,
  "*", ""
)

# Create PRCC table using ggtexttable
prcc_ggtable = ggtexttable(prcc_table, rows = NULL, theme = ttheme("light"))

# Define base theme
base_theme = theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 12))

# Panel A: Suppression
p1 = ggplot(input_df, aes(x = rel_increase * 100, y = frac_suppressed * 100,
                           color = factor(medicaid), size = incidence)) +
  geom_point(alpha = 0.8) +
  labs(x = "Mean Relative Increase in Incidence (%)",
       y = "Mean % Suppressed (2025)",
       color = "Medicaid Expansion", size = "Incidence") +
  base_theme

# Panel B: ADAP
p2 = ggplot(input_df, aes(x = rel_increase * 100, y = frac_ADAP * 100,
                           color = factor(medicaid), size = incidence)) +
  geom_point(alpha = 0.8) +
  labs(x = "Mean Relative Increase in Incidence (%)",
       y = "Mean % PWH Receiving ADAP",
       color = "Medicaid Expansion", size = "Incidence") +
  base_theme

# Panel C: OAHS
p3 = ggplot(input_df, aes(x = rel_increase * 100, y = frac_OAHS * 100,
                           color = factor(medicaid), size = incidence)) +
  geom_point(alpha = 0.8) +
  labs(x = "Mean Relative Increase in Incidence (%)",
       y = "Mean % PWH Receiving OAHS",
       color = "Medicaid Expansion", size = "Incidence") +
  base_theme

# Panel D: Non-ADAP
p4 = ggplot(input_df, aes(x = rel_increase * 100, y = frac_non_ADAP * 100,
                           color = factor(medicaid), size = incidence)) +
  geom_point(alpha = 0.8) +
  labs(x = "Mean Relative Increase in Incidence (%)",
       y = "Mean % PWH Receiving Non-ADAP Services",
       color = "Medicaid Expansion", size = "Incidence"
       ) +
  base_theme

# Panel E: Table of PRCCs
p5 = prcc_ggtable



# Top row: Panels A–D
scatterplots = ggarrange(p1, p2, p3, p4,
                     labels = c("A", "B", "C", "D"),
                     ncol = 2, nrow = 2,
                     common.legend = TRUE, legend = "bottom")

ggsave("prelim_results/new_figure_5.png", plot = scatterplots, width = 10, height = 10, dpi = 300)
ggsave("prelim_results/PRCC_stats_new_fig_5.png", plot = p5, width = 5, height = 5, dpi = 300)


  })
})

print("Script has run, check prelim_results folder")
