# Load necessary libraries
library(ggplot2)
library(tidyverse)
library(broom)
library(ks)       # Kernel density estimation
library(ggpubr)   # For merging multiple plots
library(nnet) # For multinomial regression

# Load data
file_path <- "../jheem_analyses/applications/ryan_white/rw_survey_135.csv"
df <- read.csv(file_path, stringsAsFactors = FALSE)

# Identify columns that contain "Checked"/"Unchecked" values
binary_cols <- sapply(df, function(col) all(col %in% c("Checked", "Unchecked", NA)))

# Convert "Checked" to 1 and "Unchecked" to 0
df[binary_cols] <- lapply(df[binary_cols], function(col) ifelse(col == "Checked", 1, 0))

# Save the cleaned dataset
write.csv(df, "../jheem_analyses/applications/ryan_white/rw_survey.csv", row.names = FALSE)

rw_survey = read.csv("../jheem_analyses/applications/ryan_white/rw_survey.csv")
colnames(rw_survey) <- gsub("^X\\.|\\.$", "", colnames(rw_survey))  # Remove unwanted prefixes/suffixes
colnames(rw_survey) <- gsub("\\.+", "_", colnames(rw_survey))  # Replace multiple dots with underscores
#=========== Plot original distribution =======================================#

# Reshape the data from wide to long format
rw_survey_long <- rw_survey %>%
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

compute_kl_divergence <- function(P_samples, Q_samples) {
  # Fit KDE with added regularization
  H_P <- Hpi(x = P_samples) + diag(ncol(P_samples)) * 1e-6
  H_Q <- Hpi(x = Q_samples) + diag(ncol(Q_samples)) * 1e-6
  
  P_kde <- kde(x = P_samples, H = H_P)
  Q_kde <- kde(x = Q_samples, H = H_Q)
  
  # Evaluate densities at P sample points
  P_density <- predict(P_kde, x = P_samples)
  Q_density <- predict(Q_kde, x = P_samples)
  
  # Avoid log(0) and numerical instability
  epsilon <- 1e-10
  P_density <- pmax(P_density, epsilon)
  Q_density <- pmax(Q_density, epsilon)
  
  # Compute KL divergence
  KL_div <- sum(P_density * log(P_density / Q_density)) / length(P_density)
  
  # Ensure KL is non-negative
  return(max(KL_div, 0))
}

compare_kl_divergence <- function(n, rw_survey) {
  # Extract relevant columns and remove rows with NA values
  X <- rw_survey[, c("q1_adap_loss", "q2_oahs_loss", "q3_support_loss")]
  X <- X[complete.cases(X), ] / 100
  X <- as.matrix(X)
  
  # Ensure values are strictly within (0,1)
  epsilon <- 1e-6
  X <- pmax(pmin(X, 1 - epsilon), epsilon)
  
  # Get number of rows in X
  num_rows <- nrow(X)
  
  # --- 1. Apply Transformations ---
  arcsin_X <- asin(sqrt(X))
  logit_X  <- log(X / (1 - X))
  probit_X <- qnorm(X)
  
  # --- 2. Fit KDE in Transformed Space ---
  kde_arcsin <- kde(x = arcsin_X)
  kde_logit  <- kde(x = logit_X)
  kde_probit <- kde(x = probit_X)
  kde_original <- kde(x = X)
  
  # --- 3. Sample 'n' Points from KDE ---
  sampled_arcsin <- rkde(n, kde_arcsin)
  sampled_logit  <- rkde(n, kde_logit)
  sampled_probit <- rkde(n, kde_probit)
  
  # --- 4. Back-Transform to Original Scale ---
  sampled_arcsin <- (sin(sampled_arcsin))^2
  sampled_logit  <- exp(sampled_logit) / (1 + exp(sampled_logit))
  sampled_probit <- pnorm(sampled_probit)
  
  # --- 5. Bootstrap Sampling (FIXED) ---
  sampled_bootstrap <- X[sample(1:num_rows, size = n, replace = TRUE), ]
  
  # --- 6. Compute KL Divergence ---
  kl_arcsin <- compute_kl_divergence(X, sampled_arcsin)
  kl_logit  <- compute_kl_divergence(X, sampled_logit)
  kl_probit <- compute_kl_divergence(X, sampled_probit)
  kl_bootstrap <- compute_kl_divergence(X, sampled_bootstrap)  # Corrected baseline comparison
  
  # Return KL divergence values
  return(data.frame(
    Transformation = c("Arcsin-Sqrt", "Logit", "Probit", "Bootstrap"),
    KL_Divergence = c(kl_arcsin, kl_logit, kl_probit, kl_bootstrap)
  ))
}


# Function to apply transformations, fit KDE, sample, and plot pairwise comparisons
plot_pairwise_transformations_vs_original <- function(n, rw_survey) {
  # Extract relevant columns and remove rows with NA values
  X <- rw_survey[, c("q1_adap_loss", "q2_oahs_loss", "q3_support_loss")]
  X <- X[complete.cases(X), ] / 100  # Convert percentages to proportions
  X <- as.matrix(X)
  
  # Ensure values are strictly within (0,1)
  epsilon <- 1e-6
  X <- pmax(pmin(X, 1 - epsilon), epsilon)
  
  # --- 1. Apply Transformations ---
  arcsin_X <- asin(sqrt(X))   # Arcsin-Sqrt transformation
  logit_X  <- log(X / (1 - X)) # Logit transformation
  probit_X <- qnorm(X)         # Inverse CDF (Probit)
  
  # --- 2. Fit KDE in Transformed Space ---
  kde_arcsin <- kde(x = arcsin_X)
  kde_logit  <- kde(x = logit_X)
  kde_probit <- kde(x = probit_X)
  
  # --- 3. Sample 'n' Points from KDE ---
  sampled_arcsin <- rkde(n, kde_arcsin)
  sampled_logit  <- rkde(n, kde_logit)
  sampled_probit <- rkde(n, kde_probit)
  
  # --- 4. Back-Transform to Original Scale ---
  sampled_arcsin <- (sin(sampled_arcsin))^2
  sampled_logit  <- exp(sampled_logit) / (1 + exp(sampled_logit))
  sampled_probit <- pnorm(sampled_probit)
  
  # --- 5. Bootstrap Sampling ---
  sampled_bootstrap <- X[sample(1:nrow(X), size = n, replace = TRUE), ]
  
  # --- 6. Store Transformations in a List ---
  transformations <- list(
    "Arcsin-Sqrt" = sampled_arcsin,
    "Logit" = sampled_logit,
    "Probit" = sampled_probit,
    "Bootstrap" = sampled_bootstrap
  )
  
  variables <- c("q1_adap_loss", "q2_oahs_loss", "q3_support_loss")
  
  # --- 7. Generate KDE Plots for Each Transformation and Each Variable ---
  plot_list <- list()
  for (trans_name in names(transformations)) {
    sampled_data <- transformations[[trans_name]]
    
    for (i in 1:3) {
      # Extract variable data
      original_var <- X[, i]
      transformed_var <- sampled_data[, i]
      
      # Convert to long format for ggplot
      original_df <- data.frame(Value = original_var, Type = "Original")
      transformed_df <- data.frame(Value = transformed_var, Type = trans_name)
      combined_df <- rbind(original_df, transformed_df)
      
      # Generate KDE plot
      p <- ggplot(combined_df, aes(x = Value, fill = Type)) +
        geom_density(alpha = 0.5) +
        theme_minimal() +
        labs(title = paste("Original vs.", trans_name, "\nVariable:", variables[i]),
             x = "Value", y = "Density") +
        theme(legend.position = "right")
      
      # Store plot in list
      plot_list[[paste(trans_name, variables[i], sep = "_")]] <- p
    }
  }
  
  # --- 8. Arrange all plots in a grid ---
  combined_plot <- ggarrange(plotlist = plot_list, ncol = 3, nrow = 4)
  
  return(combined_plot)
}

# Run function and plot
compare_plot <- plot_pairwise_transformations_vs_original(1000, rw_survey)
compare_kl_divergence(1000, rw_survey)
print(compare_plot)

#=========== Logistic regression of RW support loss ==========================#

# Define Medicaid expansion status and Census region for each state
state_info <- tribble(
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

rw_survey_clean <- rw_survey %>%
  rename_with(~ gsub("^X\\.choice\\.|\\.$", "", .)) %>% # Clean state column names
  dplyr::select(-c("choice_I_dont_know","choice_I_dont_know_1", "choice_I_dont_know_2","choice_I_dont_know_3"))

# Convert from wide to long format for state choices
rw_survey_long <- rw_survey_clean %>%
  pivot_longer(cols = starts_with("choice_"), names_to = "state", values_to = "selected") %>%
  filter(selected == 1) %>%  # Keep only selected states
  dplyr::select(-selected) %>%  # Remove redundant column
  mutate(state = gsub("^choice_", "", state)) 

# Merge with Medicaid expansion and Census region data
rw_survey_final <- rw_survey_long %>%
  left_join(state_info, by = "state")

rw_survey_final <- rw_survey_final %>%
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
medicaid_model <- glm(medicaid_expansion ~ q1_adap_loss + q2_oahs_loss + q3_support_loss,
                      data = rw_survey_final, family = binomial)
summary(medicaid_model)

# Logistic regression for each Census Region separately
midwest_model <- glm(census_Midwest ~ q1_adap_loss + q2_oahs_loss + q3_support_loss, 
                     data = rw_survey_final, family = binomial)

northeast_model <- glm(census_Northeast ~ q1_adap_loss + q2_oahs_loss + q3_support_loss, 
                       data = rw_survey_final, family = binomial)

south_model <- glm(census_South ~ q1_adap_loss + q2_oahs_loss + q3_support_loss, 
                   data = rw_survey_final, family = binomial)

west_model <- glm(census_West ~ q1_adap_loss + q2_oahs_loss + q3_support_loss, 
                  data = rw_survey_final, family = binomial)

# Logistic regression for Rural region
rural_model <- glm(rural_region ~ q1_adap_loss + q2_oahs_loss + q3_support_loss,
                     data = rw_survey_final, family = binomial)
summary(rural_model)


# Function to extract OR, CIs, and p-values
extract_model_data <- function(model, outcome_name) {
  tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(outcome = outcome_name)
}

# Extract results for each model
medicaid_results <- extract_model_data(medicaid_model, "Medicaid Expansion")
midwest_results <- extract_model_data(midwest_model, "Midwest")
northeast_results <- extract_model_data(northeast_model, "Northeast")
south_results <- extract_model_data(south_model, "South")
west_results <- extract_model_data(west_model, "West")
rural_results <- extract_model_data(rural_model, "Rural Region") 

# Combine results
model_results <- bind_rows(medicaid_results, rural_results, midwest_results, northeast_results, south_results, west_results)

# Clean variable names for plotting
model_results <- model_results %>%
  rename(Odds_Ratio = estimate, Lower_CI = conf.low, Upper_CI = conf.high) %>%
  filter(term != "(Intercept)")


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
library(gridExtra)

plot_arcsin_sqrt_vs_original <- function(n, rw_survey) {
  # Extract relevant columns and remove rows with NA values
  X <- rw_survey[, c("q1_adap_loss", "q2_oahs_loss", "q3_support_loss")]
  X <- X[complete.cases(X), ] / 100  # Convert percentages to proportions
  X <- as.matrix(X)
  
  # Ensure values are strictly within (0,1)
  epsilon <- 1e-6
  X <- pmax(pmin(X, 1 - epsilon), epsilon)
  
  # --- 1. Apply Arcsin-Sqrt Transformation ---
  arcsin_X <- asin(sqrt(X))
  
  # --- 2. Fit KDE in Transformed Space ---
  kde_arcsin <- kde(x = arcsin_X)
  
  # --- 3. Sample 'n' Points from KDE ---
  sampled_arcsin <- rkde(n, kde_arcsin)
  
  # --- 4. Back-Transform to Original Scale ---
  sampled_arcsin <- ((sin(sampled_arcsin))^2)
  
  # --- 5. Generate Density Plots for Each Variable ---
  variables <- c("q1_adap_loss", "q2_oahs_loss", "q3_support_loss")
  plot_list <- list()
  
  for (i in 1:3) {
    original_var <- X[, i]*100
    transformed_var <- sampled_arcsin[, i]*100
    
    # Convert to long format for ggplot
    original_df <- data.frame(Value = original_var, Type = "Original")
    transformed_df <- data.frame(Value = transformed_var, Type = "Arcsin-Sqrt")
    combined_df <- rbind(original_df, transformed_df)
    
    # Generate the histogram for the original and density for the transformed
    p <- ggplot() +
      geom_histogram(data = original_df, aes(x = Value, fill = Type), 
                     bins = 30, alpha = 0.5, position = "identity") +
      geom_density(data = transformed_df, aes(x = Value, color = Type, y = ..count..), 
                   size = 1.2, alpha = 0.8) +  # Scale density to match counts
      theme_minimal() +
      labs(x = paste("Value\nVariable:", variables[i]), y = "Count") +
      theme(legend.position = "right") +
      scale_fill_manual(values = c("Original" = "blue")) +  # Customize fill color
      scale_color_manual(values = c("Arcsin-Sqrt" = "red"))  # Customize line color
    
    # Store each plot in list
    plot_list[[variables[i]]] <- p  
  }
  
  return(plot_list)
}
# Run function
figures_1_3 <- plot_arcsin_sqrt_vs_original(1000, rw_survey)

######################
rw_survey_long <- rw_survey %>%
  dplyr::select(q4_rural_percentage) %>%  # Select columns to plot
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Plot density curves
rural = ggplot(rw_survey_long, aes(x = value, fill = variable)) +
  geom_histogram() +
  scale_fill_manual(values = c( "q4_rural_percentage"= "#00BFC4")) +
  labs(
       x = "% Rural",
       y = "Count",
       fill = "Variable") +
  theme_minimal()

rural

#######################
# First, arrange the top 4 plots in a 2x2 grid
top_grid <- ggarrange(
  figures_1_3[["q1_adap_loss"]], 
  figures_1_3[["q2_oahs_loss"]], 
  figures_1_3[["q3_support_loss"]], 
  rural, 
  ncol = 2, nrow = 2
)

# Now, center the forest plot beneath the top grid
combined_plot <- ggarrange(
  top_grid, 
  forest, 
  ncol = 1,  # One column to stack vertically
  heights = c(2, 1),  # Adjust heights (top section is twice as tall),
  widths = c(0.8)  # Increase the width of the forest plot
)

combined_plot

#=======================Supp. Analyses=========================================#

# Categorize the data and count occurrences
rw_survey_pie <- rw_survey %>%
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
rw_survey_pie <- rw_survey_final %>%
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
    Q1 = mean(q1_adap_loss, na.rm = TRUE),
    Q2 = mean(q2_oahs_loss, na.rm = TRUE),
    Q3 = mean(q3_support_loss, na.rm = TRUE)
  ) 
mediciad_summary = rw_survey_final %>%
  mutate(Group = ifelse(medicaid_expansion == "Yes", "Medicaid expansion", "No medicaid expansion")) %>%
  group_by(Group) %>%
      summarize(
        Q1 = median(q1_adap_loss, na.rm = TRUE),
        Q2 = median(q2_oahs_loss, na.rm = TRUE),
        Q3 = median(q3_support_loss, na.rm = TRUE)
      ) 
rural_summary <- rw_survey %>%
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
    Q1 = "Mean ADAP Loss",
    Q2 = "Mean OAHS Loss",
    Q3 = "Mean \n Other RWAP Support Loss"
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


