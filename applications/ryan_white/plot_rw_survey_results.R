# Load necessary libraries
library(tidyverse)
library(ks)       
library(ggplot2) 
library(ggpubr)   
library(reshape2) 
# Reshape the data from wide to long format
rw_survey_long = rw_survey %>%
  select(q1_adap_loss, q2_oahs_loss, q3_support_loss) %>%  # Select columns to plot
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

# -----Choosing the best fitting bounding techinque or bootstrapping-----------#

# Function to compute KL Divergence
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

# Function to transform, fit KDE, sample, and compute KL divergence
compare_kl_divergence = function(n, rw_survey) {
  # Extract relevant columns and remove rows with NA values
  X = rw_survey[, c("q1_adap_loss", "q2_oahs_loss", "q3_support_loss")]
  X = X[complete.cases(X), ]/100
  X = as.matrix(X)
  
  # Ensure values are strictly within (0,1)
  epsilon = 1e-6
  X = pmax(pmin(X, 1 - epsilon), epsilon)
  
  # Get number of rows in X
  num_rows = nrow(X)
  
  # --- 1. Apply Transformations ---
  arcsin_X = asin(sqrt(X))   # Arcsin-Sqrt transformation
  logit_X  = log(X / (1 - X)) # Logit transformation
  probit_X = qnorm(X)         # Inverse CDF (Probit)
  
  # --- 2. Fit KDE in Transformed Space ---
  kde_arcsin = kde(x = arcsin_X)
  kde_logit  = kde(x = logit_X)
  kde_probit = kde(x = probit_X)
  kde_original = kde(x = X)  # KDE of the original data
  
  # --- 3. Sample from KDE ---
  sampled_arcsin = rkde(n, kde_arcsin)
  sampled_logit  = rkde(n, kde_logit)
  sampled_probit = rkde(n, kde_probit)
  
  # --- 4. Back-Transform to Original Scale ---
  sampled_arcsin = (sin(sampled_arcsin))^2
  sampled_logit  = exp(sampled_logit) / (1 + exp(sampled_logit))
  sampled_probit = pnorm(sampled_probit)
  
  # --- 5. Bootstrap Sampling ---
  sampled_bootstrap = X[sample(1:num_rows, size = n, replace = TRUE), ]
  
  # --- 6. Compute KL Divergence ---
  kl_arcsin = compute_kl_divergence(X, sampled_arcsin)
  kl_logit  = compute_kl_divergence(X, sampled_logit)
  kl_probit = compute_kl_divergence(X, sampled_probit)
  kl_bootstrap = compute_kl_divergence(X, sampled_bootstrap)  # Baseline comparison
  
  # Return KL divergence values
  return(data.frame(
    Transformation = c("Arcsin-Sqrt", "Logit", "Probit", "Bootstrap"),
    KL_Divergence = c(kl_arcsin, kl_logit, kl_probit, kl_bootstrap)
  ))
}


# Run KL divergence comparison
kl_results = compare_kl_divergence(1000, rw_survey)


# Show KL divergence results
print(kl_results)

# ----------Visualizing best distribution relative original KDE----------------#

# Function to apply transformations, fit KDE, sample, and plot
plot_transformations_vs_original = function(n, rw_survey) {
  # Extract relevant columns and remove rows with NA values
  X = rw_survey[, c("q1_adap_loss", "q2_oahs_loss", "q3_support_loss")]
  X = X[complete.cases(X), ] / 100  # Convert percentages to proportions
  X = as.matrix(X)
  
  # Ensure values are strictly within (0,1)
  epsilon = 1e-6
  X = pmax(pmin(X, 1 - epsilon), epsilon)
  
  # Get number of rows in X
  num_rows = nrow(X)
  
  # --- 1. Apply Transformations ---
  arcsin_X = asin(sqrt(X))   # Arcsin-Sqrt transformation
  logit_X  = log(X / (1 - X)) # Logit transformation
  probit_X = qnorm(X)         # Inverse CDF (Probit)
  
  # --- 2. Fit KDE in Transformed Space ---
  kde_arcsin = kde(x = arcsin_X)
  kde_logit  = kde(x = logit_X)
  kde_probit = kde(x = probit_X)
  
  # --- 3. Sample from KDE ---
  sampled_arcsin = rkde(n, kde_arcsin)
  sampled_logit  = rkde(n, kde_logit)
  sampled_probit = rkde(n, kde_probit)
  
  # --- 4. Back-Transform to Original Scale ---
  sampled_arcsin = (sin(sampled_arcsin))^2
  sampled_logit  = exp(sampled_logit) / (1 + exp(sampled_logit))
  sampled_probit = pnorm(sampled_probit)
  
  # --- 5. Bootstrap Sampling ---
  sampled_bootstrap = X[sample(1:num_rows, size = n, replace = TRUE), ]
  
  
  # --- 6. Store Transformations in a List ---
  transformations = list(
    "Arcsin-Sqrt" = sampled_arcsin,
    "Logit" = sampled_logit,
    "Probit" = sampled_probit,
    "Bootstrap" = sampled_bootstrap
  )
  
  # --- 7. Generate KDE Plots for Each Variable and Each Transformation ---
  plot_list = list()
  
  # Loop through each transformation
  for (trans_name in names(transformations)) {
    sampled_data = transformations[[trans_name]]
    
    # Loop through each variable
    for (i in 1:3) {
      var_name = colnames(X)[i]
      
      # Convert to long format for ggplot
      original_df = data.frame(Value = X[, i], Type = "Original")
      transformed_df = data.frame(Value = sampled_data[, i], Type = trans_name)
      combined_df = rbind(original_df, transformed_df)
      
      # Generate KDE plot
      p = ggplot(combined_df, aes(x = Value, fill = Type)) +
        geom_density(alpha = 0.5) +
        theme_minimal() +
        labs(title = paste("Original vs.", trans_name, "-", var_name),
             x = "Value", y = "Density") +
        theme(legend.position = "right")
      
      # Store plot in list
      plot_list[[paste(trans_name, var_name, sep = "_")]] = p
    }
  }
  
  # --- 8. Arrange all plots in a grid ---
  combined_plot = ggarrange(plotlist = plot_list, ncol = 3, nrow = 4)
  
  return(combined_plot)
}

# Run function and plot
compare_plot = plot_transformations_vs_original(1000, rw_survey)
print(compare_plot)
