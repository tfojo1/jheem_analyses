# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Reshape the data from wide to long format
rw_survey_long <- rw_survey %>%
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

X = rw_survey[,c("q1_adap_loss","q2_oahs_loss","q3_support_loss")]

mask = apply(!is.na(X),1, all)

cor(X[mask,] )

# Example usage: Generate 100 samples
set.seed(42)  # For reproducibility
sample_matrix <- get_suppresion_effect(10000, rw_survey)

# View sample output
print(dim(sample_matrix))

# Convert to dataframe for plotting
sampled_df <- as.data.frame(t(sample_matrix))
colnames(sampled_df) <- c("q1_adap_loss", "q2_oahs_loss", "q3_support_loss")

# Convert to long format for ggplot
sampled_long <- sampled_df %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

rw_long <- rw_survey %>%
  select(q1_adap_loss, q2_oahs_loss, q3_support_loss) %>%
  na.omit() %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  mutate(Source = "Original")

sampled_long <- as.data.frame(t(sample_matrix)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  mutate(Source = "KDE Sampled")

compare_df <- bind_rows(rw_long, sampled_long)

# Plot KDE vs original data
ggplot(compare_df, aes(x = value, fill = Source, color = Source)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ variable, scales = "free") +  # One plot per variable
  labs(title = "Original Data vs KDE-Fitted Samples",
       x = "% Loss",
       y = "Density",
       fill = "Source") +
  theme_minimal()
