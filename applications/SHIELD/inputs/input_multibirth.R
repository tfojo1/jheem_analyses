# https://www.statista.com/statistics/276022/us-multiple-birth-rate/
  df <- as.data.frame(matrix(c(1980, 19.3,
                             1990, 23,
                             2000, 31.3,
                             2010, 34,
                             2020, 31.9,
                             2022, 32), 
                           ncol = 2, byrow = TRUE))
colnames(df) <- c("Year", "Value")
df$Period <- cut(df$Year,
                 breaks = c(1980, 1990, 2000, 2010, 2020, 2022),
                 labels = c("pre-1990", "1990-2000", "2000-2010", "2010-2020", "post-2020"),
                 include.lowest = TRUE, right = FALSE)
fit <- lm(Value ~ Year + Period, data = df)
summary(fit)

df$predicted <- predict(fit, type = "response")

# Create the plot of observed vs predicted values
ggplot(df, aes(x = Year, y = Value)) + 
  geom_point(aes(color = "Observed Data"), alpha = 0.6) +  # Plot observed data
  geom_line(aes(x = Year, y = predicted, color = "Predicted Values"), size = 1) +  # Plot predicted values
  labs(title = "Observed vs Predicted Values",
       x = "Year", y = "Response (r)") +
  scale_color_manual(values = c("Observed Data" = "blue", "Predicted Values" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())
 