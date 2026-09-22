# examples of natural spline model, and then transformation option for x and y

# Load necessary libraries
library(splines)
library(ggplot2)

# Generate strictly positive synthetic data
set.seed(123)
n <- 100
x <- seq(1, 100, length.out = n)

#
y <- 3 * sin(x / 10) + rnorm(n, sd = 0.5)+100
data <- data.frame(x = x, y = y)
plot(data)
#######
{
  # Fit a spline model with 2 knots
  knots <- c(30, 70)
  spline_model0 <- lm(y ~ bs(x, knots = knots), data = data)

  # Predict using the spline model
  data$y_pred0 <- predict(spline_model0, newdata = data)

  # Plot the results
  ggplot(data, aes(x = x, y = y)) +
    geom_point() +
    geom_line(aes(y = y_pred0), color = "black") +
    labs(title = "Spline Regression with Strictly Positive Data", x = "x", y = "y")
}
#######
{
  # Apply log transformation to the response variable y
  data$log_y <- log(data$y)

  # Fit a spline model with 2 knots on log-transformed y
  knots <- c(30, 70)
  log_spline_model1 <- lm(log_y ~ bs(x, knots = knots), data = data)

  # Predict using the spline model
  data$log_y_pred1 <- predict(log_spline_model1, newdata = data)

  # Back-transform predictions to the original scale
  data$y_pred1 <- exp(data$log_y_pred1)

  # Plot the results
  ggplot(data, aes(x = x, y = y)) +
    geom_point() +
    geom_line(aes(y = y_pred1), color = "blue") +
    labs(title = "Spline Regression with Log Transformation of y", x = "x", y = "y")
}
#######
{
  # Fit a spline model with 2 knots on log-transformed y and logtransformed knots
  # Apply log transformation to the response variable y
  data$log_y <- log(data$y)

  # Fit a spline model with 2 knots on log-transformed y
  log_knots <- log(c(30, 70))
  log_spline_model2 <- lm(log_y ~ bs(log(x), knots = log_knots), data = data)

  # Predict using the spline model
  data$log_y_pred2 <- predict(log_spline_model2, newdata = data)

  # Back-transform predictions to the original scale
  data$y_pred2 <- exp(data$log_y_pred2)

  # Plot the results
  ggplot(data, aes(x = x, y = y)) +
    geom_point() +
    geom_line(aes(y = y_pred2), color = "red") +
    geom_line(aes(y = y_pred1), color = "blue") +
    geom_line(aes(y = y_pred0), color = "black") +
    labs(title = "Spline Regression with Log Transformation of y", x = "x", y = "y")
}
