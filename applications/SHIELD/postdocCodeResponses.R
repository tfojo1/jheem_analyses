# 
# # *******************************
# # answers: 
# set.seed(1234)
# 
# global.trate = c()
# for(i in 1:3){
#   global.trate[i] = runif(1, min = 0, max = 0.08)
# }
# # 
# params1 = params2 = params3 = params
# # 
# params1["global.trate"] = global.trate[1]
# params2["global.trate"] = global.trate[2]
# params3["global.trate"] = global.trate[3]
# # 
# sim1 = engine$run(parameters = params1)
# sim2 = engine$run(parameters = params2)
# sim3 = engine$run(parameters = params3)
# 
# # Visualize the results (might need to tell them you can pass all three sims as arguments to simplot)
# simplot(sim1, sim2, sim3, outcomes = "new", 
#         dimension.values = list(year = 2007:2025))
# 
# simplot(sim1, sim2, sim3, outcomes = "diagnosed.prevalence", 
#         dimension.values = list(year = 2007:2025))
# #sim1 understiamtes new diagnosis but fits prevalence of diagnosed HIV better than 2 and 3
# # *******************************
# 
# 
# # **********************************************
# #Task1: 
# new_diagnoses=sim$new
# diagnosed_prevalence=sim$diagnosed.prevalence
# #Task2:
# dimnames(new_diagnoses)
# dimnames(diagnosed_prevalence)
# #Task3:
# #  A. 13-24 years, black, msm, and never IDU
# age_index <- which(dimnames(new)$age == "13-24 years")
# race_index <- which(dimnames(new)$race == "black")
# sex_index <- which(dimnames(new)$sex == "msm")
# risk_index <- which(dimnames(new)$risk == "never_IDU")
# 
# new_diagnoses_filtered <- new_diagnoses[,    # All locations (since only one)
#                                         ,    # All years (if you want to keep all years, leave empty or specify indices)
#                                         age_index,
#                                         race_index,
#                                         sex_index,
#                                         risk_index,
#                                         ,    # All simulations (if you want to keep all simulations, leave empty or specify indices)
#                                         drop = FALSE]
# diagnosed_prevalence_filtered <- diagnosed_prevalence[,    # All locations (since only one)
#                                                       ,    # All years (if you want to keep all years, leave empty or specify indices)
#                                                       age_index,
#                                                       race_index,
#                                                       sex_index,
#                                                       risk_index,
#                                                       ,    # All simulations (if you want to keep all simulations, leave empty or specify indices)
#                                                       drop = FALSE]
# 
# 
# plot(x=dimnames(new_diagnoses_filtered)$year,
#      y=new_diagnoses_filtered, 
#      type = "l", 
#      xlab = "Year", 
#      ylab = "New diagnoses", main = "New diagnoses over time for 13-24 years, black, msm, and never IDU")
# plot(x=dimnames(diagnosed_prevalence_filtered)$year,
#      y=diagnosed_prevalence_filtered, 
#      type = "l", 
#      xlab = "Year", 
#      ylab = "prevDiag", main = "Prev of Diagnosed HIV over time for 13-24 years, black, msm, and never IDU")
# 
# 
# 
# #Task4:
# dim(new_diagnoses)
# dimnames(new_diagnoses)
# # Find the index for the year 2020 in the dimnames of the 'year' dimension
# year_index <- which(dimnames(new_diagnoses)$year == 2020)
# 
# # Sum across all other dimensions (e.g., location, race, sex, risk) except for age to get the total diagnoses per age group
# age_distribution_2020 <- apply(new_diagnoses[year_index, , , , , ,], c(1), sum)
# 
# # Convert to a data frame for easier plotting
# age_distribution_df <- data.frame(
#   AgeGroup = dimnames(new_diagnoses)$age,
#   Diagnoses = age_distribution_2020
# )
# 
# # Plot the histogram using ggplot2
# library(ggplot2)
# 
# ggplot(age_distribution_df, aes(x = AgeGroup, y = Diagnoses)) +
#   geom_bar(stat = "identity", fill = "skyblue", color = "black") +
#   labs(title = "Age Distribution of New HIV Diagnoses in 2020", x = "Age Group", y = "Number of Diagnoses") +
#   theme_minimal()
# 
# 
# 
# ##-------------------------------------------------------------------##
# ##-- Part 3: Model Calibration                                     --##
# ##-------------------------------------------------------------------##
# 
# # Load necessary packages
# library(stats) # For optimization functions
# 
# # Define the objective function to minimize
# # This function calculates the sum of squared differences between simulated and target values
# objective_function <- function(global.trate) {
#   # Update the model with the new global.trate value
#   params["global.trate"] <- global.trate
#   sim <- engine$run(parameters = params) # Replace with your model update function
#   
#   # Extract simulated values with updated global.trate
#   new.diagnoses.sim <- sim$get("new",year=c(2008:2021))
#   diagnosed.prevalence.sim <- sim$get("diagnosed.prevalence",year=c(2008:2021)) 
#   
#   # Calculate the sum of squared differences for both outcomes
#   new.diagnosis.error <- sum((new.diagnoses.sim - new.diagnosis.target)^2)
#   # diagnosed.prevalence.error <- sum((diagnosed.prevalence.sim - diagnosed.prevalence.target)^2)
#   
#   # Combine errors into a single metric (weighted sum of squared errors)
#   # Adjust weights as necessary to balance the importance of each outcome
#   total_error <- new.diagnosis.error #+ diagnosed.prevalence.error
#   
#   return(total_error)
# }
# 
# # Define a range of possible global.trate values to test
# global.trate.values <- seq(from = 0.01, to = 0.1, by = 0.01) # Adjust range and step size as needed
# 
# # Find the global.trate that minimizes the objective function
# optimization_result <- optim(par = 0.05, # Initial guess for global.trate
#                              fn = objective_function,
#                              method = "L-BFGS-B",
#                              lower = 0.01, # Lower bound for global.trate
#                              upper = 0.1)  # Upper bound for global.trate
# 
# # Extract the optimal global.trate from the optimization result
# optimal.global.trate <- optimization_result$par
# 
# # Summary of results
# cat("Optimal global.trate:", optimal.global.trate, "\n")
# cat("Objective function value at optimal global.trate:", optimization_result$value, "\n")
# 
# # Update the model with the optimal global.trate and plot the results
# params["global.trate"] <- optimal.global.trate
# optimal_sim <- engine$run(parameters = params) # Replace with your model update function
# 
# optimal_new.diagnoses.sim <- optimal_sim$get("new",year=c(2008:2021))
# optimal_diagnosed.prevalence.sim <- optimal_sim$get("diagnosed.prevalence",year=c(2008:2021))
# 
# # Plot simulated outcomes vs. target outcomes for visual verification
# par(mfrow = c(2, 1)) # Set up a 2x1 plotting layout
# 
# # Plot for New Diagnoses
# plot(2008:2021, optimal_new.diagnoses.sim, type = "l", col = "blue", 
#      ylim = range(c(new.diagnosis.target, optimal_new.diagnoses.sim)), 
#      xlab = "Year", ylab = "New Diagnoses", main = "Fit of Simulated vs. Target New Diagnoses")
# lines(2008:2021, new.diagnosis.target, col = "red")
# legend("topright", legend = c("Simulated", "Target"), col = c("blue", "red"), lty = 1)
# 
# # Plot for Diagnosed Prevalence
# plot(2008:2021, optimal_diagnosed.prevalence.sim, type = "l", col = "blue", 
#      ylim = range(c(diagnosed.prevalence.target, optimal_diagnosed.prevalence.sim)), 
#      xlab = "Year", ylab = "Diagnosed Prevalence", main = "Fit of Simulated vs. Target Diagnosed Prevalence")
# lines(2008:2021, diagnosed.prevalence.target, col = "red")
# legend("topright", legend = c("Simulated", "Target"), col = c("blue", "red"), lty = 1)
