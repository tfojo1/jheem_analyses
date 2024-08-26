# DELIVERABLES:
# For Part 1: In a word document, include a copy of the two plots generated above and describe the fit of the simulation to the CDC data.
# For Parts 2 & 3: include you code in response to each question below and save the R script to share with us 

##---------------------------------------------------------------##
##-- PART 1: Code Setup and Implementation                     --##
##---------------------------------------------------------------##
## Goals: Testing experience with Git, ability to run R code, and ability to interpret calibration results 

##--  STEP1: Set up the environment --##
# Run the following code without making any changes

# Clone jheem2 repository from git. This repository contains the simulation engine and other necessary files.
# https://github.com/tfojo1/jheem2

# Clone jheem_analyses repository from git to access the model specification and other necessary files.
# https://github.com/tfojo1/jheem_analyses

# On you local computer, within the jheem_analyses directory: Create a "cached" folder. This directory is not synced to git because of file sizes; 
# add the following files to this cached folder: 
      # surveillance.manager.rdata
      # census.manager.rdata
      # national.surveillance.Rdata 
      # google_mobility_data.Rdata 
#@MS: Link to oneDrive????

# Pull both repositories to make sure you have the most recent version 

##--  STEP2: Create a sample run --##
# Set up the model specification by running the following (this may take a few moments)
source('../jheem_analyses/applications/EHE/ehe_specification.R')

# Create the model engine for a given location (c.12580 is the code for Baltimore City) (this may take a few moments)
engine = create.jheem.engine(version = 'ehe', location = 'c.12580', end.year=2025, max.run.time.seconds = 10)

# Load a set of parameters (set at default values) and set the global transmission rate to equal 0.01
params = suppressWarnings(get.medians(EHE.PARAMETERS.PRIOR))
params['global.trate'] = 0.01

# Using the model engine, run and save a single simulation using the transmission rate you loaded above (this may take a few moments)
sim = engine$run(parameters = params)

# Visualize and describe the simulation fit for projected "new HIV diagnosis" and "prevalence of diagnosed HIV" against CDC's reported data: 
# 1. New diagnoses
simplot(sim, outcomes = "new", dimension.values = list(year = 2007:2025))

# 2. Diagnosed prevalence 
simplot(sim, outcomes = "diagnosed.prevalence", dimension.values = list(year = 2007:2025))

##-------------------------------------------------------------------##
##-- Part 2: Code Implementation and Analysis                      --##
##-------------------------------------------------------------------##
# In Part 1, you set the global transmission rate (global.trate) to a fixed value 
# and observed its impact on the simulation outcomes. Now, we will explore the effect of variability in 
# this parameter.

## Tasks:
# Set the R seed to 1234
# Exploration: Instead of fixing the global transmission rate, you will investigate how changes in this rate affect 
# the model's predictions. Specifically:
# Sample three different values of global.trate from a uniform distribution bounded between 0 and 0.05.
# For each sampled value, update the params object and rerun the simulation.
# Simulation: Run the simulations for the three different global.trate values you sampled.
# Visualization and Analysis: Visualize the outcomes of these simulations, focusing on the "new HIV diagnoses" and 
# "prevalence of diagnosed HIV" from 2007 to 2025.
# Compare the results from the three simulations with each other and against CDC-reported data.
# Critical Evaluation: Analyze which of the three simulations provides the best fit to the observed data.
# Discuss how the variability in the global transmission rate affects the model’s predictions and the 
# implications for understanding the dynamics of HIV in the population.

# Note: You will need to develop the code to sample the parameters, run the simulations, and create the visualizations
# Please include your interpretation of the results and any insights you gained from this analysis at the end of your code

# hint: you can use the simplot function to plot outputs from several simulations:
# simplot(sim1, sim2, outcomes = "new", dimension.values = list(year = 2007:2025))

# *******************************
# answers: 
set.seed(1234)

global.trate = c()
for(i in 1:3){
  global.trate[i] = runif(1, min = 0, max = 0.08)
}
# 
params1 = params2 = params3 = params
# 
params1["global.trate"] = global.trate[1]
params2["global.trate"] = global.trate[2]
params3["global.trate"] = global.trate[3]
# 
sim1 = engine$run(parameters = params1)
sim2 = engine$run(parameters = params2)
sim3 = engine$run(parameters = params3)

# Visualize the results (might need to tell them you can pass all three sims as arguments to simplot)
simplot(sim1, sim2, sim3, outcomes = "new", 
        dimension.values = list(year = 2007:2025))

simplot(sim1, sim2, sim3, outcomes = "diagnosed.prevalence", 
        dimension.values = list(year = 2007:2025))
#sim1 understiamtes new diagnosis but fits prevalence of diagnosed HIV better than 2 and 3
# *******************************

##-------------------------------------------------------------------------------------------------------------##
##-- Part 3: Data Extraction, Filtering, and Analysis                                                        --##
##-------------------------------------------------------------------------------------------------------------##
# In this section, you'll explore and analyze simulation results related to new HIV diagnoses. You will need to extract, filter, and aggregate data from a simulation object and visualize your findings.

# Task 1: Extract the Number of New Diagnoses
# First, we need to extract the number of new diagnoses and prevalence of diagnosed HIV from the sim object created in Part 1. 
# you can call these objects new_diagnoses and diagnosed_prevalence, respectively.
# If you don’t have access to the sim object, you would load it from here: x?????????????


# Task 2: Describe the dimensions and names of the dimensions of the Extracted Objects

# Task 3: Filter Objects for Specific Subgroups
# a. Filter the new_diagnoses & diagnosed_prevalence object to focus on the subgroup: 13-24 years old, Black, MSM, and never IDU.
# b. Plot the filtered data over time
# c. interpret the trend in new diagnoses and diagnosed prevalence for this subgroup, when is the peak observed and why they dont match?

# hint: you can filter data by accessing specific indexes in eahc dimension
# new_filtered <- new[location_index, year_index, age_index, race_index, sex_index, risk_index,
#                     ,    # All simulations (if you want to keep all simulations, leave empty or specify indices)
#                     drop = FALSE]


# Task 4: Draw a histogram of the age distribution for new HIV diagnoses in the year 2020
# hint: Aggregate the new_diagnoses across all groups in year 2020 and plot the age distribution using ggplot2
# what proportion of new diagnosis in year 2020 occured among 12-24 years old?



# **********************************************
#Task1: 
new_diagnoses=sim$new
diagnosed_prevalence=sim$diagnosed.prevalence
#Task2:
dimnames(new_diagnoses)
dimnames(diagnosed_prevalence)
#Task3:
#  A. 13-24 years, black, msm, and never IDU
age_index <- which(dimnames(new)$age == "13-24 years")
race_index <- which(dimnames(new)$race == "black")
sex_index <- which(dimnames(new)$sex == "msm")
risk_index <- which(dimnames(new)$risk == "never_IDU")

new_diagnoses_filtered <- new_diagnoses[,    # All locations (since only one)
                    ,    # All years (if you want to keep all years, leave empty or specify indices)
                    age_index,
                    race_index,
                    sex_index,
                    risk_index,
                    ,    # All simulations (if you want to keep all simulations, leave empty or specify indices)
                    drop = FALSE]
diagnosed_prevalence_filtered <- diagnosed_prevalence[,    # All locations (since only one)
                    ,    # All years (if you want to keep all years, leave empty or specify indices)
                    age_index,
                    race_index,
                    sex_index,
                    risk_index,
                    ,    # All simulations (if you want to keep all simulations, leave empty or specify indices)
                    drop = FALSE]


plot(x=dimnames(new_diagnoses_filtered)$year,
     y=new_diagnoses_filtered, 
     type = "l", 
     xlab = "Year", 
     ylab = "New diagnoses", main = "New diagnoses over time for 13-24 years, black, msm, and never IDU")
plot(x=dimnames(diagnosed_prevalence_filtered)$year,
     y=diagnosed_prevalence_filtered, 
     type = "l", 
     xlab = "Year", 
     ylab = "prevDiag", main = "Prev of Diagnosed HIV over time for 13-24 years, black, msm, and never IDU")



#Task4:
dim(new_diagnoses)
dimnames(new_diagnoses)
# Find the index for the year 2020 in the dimnames of the 'year' dimension
year_index <- which(dimnames(new_diagnoses)$year == 2020)

# Sum across all other dimensions (e.g., location, race, sex, risk) except for age to get the total diagnoses per age group
age_distribution_2020 <- apply(new_diagnoses[year_index, , , , , ,], c(1), sum)

# Convert to a data frame for easier plotting
age_distribution_df <- data.frame(
  AgeGroup = dimnames(new_diagnoses)$age,
  Diagnoses = age_distribution_2020
)

# Plot the histogram using ggplot2
library(ggplot2)

ggplot(age_distribution_df, aes(x = AgeGroup, y = Diagnoses)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Age Distribution of New HIV Diagnoses in 2020", x = "Age Group", y = "Number of Diagnoses") +
  theme_minimal()




