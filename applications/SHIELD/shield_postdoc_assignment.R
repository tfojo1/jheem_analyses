# DELIVERABLES:
# Part 1:#   
#   Provide a Word document containing the two generated plots.
# Include a description of how well the simulated data fits the CDC targets.
# Parts 2 & 3:#   
#   Include the R code you developed for each question, save the R script, and share it with us.
# For each question, include your rationale for the new code and your interpretation of the results as comments throughout the script and at the end of each question.


##---------------------------------------------------------------##
##-- PART 1: Code Setup and Implementation                     --##
##---------------------------------------------------------------##
# Run the following code without making any changes

# Clone jheem2 repository from git. This repository contains the simulation engine and other necessary files.
# https://github.com/tfojo1/jheem2

# Clone jheem_analyses repository from git to access the model specification and other necessary files.
# https://github.com/tfojo1/jheem_analyses

# On you local computer, within the jheem_analyses directory: Create a "cached" folder. 
# This directory is not synced to git because of file sizes
# Use the following link to download required files:
# https://www.dropbox.com/scl/fo/5vxliw7fnq91kt0odbeab/AM5SrKajDnDVQ06M7Dmsem4?rlkey=i9rqo23thi9zg9ytav4u1lyha&dl=0
# Add the following datasets to this cached folder: 
      # surveillance.manager.rdata
      # census.manager.rdata
      # national.surveillance.Rdata 
      # google_mobility_data.Rdata 
 
# Pull both repositories to make sure you have the most recent version 

# Run the following command to install the required packages. You may be prompted to install additional packages that are dependencies for these packages as well.
# source('../jheem_analyses/first_time_setup/install_packages.R')

# Set up the model specification by running the following (this may take a few moments)
source('../jheem_analyses/applications/EHE/ehe_specification.R')

# Create the model engine for a given location (c.12580 is the code for Baltimore City) (this may take a few moments)
engine = create.jheem.engine(version = 'ehe', location = 'c.12580', end.year=2025, max.run.time.seconds = 10)

# Load a set of parameters (set at default values) 
params = suppressWarnings(get.medians(EHE.PARAMETERS.PRIOR))

# Set the global transmission rate to equal 0.01
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
# and observed its impact on the simulation outcomes. Now, we will explore the effect of variability in this parameter

## Tasks:
# Set the R seed to 1234
# Instead of fixing the global transmission rate, you will investigate how changes in this rate affect the model's predictions. 
# Sample three different values of global.trate from a uniform distribution bounded between 0 and 0.05.
# For each sampled value, update the params object and rerun the simulation.
# Run the simulations for the three different global.trate values you sampled.
# Visualize the outcomes of these simulations, focusing on the "new HIV diagnoses" and # "prevalence of diagnosed HIV" from 2007 to 2025.
# Compare the results from the three simulations with each other and against CDC-reported data.
# Analyze which of the three simulations provides the best fit to the observed data.

# Note: You will need to develop the code to sample the parameters, run the simulations, and create the visualizations
# Please include your interpretation of the results and any insights you gained from this analysis at the end of your code

# hint: you can use the simplot function to plot outputs from several simulations on the same plot:
# simplot(sim1, sim2, outcomes = "new", dimension.values = list(year = 2007:2025))


##-------------------------------------------------------------------##
##-- Part 3: Model Calibration                                     --##
##-------------------------------------------------------------------##
# In Part 2, you set the global transmission rate (global.trate) to some random values and observed the impact on the two calibration outcomes
# Now, we will explore a machanism for optimizing the fit to these outcomes

# Extract the calibration targets for number of new diagnosis (new.diagnosis) and prevalence of diagnosed HIV (diagnosed.prevalence) as follow:
new.diagnosis.target=SURVEILLANCE.MANAGER$pull(outcome="diagnoses", location="C.12580",source="cdc.aggregated.county")
diagnosed.prevalence.target= SURVEILLANCE.MANAGER$pull(outcome="diagnosed.prevalence", location="C.12580",source="cdc.aggregated.county")

# Extract the simulated values for each output over specific years from the sim object as follow:
new.diagnoses.sim=sim$get("new",year=c(2008:2021))
diagnosed.prevalence.sim=sim$get("diagnosed.prevalence",year=c(2008:2021)) 


# Write a code to find the global.trate that provides the best fit to new.diagnosis.target and diagnosed.prevalence.target over time
# you can use alternative measures of goodness of fit that you deem approperiate
# please provide comments throughout your code to describe your rationale and include a summary at the end to describe the results 

##-------------------------------------------------------------------------------------------------------------##
##-- Part 4: Data Extraction and Analysis                                                                    --##
##-------------------------------------------------------------------------------------------------------------##
# In this section, you'll explore and analyze simulation results related to new HIV diagnoses. 
# You will need to extract, filter, and aggregate data from a simulation object and visualize your findings.

# Task 1: Extract the Number of New Diagnoses
# First, we need to extract the number of new diagnoses from the sim object created in Part 1. 
# you can call this object new.diagnoses  
# If you don’t have access to the sim object, you would load it from the onedrive folder.

## Describe the dimensions and names of the dimensions of the Extracted Objects

## Filter Objects for Specific Subgroups
# a. Filter the new.diagnoses to focus on the subgroup: 13-24 years old, Black, MSM, and never IDU.
# b. Plot the filtered data over time
# c. interpret the trend in new diagnoses for this subgroup
# hint: you can filter data by accessing specific indexes in eahc dimension
# new_filtered <- new[location_index, year_index, age_index, race_index, sex_index, risk_index,
#                     ,    # All simulations (if you want to keep all simulations, leave empty or specify indices)
#                     drop = FALSE]


## Draw a histogram of the age distribution for new HIV diagnoses in the year 2020
## what proportion of new diagnosis in year 2020 occured among 12-24 years old?
# hint: you can aggregate the new.diagnoses across all groups in year 2020 and plot the age distribution using ggplot2




