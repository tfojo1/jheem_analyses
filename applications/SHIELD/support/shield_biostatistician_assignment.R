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

# Navigate to the jheem_analyses directory (setwd) and run the following code to install the required packages and set up the model specification.
# You may be prompted to install additional packages that are dependencies for these packages as well.
# source('../jheem_analyses/first_time_setup/install_packages.R')

# Once all packages are installed, the model is ready for use.

###############
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


##----------------------------------------------------##
##-- Part 5: Paper Review                           --##
##----------------------------------------------------##
# Please review the following paper and answer the below questions:
# "What Will It Take to End HIV in the United States? A Comprehensive, Local-Level Modeling Study"
# https://www.acpjournals.org/doi/10.7326/M21-1501
# you can also find a saved copy under the cashed folder: https://www.dropbox.com/scl/fo/5vxliw7fnq91kt0odbeab/AM5SrKajDnDVQ06M7Dmsem4?rlkey=i9rqo23thi9zg9ytav4u1lyha&dl=0
# unde fojo-et-al-2021-what-will-it-take-to-end-hiv-in-the-united-states.pdf

# 1.	What is the benefit of running 1000 simulations for each MSA? how would you determine if 1000 simulation is approperiate or if you need less/more?

# 2.	The authors assumed that, without any additional intervention, PrEP coverage would continue its trajectory into the future.
#     Consider a scenario where baseline PrEP coverage remains fix at 2023's level.
#     How might that change the impact of the modeled PrEP intervention to 2030?

# 3.	The authors chose to scale up the interventions linearly. How might this differ in a real-world scenario?

# 4.	Provide an interpretation of Figure 3, panel D.

# 5.  The compartmental model is deterministic by nature. What are the soures of uncertainty in the current JHEEM model
#     that contribute to shaded areas in Figure 3

# 6.	Model calibration focuses on 10 calibration target including new diagnoses, prevalence,etc. Let's assume that we have more certainty
#     in some targets than others. How can we reflect this in the likeloohood function used for calibration?



## EXAMPLE ANSWERS - REMOVE BEFORE DISTRIBUTING ##
# 2.	What is the benefit of running 1000 simulations for each MSA?
    # a. Answer: Represent uncertainty; sensitivity analyses
# 3.	The authors note that their modeled scenarios of improved viral suppression represent the combined efficacy of interventions to enhance engagement and retention in care and improved adherence to antiretrovirals. List two potential real-world interventions that could result in improved viral suppression.
    # a. Example answers: (Many acceptable answers for this question)
# 4.	The authors assumed that, without any additional intervention, PrEP coverage would continue its trajectory into the future. Consider a scenario where PrEP coverage actually dropped in the future (e.g., during COVID). How might that change the impact of the modeled PrEP intervention?
    # a. Answer: Intervention would have a greater impact because baseline is lower
# 5.	The authors chose to scale up the interventions linearly. How might this differ in a real-world scenario?
    # a. Example answer: Early adopters scale-up quickly; marginalized populations scale up more slowly as they are harder to reach
# 6.	Provide an interpretation of Figure 3, panel D.
    # a. Example answer: Reported cases trending downwards in both the simulation and data from 2010-2018; intervention implemented in 2020 leads to a short-term increase in reported cases (due to increased testing/identification of cases; not increased transmission) then a sustained decline in cases
# 7.	Scenario 6 reduced new infections in the Seattle-Tacoma-Bellevue MSA by 17% compared to 51% in the Miami-Fort Lauderdale-Pompano Beach MSA (Figure 4). What is your interpretation of this result and what are some possible explanations for this difference?
    # a. Example answer: Seattle has a small population of Black and Hispanic MSM <35y or their population of YBHMSM already has high testing/PrEP/suppression rates
# 8.	The sensitivity analysis found that HIV transmission among Black heterosexuals was the parameter most strongly associated with the estimated reduction in HV incidence. If this transmission rate were lower than expected, how would you expect that to impact the projected reduction in HIV incidence?
    # a. Answer: A lower transmission rate would lead to a lower reduction in incidence (they should be able to pull this directly from the Baton Rouge example given).
# 9.	Give 1-2 examples of research questions you could examine with this model.
    # a. Example answers: (Many acceptable answers for this question)
