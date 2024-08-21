# DELIVERABLES:
# For Part 1: In a word document, include a copy of the two plots generated above and describe the fit of the simulation to the CDC data.
# For Parts 2 & 3: include you code in response to each question below and save the R script to share with us 

##---------------------------------------------------------------##
##-- PART 1: General competency in git & R                     --##
##---------------------------------------------------------------##
## Testing experience with Git, ability to run R code, and ability to interpret calibration results 

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

# Set up the model specification (this may take a few moments)
source('../jheem_analyses/applications/EHE/ehe_specification.R')

# Load a set of parameters and set the global transmission rate to equal 0.01
params = suppressWarnings(get.medians(EHE.PARAMETERS.PRIOR))
params['global.trate'] = 0.01

# Create the model engine for a given location (this may take a few moments)
engine = create.jheem.engine(version = 'ehe', location = 'c.12580', end.year=2025, max.run.time.seconds = 10)

# Using the model engine, run and save a single simulation using the transmission rate you loaded above (this may take a few moments)
sim = engine$run(parameters = params)

# Visualize and describe the simulation fit for projected new HIV diagnosis and prevalence of diagnosed HIV against CDC's reported data: 
# 1. New diagnoses
simplot(sim, outcomes = "new", dimension.values = list(year = 2007:2025))
# 2. Diagnosed prevalence 
simplot(sim, outcomes = "diagnosed.prevalence", dimension.values = list(year = 2007:2025))



##-------------------------------------------------------------------##
##-- PART 2: Generate new code based on the following instructions --##
##-------------------------------------------------------------------##
# Now, rather than setting the global transmission rate to equal 0.01 as we did in part 1, 
# we will sample 3 different values of that parameter from a given distribution and compare the results 

# Set the R seed to 1234
# Set up a loop to sample 3 different values of "global.trate" from a uniform distribution bounded between 0 and 0.05
# Set up 3 params objects where the global.trate has been replaced by each of the 3 the sampled values
# Run a sim with each set of params objects, calling them sim1, sim2, sim3
# Visualize the results for these 3 simulations and interpret the fit to calibration targets. which simulation set provides the best fit?
# simplot(sim1, sim2, sim3, outcomes = "new", 
#         dimension.values = list(year = 2007:2025))
# simplot(sim1, sim2, sim3, outcomes = "diagnosed.prevalence", 
#         dimension.values = list(year = 2007:2025))

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
##-- PART 3: Generate new code based on the following instructions                                           --##
##-------------------------------------------------------------------------------------------------------------##
# Extract the number of new diagnosis from the sim object created in part1 above:
# (* if you have failed to creat this object, you can access it from  >> ??????????)

# A. Describe the dimension of the new object and the dimnames 
# B. Filter new diagnosis to those specific to 13-24 years, black, msm, and never IDU, and plot it over time using the plot function 
# C. Aggregate new diagnosis for all msm, and plot it over time using ggplot2 library



# **********************************************
#answrers:
new=sim$new

dim(new)
dimnames(new)
#  A. 13-24 years, black, msm, and never IDU
age_index <- which(dimnames(new)$age == "13-24 years")
race_index <- which(dimnames(new)$race == "black")
sex_index <- which(dimnames(new)$sex == "msm")
risk_index <- which(dimnames(new)$risk == "never_IDU")
new_filtered <- new[,    # All locations (since only one)
                    ,    # All years (if you want to keep all years, leave empty or specify indices)
                    age_index,
                    race_index,
                    sex_index,
                    risk_index,
                    ,    # All simulations (if you want to keep all simulations, leave empty or specify indices)
                    drop = FALSE]
plot(x=dimnames(new_filtered)$year,
     y=new_filtered, 
     type = "l", 
     xlab = "Year", 
     ylab = "New diagnoses", main = "New diagnoses over time for 13-24 years, black, msm, and never IDU")

#plot new diagnosis for all msm over time using ggplot2
# Get the index for "msm"
sex_index <- which(dimnames(new)$sex == "msm")

# Extract data for "msm"
new_filtered <- new[,  # All years
                    ,  # All locations
                    ,  # All ages
                    ,  # All races
                    sex_index,
                    ,  # All risks
                    ,  # All simulations
                    drop = FALSE]

# Sum data over all dimensions except for year
aggregated_data <- apply(new_filtered, c(1), sum)

# Convert to a data frame for plotting
years <- as.numeric(dimnames(new)$year)
aggregated_df <- data.frame(
  year = years,
  value = as.vector(aggregated_data)
)
library(ggplot2)
ggplot(aggregated_df, aes(x = year, y = value)) +
  geom_line() +
  labs(title = "Aggregated Time Series Plot for MSM",
       x = "Year",
       y = "Aggregated Value") +
  theme_minimal()
# **********************************************