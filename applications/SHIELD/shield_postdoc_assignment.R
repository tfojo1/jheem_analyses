##---------------------------------------------------------------##
##-- PART 1: Run the following code without making any changes --##
##---------------------------------------------------------------##

# Clone jheem_analyses repository from git 
# Clone jheem2 repository from git 

# Create a "cached" folder within the jheem_analyses directory (this is not synced to git because of file sizes); 
# add the following files to this cached folder: 
      # surveillance.manager.rdata
      # census.manager.rdata
      # national.surveillance.Rdata 
      # google_mobility_data.Rdata 

# Pull both repositories to make sure you have the most recent version 

# Set up the model specification: 
source('../jheem_analyses/applications/EHE/ehe_specification.R')

# Load a set of parameters and set the global transmission rate to equal 0.01
params = suppressWarnings(get.medians(EHE.PARAMETERS.PRIOR))
params['global.trate'] = 0.01

# Create the model engine 
engine = create.jheem.engine('ehe', 'c.12580', end.year=2025, max.run.time.seconds = 10)

# Using the model engine, run and save a simulation using the parameters you loaded above 
sim = engine$run(parameters = params)

# Visualize and describe the simulation fit for: 
# 1. New diagnoses
simplot(sim, outcomes = "new", dimension.values = list(year = 2007:2025))
# 2. Diagnosed prevalence 
simplot(sim, outcomes = "diagnosed.prevalence", dimension.values = list(year = 2007:2025))


##-------------------------------------------------------------------##
##-- PART 2: Generate new code based on the following instructions --##
##-------------------------------------------------------------------##
# We will remove parts of the below code so that they can fill it in
# With the given seed, the three global trates should be: 0.005685171, 0.031114970, 0.030463737

# Set the seed 
set.seed(1234)

# Now, rather than setting the global transmission rate to equal 0.01 as we did in part 1, 
# we will sample 3 different values of that parameter from a given distribution and compare the results 

# Set up a loop to sample 3 different values of "global.trate" from a uniform distribution bounded between 0 and 0.05
global.trate = c()
for(i in 1:3){
  global.trate[i] = runif(1, min = 0, max = 0.05)
}



# Set up 3 params objects where the global.trate has been replaced by each of the 3 the sampled values
params1 = params2 = params3 = params

params1["global.trate"] = global.trate[1]
params2["global.trate"] = global.trate[2]
params3["global.trate"] = global.trate[3]

# Run a sim with each set of params objects
sim1 = engine$run(parameters = params1)
sim2 = engine$run(parameters = params2)
sim3 = engine$run(parameters = params3)

# Visualize the results (might need to tell them you can pass all three sims as arguments to simplot)
simplot(sim1, sim2, sim3, outcomes = "new", 
        dimension.values = list(year = 2007:2025))
simplot(sim1, sim2, sim3, outcomes = "diagnosed.prevalence", 
        dimension.values = list(year = 2007:2025))
  
# Describe the fits, etc. 

# Could give a bonus to ask them to use the "facet.by" (="race") argument to compare the fits by race? 


