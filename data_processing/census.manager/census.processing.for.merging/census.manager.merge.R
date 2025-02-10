#Use this code to merge the different saved sections of the census manager into a complete version

# PROCESS -----------------------------------------------------------------
source('data_processing/census.manager/census.processing.for.merging/population1.processing.R')
source('data_processing/census.manager/census.processing.for.merging/population2.processing.R')
source('data_processing/census.manager/census.processing.for.merging/population3.processing.R')
source('data_processing/census.manager/census.processing.for.merging/births.deaths.processing.R')

# MERGE -------------------------------------------------------------------

#LOAD the saved sections of the census manager
population1 = load.data.manager(name="census.manager_population1", file="Q:/data_managers/data.manager.merge/census.manager_population1.rdata")
population2 = load.data.manager(name="census.manager_population2", file="Q:/data_managers/data.manager.merge/census.manager_population2.rdata")
population3 = load.data.manager(name="census.manager_population3", file="Q:/data_managers/data.manager.merge/census.manager_population3.rdata")
births.deaths1 = load.data.manager(name="census.manager_births.deaths1", file="Q:/data_managers/data.manager.merge/census.manager_births.deaths.rdata")

#MERGE 
population1$import.data(population2) #This order doesn't matter, do it this way: big.one$importdata(smaller.one)
population1$import.data(population3)
population1$import.data(births.deaths1)

#SAVE Final, Complete Census Manager to Cached
save(population1, file="../../cached/census.manager.rdata")

#SAVE Final, Complete Census Manager to Q Drive
save(population1, file="Q:/data_managers/census.manager.rdata")

#SAVE Final, Complete Census Manager, Archive a dated version to the Q Drive#
timestamp <- Sys.Date()  
filename <- paste0("Q:/data_managers/Archive/census.manager_", timestamp, ".rdata")
save(population1, file=filename)
