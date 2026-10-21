#Use this code to merge the different saved sections of the census manager into a complete version

# PROCESS -----------------------------------------------------------------
# source('data_processing/census.manager/census.processing.for.merging/section.1.processing.R')
# source('data_processing/census.manager/census.processing.for.merging/section.2.processing.R')
# source('data_processing/census.manager/census.processing.for.merging/section.3.processing.R')

# MERGE -------------------------------------------------------------------

#LOAD the saved sections of the census manager
section1 = load.data.manager(name="census.manager_section1", file="Q:/data_managers/data.manager.merge/census.manager_section1.rdata")
section2 = load.data.manager(name="census.manager_section2", file="Q:/data_managers/data.manager.merge/census.manager_section2.rdata")
section3 = load.data.manager(name="census.manager_section3", file="Q:/data_managers/data.manager.merge/census.manager_section3.rdata")

#MERGE 
section1$import.data(section2) #This order doesn't matter, do it this way: big.one$importdata(smaller.one)
section1$import.data(section3)

census.manager = section1

#SAVE Final, Complete Census Manager to Cached
save(census.manager, file="../../cached/census.manager.rdata")

#SAVE Final, Complete Census Manager to Q Drive
save(census.manager, file="Q:/data_managers/census.manager.rdata")

#SAVE Final, Complete Census Manager, Archive a dated version to the Q Drive#
timestamp <- Sys.Date()  
filename <- paste0("Q:/data_managers/Archive/census.manager_", timestamp, ".rdata")
save(population1, file=filename)
