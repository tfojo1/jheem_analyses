#Use this code to merge the different saved sections of the census manager into a complete version

# PROCESS -----------------------------------------------------------------
source('data_processing/hiv.surveillance.processing.for.merging/section.1.processing.R')
source('data_processing/hiv.surveillance.processing.for.merging/section.2.processing.R')
source('data_processing/hiv.surveillance.processing.for.merging/section.3.processing.R')
source('data_processing/hiv.surveillance.processing.for.merging/section.4.processing.R')
source('data_processing/hiv.surveillance.processing.for.merging/section.5.processing.R')

# MERGE -------------------------------------------------------------------

#LOAD the saved sections of the census manager
section1 = load.data.manager(name="surveillance.manager_section1", file="Q:/data_managers/data.manager.merge/surveillance.manager_section1.rdata")
section2 = load.data.manager(name="surveillance.manager_section2", file="Q:/data_managers/data.manager.merge/surveillance.manager_section2.rdata")
section3 = load.data.manager(name="surveillance.manager_section3", file="Q:/data_managers/data.manager.merge/surveillance.manager_section3.rdata")
section4 = load.data.manager(name="surveillance.manager_section4", file="Q:/data_managers/data.manager.merge/surveillance.manager_section4.rdata")
section5 = load.data.manager(name="surveillance.manager_section5", file="Q:/data_managers/data.manager.merge/surveillance.manager_section5.rdata")


#FIGURE OUT SIZE OF FILES THEN MERGE

# #MERGE 
# population1$import.data(population2) #This order doesn't matter, do it this way: big.one$importdata(smaller.one)
# population1$import.data(population3)
# population1$import.data(births.deaths1)
# 


# Run this code- it uses both HIV data and adult.population ---------------
source('data_processing/tests.per.population.R') # Source code to create hiv.tests.per.population 

# Remove outliers ---------------------------------------------------------
save(surveillance.manager, file="../../cached/surveillance.manager.before.outliers.rdata")
source('data_processing/outliers/outlier.remover.total.level.R')
source('data_processing/outliers/outlier.remover.one.way.strata.R')
source('data_processing/outliers/outlier.remover.two.way.strata.R')



# #SAVE Final, Complete Census Manager to Cached
# save(population1, file="../../cached/census.manager.merged.check.rdata")
# 
# #SAVE Final, Complete Census Manager to Q Drive
# save(population1, file="Q:/data_managers/census.manager.rdata")
# 
# #SAVE Final, Complete Census Manager, Archive a dated version to the Q Drive#
# timestamp <- Sys.Date()  
# filename <- paste0("Q:/data_managers/Archive/census.manager_", timestamp, ".rdata")
# save(population1, file=filename)


###Save surveillance manager####
save(surveillance.manager, file="../../cached/surveillance.manager.rdata")

#Also save to Q drive
save(surveillance.manager, file="Q:/data_managers/surveillance.manager.rdata")

#Archive a version with the date to the Q Drive#
timestamp <- Sys.Date()  
filename <- paste0("Q:/data_managers/Archive/surveillance.manager_", timestamp, ".rdata")
save(surveillance.manager, file=filename)
