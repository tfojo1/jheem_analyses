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

#MERGE
section1$import.data(section2) #This order doesn't matter, do it this way: big.one$importdata(smaller.one)
section1$import.data(section4)
section1$import.data(section3)
section1$import.data(section5)

surveillance.manager = section1

# Run this code- it uses both HIV data and adult.population ---------------
source('data_processing/tests.per.population.R') # Source code to create hiv.tests.per.population 

# Remove outliers ---------------------------------------------------------
save(surveillance.manager, file="../../cached/surveillance.manager.before.outliers.rdata")
source('data_processing/outliers/outlier.remover.total.level.R')
source('data_processing/outliers/outlier.remover.one.way.strata.R')
source('data_processing/outliers/outlier.remover.two.way.strata.R')


###Save surveillance manager####
save(surveillance.manager, file="../../cached/surveillance.manager.rdata")

#Also save to Q drive
save(surveillance.manager, file="Q:/data_managers/surveillance.manager.rdata")

#Archive a version with the date to the Q Drive#
timestamp <- Sys.Date()
filename <- paste0("Q:/data_managers/Archive/surveillance.manager_", timestamp, ".rdata")
save(surveillance.manager, file=filename)
