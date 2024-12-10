#Use this code to merge the different saved sections of the census manager into a complete version

# PROCESS -----------------------------------------------------------------
source('data_processing/syphilis.manager/syphilis.processing.for.merging/section1.processing.R')
source('data_processing/syphilis.manager/syphilis.processing.for.merging/section2.processing.R')
source('data_processing/syphilis.manager/syphilis.processing.for.merging/section3.processing.R')
source('data_processing/syphilis.manager/syphilis.processing.for.merging/section4.processing.R')

# MERGE -------------------------------------------------------------------

#LOAD the saved sections of the syphilis manager
section1 = load.data.manager(name="syphilis.manager_section1", file="Q:/data_managers/data.manager.merge/syphilis.manager_section1.rdata")
section2 = load.data.manager(name="syphilis.manager_section2", file="Q:/data_managers/data.manager.merge/syphilis.manager_section2.rdata")
section3 = load.data.manager(name="syphilis.manager_section3", file="Q:/data_managers/data.manager.merge/syphilis.manager_section3.rdata")
section4 = load.data.manager(name="syphilis.manager_section1", file="Q:/data_managers/data.manager.merge/syphilis.manager_section4.rdata")

#You'll need to figure out which is the largest#

#MERGE 
# population1$import.data(population2) #This order doesn't matter, do it this way: big.one$importdata(smaller.one)
# population1$import.data(population3)
# population1$import.data(births.deaths1)
# 
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
