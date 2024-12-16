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
section3$import.data(section1) #This order doesn't matter, do it this way: big.one$importdata(smaller.one)
section3$import.data(section2)
section3$import.data(section4)
 
#SAVE Final, Complete Syphilis Manager to Cached
save(section3, file="../../cached/syphilis.manager.rdata")

#SAVE Final, Complete Syphilis Manager to Q Drive
save(section3, file="Q:/data_managers/syphilis.manager.rdata")

#SAVE Final, Complete Syphilis Manager, Archive a dated version to the Q Drive#
timestamp <- Sys.Date()
filename <- paste0("Q:/data_managers/Archive/syphilis.manager_", timestamp, ".rdata")
save(section3, file=filename)
