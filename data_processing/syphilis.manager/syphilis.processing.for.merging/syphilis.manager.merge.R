#Use this code to merge the different saved sections of the census manager into a complete version

# RUN ALL OF THESE SECTION TO RE-CREATE THE SYPHILIS MANAGER ENTIRELY -----------------------------------------------------------------
# source('data_processing/syphilis.manager/syphilis.processing.for.merging/section1.processing.R')
# source('data_processing/syphilis.manager/syphilis.processing.for.merging/section2.processing.R')
# source('data_processing/syphilis.manager/syphilis.processing.for.merging/section3.processing.R')
# source('data_processing/syphilis.manager/syphilis.processing.for.merging/section4.processing.R')

# MERGE -------------------------------------------------------------------

#LOAD the saved sections of the syphilis manager (after you've made the changes to a particular section)
section1 = load.data.manager(name="syphilis.manager_section1", file="Q:/data_managers/data.manager.merge/syphilis.manager_section1.rdata")
section2 = load.data.manager(name="syphilis.manager_section2", file="Q:/data_managers/data.manager.merge/syphilis.manager_section2.rdata")
section3 = load.data.manager(name="syphilis.manager_section3", file="Q:/data_managers/data.manager.merge/syphilis.manager_section3.rdata")
section4 = load.data.manager(name="syphilis.manager_section1", file="Q:/data_managers/data.manager.merge/syphilis.manager_section4.rdata")

#MERGE the Sections
section3$import.data(section1) #This order doesn't matter, do it this way: big.one$importdata(smaller.one)
section3$import.data(section2)
section3$import.data(section4)

syphilis.manager = section3

#Source code to create proportion of congenital syphilis births:
source('data_processing/syphilis.manager/proportion.of.congenital.syphilis.births.R')

# Source Nick's Codes from Leave: -----------------------------------------

#Add Adult population from HIV surveillance manager to syphilis manager:
source('data_processing/transfer_adult_population_to_syphilis_manager.R')

#Aggregate syphilis data into total.syphilis.diagnoses:
source('data_processing/syphilis.manager/data_quality_fix/test_total_with_restratification.R')

#Remove certain racial data that doesn't sum to proper totals:
source('data_processing/syphilis.manager/data_quality_fix/implement_removals.R')

# Save: -------------------------------------------------------------------
 
#SAVE Final, Complete Syphilis Manager to Cached
save(syphilis.manager, file="../../cached/syphilis.manager.rdata")

#SAVE Final, Complete Syphilis Manager to Q Drive
save(syphilis.manager, file="Q:/data_managers/syphilis.manager.rdata")

#SAVE Final, Complete Syphilis Manager, Archive a dated version to the Q Drive#
timestamp <- Sys.Date()
filename <- paste0("Q:/data_managers/Archive/syphilis.manager_", timestamp, ".rdata")
save(syphilis.manager, file=filename)
