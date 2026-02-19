#Use this code to merge the different saved sections of the census manager into a complete version

# RUN ALL OF THESE SECTION TO RE-CREATE THE SYPHILIS MANAGER ENTIRELY -----------------------------------------------------------------
# source('data_processing/syphilis.manager/syphilis.processing.for.merging/section1.processing.R')
# source('data_processing/syphilis.manager/syphilis.processing.for.merging/section2.processing.R')
# source('data_processing/syphilis.manager/syphilis.processing.for.merging/section3.processing.R')
# source('data_processing/syphilis.manager/syphilis.processing.for.merging/section4.processing.R')

# MERGE -------------------------------------------------------------------

#LOAD the saved sections of the syphilis manager (after you've made the changes to a particular section)
section_dir <- Sys.getenv("SECTION_DIR", "Q:/data_managers/data.manager.merge")
section1 = load.data.manager(name="syphilis.manager_section1", file=file.path(section_dir, "syphilis.manager_section1.rdata"))
section2 = load.data.manager(name="syphilis.manager_section2", file=file.path(section_dir, "syphilis.manager_section2.rdata"))
section3 = load.data.manager(name="syphilis.manager_section3", file=file.path(section_dir, "syphilis.manager_section3.rdata"))
section4 = load.data.manager(name="syphilis.manager_section1", file=file.path(section_dir, "syphilis.manager_section4.rdata"))

#MERGE the Sections
section3$import.data(section1) #This order doesn't matter, do it this way: big.one$importdata(smaller.one)
section3$import.data(section2)
section3$import.data(section4)

syphilis.manager = section3

#Source code to create proportion of congenital syphilis births:
source('data_processing/syphilis.manager/proportion.of.congenital.syphilis.births.R')

# Source Nick's Codes to transfer adult population: -----------------------------------------

#Add Adult population from HIV surveillance manager to syphilis manager:
source('data_processing/transfer_adult_population_to_syphilis_manager.R')

# Test Andrew's new Aggregation Code on Diagnoses: 1-30-26 ----------------

source('data_processing/syphilis.manager/syphilis.processing.for.merging/new.aggregation.method.R')

# Source Nick's QA Codes: -----------------------------------------

#Aggregate syphilis data into total.syphilis.diagnoses:
source('data_processing/syphilis.manager/data_quality_fix/test_total_with_restratification.R')

#Remove certain racial data that doesn't sum to proper totals:
source('data_processing/syphilis.manager/data_quality_fix/implement_removals.R')

# Save: -------------------------------------------------------------------
 
#SAVE Final, Complete Syphilis Manager to Cached
save(syphilis.manager, file="../../cached/syphilis.manager.rdata")

#SAVE Final, Complete Syphilis Manager to Q Drive
output_dir <- Sys.getenv("OUTPUT_DIR", "Q:/data_managers")
save(syphilis.manager, file=file.path(output_dir, "syphilis.manager.rdata"))

#SAVE Final, Complete Syphilis Manager, Archive a dated version to the Q Drive#
timestamp <- Sys.Date()
archive_dir <- Sys.getenv("ARCHIVE_DIR", file.path(output_dir, "Archive"))
filename <- file.path(archive_dir, paste0("syphilis.manager_", timestamp, ".rdata"))
save(syphilis.manager, file=filename)
