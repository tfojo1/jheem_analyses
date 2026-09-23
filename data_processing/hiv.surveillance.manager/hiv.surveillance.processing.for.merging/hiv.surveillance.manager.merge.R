#Use this code to merge the different saved sections of the HIV surveillance manager into a complete version

# PROCESS -----------------------------------------------------------------
# source('data_processing/hiv.surveillance.processing.for.merging/section.1.processing.R')
# source('data_processing/hiv.surveillance.processing.for.merging/section.2.processing.R')
# source('data_processing/hiv.surveillance.processing.for.merging/section.3.processing.R')
# source('data_processing/hiv.surveillance.processing.for.merging/section.4.processing.R')
# source('data_processing/hiv.surveillance.processing.for.merging/section.5.processing.R')

# MERGE -------------------------------------------------------------------

# The shared-drive path remains the interactive default. Candidate builds can
# provide isolated inputs and outputs without changing the data transformations.
Q_ROOT <- Sys.getenv("Q_ROOT", "Q:")
SECTION_DIR <- Sys.getenv("SECTION_DIR", file.path(Q_ROOT, "data_managers/data.manager.merge"))
CACHED_DIR <- Sys.getenv("CACHED_DIR", "../../cached")
SHARED_MANAGER_DIR <- Sys.getenv("SHARED_MANAGER_DIR", file.path(Q_ROOT, "data_managers"))
ARCHIVE_DIR <- Sys.getenv("ARCHIVE_DIR", file.path(SHARED_MANAGER_DIR, "Archive"))
write.shared <- tolower(Sys.getenv("WRITE_SHARED_OUTPUT", "true"))
if (!(write.shared %in% c("true", "false")))
    stop("WRITE_SHARED_OUTPUT must be 'true' or 'false'")
write.shared <- identical(write.shared, "true")
if (!write.shared) {
    required.paths <- c(
        "SECTION_DIR", "CACHED_DIR", "CENSUS_MANAGER_CACHE_FILE",
        "CENSUS_MANAGER_SHARED_FILE", "COUNTY_TO_COUNTY_DIR"
    )
    missing.paths <- required.paths[!nzchar(Sys.getenv(required.paths, unset=""))]
    if (length(missing.paths))
        stop("Candidate merge requires explicit paths: ", paste(missing.paths, collapse=", "))
}

#LOAD the saved sections of the surevillance manager
section1 = load.data.manager(name="surveillance.manager_section1", file=file.path(SECTION_DIR, "surveillance.manager_section1.rdata"))
section2 = load.data.manager(name="surveillance.manager_section2", file=file.path(SECTION_DIR, "surveillance.manager_section2.rdata"))
section3 = load.data.manager(name="surveillance.manager_section3", file=file.path(SECTION_DIR, "surveillance.manager_section3.rdata"))
section4 = load.data.manager(name="surveillance.manager_section4", file=file.path(SECTION_DIR, "surveillance.manager_section4.rdata"))
section5 = load.data.manager(name="surveillance.manager_section5", file=file.path(SECTION_DIR, "surveillance.manager_section5.rdata"))

#MERGE
section1$import.data(section2) #This order doesn't matter, do it this way: big.one$importdata(smaller.one)
section1$import.data(section4)
section1$import.data(section3)
section1$import.data(section5)

surveillance.manager = section1

# Aggregate all outcomes --------------------
source('data_processing/hiv.surveillance.manager/hiv.surveillance.processing.for.merging/updated.data.aggregation.jheem.R') # Aggregates outcomes using updated methods for 2026

#Calculates STI Ratio data --------------------
source('data_processing/hiv.surveillance.manager/sti_ratio_calculation.R') 

# Run this code- it uses both HIV data and adult.population --------------------
source('data_processing/hiv.surveillance.manager/tests.per.population.R') # Source code to create hiv.tests.per.population

# Source code to aggregate proportion.msm  -------------------------------------
source('data_processing/hiv.surveillance.manager/aggregating.proportion.msm.new.R')

# Remove outliers --------------------------------------------------------------
dir.create(CACHED_DIR, recursive=TRUE, showWarnings=FALSE)
save(surveillance.manager, file=file.path(CACHED_DIR, "surveillance.manager.before.outliers.rdata"))

source('data_processing/outliers/outlier.remover.total.level.R')
source('data_processing/outliers/outlier.remover.one.way.strata.R')
source('data_processing/outliers/outlier.remover.two.way.strata.R')

#Update for 7-1-26: Oakland TGA adding migration values for modeling -----------
source('data_processing/hiv.surveillance.manager/add.oakland.tga.migration.values.R')
#Update for 7-30-26: Oakland TGA adding HIV and population data-----------
source('data_processing/hiv.surveillance.manager/add.oakland.tga.hiv.data.R')

###Save surveillance manager####
save(surveillance.manager, file=file.path(CACHED_DIR, "surveillance.manager.rdata"))

#Also save to Q drive
if (write.shared)
    save(surveillance.manager, file=file.path(SHARED_MANAGER_DIR, "surveillance.manager.rdata"))

#Archive a version with the date to the Q Drive#
if (write.shared) {
    timestamp <- Sys.Date()
    filename <- file.path(ARCHIVE_DIR, paste0("surveillance.manager_", timestamp, ".rdata"))
    save(surveillance.manager, file=filename)
}
