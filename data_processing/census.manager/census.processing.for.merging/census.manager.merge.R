#Use this code to merge the different saved sections of the census manager into a complete version

# PROCESS -----------------------------------------------------------------
# source('data_processing/census.manager/census.processing.for.merging/section.1.processing.R')
# source('data_processing/census.manager/census.processing.for.merging/section.2.processing.R')
# source('data_processing/census.manager/census.processing.for.merging/section.3.processing.R')

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
    required.paths <- c("SECTION_DIR", "CACHED_DIR")
    missing.paths <- required.paths[!nzchar(Sys.getenv(required.paths, unset=""))]
    if (length(missing.paths))
        stop("Candidate merge requires explicit paths: ", paste(missing.paths, collapse=", "))
}

#LOAD the saved sections of the census manager
#(The merged manager is section 1, so it carries section 1's name.)
section1 = load.data.manager(name="census.manager", file=file.path(SECTION_DIR, "census.manager_section1.rdata"))
section2 = load.data.manager(name="census.manager_section2", file=file.path(SECTION_DIR, "census.manager_section2.rdata"))
section3 = load.data.manager(name="census.manager_section3", file=file.path(SECTION_DIR, "census.manager_section3.rdata"))

#MERGE
section1$import.data(section2) #This order doesn't matter, do it this way: big.one$importdata(smaller.one)
section1$import.data(section3)

census.manager = section1

#SAVE Final, Complete Census Manager to Cached
dir.create(CACHED_DIR, recursive = TRUE, showWarnings = FALSE)
save(census.manager, file=file.path(CACHED_DIR, "census.manager.rdata"))

if (write.shared) {
    #SAVE Final, Complete Census Manager to Q Drive
    save(census.manager, file=file.path(SHARED_MANAGER_DIR, "census.manager.rdata"))

    #SAVE Final, Complete Census Manager, Archive a dated version to the Q Drive#
    timestamp <- Sys.Date()
    filename <- file.path(ARCHIVE_DIR, paste0("census.manager_", timestamp, ".rdata"))
    save(census.manager, file=filename)
}
