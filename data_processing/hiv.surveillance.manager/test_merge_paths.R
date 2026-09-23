# Small path/side-effect contract for the HIV section-to-final merge.
# Run from the jheem_analyses repository root with Rscript.
# Sourced processors are intentionally replaced here; this does not validate
# the manager's data or scientific transformations.

merge.script <- paste0(
  "data_processing/hiv.surveillance.manager/",
  "hiv.surveillance.processing.for.merging/",
  "hiv.surveillance.manager.merge.R"
)

run.path.case <- function(write.shared = NULL) {
  root <- tempfile("hiv-merge-paths-")
  section.dir <- file.path(root, "sections")
  cached.dir <- file.path(root, "candidate")
  shared.dir <- file.path(root, "shared")
  archive.dir <- file.path(shared.dir, "Archive")
  if (is.null(write.shared)) {
    # Preserve the original Q: reads and shared writes when no opt-in overrides
    # are present. Only redirect the local cache so the test creates no repo files.
    Sys.unsetenv(c(
      "Q_ROOT", "SECTION_DIR", "SHARED_MANAGER_DIR", "ARCHIVE_DIR",
      "WRITE_SHARED_OUTPUT"
    ))
    Sys.setenv(CACHED_DIR = cached.dir)
    section.dir <- file.path("Q:", "data_managers/data.manager.merge")
    shared.dir <- file.path("Q:", "data_managers")
    archive.dir <- file.path(shared.dir, "Archive")
    write.shared <- TRUE
  } else {
    Sys.setenv(
      Q_ROOT = root,
      SECTION_DIR = section.dir,
      CACHED_DIR = cached.dir,
      SHARED_MANAGER_DIR = shared.dir,
      ARCHIVE_DIR = archive.dir,
      CENSUS_MANAGER_CACHE_FILE = file.path(root, "census-cache.rdata"),
      CENSUS_MANAGER_SHARED_FILE = file.path(root, "census-shared.rdata"),
      COUNTY_TO_COUNTY_DIR = file.path(root, "county.to.county"),
      WRITE_SHARED_OUTPUT = tolower(as.character(write.shared))
    )
  }

  reads <- character()
  writes <- character()
  sourced <- character()
  scope <- new.env(parent = globalenv())
  scope$load.data.manager <- function(name, file) {
    reads <<- c(reads, file)
    list(import.data = function(other) invisible(NULL))
  }
  scope$source <- function(file, ...) {
    sourced <<- c(sourced, file)
    invisible(NULL)
  }
  scope$save <- function(..., file) {
    writes <<- c(writes, file)
    invisible(NULL)
  }

  sys.source(merge.script, envir = scope)

  expected.reads <- file.path(
    section.dir,
    paste0("surveillance.manager_section", 1:5, ".rdata")
  )
  expected.writes <- file.path(
    cached.dir,
    c("surveillance.manager.before.outliers.rdata", "surveillance.manager.rdata")
  )
  if (write.shared) {
    expected.writes <- c(
      expected.writes,
      file.path(shared.dir, "surveillance.manager.rdata"),
      file.path(archive.dir, paste0("surveillance.manager_", Sys.Date(), ".rdata"))
    )
  }
  stopifnot(
    identical(reads, expected.reads),
    identical(writes, expected.writes),
    length(sourced) == 9L,
    dir.exists(cached.dir)
  )
}

run.path.case(FALSE)
run.path.case(TRUE)
run.path.case()

Sys.setenv(WRITE_SHARED_OUTPUT = "false")
Sys.unsetenv("COUNTY_TO_COUNTY_DIR")
missing.input <- tryCatch(
  sys.source(merge.script, envir = new.env(parent = globalenv())),
  error = function(error) conditionMessage(error)
)
stopifnot(grepl("COUNTY_TO_COUNTY_DIR", missing.input, fixed = TRUE))

Sys.setenv(WRITE_SHARED_OUTPUT = "maybe")
invalid.flag <- tryCatch(
  sys.source(merge.script, envir = new.env(parent = globalenv())),
  error = function(error) conditionMessage(error)
)
stopifnot(identical(invalid.flag, "WRITE_SHARED_OUTPUT must be 'true' or 'false'"))

message("HIV merge input/output path contract passed")
