# Small path/side-effect contract for the census section-to-final merge.
# Run from the jheem_analyses repository root with Rscript.
# Loading and saving are intercepted here; this does not validate the manager's data.

merge.script <- "data_processing/census.manager/census.processing.for.merging/census.manager.merge.R"

run.path.case <- function(write.shared = NULL) {
  root <- tempfile("census-merge-paths-")
  section.dir <- file.path(root, "sections")
  cached.dir <- file.path(root, "candidate")
  shared.dir <- file.path(root, "shared")
  archive.dir <- file.path(shared.dir, "Archive")
  if (is.null(write.shared)) {
    # Without opt-in overrides the original Q: reads and shared writes are kept.
    # Only redirect the local cache so the test creates no repository files.
    Sys.unsetenv(c("Q_ROOT", "SECTION_DIR", "SHARED_MANAGER_DIR", "ARCHIVE_DIR",
                   "WRITE_SHARED_OUTPUT"))
    Sys.setenv(CACHED_DIR = cached.dir)
    section.dir <- file.path("Q:", "data_managers/data.manager.merge")
    shared.dir <- file.path("Q:", "data_managers")
    archive.dir <- file.path(shared.dir, "Archive")
    write.shared <- TRUE
  } else {
    Sys.setenv(Q_ROOT = root, SECTION_DIR = section.dir, CACHED_DIR = cached.dir,
               SHARED_MANAGER_DIR = shared.dir, ARCHIVE_DIR = archive.dir,
               WRITE_SHARED_OUTPUT = tolower(as.character(write.shared)))
  }

  reads <- character()
  names <- character()
  imports <- character()
  writes <- character()
  scope <- new.env(parent = globalenv())
  scope$load.data.manager <- function(name, file) {
    reads <<- c(reads, file)
    names <<- c(names, name)
    list(import.data = function(other) { imports <<- c(imports, "import"); invisible(NULL) })
  }
  scope$save <- function(..., file) {
    writes <<- c(writes, file)
    invisible(NULL)
  }
  sys.source(merge.script, envir = scope)

  expected.writes <- file.path(cached.dir, "census.manager.rdata")
  if (write.shared) {
    expected.writes <- c(expected.writes,
                         file.path(shared.dir, "census.manager.rdata"),
                         file.path(archive.dir, paste0("census.manager_", Sys.Date(), ".rdata")))
  }
  stopifnot(
    identical(reads, file.path(section.dir, paste0("census.manager_section", 1:3, ".rdata"))),
    identical(names[[1]], "census.manager"),
    length(imports) == 2L,
    identical(writes, expected.writes),
    dir.exists(cached.dir)
  )
}

run.path.case(FALSE)
run.path.case(TRUE)
run.path.case()

Sys.setenv(WRITE_SHARED_OUTPUT = "false")
Sys.unsetenv("SECTION_DIR")
missing.input <- tryCatch(
  sys.source(merge.script, envir = new.env(parent = globalenv())),
  error = function(error) conditionMessage(error)
)
stopifnot(grepl("SECTION_DIR", missing.input, fixed = TRUE))

Sys.setenv(WRITE_SHARED_OUTPUT = "maybe")
invalid.flag <- tryCatch(
  sys.source(merge.script, envir = new.env(parent = globalenv())),
  error = function(error) conditionMessage(error)
)
stopifnot(identical(invalid.flag, "WRITE_SHARED_OUTPUT must be 'true' or 'false'"))

message("Census merge input/output path contract passed")
