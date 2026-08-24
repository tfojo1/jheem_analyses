## =============================================================================
## Shield_source_code.R
## -----------------------------------------------------------------------------
##   1. syncs the jheem_analyses and jheem2 repositories
##   2. loads JHEEM2 (installed package OR sourced from local clone)
##   3. sources common JHEEM code and SHIELD-specific code
##   4. loads cached data managers (census, syphilis surveillance)
##   5. defines global model constants
##
## Assumes the working directory is a sibling of ../jheem_analyses and ../jheem2.
## =============================================================================

cat("*** Running Shield_source_code.R ***\n")

## =============================================================================
## 0. CONFIGURATION
## =============================================================================

JHEEM.ANALYSES.PATH <- "../jheem_analyses"
JHEEM2.PATH         <- "../jheem2"
JHEEM2.BRANCH       <- "dev"      # branch required for all SHIELD work

## =============================================================================
## 1. PACKAGES
## =============================================================================
## jheem2 itself is attached later (section 4) - it may not be installed at all
## if USE.JHEEM2.PACKAGE is FALSE.

library(ggplot2)
library(reshape2)
library(locations)
library(distributions)

## =============================================================================
## 2. GIT HELPER
## =============================================================================

## Sync a local clone to origin/<branch>.
##   force = TRUE  -> discards local edits to TRACKED files (untracked left alone)
## Returns invisibly; stops on any unrecoverable git failure.
sync.repo.to.branch <- function(repo.path, branch, force = TRUE)
{
  if (nchar(Sys.which("git")) == 0)
    stop("git executable not found on PATH")
  if (!dir.exists(file.path(repo.path, ".git")))
    stop("Not a git repository: ", repo.path)
  
  repo <- shQuote(normalizePath(repo.path, mustWork = TRUE))
  git  <- function(..., capture = FALSE) {
    args <- c("-C", repo, ...)                              # NULL args drop out
    if (capture) system2("git", args, stdout = TRUE, stderr = TRUE)
    else         system2("git", args)                       # returns exit status
  }
  
  ## refresh remote refs first so origin/<branch> exists for the checkout
  if (git("fetch", "--prune", "origin") != 0L)
    stop("git fetch failed for ", repo.path)
  
  current <- git("rev-parse", "--abbrev-ref", "HEAD", capture = TRUE)[1]
  cat("  currently on '", current, "'\n", sep = "")
  
  if (force) git("reset", "--hard", "HEAD")                   # drop tracked edits
  
  if (!identical(current, branch)) {
    cat("  switching to '", branch, "'\n", sep = "")
    if (git("checkout", if (force) "-f", branch) != 0L)
      stop("could not checkout '", branch, "' in ", repo.path)
  }
  
  ## fast-forward only; fall back to a hard reset if local history diverged
  if (git("pull", "--ff-only", "origin", branch) != 0L) {
    cat("  fast-forward failed - resetting to origin/", branch, "\n", sep = "")
    if (git("reset", "--hard", paste0("origin/", branch)) != 0L)
      stop("could not sync ", repo.path, " to origin/", branch)
  }
  
  cat("  synced to ", branch, " @ ",
      git("rev-parse", "--short", "HEAD", capture = TRUE)[1], "\n", sep = "")
  invisible(TRUE)
}

## =============================================================================
## 3. SYNC REPOSITORIES
## =============================================================================

## --- jheem_analyses: plain pull on whatever branch is checked out ------------
cat("Checking JHEEM_ANALYSES repository status....\n")
if (dir.exists(JHEEM.ANALYSES.PATH)) {
  system2("git", c("-C", shQuote(normalizePath(JHEEM.ANALYSES.PATH)), "pull"))
} else {
  cat("Cannot pull from JHEEM_ANALYSES: ", JHEEM.ANALYSES.PATH, "\n", sep = "")
}

## Defines USE.JHEEM2.PACKAGE. Sourced AFTER the pull so we honor the current
## setting in the repo rather than a stale local copy.
source(file.path(JHEEM.ANALYSES.PATH, "use_jheem2_package_setting.R"))

## =============================================================================
## 4. LOAD JHEEM2
## =============================================================================

if (USE.JHEEM2.PACKAGE) {
  ## --- option 1: installed package ----------------------------------------
  cat("Using JHEEM2 package ...\n")
  update.jheem2.package()          # checks version and reinstalls as needed
  library(jheem2)
  print(check.jheem2.version())
  
} else {
  ## --- option 2: source directly from the local clone ----------------------
  ## devtools::install_github('tfojo1/jheem2', ref = JHEEM2.BRANCH)
  cat("Using JHEEM2 source code ...\n")
  cat("Checking JHEEM2 repository status....\n")
  
  sync.repo.to.branch(JHEEM2.PATH, branch = JHEEM2.BRANCH, force = TRUE)
  source(file.path(JHEEM2.PATH, "R/tests/source_jheem2_package.R"))
}

## =============================================================================
## 5. COMMON JHEEM CODE
## =============================================================================
## cache_manager.R is sourced after JHEEM2 so its definitions can rely on the
## package being available.

source(file.path(JHEEM.ANALYSES.PATH, "commoncode/cache_manager.R"))
clear.all.managers()

source(file.path(JHEEM.ANALYSES.PATH, "commoncode/target_populations.R"))
source(file.path(JHEEM.ANALYSES.PATH, "commoncode/age_mappings.R"))
source(file.path(JHEEM.ANALYSES.PATH, "commoncode/cache_object_for_version_functions.R"))
source(file.path(JHEEM.ANALYSES.PATH, "commoncode/logitnorm_helpers.R"))
source(file.path(JHEEM.ANALYSES.PATH, "commoncode/file_paths.R"))   # defines ROOT.DIR, JHEEM.CACHE.DIR
source(file.path(JHEEM.ANALYSES.PATH, "commoncode/locations_of_interest.R"))

set.jheem.root.directory(ROOT.DIR)

## =============================================================================
## 6. CACHED DATA
## =============================================================================

## --- Google mobility (COVID-era contact adjustment) --------------------------
load(file.path(JHEEM.CACHE.DIR, "google_mobility_data.Rdata"))

## --- Census manager ----------------------------------------------------------
## Large; only needed to generate the initial population. Not set as default.
if (!exists("CENSUS.MANAGER")) {
  cat("Reading census manager ...\n")
  CENSUS.MANAGER <- load.data.manager.from.cache("census.manager.rdata",
                                                 set.as.default = FALSE)
  cat("Census manager read\n")
}

## --- Syphilis surveillance manager -------------------------------------------
## All calibration and plotting data, at county / MSA / national aggregation.
## Set as default so plotting functions pull outcomes from it.
if (!exists("SURVEILLANCE.MANAGER")) {
  cat("Reading syphilis surveillance manager ...\n")
  SURVEILLANCE.MANAGER <- load.data.manager.from.cache("syphilis.manager.rdata",
                                                       set.as.default = TRUE)
  cat("Syphilis surveillance manager read\n")
}

## =============================================================================
## 7. SHIELD-SPECIFIC CODE
## =============================================================================

SHIELD.DIR <- file.path(JHEEM.ANALYSES.PATH, "applications/SHIELD")

for (f in c("shield_calib_parameters.R",
            "shield_base_parameters.R",
            "R/shield_ontology_mappings.R",
            "R/shield_specification_helpers.R",
            "R/shield_inputManager_pairing.R",
            "R/shield_inputManager_helpers.R",
            "R/shield_inputManager_covid.R")) {
  source(file.path(SHIELD.DIR, f))
  cat(basename(f), " sourced\n", sep = "")
}
rm(f)

PAIRING.INPUT.MANAGER <- create.pairing.manager(dir = file.path(SHIELD.DIR,
                                                                "data_files/pairing"))
cat("PAIRING.INPUT.MANAGER created\n")

## =============================================================================
## 8. GLOBAL CONSTANTS
## =============================================================================

## Census age strata, as lower bounds (character), ascending
CENSUS.AGES <- as.character(sort(
  parse.age.strata.names(CENSUS.MANAGER$ontologies$census$age)$lower))
cat("CENSUS.AGES set to ", paste(CENSUS.AGES, collapse = ", "), "\n", sep = "")

## --- Simulation timeline -----------------------------------------------------
## Most demographic data begin in 2007 or 2010. Functional forms are mapped back
## to ~2005 for a smooth transition; values are held constant before the
## functional.form.from.time year.
DEFAULT.START.YEAR            <- 1970   # simulation start
DEFAULT.FIX.STRATA.YEAR       <- 2010   # full population breakdown available post-2010
# (also used for proportion-MSM estimation)
DEFAULT.POPULATION.YEARS      <- 2010   # initial population + sexual contact O/E by race
DEFAULT.AGING.START.YEAR      <- 2005
DEFAULT.MIGRATION.START.YEAR  <- 2005
DEFAULT.MORTALITY.RATE.YEARS  <- c("2001-2010", "2011-2020")

## --- Fertility ---------------------------------------------------------------
DEFAULT.FERTILITY.START.YEARS <- 2005
DEFAULT.FERTILITY.RATE.YEARS  <- 2007:2023
FERTILE.AGES     <- c("15-19 years", "20-24 years", "25-29 years",
                      "30-34 years", "35-39 years", "40-44 years")
NON.FERTILE.AGES <- c("0-14 years", "45-49 years", "50-54 years",
                      "55-64 years", "65+ years")
SEXUAL.ACTIVITY.AGES <- c("15-19 years", "20-24 years", "25-29 years",
                          "30-34 years", "35-39 years", "40-44 years",
                          "45-49 years", "50-54 years", "55-64 years")

## --- Intervention / testing start years --------------------------------------
## Projections are held at these years' values for all earlier years.
DEFAULT.STI.SCREENING.START.YEAR <- 1980
DEFAULT.PRENATAL.CARE.START.YEAR <- 1980
DEFAULT.TRANSMISSION.START.YEAR  <- 1980
DEFAULT.HIV.TESTING.START.YEAR   <- 2010

cat("Global variables are defined\n")
cat("*** Shield_source_code.R completed! ***\n")