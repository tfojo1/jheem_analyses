## =============================================================================
## Shield_source_code.R
## -----------------------------------------------------------------------------
##   1. resolves explicit source, cache, and state paths
##   2. loads JHEEM2 (installed package OR sourced from a local checkout)
##   3. sources common JHEEM code and SHIELD-specific code
##   4. loads cached data managers (census, syphilis surveillance)
##   5. defines global model constants
##
## This startup path is read-only with respect to source control and package
## libraries. Source revisions and dependencies must be prepared before launch.
## =============================================================================

cat("*** Running Shield_source_code.R ***\n")

## =============================================================================
## 0. CONFIGURATION
## =============================================================================

configured.analyses.path <- trimws(Sys.getenv("JHEEM_ANALYSES_PATH"))
if (!nzchar(configured.analyses.path)) configured.analyses.path <- "../jheem_analyses"
source(file.path(configured.analyses.path, "applications/SHIELD/R/shield_runtime.R"))

SHIELD.RUNTIME.CONFIG <- resolve.shield.runtime.config()
JHEEM.ANALYSES.PATH <- SHIELD.RUNTIME.CONFIG$analyses_path
JHEEM2.PATH         <- SHIELD.RUNTIME.CONFIG$jheem2_path
ROOT.DIR            <- SHIELD.RUNTIME.CONFIG$root_dir
JHEEM.CACHE.DIR     <- SHIELD.RUNTIME.CONFIG$cache_dir
SYPHILIS.MANAGER.RELEASE.TAG <- SHIELD.RUNTIME.CONFIG$syphilis_manager_tag
rm(configured.analyses.path)

## Development runs may use the existing local manager when the tag is NULL.
## Recorded runs set SHIELD_REQUIRE_IMMUTABLE_INPUTS=true, which makes an exact
## JHEEM_SYPHILIS_MANAGER_TAG mandatory during configuration.

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
## 2. LOAD JHEEM2
## =============================================================================

USE.JHEEM2.PACKAGE <- identical(SHIELD.RUNTIME.CONFIG$jheem2_mode, "package")
if (USE.JHEEM2.PACKAGE) {
  ## --- option 1: installed package ----------------------------------------
  cat("Using JHEEM2 package ...\n")
  library(jheem2)
  cat("jheem2 package version: ", as.character(packageVersion("jheem2")), "\n", sep = "")
  
} else {
  ## --- option 2: source directly from the local clone ----------------------
  cat("Using JHEEM2 source code ...\n")
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("The pkgload package is required when JHEEM2_MODE=source")
  }
  pkgload::load_all(JHEEM2.PATH, export_all = TRUE, helpers = FALSE, quiet = TRUE)
}

## =============================================================================
## 3. COMMON JHEEM CODE
## =============================================================================
## cache_manager.R is sourced after JHEEM2 so its definitions can rely on the
## package being available.

source(file.path(JHEEM.ANALYSES.PATH, "commoncode/cache_manager.R"))
source(file.path(JHEEM.ANALYSES.PATH, "commoncode/run_provenance.R"))
clear.all.managers()

source(file.path(JHEEM.ANALYSES.PATH, "commoncode/target_populations.R"))
source(file.path(JHEEM.ANALYSES.PATH, "commoncode/age_mappings.R"))
source(file.path(JHEEM.ANALYSES.PATH, "commoncode/cache_object_for_version_functions.R"))
source(file.path(JHEEM.ANALYSES.PATH, "commoncode/logitnorm_helpers.R"))
source(file.path(JHEEM.ANALYSES.PATH, "commoncode/locations_of_interest.R"))

set.jheem.root.directory(ROOT.DIR)

## =============================================================================
## 4. CACHED DATA
## =============================================================================

## --- Google mobility (COVID-era contact adjustment) --------------------------
load(file.path(JHEEM.CACHE.DIR, "google_mobility_data.Rdata"))

## --- Census manager ----------------------------------------------------------
## Large; only needed to generate the initial population. Not set as default.
if (!exists("CENSUS.MANAGER")) {
  cat("Reading census manager ...\n")
  CENSUS.MANAGER <- load.data.manager.from.cache("census.manager.rdata",
                                                 set.as.default = FALSE,
                                                 offline = SHIELD.RUNTIME.CONFIG$input_offline)
  cat("Census manager read\n")
}

## --- Syphilis surveillance manager -------------------------------------------
## All calibration and plotting data, at county / MSA / national aggregation.
## Set as default so plotting functions pull outcomes from it.
if (!exists("SURVEILLANCE.MANAGER")) {
  cat("Reading syphilis surveillance manager ...\n")
  SURVEILLANCE.MANAGER <- load.data.manager.from.cache("syphilis.manager.rdata",
                                                       set.as.default = TRUE,
                                                       offline = SHIELD.RUNTIME.CONFIG$input_offline,
                                                       release.tag = SYPHILIS.MANAGER.RELEASE.TAG)
  cat("Syphilis surveillance manager read\n")
} else if (!is.null(SYPHILIS.MANAGER.RELEASE.TAG)) {
  warning("SYPHILIS.MANAGER.RELEASE.TAG was ignored because SURVEILLANCE.MANAGER was already loaded")
}

## =============================================================================
## 5. SHIELD-SPECIFIC CODE
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
## 6. GLOBAL CONSTANTS
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
