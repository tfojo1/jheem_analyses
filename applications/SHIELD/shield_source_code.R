## =============================================================================
## Shield_source_code.R
## -----------------------------------------------------------------------------
##   1. checks jheem2 is on the dev branch; pulls both repos (interactive only)
##   2. loads JHEEM2 (installed package OR sourced from local clone)
##   3. sources common JHEEM code and SHIELD-specific code
##   4. loads cached data managers (census, syphilis surveillance)
##   5. defines global model constants
##
## Assumes the working directory is a sibling of ../jheem_analyses and ../jheem2.
## =============================================================================

cat("*** Running Shield_source_code.R ***\n")

# WHICH SURVEILLANCE MANAGER SHOULD WE USE?
# Any of the dated syphilis managers from https://github.com/tfojo1/jheem_analyses/releases
# NULL = whichever manager is promoted now. To pin an older one, put its tag
# here instead (the commented line is an example).
# SYPHILIS.MANAGER.RELEASE.TAG <- NULL
SYPHILIS.MANAGER.RELEASE.TAG <- "syphilis-manager-v2026.07.27"

if (!is.null(SYPHILIS.MANAGER.RELEASE.TAG)) { print(paste("!!! 1-Using a potentially old Surveillance Manager :",SYPHILIS.MANAGER.RELEASE.TAG))
  }else{print("1-Using the most up to date Surveillance manager")}


# SHOULD WE PULL GIT UPDATES?
# Only in an interactive session. `git pull` writes to the index, the refs and
# the working tree, so parallel chains pulling one clone collide ("Unable to
# create index.lock"); a mid-batch pull would also split a batch across two
# jheem2 commits. Chains verify the branch and log the SHA instead.
# Launching straight from the terminal? Sync once by hand first:
#   git -C ../jheem2 pull --ff-only origin dev
PULL.GIT.UPDATES <- interactive()
if (PULL.GIT.UPDATES) { print("2-Pulling git updates")
}else{print("!!!2-Skipping git pulls (branch check still enforced)")}


## =============================================================================
## 0. CONFIGURATION
## =============================================================================

## Set by the entry point, which resolves it from its own file path. The
## literal below is the fallback for callers that have not been converted yet;
## it only resolves when the working directory is the repo root AND the
## checkout is named "jheem_analyses".
if (!exists("JHEEM.ANALYSES.PATH")) JHEEM.ANALYSES.PATH <- "../jheem_analyses"

## jheem2 is a sibling of the analyses repo. Derive it rather than hardcoding
## "../jheem2": this path feeds require.repo.branch(), which checks the branch
## and may pull, so it must point at the sibling of the clone we are actually
## running from. Identical to "../jheem2" in the fallback case.
JHEEM2.PATH         <- file.path(dirname(JHEEM.ANALYSES.PATH), "jheem2")
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

## require.repo.branch ----
## Check that a local clone is on the required branch, then optionally pull.
##
## The branch check is read-only (`git rev-parse`): no network, no git locks,
## so it is safe to run from many parallel processes at once. The pull is NOT
## parallel-safe - gate it with PULL.GIT.UPDATES.
##
## Nothing here ever discards local work: no reset, no checkout, no -f. On the
## wrong branch we stop and tell you how to fix it by hand.
require.repo.branch <- function(repo.path, branch, pull = TRUE)
{
  if (nchar(Sys.which("git")) == 0)
    stop("Git executable not found on PATH")
  if (!dir.exists(file.path(repo.path, ".git")))
    stop("Not a git repository: ", repo.path)

  repo <- normalizePath(repo.path, mustWork = TRUE)
  git  <- function(..., capture = FALSE) {
    args <- c("-C", shQuote(repo), ...)
    if (capture) suppressWarnings(system2("git", args, stdout = TRUE, stderr = TRUE))
    else         system2("git", args)                       # returns exit status
  }

  ## --- 1. branch check: read-only, parallel-safe ----------------------------
  current <- git("rev-parse", "--abbrev-ref", "HEAD", capture = TRUE)[1]

  if (identical(current, "HEAD"))
    stop("\n", repo, " is in a DETACHED HEAD state.\n",
         "SHIELD requires branch '", branch, "'. Nothing was changed.\n",
         "Fix it by hand, then re-run:\n",
         "    cd ", repo, "\n",
         "    git checkout ", branch, "\n")

  if (!identical(current, branch))
    stop("\n", repo, " is on branch '", current, "'.\n",
         "SHIELD requires branch '", branch, "'.\n",
         "Nothing was changed - any uncommitted work on '", current, "' is untouched.\n",
         "Check that your work is safe, then re-run:\n",
         "    cd ", repo, "\n",
         "    git status\n",
         "    git checkout ", branch, "\n")

  cat("  on '", current, "' @ ",
      git("rev-parse", "--short", "HEAD", capture = TRUE)[1], "\n", sep = "")

  ## --- 2. pull: only when asked ---------------------------------------------
  if (pull) {
    cat("  pulling origin/", branch, " ...\n", sep = "")
    if (git("pull", "--ff-only", "origin", branch) != 0L)
      stop("\n'git pull --ff-only' failed in ", repo, " (branch '", branch, "').\n",
           "This usually means local commits have diverged from origin/", branch,
           ", or that uncommitted changes would be overwritten.\n",
           "Nothing was changed. Resolve it by hand, then re-run.\n")
    cat("  now at ", git("rev-parse", "--short", "HEAD", capture = TRUE)[1], "\n", sep = "")
  } else {
    cat("  pull skipped\n")
  }

  invisible(TRUE)
}

## =============================================================================
## 3. SYNC REPOSITORIES
## =============================================================================

## --- jheem_analyses: plain pull on whatever branch is checked out ------------
cat("3-Checking JHEEM_ANALYSES repository status....\n")
if (dir.exists(JHEEM.ANALYSES.PATH)) {
  if (PULL.GIT.UPDATES)
    system2("git", c("-C", shQuote(normalizePath(JHEEM.ANALYSES.PATH)), "pull"))
  else
    cat("  pull skipped\n")
} else {
  cat("Cannot pull from JHEEM_ANALYSES: ", JHEEM.ANALYSES.PATH, "\n", sep = "")
}

## Defines USE.JHEEM2.PACKAGE. Sourced AFTER the pull so we honor the current
## setting in the repo rather than a stale local copy.
source(file.path(JHEEM.ANALYSES.PATH, "use_jheem2_package_setting.R"))

## =============================================================================
## 4. LOAD JHEEM2
## =============================================================================
cat("4-Checking JHEEM2 repository status....\n")
if (USE.JHEEM2.PACKAGE) {
  ## --- option 1: installed package ----------------------------------------
  cat("--Using JHEEM2 package: \n")
  update.jheem2.package()          # checks version and reinstalls as needed
  library(jheem2)
  print(check.jheem2.version())
  
} else {
  ## --- option 2: source directly from the local clone ----------------------
  ## devtools::install_github('tfojo1/jheem2', ref = JHEEM2.BRANCH)
  cat("--Using JHEEM2 source code: \n")

  require.repo.branch(JHEEM2.PATH, branch = JHEEM2.BRANCH, pull = PULL.GIT.UPDATES)
  source(file.path(JHEEM2.PATH, "R/tests/source_jheem2_package.R"))
}

## =============================================================================
## 5. COMMON JHEEM CODE
## =============================================================================
## cache_manager.R is sourced after JHEEM2 so its definitions can rely on the
## package being available.
cat("5-Sourcing Commoncodes...\n")
source(file.path(JHEEM.ANALYSES.PATH, "commoncode/cache_manager.R"))
clear.all.managers()

source(file.path(JHEEM.ANALYSES.PATH, "commoncode/target_populations.R"))
source(file.path(JHEEM.ANALYSES.PATH, "commoncode/age_mappings.R"))
source(file.path(JHEEM.ANALYSES.PATH, "commoncode/cache_object_for_version_functions.R"))
source(file.path(JHEEM.ANALYSES.PATH, "commoncode/logitnorm_helpers.R"))
source(file.path(JHEEM.ANALYSES.PATH, "commoncode/file_paths.R"))   # defines ROOT.DIR, JHEEM.CACHE.DIR
source(file.path(JHEEM.ANALYSES.PATH, "commoncode/locations_of_interest.R"))

set.jheem.root.directory(ROOT.DIR)
cat(paste0("The root director is set to ",ROOT.DIR))
## =============================================================================
## 6. CACHED DATA
## =============================================================================

## --- Google mobility (COVID-era contact adjustment) --------------------------
load(file.path(JHEEM.CACHE.DIR, "google_mobility_data.Rdata"))

## --- Census manager ----------------------------------------------------------
## Large; only needed to generate the initial population. Not set as default.
if (!exists("CENSUS.MANAGER")) {
  cat("6-Reading census manager ...\n")
  CENSUS.MANAGER <- load.data.manager.from.cache("census.manager.rdata",
                                                 set.as.default = FALSE)
  cat("Census manager read\n")
}

## --- Syphilis surveillance manager -------------------------------------------
## All calibration and plotting data, at county / MSA / national aggregation.
## Set as default so plotting functions pull outcomes from it.
if (!exists("SURVEILLANCE.MANAGER")) {
  cat("7-Reading syphilis surveillance manager ...\n")
  SURVEILLANCE.MANAGER <- load.data.manager.from.cache("syphilis.manager.rdata",
                                                       set.as.default = TRUE,
                                                       release.tag = SYPHILIS.MANAGER.RELEASE.TAG)
  cat("Syphilis surveillance manager read\n")
} else if (!is.null(SYPHILIS.MANAGER.RELEASE.TAG)) {
  warning("SYPHILIS.MANAGER.RELEASE.TAG was ignored because SURVEILLANCE.MANAGER was already loaded")
}


## =============================================================================
## 7. SHIELD-SPECIFIC CODE
## =============================================================================
cat("8-Sourcig SHIELD helpers...\n")
if (!exists("SHIELD.DIR")) SHIELD.DIR <- file.path(JHEEM.ANALYSES.PATH, "applications/SHIELD")

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
DEFAULT.SEXUAL.DEBUT.AGE <- 13

cat("Global variables are defined\n")
cat("*** Shield_source_code.R completed! ***\n")
