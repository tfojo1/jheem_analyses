# Initialize the SHIELD runtime from already-resolved inputs.
#
# This file deliberately does not pull repositories, install packages, download
# data managers, or infer a storage root. Callers prepare those inputs first and
# pass them here. Keeping initialization separate from materialization lets CI
# exercise the same model-loading path without mutating a checkout or production
# cache.

initialize.shield.runtime <- function(census.manager,
                                      surveillance.manager,
                                      root.dir,
                                      analyses.path = ".",
                                      envir = .GlobalEnv)
{
  error.prefix <- "Cannot initialize SHIELD runtime: "

  if (!is.environment(envir))
    stop(paste0(error.prefix, "'envir' must be an environment"))
  if (!is.character(analyses.path) || length(analyses.path) != 1 ||
      is.na(analyses.path) || !dir.exists(analyses.path))
    stop(paste0(error.prefix, "'analyses.path' must identify the jheem_analyses directory"))
  if (!is.character(root.dir) || length(root.dir) != 1 || is.na(root.dir))
    stop(paste0(error.prefix, "'root.dir' must be a single non-NA path"))

  analyses.path <- normalizePath(analyses.path, mustWork = TRUE)
  shield.dir <- file.path(analyses.path, "applications", "SHIELD")
  if (!dir.exists(shield.dir))
    stop(paste0(error.prefix, "SHIELD was not found below 'analyses.path'"))

  required.packages <- c("jheem2", "ggplot2", "reshape2", "locations", "distributions")
  missing.packages <- required.packages[
    !vapply(required.packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing.packages) > 0)
    stop(paste0(error.prefix, "missing packages: ", paste(missing.packages, collapse = ", ")))
  for (package in required.packages)
    suppressPackageStartupMessages(
      library(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
    )

  load.manager <- function(manager, argument.name) {
    if (is.character(manager) && length(manager) == 1 && !is.na(manager)) {
      if (!file.exists(manager))
        stop(paste0(error.prefix, "'", argument.name, "' does not exist: ", manager))
      manager <- load.data.manager(manager, set.as.default = FALSE)
    }
    if (!R6::is.R6(manager) || !is(manager, "jheem.data.manager"))
      stop(paste0(error.prefix, "'", argument.name,
                  "' must be a data-manager object or path to one"))
    manager
  }

  census.manager <- load.manager(census.manager, "census.manager")
  surveillance.manager <- load.manager(surveillance.manager, "surveillance.manager")

  old.working.directory <- getwd()
  on.exit(setwd(old.working.directory), add = TRUE)
  setwd(analyses.path)

  clear.all.managers()
  for (file in c("target_populations.R",
                 "age_mappings.R",
                 "cache_object_for_version_functions.R",
                 "logitnorm_helpers.R",
                 "locations_of_interest.R"))
    sys.source(file.path(analyses.path, "commoncode", file), envir = envir)

  assign("JHEEM.ANALYSES.PATH", analyses.path, envir = envir)
  assign("SHIELD.DIR", shield.dir, envir = envir)
  assign("ROOT.DIR", root.dir, envir = envir)
  assign("CENSUS.MANAGER", census.manager, envir = envir)
  assign("SURVEILLANCE.MANAGER", surveillance.manager, envir = envir)
  set.default.data.manager(surveillance.manager)
  set.jheem.root.directory(root.dir)

  for (file in c("shield_calib_parameters.R",
                 "shield_base_parameters.R",
                 "R/shield_ontology_mappings.R",
                 "R/shield_specification_helpers.R",
                 "R/shield_inputManager_pairing.R",
                 "R/shield_inputManager_helpers.R",
                 "R/shield_inputManager_covid.R"))
    sys.source(file.path(shield.dir, file), envir = envir)

  pairing.input.manager <- get("create.pairing.manager", envir = envir)(
    dir = file.path(shield.dir, "data_files", "pairing")
  )
  assign("PAIRING.INPUT.MANAGER", pairing.input.manager, envir = envir)

  census.ages <- as.character(sort(
    parse.age.strata.names(census.manager$ontologies$census$age)$lower
  ))
  constants <- list(
    CENSUS.AGES = census.ages,
    DEFAULT.START.YEAR = 1970,
    DEFAULT.FIX.STRATA.YEAR = 2010,
    DEFAULT.POPULATION.YEARS = 2010,
    DEFAULT.AGING.START.YEAR = 2005,
    DEFAULT.MIGRATION.START.YEAR = 2005,
    DEFAULT.MORTALITY.RATE.YEARS = c("2001-2010", "2011-2020"),
    DEFAULT.FERTILITY.START.YEARS = 2005,
    DEFAULT.FERTILITY.RATE.YEARS = 2007:2023,
    FERTILE.AGES = c("15-19 years", "20-24 years", "25-29 years",
                     "30-34 years", "35-39 years", "40-44 years"),
    NON.FERTILE.AGES = c("0-14 years", "45-49 years", "50-54 years",
                         "55-64 years", "65+ years"),
    SEXUAL.ACTIVITY.AGES = c("15-19 years", "20-24 years", "25-29 years",
                             "30-34 years", "35-39 years", "40-44 years",
                             "45-49 years", "50-54 years", "55-64 years"),
    DEFAULT.STI.SCREENING.START.YEAR = 1980,
    DEFAULT.PRENATAL.CARE.START.YEAR = 1980,
    DEFAULT.TRANSMISSION.START.YEAR = 1980,
    DEFAULT.HIV.TESTING.START.YEAR = 2010
  )
  list2env(constants, envir = envir)
  assign("SHIELD.RUNTIME.INITIALIZED", TRUE, envir = envir)

  invisible(list(
    census.manager = census.manager,
    surveillance.manager = surveillance.manager,
    root.dir = root.dir,
    analyses.path = analyses.path
  ))
}
