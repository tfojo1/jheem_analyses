
# The file to source to load all necessary packages, cached data, code

library(locations)
library(distributions)

source('../jheem_analyses/commoncode/cache_manager.R')


# This 'source' call is equivalent to loading the jheem2 package
source('../jheem2/R/tests/source_jheem2_package.R')
source('../jheem_analyses/commoncode/file_paths.R')
set.jheem.root.directory(ROOT.DIR)

# Set up file paths
# Load the data managers
if (is.null(get.default.data.manager())) #if it's in memory, it wont reload it
{
  cat("Loading Surveillance Manager (may take a minute or two)...")
  SURVEILLANCE.MANAGER = load.data.manager.from.cache('surveillance.manager.rdata', set.as.default=T)
  cat("Done!\n")
}

# SURVEILLANCE.MANAGER = load.data.manager('cached/syphilis.manager.rdata',set.as.default = T) #plotting function will use this data manager for outcomes
# SURVEILLANCE.MANAGER$data$population$estimate$census.aggregated.population$census.data$year__location #doesnt have US @ZOE
# SURVEILLANCE.MANAGER$data$population$estimate$census.aggregated.population$stratified.census$year__location__age

if (!exists('CENSUS.MANAGER'))
{
  cat("Loading Census Manager (may take a minute or two)...")
  CENSUS.MANAGER = load.data.manager.from.cache('census.manager.rdata', set.as.default=F)
  cat("Done!\n")
}

if (!exists('national.surveillance'))
  load(file.path(JHEEM.CACHE.DIR, 'national.surveillance.Rdata'))

# SHIELD

source('applications/SHIELD/shield_calib_parameters.R') #
source('applications/SHIELD/shield_base_parameters.R') #
source('applications/SHIELD/shield_ontology_mappings.R') #
source('applications/SHIELD/R/shield_specification_helpers.R') #

# Input Managers
source('applications/SHIELD/R/shield_inputManager_pairing.R') #inputManager to create sexual contact pairings
source('applications/SHIELD/R/shield_inputManager_helpers.R') #

# Common code from JHEEM
source('../jheem_analyses/commoncode/target_populations.R')
source('../jheem_analyses/commoncode/age_mappings.R')
source('../jheem_analyses/commoncode/cache_object_for_version_functions.R')
source('../jheem_analyses/commoncode/logitnorm_helpers.R')

# Data Loads
load(file.path(JHEEM.CACHE.DIR, 'google_mobility_data.Rdata'))
