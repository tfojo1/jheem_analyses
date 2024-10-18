cat("*** Running Shiled_source_code.R ***\n")
NEW.SOURCE=F
 
# The file to source to load all necessary packages, cached data, code
library(reshape2)
library(locations)
library(distributions)
# This 'source' call is equivalent to loading the jheem2 package
source('../jheem2/R/tests/source_jheem2_package.R')

# Common code from JHEEM ----
source('../jheem_analyses/commoncode/cache_manager.R')
source('../jheem_analyses/commoncode/target_populations.R')
source('../jheem_analyses/commoncode/age_mappings.R')
source('../jheem_analyses/commoncode/cache_object_for_version_functions.R')
source('../jheem_analyses/commoncode/logitnorm_helpers.R')
source('../jheem_analyses/commoncode/file_paths.R')

# Set the root directory ----
set.jheem.root.directory(ROOT.DIR)

# Google Mobility Data ----
load(file.path(JHEEM.CACHE.DIR, 'google_mobility_data.Rdata'))

# CENSUS.MANAGE ----
# it's a big file with a lot of information that is only needed for generating the initial population
if (!exists('CENSUS.MANAGER') | NEW.SOURCE){
  # cat("Loading Census Manager (may take a minute or two)...")
  CENSUS.MANAGER = load.data.manager.from.cache('census.manager.rdata', set.as.default=F)
  print("Census manager read")
}


# Syphilis SURVEILLANCE.MANAGER ----
# includes all the data used for calibration and plotting
# county-, MSA- and US- level aggregation
if (is.null(get.default.data.manager()) | NEW.SOURCE){ #if it's in memory, it wont reload it
  SURVEILLANCE.MANAGER = load.data.manager('cached/syphilis.manager.rdata',set.as.default = T) #plotting function will use this data manager for outcomes
  print("Syphilis survillance manager read")
}

# JHEEM survillance manager:
# Load the data managers
# if (is.null(get.default.data.manager())){ #if it's in memory, it wont reload it
#   cat("Loading Surveillance Manager (may take a minute or two)...")
#   SURVEILLANCE.MANAGER = load.data.manager.from.cache('surveillance.manager.rdata', set.as.default=T) #plotting function will use this data manager for outcomes
#   cat("Done")
# }


# SHIELD specific code ----
source('applications/SHIELD/shield_calib_parameters.R') ; print("shield_calib_parameters.R sourced")
source('applications/SHIELD/shield_base_parameters.R') ; print("shield_base_parameters.R sourced")
source('applications/SHIELD/shield_ontology_mappings.R') ; print("shield_ontology_mappings.R sourced")
source('applications/SHIELD/R/shield_specification_helpers.R'); print("shield_specification_helpers.R sourced")
# Input Managers
source('applications/SHIELD/R/shield_inputManager_pairing.R') ; print("shield_inputManager_pairing.R sourced")
source('applications/SHIELD/R/shield_inputManager_helpers.R') ; print("shield_inputManager_helpers.R sourced")

##-- CREATE and STORE --## ----
PAIRING.INPUT.MANAGER = create.pairing.manager(dir=paste0("applications/SHIELD/data_files/pairing"))
print("PAIRING.INPUT.MANAGER created")

# Global variables used in the code:
CENSUS.AGES = as.character(sort( parse.age.strata.names(CENSUS.MANAGER$ontologies$census$age)$lower ))
cat("CENSUS.AGES set to ",CENSUS.AGES,"\n")

# DEFAULT.POPULATION.YEARS = 2007
# cat("DEFAULT.POPULATION.YEARS set to ",DEFAULT.POPULATION.YEARS,"\n")

FERTILE.AGES=c('15-19 years','20-24 years','25-29 years','30-34 years','35-39 years','40-44 years')
cat("FERTILE.AGES set to ",FERTILE.AGES,"\n")


cat("*** Shiled_source_code.R completed! ***\n")
