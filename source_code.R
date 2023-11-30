
# The file to source to load all necessary packages, cached data, code

library(locations)
library(distributions)

# Load the data manager
if (is.null(get.default.data.manager()))
  SURVEILLANCE.MANAGER = load.data.manager('../jheem_analyses/cached/surveillance.manager.rdata', set.as.default = T)

# Load the data - this will eventually be replaced with jheem2's native data manager
if (!exists('ALL.DATA.MANAGERS'))
  load('../jheem_analyses/cached/ALL.DATA.MANAGERS.Rdata')
if (!exists('DEFAULT.LOCALE.MAPPING'))
  load('../jheem_analyses/cached/DEFAULT.LOCALE.MAPPING.Rdata')
if (!exists('msa.surveillance'))
  load('../jheem_analyses/cached/msa.surveillance.Rdata')

# These supporting files are hold-overs from the previous version, and will eventually need to be replaced
source('../jheem_analyses/data_managers/census_manager.R')
source('../jheem_analyses/data_managers/prep_manager_2.R')
source('../jheem_analyses/data_managers/continuum_manager_3.R')
source('../jheem_analyses/data_managers/idu_manager.R')
source('../jheem_analyses/data_managers/locale_mappings.R')
source('../jheem_analyses/data_managers/hiv_surveillance_manager.R')
source('../jheem_analyses/data_managers/mortality_manager.R')
source('../jheem_analyses/data_managers/natality_manager.R')
source('../jheem_analyses/data_managers/pairing_manager.R')

# These are the real supporting files that will be in the final version

# Input Managers
source('../jheem_analyses/input_managers/covid_mobility_manager.R')

# For the EHE version
source('../jheem_analyses/applications/EHE/ehe_base_parameters.R')
source('../jheem_analyses/applications/EHE/ehe_specification_helpers.R')
source('../jheem_analyses/applications/EHE/ehe_ontology_mappings.R')

# General
source('../jheem_analyses/commoncode/target_populations.R')
source('../jheem_analyses/commoncode/age_mappings.R')

# Data Loads
load('../jheem_analyses/cached/google_mobility_data.Rdata')
