
# The file to source to load all necessary packages, cached data, code

library(locations)
library(distributions)

source('../jheem_analyses/commoncode/cache_manager.R')


# This 'source' call is equivalent to loading the jheem2 package

# Make sure that the jheem2 version is up-to-date
# library(jheem2)
source('../jheem2/R/tests/source_jheem2_package.R')
clear.all.managers()
source('../jheem_analyses/commoncode/file_paths.R')
set.jheem.root.directory(ROOT.DIR)

# Set up file paths
# Load the data managers
if (!exists('SURVEILLANCE.MANAGER'))
{
    cat("Loading Surveillance Manager (may take a minute or two)...")
    SURVEILLANCE.MANAGER = load.data.manager.from.cache('surveillance.manager.rdata', set.as.default=T)
    cat("Done!\n")
}
  

if (!exists('CENSUS.MANAGER'))
{
    cat("Loading Census Manager (may take a minute or two)...")
    CENSUS.MANAGER = load.data.manager.from.cache('census.manager.rdata', set.as.default=F)
    cat("Done!\n")
}
  

# Load the data - this will eventually be replaced with jheem2's native data manager
#if (!exists('ALL.DATA.MANAGERS'))
#    load(file.path(JHEEM.CACHE.DIR, 'ALL.DATA.MANAGERS.Rdata'))
#if (!exists('DEFAULT.LOCALE.MAPPING'))
#    load(file.path(JHEEM.CACHE.DIR, 'DEFAULT.LOCALE.MAPPING.Rdata'))
#if (!exists('msa.surveillance'))
#    load(file.path(JHEEM.CACHE.DIR, 'msa.surveillance.Rdata'))
#if (!exists('national.surveillance'))
#  load(file.path(JHEEM.CACHE.DIR, 'national.surveillance.Rdata'))


# These supporting files are hold-overs from the previous version, and will eventually need to be replaced
#source('../jheem_analyses/data_managers/census_manager.R')
#source('../jheem_analyses/data_managers/prep_manager_2.R')
#source('../jheem_analyses/data_managers/continuum_manager_3.R')
#source('../jheem_analyses/data_managers/idu_manager.R')
#source('../jheem_analyses/data_managers/locale_mappings.R')
#source('../jheem_analyses/data_managers/hiv_surveillance_manager.R')
#source('../jheem_analyses/data_managers/mortality_manager.R')
#source('../jheem_analyses/data_managers/natality_manager.R')
#source('../jheem_analyses/data_managers/pairing_manager.R')

# These are the real supporting files that will be in the final version

# Input Managers
source('../jheem_analyses/input_managers/input_helpers.R')
source('../jheem_analyses/input_managers/covid_mobility_manager.R')
source('../jheem_analyses/input_managers/covid_input_manager.R')
source('../jheem_analyses/input_managers/idu_input_manager.R')
source('../jheem_analyses/input_managers/prep_input_manager.R')
source('../jheem_analyses/input_managers/pairing_input_manager.R')
source('../jheem_analyses/input_managers/continuum_input_manager.R')
source('../jheem_analyses/input_managers/idu_sexual_oes.R')

# For the EHE version
source('../jheem_analyses/applications/EHE/ehe_base_parameters.R')
source('../jheem_analyses/applications/EHE/ehe_specification_helpers.R')
source('../jheem_analyses/applications/EHE/ehe_ontology_mappings.R')

# General
source('../jheem_analyses/commoncode/target_populations.R')
source('../jheem_analyses/commoncode/age_mappings.R')
source('../jheem_analyses/commoncode/cache_object_for_version_functions.R')
source('../jheem_analyses/commoncode/logitnorm_helpers.R')

# Data Loads
load(file.path(JHEEM.CACHE.DIR, 'google_mobility_data.Rdata'))
