#===============================================================================

#This code aggregates county level population data from the Census from
#county to national level using our aggregation function.

#This is for years 2000-2025.

#===============================================================================

#Source aggregation related codes:
source('data_processing/aggregate_county_to_msa_new.R') #This aggregates county level data to other locations (updated 2026)

source('../jheem2/R/HELPERS_array_helpers.R')
source('commoncode/locations_of_interest.R')
source('commoncode/additional_locations_of_interest.R')

#===============================================================================

#Aggregate Total Level from County to National

#===============================================================================

#County --> National (Total)
put.msa.data.as.new.source.NEW(outcome = 'adult.population',
                               from.source.name = 'census.population',
                               to.source.name = 'census.aggregated.adult.population',
                               to.locations =  state.vector, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'STATE',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census')

put.msa.data.as.new.source.NEW(outcome = 'adult.population',
                               from.source.name = 'census.population',
                               to.source.name = 'census.aggregated.adult.population',
                               to.locations =  state.vector, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'STATE',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.grouped.age')