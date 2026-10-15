#===============================================================================

#This code aggregates county level population data from the Census from
#county to national level using our aggregation function.

#This is for years 2000-2025.

#===============================================================================

#Source aggregation related codes:
source('data_processing/aggregate_county_to_msa_new.R') #This aggregates county level data to other locations (updated 2026)

source('../jheem2/R/HELPERS_array_helpers.R')

#===============================================================================

#Aggregate Total Level from County to National

#===============================================================================

census.manager$register.source('census.aggregated.population', parent.source= "census", full.name = 'Census Aggregated Population', short.name = 'census.agg.pop')

national <- c("US")

#County --> National 
put.msa.data.as.new.source.NEW(outcome = 'poulation',
                               from.source.name = 'census.population',
                               to.source.name = 'census.aggregated.population',
                               to.locations =  national, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'COUNTRY',
                               details.for.new.data = 'estimated from county data',
                               data.manager = census.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census')


put.msa.data.as.new.source.NEW(outcome = 'poulation',
                               from.source.name = 'census.population',
                               to.source.name = 'census.aggregated.population',
                               to.locations =  state.vector, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'COUNTRY',
                               details.for.new.data = 'estimated from county data',
                               data.manager = census.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'single.year.age.census')
