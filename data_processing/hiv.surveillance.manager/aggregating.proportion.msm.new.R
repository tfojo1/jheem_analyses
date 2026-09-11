#This code is used to aggregation proportion.msm data from Emory estimates for 2013 data only.
#2013 data is only available by county so we aggregate it to state and MSA.  2020 updated Emory data is
#available at county, state, and MSA level.

source('data_processing/aggregate_county_to_msa_new.R') #updated for 2026.
source('../jheem2/R/HELPERS_array_helpers.R') 
source('commoncode/locations_of_interest.R')

state.vector = state.abb
state.vector = state.vector[!state.vector == "CT"] #Removing states with county issues that lead to incomplete population data
state.vector = state.vector[!state.vector == "AK"]
state.vector = state.vector[!state.vector == "MT"]

put.msa.data.as.new.source.NEW(outcome = 'proportion.msm',
                               from.source.name = 'emory',
                               to.source.name = 'emory.aggregated',
                               years = 2013,
                               to.locations =  MSAS.OF.INTEREST,
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.grouped.age',
                               source.for.denominator='census.population',
                               ontology.for.denominator='census.grouped.age',
                               details.for.new.data= "aggregated from 2013 Emory estimates")

put.msa.data.as.new.source.NEW(outcome = 'proportion.msm',
                               from.source.name = 'emory',
                               to.source.name = 'emory.aggregated',
                               years = 2013,
                               to.locations =  state.vector,
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'STATE',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.grouped.age',
                               source.for.denominator='census.population',
                               ontology.for.denominator='census.grouped.age',
                               details.for.new.data = "aggregated from 2013 Emory estimates")