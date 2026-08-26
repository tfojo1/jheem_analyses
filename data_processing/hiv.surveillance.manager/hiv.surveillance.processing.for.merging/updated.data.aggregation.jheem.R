census.manager = load.data.manager("../../cached/census.manager.rdata")

source('data_processing/aggregate_county_to_msa_new.R') #This aggregates county level data to other locations (updated 2026)

source('../jheem2/R/HELPERS_array_helpers.R')
source('commoncode/locations_of_interest.R')
source('commoncode/additional_locations_of_interest.R')

#===============================================================================

#SECTION 1

#===============================================================================

#This aggregates county level data to state level for the recent census years for adult.population (as well as county to MSAs of interest)
#where I wrote the restructure.recent.age.groups code to estimate for adult.pop

state.vector = state.abb
state.vector = state.vector[!state.vector == "CT"] #Removing states with county issues that lead to incomplete population data
state.vector = state.vector[!state.vector == "AK"]
state.vector = state.vector[!state.vector == "MT"]#Previously we used agg with whatever we have

#county --> state
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


#county --> MSA
put.msa.data.as.new.source.NEW(outcome = 'adult.population',
                               from.source.name = 'census.population',
                               to.source.name = 'census.aggregated.adult.population',
                               to.locations =  MSAS.OF.INTEREST, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census')

put.msa.data.as.new.source.NEW(outcome = 'adult.population',
                               from.source.name = 'census.population',
                               to.source.name = 'census.aggregated.adult.population',
                               to.locations =  MSAS.OF.INTEREST, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.grouped.age')

#county --> NSDUH Substate Regions

to_remove <- c("IL.1", "IL.2", "IL.3", "IL.4", "IL.5", "IL.6", "IL.7", "DC.2", "DC.3", "DC.4", "DC.5", "DC.1", "DC.6", "DC.7", "DC.8", "DE.4", "DE.2", "MA.2", "MA.3")

NSDUH.REGIONS.CONTAINING.LOCATIONS.OF.INTEREST <- setdiff(
    NSDUH.REGIONS.CONTAINING.LOCATIONS.OF.INTEREST,
    to_remove
)

put.msa.data.as.new.source.NEW(outcome = 'adult.population',
                               from.source.name = 'census.population',
                               to.source.name = 'census.aggregated.adult.population',
                               to.locations =  NSDUH.REGIONS.CONTAINING.LOCATIONS.OF.INTEREST, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               skip.coverage.condition=T,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census')

put.msa.data.as.new.source.NEW(outcome = 'adult.population',
                               from.source.name = 'census.population',
                               to.source.name = 'census.aggregated.adult.population',
                               to.locations =  NSDUH.REGIONS.CONTAINING.LOCATIONS.OF.INTEREST, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.grouped.age')

#Deaths county --> MSA
census.manager = load.data.manager(name="census.manager", file="Q:/data_managers/census.manager.rdata")

census.deaths.by.county <- as.data.frame.table(census.manager$data$deaths$estimate$census.deaths$census$year__location)%>%
    mutate(outcome = 'deaths',
           value = Freq,
           year = as.character(year),
           location= as.character(location))


surveillance.manager$put.long.form(
    data = census.deaths.by.county,
    ontology.name = 'census',
    source = 'census.deaths',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')


put.msa.data.as.new.source.NEW(outcome = 'deaths',
                               from.source.name = 'census.deaths',
                               to.source.name = 'census.deaths.aggregated',
                               to.locations =  MSAS.OF.INTEREST, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census')

put.msa.data.as.new.source.NEW(outcome = 'deaths',
                               from.source.name = 'census.deaths',
                               to.source.name = 'census.deaths.aggregated',
                               to.locations =  MSAS.OF.INTEREST, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.grouped.age')

#===============================================================================

#SECTION 2

#===============================================================================
msas.minus.riverside = MSAS.OF.INTEREST[ !MSAS.OF.INTEREST == "C.40140"] #Update for March 2025- removing Riverside from Diagnosed Prevalence data

#county --> MSA
put.msa.data.as.new.source.NEW(outcome = 'diagnosed.prevalence',
                               from.source.name = 'cdc.hiv',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  msas.minus.riverside, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census')

put.msa.data.as.new.source.NEW(outcome = 'diagnosed.prevalence',
                               from.source.name = 'cdc.hiv',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  msas.minus.riverside, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.grouped.age')

put.msa.data.as.new.source.NEW(outcome = 'diagnoses',
                               from.source.name = 'cdc.hiv',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  MSAS.OF.INTEREST, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census')

put.msa.data.as.new.source.NEW(outcome = 'diagnoses',
                               from.source.name = 'cdc.hiv',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  MSAS.OF.INTEREST, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.grouped.age')

put.msa.data.as.new.source.NEW(outcome = 'total.prevalence',
                               from.source.name = 'cdc.hiv',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  MSAS.OF.INTEREST, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census')

put.msa.data.as.new.source.NEW(outcome = 'total.prevalence',
                               from.source.name = 'cdc.hiv',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  MSAS.OF.INTEREST, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.grouped.age')

#Prep data
put.msa.data.as.new.source.NEW(outcome = 'prep',
                               from.source.name = 'cdc.prep',
                               to.source.name = 'prep.cdc.aggregated.county',
                               to.locations =  MSAS.OF.INTEREST, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census')

put.msa.data.as.new.source.NEW(outcome = 'prep',
                               from.source.name = 'cdc.prep',
                               to.source.name = 'prep.cdc.aggregated.county',
                               to.locations =  MSAS.OF.INTEREST, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.grouped.age')

put.msa.data.as.new.source.NEW(outcome = 'prep',
                               from.source.name = 'aidsvu',
                               to.source.name = 'prep.aidsvu.aggregated.county',
                               to.locations =  MSAS.OF.INTEREST, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census')

put.msa.data.as.new.source.NEW(outcome = 'prep',
                               from.source.name = 'aidsvu',
                               to.source.name = 'prep.aidsvu.aggregated.county',
                               to.locations =  MSAS.OF.INTEREST, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.grouped.age')

put.msa.data.as.new.source.NEW(outcome = 'prep.indications',
                               from.source.name = 'cdc.prep.indications',
                               to.source.name = 'prep.indications.aggregated.county',
                               to.locations =  MSAS.OF.INTEREST, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census')

put.msa.data.as.new.source.NEW(outcome = 'prep.indications',
                               from.source.name = 'cdc.prep.indications',
                               to.source.name = 'prep.indications.aggregated.county',
                               to.locations =  MSAS.OF.INTEREST, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.grouped.age')

#Put proportion outcomes (awareness and suppression)
put.msa.data.as.new.source.NEW(outcome = 'awareness',
                               from.source.name = 'cdc.hiv',
                               to.source.name = 'cdc.aggregated.proportion',
                               to.locations =  MSAS.OF.INTEREST, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census',
                               source.for.denominator='cdc.hiv',
                               ontology.for.denominator='cdc')


put.msa.data.as.new.source.NEW(outcome = 'awareness',
                               from.source.name = 'cdc.hiv',
                               to.source.name = 'cdc.aggregated.proportion',
                               to.locations =  MSAS.OF.INTEREST, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.grouped.age',
                               source.for.denominator='cdc.hiv',
                               ontology.for.denominator='cdc')

put.msa.data.as.new.source.NEW(outcome = 'suppression',
                               from.source.name = 'cdc.hiv',
                               to.source.name = 'cdc.aggregated.proportion',
                               to.locations =  MSAS.OF.INTEREST, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census',
                               source.for.denominator='cdc.hiv',
                               ontology.for.denominator='cdc')

put.msa.data.as.new.source.NEW(outcome = 'suppression',
                               from.source.name = 'cdc.hiv',
                               to.source.name = 'cdc.aggregated.proportion',
                               to.locations =  MSAS.OF.INTEREST, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.grouped.age',
                               source.for.denominator='cdc.hiv',
                               ontology.for.denominator='cdc')

#===============================================================================

#SECTION 3 / Section 5

#NONE
#===============================================================================


#===============================================================================

#SECTION 4

#===============================================================================
#surveillance.manager = data.manager

#county --> MSA
put.msa.data.as.new.source.NEW(outcome = 'gonorrhea',
                               from.source.name = 'cdc.sti',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  MSAS.OF.INTEREST, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census')

put.msa.data.as.new.source.NEW(outcome = 'gonorrhea',
                               from.source.name = 'cdc.sti',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  MSAS.OF.INTEREST, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.grouped.age')


#county --> MSA
put.msa.data.as.new.source.NEW(outcome = 'ps.syphilis',
                               from.source.name = 'cdc.sti',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  MSAS.OF.INTEREST, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census')

put.msa.data.as.new.source.NEW(outcome = 'ps.syphilis',
                               from.source.name = 'cdc.sti',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  MSAS.OF.INTEREST, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.grouped.age')



