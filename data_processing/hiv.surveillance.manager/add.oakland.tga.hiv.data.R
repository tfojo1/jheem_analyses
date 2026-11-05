library(tidyverse)

###############################################################################

#This code adds HIV data to represent the Oakland TGA
#Oakland TGA = Alameda county (06001) + Contra Costa County (06013)

###############################################################################
oakland <- c('TGA.OAKLAND')

###############################################################################

#Source Necessary Surveillance Managers and Codes
# (May Comment Out depending on location of code)

###############################################################################
# 
#surveillance.manager = load.data.manager(name="surveillance.manager", file="Q:/data_managers/surveillance.manager.rdata")
# census.manager = load.data.manager("../../cached/census.manager.rdata")
# 
#source('data_processing/aggregate_county_to_msa_new.R') #Not using this - because of the relative contribution outcome not enough data is aggregated.  This aggregates county level data to other locations
# source('data_processing/put_msa_data_as_new_source_script.R') #This aggregates county level data to other locations
# source('../jheem2/R/HELPERS_array_helpers.R')
# source('commoncode/locations_of_interest.R') #Source locations of interest to create MSA vectors


#===============================================================================

#Add adult.population

#===============================================================================

put.msa.data.as.new.source.NEW(outcome = 'adult.population',
                               from.source.name = 'census.population',
                               to.source.name = 'census.aggregated.adult.population',
                               to.locations =  oakland, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'TGA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.adults')

put.msa.data.as.new.source.NEW(outcome = 'adult.population',
                               from.source.name = 'census.population',
                               to.source.name = 'census.aggregated.adult.population',
                               to.locations =  oakland, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'CBSA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'single.year.age.census')

put.msa.data.as.new.source.NEW(outcome = 'deaths',
                               from.source.name = 'census.deaths',
                               to.source.name = 'census.deaths.aggregated',
                               to.locations =  oakland, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'TGA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.adults')

#===============================================================================

#Add HIV Outcomes and STIs

#===============================================================================


put.msa.data.as.new.source.NEW(outcome = 'diagnoses',
                               from.source.name = 'cdc.hiv',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  oakland, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'TGA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.adults')

put.msa.data.as.new.source.NEW(outcome = 'diagnoses',
                               from.source.name = 'cdc.hiv',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  oakland, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'TGA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'single.year.age.census')

put.msa.data.as.new.source.NEW(outcome = 'diagnosed.prevalence',
                               from.source.name = 'cdc.hiv',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  oakland, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'TGA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.adults')

put.msa.data.as.new.source.NEW(outcome = 'diagnosed.prevalence',
                               from.source.name = 'cdc.hiv',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  oakland, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'TGA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'single.year.age.census')

put.msa.data.as.new.source.NEW(outcome = 'suppression',
                               from.source.name = 'cdc.hiv',
                               to.source.name = 'cdc.aggregated.proportion',
                               to.locations =  oakland, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'TGA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.adults',
                               source.for.denominator='cdc.hiv',
                               ontology.for.denominator='cdc')

put.msa.data.as.new.source.NEW(outcome = 'suppression',
                               from.source.name = 'cdc.hiv',
                               to.source.name = 'cdc.aggregated.proportion',
                               to.locations =  oakland, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'TGA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'single.year.age.census',
                               source.for.denominator='cdc.hiv',
                               ontology.for.denominator='cdc')

put.msa.data.as.new.source.NEW(outcome = 'prep',
                               from.source.name = 'aidsvu',
                               to.source.name = 'prep.aidsvu.aggregated.county',
                               to.locations =  oakland, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'TGA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.adults')

put.msa.data.as.new.source.NEW(outcome = 'prep',
                               from.source.name = 'aidsvu',
                               to.source.name = 'prep.aidsvu.aggregated.county',
                               to.locations =  oakland, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'TGA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'single.year.age.census')

put.msa.data.as.new.source.NEW(outcome = 'prep',
                               from.source.name = 'cdc.prep',
                               to.source.name = 'prep.cdc.aggregated.county',
                               to.locations =  oakland, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'TGA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.adults')

put.msa.data.as.new.source.NEW(outcome = 'prep',
                               from.source.name = 'cdc.prep',
                               to.source.name = 'prep.cdc.aggregated.county',
                               to.locations =  oakland, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'TGA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'single.year.age.census')

put.msa.data.as.new.source.NEW(outcome = 'prep.indications',
                               from.source.name = 'cdc.prep.indications',
                               to.source.name = 'prep.indications.aggregated.county',
                               to.locations =  oakland, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'TGA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.adults')

put.msa.data.as.new.source.NEW(outcome = 'prep.indications',
                               from.source.name = 'cdc.prep.indications',
                               to.source.name = 'prep.indications.aggregated.county',
                               to.locations =  oakland, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'TGA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'single.year.age.census')

put.msa.data.as.new.source.NEW(outcome = 'linkage_1mo',
                               from.source.name = 'cdc.hiv',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  oakland, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'TGA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.adults')

put.msa.data.as.new.source.NEW(outcome = 'linkage_1mo',
                               from.source.name = 'cdc.hiv',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  oakland, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'TGA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'single.year.age.census')

put.msa.data.as.new.source.NEW(outcome = 'engagement',
                               from.source.name = 'cdc.hiv',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  oakland, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'TGA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.adults')

put.msa.data.as.new.source.NEW(outcome = 'engagement',
                               from.source.name = 'cdc.hiv',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  oakland, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'TGA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'single.year.age.census')

put.msa.data.as.new.source.NEW(outcome = 'gonorrhea',
                               from.source.name = 'cdc.sti',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  oakland, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'TGA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.adults')

put.msa.data.as.new.source.NEW(outcome = 'gonorrhea',
                               from.source.name = 'cdc.sti',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  oakland, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'TGA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'single.year.age.census')

put.msa.data.as.new.source.NEW(outcome = 'ps.syphilis',
                               from.source.name = 'cdc.sti',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  oakland, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'TGA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.adults')

put.msa.data.as.new.source.NEW(outcome = 'ps.syphilis',
                               from.source.name = 'cdc.sti',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  oakland, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'TGA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'single.year.age.census')

put.msa.data.as.new.source.NEW(outcome = 'early.syphilis',
                               from.source.name = 'cdc.sti',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  oakland, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'TGA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.adults')

put.msa.data.as.new.source.NEW(outcome = 'early.syphilis',
                               from.source.name = 'cdc.sti',
                               to.source.name = 'cdc.aggregated.county',
                               to.locations =  oakland, 
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'TGA',
                               details.for.new.data = 'estimated from county data',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'single.year.age.census')

#===============================================================================

# Aggregate proportion.msm to MSA 

#===============================================================================

put.msa.data.as.new.source.NEW(outcome = 'proportion.msm',
                               from.source.name = 'emory',
                               to.source.name = 'emory.aggregated',
                               to.locations =  oakland,
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'TGA',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'census.adults',
                               source.for.denominator='census.population',
                               ontology.for.denominator='census.adults',
                               details.for.new.data= "aggregated from 2013 Emory estimates")

put.msa.data.as.new.source.NEW(outcome = 'proportion.msm',
                               from.source.name = 'emory',
                               to.source.name = 'emory.aggregated',
                               to.locations =  oakland,
                               geographic.type.from = 'COUNTY',
                               geographic.type.to = 'TGA',
                               data.manager = surveillance.manager,
                               required.coverage=0.95,
                               outcome.for.relative.contribution = 'adult.population',
                               source.for.relative.contribution = 'census.population',
                               ontology.for.relative.contribution = 'single.year.age.census',
                               source.for.denominator='census.population',
                               ontology.for.denominator='single.year.age.census',
                               details.for.new.data = "aggregated from 2013 Emory estimates")
#===============================================================================

# Add NSDUH data for Oakland substate regions 

#===============================================================================

oakland.counties <- c('06001', '06013')

oakland.nsduh = locations::get.containing.locations(oakland.counties, 'NSDUH') # NSDUH substate regions


oakland.cocaine.aggregated = surveillance.manager$pull(outcome = "cocaine",
                                                       metric = "estimate",
                                                       dimension.values=list(location=oakland.nsduh),
                                                       keep.dimensions='year')

oakland.heroin.aggregated = surveillance.manager$pull(outcome = "heroin",
                                                      metric = "estimate",
                                                      dimension.values=list(location=oakland.nsduh),
                                                      keep.dimensions='year')


oakland.cocaine = as.data.frame.table(oakland.cocaine.aggregated)%>%
    mutate(year = as.character(year))%>%
    mutate(value = as.numeric(Freq))%>%
    mutate(outcome = "cocaine")%>%
    mutate(location = 'TGA.OAKLAND')

oakland.heroin = as.data.frame.table(oakland.heroin.aggregated)%>%
    mutate(year = as.character(year))%>%
    mutate(value = as.numeric(Freq))%>%
    mutate(outcome = "heroin")%>%
    mutate(location = 'TGA.OAKLAND')

aggregated.nsduh.data.oakland <- rbind(oakland.heroin, oakland.cocaine)

surveillance.manager$put.long.form(
    data = aggregated.nsduh.data.oakland,
    ontology.name = 'nsduh',
    source = 'nsduh',
    dimension.values = list(),
    url = 'https://pdas.samhsa.gov/saes/substate',
    details = 'NSDUH Substate Estimates, aggregated from substate to MSA')

