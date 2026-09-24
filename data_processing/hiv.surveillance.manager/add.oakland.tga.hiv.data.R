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


###############################################################################

#Add Population Data First 
#This will be the relative contribution outcome
#which will allow aggregation of other outcomes

###############################################################################

# ###ONCE YOU FIX THE POPULATION ISSUE YOU CAN TAKE THIS OUT AND AGGREGATE POPULATION FIRST
# 
# #2020-2023
# alameda.20.23 <- enframe(surveillance.manager$data$adult.population$estimate$census.population$census$year__location[, "06001"], name = "year", value = "value")
# contra.20.23 <- enframe(surveillance.manager$data$adult.population$estimate$census.population$census$year__location[, "06013"], name = "year", value = "value")
# 
# #2010-2019
# alameda.10.19 <-enframe(surveillance.manager$data$adult.population$estimate$census.population$census.grouped.age$year__location[, "06001"], name = "year", value = "value")
# contra.10.19 <- enframe(surveillance.manager$data$adult.population$estimate$census.population$census.grouped.age$year__location[, "06013"], name = "year", value = "value")
# 
# oakland.tga.population <- rbind(alameda.20.23, contra.20.23, alameda.10.19, contra.10.19)
# 
# oakland.tga.population <- oakland.tga.population %>%
#     mutate(value = as.numeric(value))%>%
#     group_by(year)%>%
#     mutate(summed.population = sum(value))%>%
#     select(-value)%>%
#     rename(value = summed.population)%>%
#     distinct()%>%
#     mutate(outcome = "adult.population")%>%
#     mutate(location = "TGA.OAKLAND")
# 
# oakland.tga.population <- as.data.frame(oakland.tga.population)
# 
# surveillance.manager$put.long.form(
#     data = oakland.tga.population,
#     ontology.name = 'census', 
#     source = 'census.population',
#     dimension.values = list(),
#     url = 'www.census.gov',
#     details = 'Census Reporting')

###############################################################################

#Add HIV Outcomes

###############################################################################

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
                               ontology.for.relative.contribution = 'census')

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
                               ontology.for.relative.contribution = 'census.grouped.age')


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
                               ontology.for.relative.contribution = 'census')

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
                               ontology.for.relative.contribution = 'census.grouped.age')

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
                               ontology.for.relative.contribution = 'census')

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
                               ontology.for.relative.contribution = 'census.grouped.age')

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
                               ontology.for.relative.contribution = 'census',
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
                               ontology.for.relative.contribution = 'census.grouped.age',
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
                               ontology.for.relative.contribution = 'census')

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
                               ontology.for.relative.contribution = 'census.grouped.age')

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
                               ontology.for.relative.contribution = 'census')

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
                               ontology.for.relative.contribution = 'census.grouped.age')

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
                               ontology.for.relative.contribution = 'census')

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
                               ontology.for.relative.contribution = 'census.grouped.age')

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
                               ontology.for.relative.contribution = 'census')

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
                               ontology.for.relative.contribution = 'census.grouped.age')

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
                               ontology.for.relative.contribution = 'census')

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
                               ontology.for.relative.contribution = 'census.grouped.age')

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
                               ontology.for.relative.contribution = 'census')

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
                               ontology.for.relative.contribution = 'census.grouped.age')

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
                               ontology.for.relative.contribution = 'census')

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
                               ontology.for.relative.contribution = 'census.grouped.age')

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
                               ontology.for.relative.contribution = 'census')

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
                               ontology.for.relative.contribution = 'census.grouped.age')


# Aggregate proportion.msm to MSA ---------------------------------------
    
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
                               ontology.for.relative.contribution = 'census.grouped.age',
                               source.for.denominator='census.population',
                               ontology.for.denominator='census.grouped.age',
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
                               ontology.for.relative.contribution = 'census.grouped.age',
                               source.for.denominator='census.population',
                               ontology.for.denominator='census.grouped.age',
                               details.for.new.data = "aggregated from 2013 Emory estimates")