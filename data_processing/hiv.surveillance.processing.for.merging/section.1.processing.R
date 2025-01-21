
library(jheem2)
library(tidyverse)
library(readxl)
library(stringr)
library(haven)
library(locations)
library(tools)

###############################################################################

#####SECTION 1#####
#Population related data
###############################################################################

data.manager = create.data.manager('surveillance', description='surveillance data manager')

#Register outcomes:

data.manager$register.outcome(
  'adult.population',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Adult Population',
    axis.name = 'Adult Population',
    units = 'population',
    description = "Adult Population Estimate, Ages 13 and over"))

data.manager$register.outcome(
  'immigration',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Immigration',
    axis.name = 'Immigration',
    units = 'population',
    description = "Metro Immigration"))

data.manager$register.outcome(
  'emigration',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Emigration',
    axis.name = 'Emigration',
    units = 'population',
    description = "Metro Emigration"))

data.manager$register.outcome(
  'adult.immigration',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Adult Immigration',
    axis.name = 'Adult Immigration',
    units = 'population',
    description = "Metro Immigration Ages 13+"))

data.manager$register.outcome(
  'adult.emigration',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Emigration',
    axis.name = 'Emigration',
    units = 'population',
    description = "Metro Emigration Ages 13+"))

data.manager$register.outcome(
  'deaths',
  metadata = create.outcome.metadata(  #Registering 'deaths' as an outcome for the deaths from the census manager aggregated from county to MSA in the surveillance manager
                                               #this is general mortality (NOT hiv specific deaths)
    scale = 'non.negative.number',
    display.name = 'Deaths',
    axis.name = 'Deaths',
    units = 'deaths',
    description = "Deaths"))

#Register Sources:
data.manager$register.parent.source('ACS', full.name = 'American Community Survey', short.name= "ACS") #parent
data.manager$register.parent.source('NCHS', full.name = 'National Center for Health Statistics', short.name= "NCHS") #parent

data.manager$register.source('census.population', parent.source= "ACS", full.name = "US Census Bureau Population Data", short.name='census.population') #child
data.manager$register.source('census.deaths', parent.source= "NCHS", full.name = "US Census Bureau Death Data", short.name='census.deaths') #child
data.manager$register.source('census.deaths.aggregated', parent.source= "NCHS", full.name = 'Census Deaths Aggregated', short.name = 'census deaths aggregated') #child

#Register Ontologies:

data.manager$register.ontology(
  'census',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('13 years', '14 years', '15 years', '16 years', '17 years', '18 years', '19 years', '20 years',
          '21 years', '22 years', '23 years', '24 years', '25 years', '26 years', '27 years', '28 years', '29 years', '30 years',
          '31 years', '32 years', '33 years', '34 years', '35 years', '36 years', '37 years', '38 years', '39 years', '40 years',
          '41 years', '42 years', '43 years', '44 years', '45 years', '46 years', '47 years', '48 years', '49 years', '50 years',
          '51 years', '52 years', '53 years', '54 years', '55 years', '56 years', '57 years', '58 years', '59 years', '60 years',
          '61 years', '62 years', '63 years', '64 years', '65 years', '66 years', '67 years', '68 years', '69 years', '70 years',
          '71 years', '72 years', '73 years', '74 years', '75 years', '76 years', '77 years', '78 years', '79 years', '80 years',
          '81 years', '82 years', '83 years', '84 years', '85+ years'),
    race=c('white', 'black', 'american indian or alaska native', 'asian or pacific islander'),
    ethnicity=c('hispanic', 'not hispanic'),
    sex=c('male','female')
  ))

data.manager$register.ontology(
  'census.immigration',
  ont = ontology(
    year= c("2006-2010", "2011-2015", "2016-2020"),
    location= NULL,
    age = c("1-4 years", "5-17 years", "18-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years",
            "65-69 years", "70-74 years", "75+ years"),
    race=c("hispanic or latino", "black", 'other'),
    sex=c('male','female'),
    incomplete.dimensions = c("year", "location")
  ))

data.manager$register.ontology(
  'census.immigration.adults',
  ont = ontology(
    year= c("2011-2015", "2012-2016", "2013-2017", "2014-2018", "2015-2019", "2016-2020"),
    location= NULL,
    age = c("1-12 years", "13-17 years", "18-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years",
            "65-69 years", "70-74 years", "75+ years"),
    race=c("hispanic or latino", "black", 'other'),
    sex=c('male','female'),
    incomplete.dimensions = c("year", "location")
  ))

#Codes:

source('data_processing/immigration.R')
source('data_processing/immigration_age_calculations.R')

#Aggregate Outcomes:

census.manager = load.data.manager("../../cached/census.manager.rdata")

source('data_processing/put_msa_data_without_estimation_script.R') #Creates adult.population for single year ages
source('data_processing/put_msa_data_as_new_source_script.R') #This aggregates county level data to other locations
source('data_processing/adult.population.10.23.R') #creates adult population for 2010-2023, stratified data
source('../jheem2/R/HELPERS_array_helpers.R')

#This aggregates county level data to state level for the recent census years for adult.population (as well as county to MSAs of interest)
#where I wrote the restructure.recent.age.groups code to estimate for adult.pop
all.states = locations::get.all.for.type('state')

put.msa.data.as.new.source(outcome = 'adult.population',
                           from.source.name = 'census.population',
                           to.source.name = 'census.aggregated.adult.population',
                           to.locations =  all.states,
                           geographic.type.from = 'COUNTY',
                           geographic.type.to = 'STATE',
                           details.for.new.data = 'estimated from county data',
                           data.manager = surveillance.manager)

put.msa.data.as.new.source(outcome = 'adult.population',
                           from.source.name = 'census.population',
                           to.source.name = 'census.aggregated.adult.population',
                           to.locations =  MSAS.OF.INTEREST,
                           geographic.type.from = 'COUNTY',
                           geographic.type.to = 'CBSA',
                           details.for.new.data = 'estimated from county data',
                           data.manager = surveillance.manager)

put.msa.data.as.new.source(outcome = 'adult.population',
                           from.source.name = 'census.population',
                           to.source.name = 'census.aggregated.adult.population',
                           to.locations =  NSDUH.REGIONS.CONTAINING.LOCATIONS.OF.INTEREST,
                           geographic.type.from = 'COUNTY',
                           geographic.type.to = 'CBSA',
                           details.for.new.data = 'estimated from county data',
                           data.manager = surveillance.manager)

source('data_processing/simple_aggregate_county_to_msa_script.R')

get.msa.totals.from.county.simple(outcome= 'deaths',  #Sum deaths by county into deaths by MSA using this code/function
                                  metric='estimate',
                                  msas= MSAS.OF.INTEREST,
                                  source.from = 'census.deaths', 
                                  source.to='census.deaths.aggregated',
                                  details.for.put= 'estimated from county data',
                                  data.manager.from=census.manager,
                                  data.manager.to= surveillance.manager)

#Save:
save(data.manager, file="Q:/data_managers/data.manager.merge/surveillance.manager_section1.rdata")