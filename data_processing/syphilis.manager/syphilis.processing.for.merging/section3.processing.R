library(jheem2)
library(tidyverse)
library(readxl)
library(tools)

###############################################################################

#####SECTION 3#####

###############################################################################

data.manager = create.data.manager('syphilis', description='syphilis data manager')

#Register outcomes:
data.manager$register.outcome(
  'population',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Population',
    axis.name = 'Population',
    units = 'population',
    description = "Populaion Estimate"))

data.manager$register.outcome(
  'births',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Births',
    axis.name = 'Births',
    units = 'births',
    description = "Births"))

data.manager$register.outcome(
  'births.denominator',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Births Denominator',
    axis.name = 'Births Denominator',
    units = 'births',
    description = "Births Denominator"))

data.manager$register.outcome(
  'female.population', 
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Female Population',
    axis.name = 'Female Population',
    units = 'cases',
    description = "Female Population Age 15-44"))

data.manager$register.outcome(
  'fertility.rate', 
  metadata = create.outcome.metadata(
    scale = 'rate',
    display.name = 'Fertility Rate',
    axis.name = 'Fertility Rate',
    units = '%',
    description = "Fertility Rate"), denominator.outcome = 'female.population') 

data.manager$register.outcome(
  'deaths',
  metadata = create.outcome.metadata(  #This represent deaths from the census
    scale = 'non.negative.number',
    display.name = 'Deaths',
    axis.name = 'Deaths',
    units = 'deaths',
    description = "Deaths"))

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
  'inadequate.prenatal.care',
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Inadequate Prenatal Care',
    axis.name = 'Inadequate Prenatal Care',
    units = '%',
    description = "Inadequate Prenatal Care"), denominator.outcome = 'female.population')

#Register Sources:
data.manager$register.parent.source('NVSS', full.name = 'National Vital Statistics System', short.name= "NVSS")
data.manager$register.parent.source('NCHS', full.name = 'National Center for Health Statistics', short.name= "NCHS")
data.manager$register.parent.source('ACS', full.name = 'American Community Survey', short.name= "ACS")
data.manager$register.parent.source('census', full.name = 'United States Census Bureau', short.name= "census")

data.manager$register.source('cdc.wonder.natality', parent.source= "NVSS", full.name = "CDC Wonder Natality Data", short.name='cdc.wonder.natality') #children
data.manager$register.source('cdc_wonder', parent.source= "NCHS", full.name = "CDC Wonder", short.name='cdc_wonder')
data.manager$register.source('census.aggregated.population', parent.source= "census", full.name = 'Census Aggregated Adult Population', short.name = 'census.agg.pop')
data.manager$register.source('census.population', parent.source= "ACS", full.name = "US Census Bureau Population Data", short.name='census.population')
data.manager$register.source('census.deaths', parent.source= "NCHS", full.name = "Census Death Data", short.name='census.deaths')
data.manager$register.source('march.of.dimes', parent.source= "NCHS", full.name = "March of Dimes Peristats", short.name='march.of.dimes')

#Register Ontologies:
data.manager$register.ontology(
  'census',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('< 1 year', '1 year', '2 years', '3 years', '4 years', '5 years', '6 years', '7 years', '8 years', '9 years', '10 years',
          '11 years', '12 years', '13 years', '14 years', '15 years', '16 years', '17 years', '18 years', '19 years', '20 years',
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
  'cdc.fertility',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('15-19 years', '20-24 years', '25-29 years', '30-34 years','35-39 years', '40-44 years'),
    race=c('american indian or alaska native', 'asian or pacific islander', 'black or african american', 'white'),
    ethnicity = c('hispanic or latino', 'not hispanic or latino')
  ))
data.manager$register.ontology(     #This is for the births+births denominator data pulled from Census

  'census.cdc.wonder.births.deaths',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c("< 1 year", "1-4 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years",
          "45-49 years", "5-9 years", "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", 
          "85+ years"),
    race=c('american indian or alaska native', 'asian or pacific islander', 'black or african american', 'white'),
    ethnicity=c('hispanic or latino', 'not hispanic or latino'),
    sex=c('male','female')
  ))
data.manager$register.ontology(
  'stratified.census',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('0-4 years', '5-9 years', '10-14 years', '15-19 years', '20-24 years', '25-29 years', '30-34 years', '35-39 years',
          '40-44 years', '45-49 years', '50-54 years', '55-59 years', '60-64 years', '65-69 years', '70-74 years', 
          '75-79 years', '80-84 years', '85+ years'),
    race=c('white', 'black', 'asian', 'american indian and alaska native', 'native hawaiian and other pacific islander'),
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
  'census.immigration.national',
  ont = ontology(
    year= NULL,
    location= NULL,
    age = c("1-4 years", "5-17 years", "18-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years",
            "65-69 years", "70-74 years", "75+ years"),
    race=c('black', 'hispanic', 'other'),
    sex=c('male','female'),
    incomplete.dimensions = c("year", "location")
  ))

data.manager$register.ontology(
  'prenatal.care',
  ont = ontology(
    year= NULL,
    location= NULL,
    age = c("<20 years", "20-29 years", "30-39 years", ">= 40 years"), 
    race=c('black', 'hispanic', 'other', 'white'),
    sex=c('male','female'),
    incomplete.dimensions = c("year", "location")
  ))


#Codes:
source('data_processing/syphilis.manager/cached.census.data.R')
source('data_processing/syphilis.manager/cached.fertility.data.R')
source('data_processing/syphilis.manager/msa_immigration.R')
source('data_processing/syphilis.manager/national_immigration.R')
source('data_processing/syphilis.manager/inadequate.prenatal.care.R')

#Aggregate Outcomes to MSA 
syphilis.manager = data.manager
source('commoncode/locations_of_interest.R') #Source locations of interest to create MSA vectors
source('commoncode/additional_locations_of_interest.R') #Additional locations of interest
source('../jheem2/R/HELPERS_array_helpers.R') 
source('data_processing/put_msa_data_as_new_source_script.R') #This aggregates county level data to other locations

put.msa.data.as.new.source(outcome = 'deaths',
                           from.source.name= 'census.deaths',
                           to.source.name = 'census.aggregated.population', 
                           to.locations = MSAS.OF.INTEREST,
                           geographic.type.from = 'COUNTY',
                           geographic.type.to = 'CBSA',
                           details.for.new.data = 'estimated from county data',
                           data.manager= syphilis.manager) 

all.states = locations::get.all.for.type('state')

put.msa.data.as.new.source(outcome = 'population',
                           from.source.name = 'census.population',
                           to.source.name = 'census.aggregated.population',
                           to.locations =  all.states,
                           geographic.type.from = 'COUNTY',
                           geographic.type.to = 'STATE',
                           details.for.new.data = 'estimated from county data',
                           data.manager = syphilis.manager)

put.msa.data.as.new.source(outcome = 'population',
                           from.source.name = 'census.population',
                           to.source.name = 'census.aggregated.population',
                           to.locations =  MSAS.OF.INTEREST,
                           geographic.type.from = 'COUNTY',
                           geographic.type.to = 'CBSA',
                           details.for.new.data = 'estimated from county data',
                           data.manager = syphilis.manager)


# Source Code to Calculate Fertility Rate by MSA 
source('data_processing/syphilis.manager/fertility.rate.msa.R')

#Save:
save(syphilis.manager, file="Q:/data_managers/data.manager.merge/syphilis.manager_section3.rdata")