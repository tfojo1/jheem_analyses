
library(jheem2)
library(tidyverse)
library(readr)
library(readxl) 
library(haven)
library(locations)

#===============================================================================

#####BIRTHS AND DEATHS#####

#===============================================================================

census.manager = create.data.manager('census_manager', description='an additional data manager for census data')

#Register outcomes:
census.manager$register.outcome(
  'births',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Births',
    axis.name = 'Births',
    units = 'births',
    description = "Births"))

census.manager$register.outcome(
  'births.denominator',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Births Denominator',
    axis.name = 'Births Denominator',
    units = 'births',
    description = "Births Denominator"))

census.manager$register.outcome(
  'deaths',
  metadata = create.outcome.metadata(  #This represent deaths from the census
    scale = 'non.negative.number',
    display.name = 'Deaths',
    axis.name = 'Deaths',
    units = 'deaths',
    description = "Deaths"))

census.manager$register.outcome(
  'metro.deaths',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Metro Deaths',
    axis.name = 'Metro Deaths',
    units = 'deaths',
    description = "Metro Deaths"))

census.manager$register.outcome(
  'metro.deaths.denominator',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Metro Deaths Denominator',
    axis.name = 'Metro Deaths Denominator',
    units = 'deaths',
    description = "Metro Deaths Denominator"))

#I'm not sure if we need this- this is the denominator value for the national level deaths from CDC Wonder (corresponds to outcoem = deaths, source= cdc wonder)
census.manager$register.outcome(
  'deaths.denominator',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Deaths Denominator',
    axis.name = 'Deaths Denominator',
    units = 'deaths',
    description = "Deaths Denominator"))

census.manager$register.outcome(
  'female.population', 
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Female Population',
    axis.name = 'Female Population',
    units = 'cases',
    description = "Female Population Age 15-44"))

census.manager$register.outcome(
  'fertility.rate', 
  metadata = create.outcome.metadata(
    scale = 'rate',
    display.name = 'Fertility Rate',
    axis.name = 'Fertility Rate',
    units = '%',
    description = "Fertility Rate"), denominator.outcome = 'female.population')

census.manager$register.outcome(
    'state.deaths.denominator',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'State Deaths Denominator',
        axis.name = 'State Deaths Denominator',
        units = 'deaths',
        description = "State Deaths Denominator"))

census.manager$register.outcome(
    'state.deaths',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'State Deaths',
        axis.name = 'State Deaths',
        units = 'deaths',
        description = "State Deaths"))

#Register Sources:
census.manager$register.parent.source('census', full.name = 'United States Census Bureau', short.name= "census") #parent
census.manager$register.parent.source('NCHS', full.name = 'National Center for Health Statistics', short.name= "NCHS") #parent
census.manager$register.parent.source('NVSS', full.name = 'National Vital Statistics System', short.name= "NVSS") #parent
census.manager$register.source('census.population', parent.source= "census", full.name = "Census Population Data", short.name='census.population') #child
census.manager$register.source('census.deaths', parent.source= "NCHS", full.name = "Census Death Data", short.name='census.deaths') #child
census.manager$register.source('cdc_wonder', parent.source= "NCHS", full.name = "CDC Wonder", short.name='cdc_wonder') #child
census.manager$register.source('cdc.wonder.natality', parent.source= "NVSS", full.name = "CDC Wonder Natality Data", short.name='cdc.wonder.natality') #child

#Register Ontologies:
census.manager$register.ontology(
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

#Creating a metro deaths ontology because metro deaths use year ranges
census.manager$register.ontology(
  'metro.deaths',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c("< 1 year", "1-4 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years",
          "45-49 years", "5-9 years", "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", 
          "85-89 years", "90-94 years", "95-99 years", "100+ years"),
    race=c('american indian or alaska native', 'asian or pacific islander', 'black or african american', 'white'),
    ethnicity=c('hispanic or latino', 'not hispanic or latino'),
    sex=c('male','female')
  ))


census.manager$register.ontology(
  'cdc.fertility',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('15-19 years', '20-24 years', '25-29 years', '30-34 years','35-39 years', '40-44 years'),
    race=c('american indian or alaska native', 'asian or pacific islander', 'black or african american',   'white'),
    ethnicity = c('hispanic or latino', 'not hispanic or latino')
  ))

#Codes:
source('data_processing/census.manager/section3_births_and_deaths.R') #This pulls birth and death data from CDC Wonder#
source('data_processing/census.manager/section3_fertility.rate.R') #Fertility rates and female.population
source('data_processing/census.manager/section3_mortality.cdc.wonder.R') #National Level 'metro' deaths
source('data_processing/census.manager/section3_state.deaths.R') #This is for 'state.deaths' which mirrors metro.deaths

#Save:
save(census.manager, file="Q:/data_managers/data.manager.merge/census.manager_section3.rdata")
