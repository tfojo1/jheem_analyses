#This file is being used as a draft to adjust the census processing to save as several smaller data managers that can be merged to cut processing time

library(jheem2)
library(tidyverse)
library(readr)
library(readxl) 
library(haven)
library(locations)

###############################################################################

#####POPULATION 2######

###############################################################################

census.manager = create.data.manager('census_manager', description='an additional data manager for census data')

#Register outcomes:
census.manager$register.outcome(
  'population',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Population',
    axis.name = 'Population',
    units = 'population',
    description = "Populaion Estimate"))

#Register Sources:
census.manager$register.parent.source('census', full.name = 'United States Census Bureau', short.name= "census") #parent
census.manager$register.source('census.population', parent.source= "census", full.name = "Census Population Data", short.name='census.population') #child

#Register Ontologies:
census.manager$register.ontology(
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

census.manager$register.ontology(
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

#Codes:
source('data_processing/census.manager/census.population.10.19.R') #Sourcing more recent census population data that is stratified but not by single year age just by age group which is why we added it so much later
source('data_processing/census.manager/census.population.20.23.R') #UPDATE 7-16: Temporarily commenting out 181 to source the newer stratified data I'll use this to decide if we want age groups or single year
source('data_processing/census.manager/aggregating.national.population.census.R') # Aggregating County Population to National

#Save:
save(census.manager, file="Q:/data_managers/data.manager.merge/census.manager_population2.rdata")
