library(jheem2)
library(locations)
library(tidyverse)
library(readxl)
library(haven)

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

#Register Sources:

#Register Ontologies:

#Codes:
source('data_processing/syphilis.manager/cached.census.data.R')
source('data_processing/syphilis.manager/cached.fertility.data.R')

#Save:
save(data.manager, file="Q:/data_managers/data.manager.merge/syphilis.manager_section1.rdata")