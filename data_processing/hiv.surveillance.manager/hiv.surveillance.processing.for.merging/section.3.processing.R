
library(jheem2)
library(tidyverse)
library(readxl)
library(stringr)
library(haven)
library(locations)
library(tools)

###############################################################################

#####SECTION 3#####
#Proportion.tested; proportion.msm (mainly data from BRFSS)

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
  'proportion.tested.n',           #Will have option in code to make this only people at risk or everyone#
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Denominator value for proportion tested in past year',
    axis.name = 'Denominator value for proportion tested in past year',
    units = '%',
    description = "Denominator value for proportion tested in past year"))

data.manager$register.outcome(
  'proportion.tested', 
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Proportion Tested in Past Year',
    axis.name = 'Proportion Tested in Past Year',
    units = '%',
    description = "Proportion of People who have received an HIV test in the last year"), denominator.outcome = 'proportion.tested.n')

data.manager$register.outcome(
    'proportion.msm.n',           
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Proportion msm n value from BRFSS',
        axis.name = 'Proportion msm n value from BRFSS',
        units = '%',
        description = "Proportion msm n value from BRFSS"))

data.manager$register.outcome(
    'proportion.msm', 
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Proportion of MSM',
        axis.name = 'Proportion of MSM',
        units = '%',
        description = "Proportion of Men who have sex with Men"), denominator.outcome = 'adult.population')

data.manager$register.outcome(
  'unweighted.denominator',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'unweighted.denominator',
    axis.name = 'unweighted.denominator)',
    units = 'cases',
    description = "BRFSS Unweighted Denominator Value"))

#Register Sources:
data.manager$register.parent.source('BRFSS', full.name = 'Behavioral Risk Factor Surveillance System', short.name= "BRFSS") #parent
data.manager$register.parent.source('ACS', full.name = 'American Community Survey', short.name= "ACS") #parent

data.manager$register.source('brfss', parent.source= "BRFSS", full.name = "Behavioral Risk Factor Surveillance System", short.name='brfss') #child
data.manager$register.source('emory', parent.source= "ACS", full.name = "Emory University", short.name='emory') #child
data.manager$register.source('emory.aggregated', parent.source= "ACS", full.name = "Emory University- Aggregated", short.name='emory.aggregated') 

#Register Ontologies:
data.manager$register.ontology(
  'brfss',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('18-24 years', '25-29 years', '30-34 years', '35-39 years', '40-44 years', '45-49 years', '50-54 years', '55-59 years', '60-64 years', '65-69 years', '70-74 years', '75-79 years', '80+ years'),
    race=c('white', 'black', 'american indian/alaska native', 'asian', 'native hawaiian/other pacific islander', 'hispanic', 'other race'),
    sex=c('male','female'),
    risk=c('msm', 'not_msm')
  ))

data.manager$register.ontology(
  'emory',
  ont = ontology(
    year= NULL,
    location= NULL,
    sex=c('male', 'female') #needs to have male and female here to represent all possible outcomes
  ))

#Codes:
source('data_processing/hiv.surveillance.manager/brfss_state_weighted.R') #Source BRFSS testing data
source('data_processing/hiv.surveillance.manager/brfss_msa_weighted.R') #Source BRFSS testing data
source('data_processing/hiv.surveillance.manager/msm.R') #Source msm
source('data_processing/hiv.surveillance.manager/brfss_unweighted_denominators.R') #Source BRFSS Unweighted Denominator values

#Save:
save(data.manager, file="Q:/data_managers/data.manager.merge/surveillance.manager_section3.rdata")