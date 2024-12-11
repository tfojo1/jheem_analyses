library(jheem2)
library(locations)
library(tidyverse)
library(readxl)
library(haven)

###############################################################################

#####SECTION 4#####

###############################################################################

data.manager = create.data.manager('syphilis', description='syphilis data manager')

#Register outcomes:
data.manager$register.outcome(
  'proportion.msm', 
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Proportion of MSM',
    axis.name = 'Proportion of MSM',
    units = '%',
    description = "Proportion of Men who have sex with Men"), denominator.outcome = 'population') 

data.manager$register.outcome(
  'proportion.tested.n',       
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

#Register Sources:
data.manager$register.parent.source('BRFSS', full.name = 'Behavioral Risk Factor Surveillance System', short.name= "BRFSS") #parent
data.manager$register.parent.source('ACS', full.name = 'American Community Survey', short.name= "ACS") #parent

data.manager$register.source('emory', parent.source= "ACS", full.name = "Emory University", short.name='emory') #child
data.manager$register.source('brfss', parent.source= "BRFSS", full.name = "Behavioral Risk Factor Surveillance System", short.name='brfss') #child

#Register Ontologies:
data.manager$register.ontology(
  'emory',
  ont = ontology(
    year= NULL,
    location= NULL,
    sex=c('male', 'female') #needs to have male and female here to represent all possible outcomes
  ))

data.manager$register.ontology(
  'brfss',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('18-24 years', '25-29 years', '30-34 years', '35-39 years', '40-44 years', '45-49 years', '50-54 years', '55-59 years', '60-64 years', '65-69 years', '70-74 years', '75-79 years', '80+ years'),
    race=c('white', 'black', 'american indian/alaska native', 'asian', 'native hawaiian/other pacific islander', 'other race', 'hispanic'),
    sex=c('male','female'),
    risk=c('msm', 'not_msm')
  ))

#Codes:
source('data_processing/syphilis.manager/cached.proportion.msm.R')
source('data_processing/syphilis.manager/brfss_national_weighted_tested.R') #This is used for national level proportion.tested
source('data_processing/syphilis.manager/brfss_national_weighted_msm.R') #This is used for national level proportion.msm

#Save:
save(data.manager, file="Q:/data_managers/data.manager.merge/syphilis.manager_section4.rdata")