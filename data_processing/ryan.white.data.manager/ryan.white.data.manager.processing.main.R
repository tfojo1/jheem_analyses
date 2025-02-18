
library(jheem2)
library(tidyverse)
library(locations)

data.manager = create.data.manager('ryan.white.data.manager', description='ryan.white.data.manager')

#Register outcomes:

data.manager$register.outcome(
  'adap.clients',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'ADAP Clients',
    axis.name = 'ADAP Clients',
    units = 'population',
    description = "AIDS Drug Assistance Program Clients"))

data.manager$register.outcome(
  'non.adap.clients',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Non-ADAP Clients',
    axis.name = 'Non-ADAP Clients',
    units = 'population',
    description = "Non-ADAP Clients"))

data.manager$register.outcome(
  'oahs.clients', #was previously ambulatory.care.past.year
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Received Ambulatory Care in Past Year',
    axis.name = 'Received Ambulatory Care in Past Year',
    units = 'population',
    description = "Received Ambulatory Care in Past Year"))

data.manager$register.outcome(
  'non.adap.viral.suppression',
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Non-ADAP Viral Suppression',
    axis.name = 'Non-ADAP Viral Suppression',
    units = '%',
    description = "Non-ADAP Viral Suppression"), denominator.outcome = 'oahs.clients')

#Register Sources:
data.manager$register.parent.source('HRSA', full.name = 'Health Resources and Services Administration', short.name= "HRSA") #parent
data.manager$register.source('ryan.white.program', parent.source= "HRSA", full.name = "Ryan White HIV/AIDS Program Annual Data Report", short.name='ryan.white.program') #child

#Register Ontologies:
data.manager$register.ontology(
  'ryan.white.pdfs',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55-64 years', '65+ years'),
    race=c('white', 'black', 'american indian alaska native', 'native hawaiian pacific islander', 'hispanic', 'asian'),
    sex=c('male','female'),
    risk = c('msm', "msm_idu", 'heterosexual', 'other', 'idu'),
    fpl = c('0-100', '101-138', '139-250', '251-400', '>400'),
    service.received = c('full pay medication support', 'insurance premium assistance', 'medication co pay/deductible', 'multiple services')
  ))

# Source ------------------------------------------------------------------
source('data_processing/ryan.white.data.manager/ryan.white.total.and.stratified.level.R')
source('data_processing/ryan.white.data.manager/ryan.white.viral.suppression.R')
source('data_processing/ryan.white.data.manager/ryan.white.ambulatory.R')

# Save --------------------------------------------------------------------

save(data.manager, file="Q:/data_managers/ryan.white.data.manager.rdata")



