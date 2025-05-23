
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
  'adap.proportion',  #Decided to make this a ratio bc some states have more adap than non and it creates a proportion >1
  metadata = create.outcome.metadata(
    scale = 'ratio',
    display.name = 'ADAP Ratio',
    axis.name = 'ADAP Ratio',
    units = '%',
    display.as.percent = T,
    description = "ADAP Ratio"), denominator.outcome = 'non.adap.clients')

data.manager$register.outcome(
  'oahs.clients', #was previously ambulatory.care.past.year
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Received Ambulatory Care in Past Year',
    axis.name = 'Received Ambulatory Care in Past Year',
    units = 'population',
    description = "Received Ambulatory Care in Past Year"))

data.manager$register.outcome(
  'oahs.suppression', #previously non.adap.viral.suppression
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Non-ADAP Viral Suppression',
    axis.name = 'Non-ADAP Viral Suppression',
    units = '%',
    description = "Non-ADAP Viral Suppression"), denominator.outcome = 'oahs.clients')

data.manager$register.outcome(
  'adap.suppression', 
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'ADAP Viral Suppression',
    axis.name = 'ADAP Viral Suppression',
    units = '%',
    description = "ADAP Viral Suppression"), denominator.outcome = 'adap.clients')

data.manager$register.outcome(
  'diagnosed.prevalence', #Changing this from prevalence to diagnosed.prevalence bc CDC's prevalence only includes people who know their status#
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Diganosed Prevalence',
    axis.name = 'Diganosed Prevalence (n)',
    units = 'cases',
    description = "Diagnosed HIV Prevalence"))

data.manager$register.outcome(
  'adap.proportion.of.diagnosed',
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'ADAP Proportion of Diagnosed',
    axis.name = 'ADAP Proportion of Diagnosed',
    units = '%',
    description = "ADAP Proportion of Diagnosed"), denominator.outcome = 'diagnosed.prevalence')

data.manager$register.outcome(
  'adap.suppressed.proportion.of.diagnosed',
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'ADAP Suppressed Proportion of Diagnosed',
    axis.name = 'ADAP Suppressed Proportion of Diagnosed',
    units = '%',
    description = "ADAP Suppressed Proportion of Diagnosed"), denominator.outcome = 'diagnosed.prevalence')

#Register Sources:
data.manager$register.parent.source('HRSA', full.name = 'Health Resources and Services Administration', short.name= "HRSA") #parent
data.manager$register.parent.source('NASTAD', full.name = 'National Alliance of State and Territorial AIDS Directors', short.name= "NASTAD") #parent
data.manager$register.parent.source('NHSS', full.name = 'National HIV Surveillance System', short.name= "NHSS") #parent

data.manager$register.source('ryan.white.program', parent.source= "HRSA", full.name = "Ryan White HIV/AIDS Program Annual Data Report", short.name='ryan.white.program') #child
data.manager$register.source('nastad.adap', parent.source= "NASTAD", full.name = "ADAP Monitoring Project Annual Report", short.name='nastad.adap') #child
data.manager$register.source('cdc.hiv', parent.source= "NHSS", full.name = "CDC HIV Outcomes Data", short.name='cdc.hiv')


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

data.manager$register.ontology(
  'cdc',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
    race=c('american indian/alaska native', 'asian', 'black/african american', 'hispanic/latino', 'native hawaiian/other pacific islander', 'white'),
    sex=c('male','female'),
    risk=c('msm','idu','msm_idu','heterosexual','other')
  ))

# Source ------------------------------------------------------------------
source('data_processing/ryan.white.data.manager/ryan.white.total.and.stratified.level.R') #oucome = non.adap.clients; adap.clients
source('data_processing/ryan.white.data.manager/ryan.white.oahs.suppression.R') #outcome = oahs.suppression
source('data_processing/ryan.white.data.manager/ryan.white.ambulatory.R') #outcome = oahs.clients
source('data_processing/ryan.white.data.manager/ryan.white.adap.proportion.R') #outcome = adap.proportion
source('data_processing/ryan.white.data.manager/ryan.white.adap.suppression.R') #outcome = adap.suppression
source('data_processing/ryan.white.data.manager/proportions.of.diagnosed.R') #outcomes = diagnosed.prevalence; adap.proportion.of.diagnosed; adap.suppressed.proportion.of.diagnosed

# Save --------------------------------------------------------------------
save(data.manager, file="Q:/data_managers/ryan.white.data.manager.rdata")
save(data.manager, file = '../../cached/ryan.white.data.manager.rdata')

# Source Todd's Additional Cleaning Code ----------------------------------
source('applications/EHE/ehe_ontology_mappings.R')
source('applications/ryan_white/ryan_white_data_ontology_mappings.R')
source('applications/ryan_white/ryan_white_data_cleanup.R')

