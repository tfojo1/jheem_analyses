
library(jheem2)
library(tidyverse)
library(readxl)
library(stringr)
library(haven)
library(locations)
library(tools)

###############################################################################

#####SECTION 5#####
#AIDS data
#Also adding data from CDC Surveillance Reports that include diagnosed.prevalence
#hiv.deaths and diagnoses
###############################################################################

data.manager = create.data.manager('surveillance', description='surveillance data manager')

#Register outcomes:
data.manager$register.outcome(
  'aids.diagnoses',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'AIDS Diagnoses',
    axis.name = 'AIDS Diagnoses',
    units = 'cases',
    description = "AIDS Diagnoses"))

data.manager$register.outcome(
  'aids.diagnosed.prevalence', #changed from aids.diagnosis to aids.diagnosed.prevalence
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'AIDS Diagnosed Prevalence',
    axis.name = 'AIDS Diagnosed Prevalence',
    units = 'cases',
    description = "AIDS Diagnosed Prevalence"))

data.manager$register.outcome(
  'aids.diagnoses.alive.by.2001',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'AIDS Diagnoses Alive by 2001',
    axis.name = 'AIDS Diagnoses Alive by 2001',
    units = 'population',
    description = "AIDS Diagnoses Alive by 2001"))

data.manager$register.outcome(
  'aids.diagnoses.deceased.by.2001',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'AIDS Diagnoses Deceased by 2001',
    axis.name = 'AIDS Diagnoses Deceased by 2001',
    units = 'population',
    description = "AIDS Diagnoses Deceased by 2001"))

data.manager$register.outcome(
  'aids.deaths',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'AIDS Deaths',
    axis.name = 'AIDS Deaths',
    units = 'population',
    description = "AIDS Deaths"))
data.manager$register.outcome(
  'diagnoses',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'New Diagnoses',
    axis.name = 'New Diagnoses (n)',
    units = 'cases',
    description = "New HIV Cases Diagnosed in a Year"))

data.manager$register.outcome(
  'hiv.deaths',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'HIV Deaths',
    axis.name = 'HIV Deaths (n)',
    units = 'cases',
    description = "HIV Deaths"))

data.manager$register.outcome(
  'diagnosed.prevalence', #Changing this from prevalence to diagnosed.prevalence bc CDC's prevalence only includes people who know their status#
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Diganosed Prevalence',
    axis.name = 'Diganosed Prevalence (n)',
    units = 'cases',
    description = "Diagnosed HIV Prevalence"))

#Register Sources:
data.manager$register.parent.source('NHSS', full.name = 'National HIV Surveillance System', short.name= "NHSS") #parent

data.manager$register.source('cdc.surveillance.reports', parent.source= "NHSS", full.name = "CDC HIV Surveillance Report", short.name='cdc.surveillance.reports') #child
data.manager$register.source('cdc.aids', parent.source= "NHSS", full.name = "CDC Wonder AIDS Public Information Data", short.name='cdc.aids') #child

#Register Ontologies:

data.manager$register.ontology(
  'cdc.msa.reports',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
    race=c('american indian/alaska native', 'asian', 'black/african american', 'hispanic/latino', 'white'),
    sex=c('male','female'),
    risk=c('msm','idu','msm_idu','heterosexual','other')
    
  ))
data.manager$register.ontology(
  'cdc.aids',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c( '< 1 year', '1-12 years', '13-19 years', '20-24 years', '25-29 years', '30-34 years', '35-39 years', '40-44 years', '45-49 years', '50-54 years', '55-59 years', '60-64 years','65+ years'),
    race=c('american indian/alaska native', 'asian', 'black', 'hispanic', 'white'),
    sex=c('male','female'),
    risk=c('msm','idu','msm_idu','heterosexual')
  ))

#Codes:
source('data_processing/aids_data_1981_2001.R') #CDC Wonder AIDS data from 1981-2001
source('data_processing/msa_reports_processing.R') #Source in CDC MSA PDF Reports data and cleaning

#Aggregate Outcomes:#None

#Save:
save(data.manager, file="Q:/data_managers/data.manager.merge/surveillance.manager_section5.rdata")