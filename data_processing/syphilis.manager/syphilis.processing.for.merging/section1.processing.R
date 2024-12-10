library(jheem2)
library(locations)
library(tidyverse)
library(readxl)
library(haven)

###############################################################################

#####SECTION 1#####

###############################################################################

data.manager = create.data.manager('syphilis', description='syphilis data manager')


#Register outcomes:


data.manager$register.outcome(
  'uninsured.denominator',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Uninsured Denominator',
    axis.name = 'Uninsured Denominator',
    units = 'count',
    description = "Uninsured Denominator"))

data.manager$register.outcome(
  'uninsured',
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Uninsured',
    axis.name = 'Uninsured',
    units = '%',
    description = "Uninsured"), denominator.outcome = 'uninsured.denominator')

data.manager$register.outcome(
  'vacant.housing.denominator',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Vacant Housing Denominator',
    axis.name = 'Vacant Housing Denominator',
    units = 'count',
    description = "Vacant Housing Denominator"))

data.manager$register.outcome(
  'vacant.housing',
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Vacant Housing Units',
    axis.name = 'Vacant Housing Units',
    units = '%',
    description = "Vacant Housing Units"), denominator.outcome = 'vacant.housing.denominator')

data.manager$register.outcome(
  'not.highschool.graduate.denominator',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Population 25 years and older without HS diploma Denominator',
    axis.name = 'Population 25 years and older without HS diploma Denominator',
    units = 'count',
    description = "Population 25 years and older without HS diploma Denominator"))

data.manager$register.outcome(
  'not.highschool.graduate',
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Population 25 years and older without HS diploma',
    axis.name = 'Population 25 years and older without HS diploma',
    units = '%',
    description = "Population 25 years and older without HS diploma"), denominator.outcome = 'not.highschool.graduate.denominator')

data.manager$register.outcome(
  'below.fpl.denominator',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Households living below the federal poverty level Denominator',
    axis.name = 'Households living below the federal poverty level Denominator',
    units = 'count',
    description = "Households living below the federal poverty level Denominator"))

data.manager$register.outcome(
  'below.fpl',
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Households living below the federal poverty level',
    axis.name = 'Households living below the federal poverty level',
    units = '%',
    description = "Households living below the federal poverty level"), denominator.outcome = 'below.fpl.denominator')

data.manager$register.outcome(
  'rural.area.denominator',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Population living in a rural area Denominator',
    axis.name = 'Population living in a rural area Denominator',
    units = 'count',
    description = "Population living in a rural area Denominator"))

data.manager$register.outcome(
  'rural.area',
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Population living in a rural area',
    axis.name = 'Population living in a rural area',
    units = '%',
    description = "Population living in a rural area"), denominator.outcome = 'rural.area.denominator')

data.manager$register.outcome(
  'hiv.diagnoses',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'New Diagnoses',
    axis.name = 'New Diagnoses (n)',
    units = 'cases',
    description = "New HIV Cases Diagnosed in a Year"))

data.manager$register.outcome(
  'hiv.diagnosed.prevalence', #Changing this from prevalence to diagnosed.prevalence bc CDC's prevalence only includes people who know their status#
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Diganosed Prevalence',
    axis.name = 'Diganosed Prevalence (n)',
    units = 'cases',
    description = "Diagnosed HIV Prevalence"))

data.manager$register.outcome(  #Adding this as a denominator value for awareness/knowledge of status- bc the cdc denominator is an estimated prevalence value of both known and unknown status#
  'hiv.total.prevalence',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Total Prevalence',
    axis.name = 'Total Prevalence',
    units = 'cases',
    description = "Estimated Prevalence of Known and Unknown Status"))

data.manager$register.outcome(
  'hiv.engagement', #changed from receipt to engagement
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Engagement in Care',
    axis.name = 'Proportion of Engaged in Care',
    units = '%',
    description = "Engagement in  HIV medical care"), denominator.outcome = 'hiv.diagnosed.prevalence')

data.manager$register.outcome(
  'hiv.suppression', 
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Viral Suppression',
    axis.name = 'Proportion Virally Suppressed',
    units = '%',
    description = "HIV Viral Suppression"), denominator.outcome = 'hiv.diagnosed.prevalence')

data.manager$register.outcome(
  'prep', 
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Prescribed PrEP',
    axis.name = 'Number Prescribed PrEP',
    units = 'cases',
    description = "Number Prescribed PrEP"))

data.manager$register.outcome(
  'prep.indications', 
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'PrEP Indications',
    axis.name = 'Number with PrEP Indications',
    units = 'cases',
    description = "Estimated Number of Persons with PrEP Indications"))

#Register Sources:

#Register Ontologies:

#Codes:
surveillance.manager = load.data.manager(name="surveillance.manager", file="../../cached/surveillance.manager.rdata")
source('data_processing/syphilis.manager/social.determinants.of.health.R')
source('data_processing/syphilis.manager/hiv.data.for.syphilis.manager.R')
source('data_processing/syphilis.manager/prep.data.R')

#Save:
save(data.manager, file="Q:/data_managers/data.manager.merge/syphilis.manager_section1.rdata")