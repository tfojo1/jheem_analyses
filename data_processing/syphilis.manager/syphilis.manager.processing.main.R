#Main Data Processing file for the Syphilis Manager

library(jheem2)
library(locations)
library(tidyverse)

# Create Syphilis Manager -------------------------------------------------

data.manager = create.data.manager('syphilis', description='syphilis data manager')

data.manager$register.outcome(
  'uninsured',
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Uninsured',
    axis.name = 'Uninsured',
    units = '%',
    description = "Uninsured"), denominator.outcome = 'uninsured.denominator')

data.manager$register.outcome(
  'uninsured.denominator',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Uninsured Denominator',
    axis.name = 'Uninsured Denominator',
    units = 'count',
    description = "Uninsured Denominator"))

data.manager$register.outcome(
  'vacant.housing',
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Vacant Housing Units',
    axis.name = 'Vacant Housing Units',
    units = '%',
    description = "Vacant Housing Units"), denominator.outcome = 'vacant.housing.denominator')

data.manager$register.outcome(
  'vacant.housing.denominator',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Vacant Housing Denominator',
    axis.name = 'Vacant Housing Denominator',
    units = 'count',
    description = "Vacant Housing Denominator"))

data.manager$register.outcome(
  'not.highschool.graduate',
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Population 25 years and older without HS diploma',
    axis.name = 'Population 25 years and older without HS diploma',
    units = '%',
    description = "Population 25 years and older without HS diploma"), denominator.outcome = 'not.highschool.graduate.denominator')

data.manager$register.outcome(
  'not.highschool.graduate.denominator',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Population 25 years and older without HS diploma Denominator',
    axis.name = 'Population 25 years and older without HS diploma Denominator',
    units = 'count',
    description = "Population 25 years and older without HS diploma Denominator"))

data.manager$register.outcome(
  'below.fpl',
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Households living below the federal poverty level',
    axis.name = 'Households living below the federal poverty level',
    units = '%',
    description = "Households living below the federal poverty level"), denominator.outcome = 'below.fpl.denominator')

data.manager$register.outcome(
  'below.fpl.denominator',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Households living below the federal poverty level Denominator',
    axis.name = 'Households living below the federal poverty level Denominator',
    units = 'count',
    description = "Households living below the federal poverty level Denominator"))

data.manager$register.outcome(
  'rural.area',
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Population living in a rural area',
    axis.name = 'Population living in a rural area',
    units = '%',
    description = "Population living in a rural area"), denominator.outcome = 'rural.area.denominator')

data.manager$register.outcome(
  'rural.area.denominator',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Population living in a rural area Denominator',
    axis.name = 'Population living in a rural area Denominator',
    units = 'count',
    description = "Population living in a rural area Denominator"))

data.manager$register.outcome(
  'congenital.syphilis',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Congenital Syphilis',
    axis.name = 'Congenital Syphilis',
    units = 'cases',
    description = "Congenital Syphilis"))

data.manager$register.outcome(
  'early.syphilis',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Early, non-primary, non-Secondary Syphilis',
    axis.name = 'Early, non-primary, non-Secondary Syphilis',
    units = 'cases',
    description = "Early, non-primary, non-Secondary Syphilis"))

data.manager$register.outcome(
  'ps.syphilis',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Primary and Secondary Syphilis',
    axis.name = 'Primary and Secondary Syphilis',
    units = 'cases',
    description = "Primary and Secondary Syphilis"))

data.manager$register.outcome(
  'unknown.duration.or.late.syphilis',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Unknown Duration or Late Syphilis',
    axis.name = 'Unknown Duration or Late Syphilis',
    units = 'cases',
    description = "Unknown Duration or Late Syphilis"))


# Create Sources + Parent Sources -----------------------------------------

##Register "Parent" Sources
data.manager$register.parent.source('ACS', full.name = 'American Community Survey', short.name= "ACS")
data.manager$register.parent.source('NNDSS', full.name = 'National Notifiable Disease Surveillance System', short.name= "NNDSS")
data.manager$register.parent.source('census', full.name = 'United States Census Bureau', short.name= "census")
data.manager$register.parent.source('USDA', full.name = 'Economic Research Services of the United States Department of Agriculture', short.name= "USDA")


##Register Data Sources ('children')
data.manager$register.source('cdc.sti', parent.source= "NNDSS", full.name = "Atlas Plus STI Data", short.name='cdc.sti')
data.manager$register.source('cdc.sdh', parent.source= "ACS", full.name = "Atlas Plus SDH Data", short.name='cdc.sdh')
data.manager$register.source('cdc.rural', parent.source= "census", full.name = "Atlas Plus Rural Area Data", short.name='cdc.rural')
data.manager$register.source('cdc.urban', parent.source= "USDA", full.name = "Atlas Plus Urbanization Level", short.name='cdc.urban')


# Establish Ontologies ----------------------------------------------------

data.manager$register.ontology(
  'cdc.shd',
  ont = ontology(
    year= c("2018-2022"),
    location= NULL,
    incomplete.dimensions = c("year", "location") #Is this the right way to code the 5 year ranges?
  ))

data.manager$register.ontology(
  'cdc.sti',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('13-14 years', '15-19 years', '20-24 years', '25-29 years', '30-34 years', '35-39 years', '40-44 years', '45-54 years', '55-64 years', '65+ years'),
    race=c('American Indian/Alaska Native', 'Asian', 'Black/African American', 'Hispanic/Latino', 'Multiracial', 'Native Hawaiian/Other Pacific Islander', 'White'),
    sex=c('male','female'),
    risk=c('msm','idu','msm_idu','heterosexual','other')
  ))


# Source Data Cleaning and Processing Files -------------------------------

source('data_processing/syphilis.manager/social.determinants.of.health.R')
source('data_processing/syphilis.manager/syphilis.data.R')
