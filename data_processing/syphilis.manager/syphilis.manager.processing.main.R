#Main Data Processing file for the Syphilis Manager

library(jheem2)
library(locations)
library(tidyverse)


# Create Syphilis Manager -------------------------------------------------

data.manager = create.data.manager('syphilis', description='syphilis data manager')

data.manager$register.outcome(
  'uninsured',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Uninsured',
    axis.name = 'Uninsured',
    units = 'count',
    description = "Uninsured"))

data.manager$register.outcome(
  'vacant.housing',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Vacant Housing Units',
    axis.name = 'Vacant Housing Units',
    units = 'count',
    description = "Vacant Housing Units"))

data.manager$register.outcome(
  'not.highschool.graduate',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Population 25 years and older without HS diploma',
    axis.name = 'Population 25 years and older without HS diploma',
    units = 'count',
    description = "Population 25 years and older without HS diploma"))

data.manager$register.outcome(
  'below.fpl',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Households living below the federal poverty level',
    axis.name = 'Households living below the federal poverty level',
    units = 'count',
    description = "Households living below the federal poverty level"))

data.manager$register.outcome(
  'rural.area',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Population living in a rural area',
    axis.name = 'Population living in a rural area',
    units = 'count',
    description = "Population living in a rural area"))

data.manager$register.outcome(
  'urbanization.level',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Urbanization level: rural, suburban, urban',
    axis.name = 'Urbanization level: rural, suburban, urban',
    units = 'count',
    description = "Urbanization level: rural, suburban, urban"))



# Create Sources + Parent Sources -----------------------------------------


# data.manager$register.parent.source('IQVIA', full.name = 'IQVIA', short.name= "IQVIA")
# 
# data.manager$register.source('aidsvu', parent.source= "IQVIA", full.name = "AIDS Vu", short.name='aidsvu')


# Establish Ontologies ----------------------------------------------------


# data.manager$register.ontology(
#   'cdc',
#   ont = ontology(
#     year= NULL,
#     location= NULL,
#     age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
#     race=c('American Indian/Alaska Native', 'Asian', 'Black/African American', 'Hispanic/Latino', 'Multiracial', 'Native Hawaiian/Other Pacific Islander', 'White'),
#     sex=c('male','female'),
#     risk=c('msm','idu','msm_idu','heterosexual','other')
#   ))


# Source Data Cleaning and Processing Files -------------------------------


