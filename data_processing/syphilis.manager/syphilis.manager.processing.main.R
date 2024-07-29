#Main Data Processing file for the Syphilis Manager

library(jheem2)
library(locations)
library(tidyverse)


# Create Syphilis Manager -------------------------------------------------



data.manager = create.data.manager('surveillance', description='surveillance data manager')

data.manager$register.outcome(
  'adult.population',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Adult Population',
    axis.name = 'Adult Population',
    units = 'population',
    description = "Adult Population Estimate, Ages 13 and over"))


# Create Sources + Parent Sources -----------------------------------------


data.manager$register.parent.source('IQVIA', full.name = 'IQVIA', short.name= "IQVIA")

data.manager$register.source('aidsvu', parent.source= "IQVIA", full.name = "AIDS Vu", short.name='aidsvu')


# Establish Ontologies ----------------------------------------------------


data.manager$register.ontology(
  'cdc',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
    race=c('American Indian/Alaska Native', 'Asian', 'Black/African American', 'Hispanic/Latino', 'Multiracial', 'Native Hawaiian/Other Pacific Islander', 'White'),
    sex=c('male','female'),
    risk=c('msm','idu','msm_idu','heterosexual','other')
  ))


# Source Data Cleaning and Processing Files -------------------------------


