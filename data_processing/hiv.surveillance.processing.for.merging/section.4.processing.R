
library(jheem2)
library(tidyverse)
library(readxl)
library(stringr)
library(haven)
library(locations)
library(tools)

###############################################################################

#####SECTION 4#####
#STI data + NSDUH data (drugs+depression)
###############################################################################

data.manager = create.data.manager('surveillance', description='surveillance data manager')

data.manager$register.outcome(
  'adult.population',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Adult Population',
    axis.name = 'Adult Population',
    units = 'population',
    description = "Adult Population Estimate, Ages 13 and over"))

#Register outcomes:
data.manager$register.outcome(
  'gonorrhea',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Gonorrhea',
    axis.name = 'Gonorrhea',
    units = 'cases',
    description = "Gonorrhea"))

data.manager$register.outcome(
  'gonorrhea.ratio',
  denominator.lags.by.one.year = T,
  metadata = create.outcome.metadata(
    scale = 'ratio',
    display.name = 'Gonorrhea Ratio',
    axis.name = 'Gonorrhea Ratio',
    units = 'cases',
    description = "Year Over Year Ratio of Gonorrhea Cases"), denominator.outcome = 'gonorrhea')

data.manager$register.outcome(
  'ps.syphilis',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Primary and Secondary Syphilis',
    axis.name = 'Primary and Secondary Syphilis',
    units = 'cases',
    description = "Primary and Secondary Syphilis"))

data.manager$register.outcome(
  'ps.syphilis.ratio',
  denominator.lags.by.one.year = T,
  metadata = create.outcome.metadata(
    scale = 'ratio',
    display.name = 'Syphilis Ratio',
    axis.name = 'Syphilis Ratio',
    units = 'cases',
    description = "Year Over Year Ratio of Primary and Secondary Syphilis Cases"), denominator.outcome = 'ps.syphilis')

data.manager$register.outcome(
  'early.syphilis',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Early, non-primary, non-Secondary Syphilis',
    axis.name = 'Early, non-primary, non-Secondary Syphilis',
    units = 'cases',
    description = "Early, non-primary, non-Secondary Syphilis"))

data.manager$register.outcome(
  'congenital.syphilis',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Congenital Syphilis',
    axis.name = 'Congenital Syphilis',
    units = 'cases',
    description = "Congenital Syphilis"))

data.manager$register.outcome(
  'unknown.duration.or.late.syphilis',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Unknown Duration or Late Syphilis',
    axis.name = 'Unknown Duration or Late Syphilis',
    units = 'cases',
    description = "Unknown Duration or Late Syphilis"))

data.manager$register.outcome(
  'heroin', #can change to heroin use but leave display name the same#
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Heroin Use in the Past Year',
    axis.name = 'Heroin Use in the Past Year',
    units = '%',
    description = "Heroin Use in the Past Year"), denominator.outcome = 'adult.population')

data.manager$register.outcome(
  'cocaine', 
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Cocaine use in the Past Year',
    axis.name = 'Cocaine use in the Past Year',
    units = '%',
    description = "Cocaine Use in the Past Year"), denominator.outcome = 'adult.population')

data.manager$register.outcome(
  'depression',
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Major Depressive Episode in the Past Year',
    axis.name = 'Depression',
    units =  '%',
    description = "Major Depressive Episode in the Past Year"), denominator.outcome = 'adult.population')

#Register Sources:
data.manager$register.parent.source('NHSS', full.name = 'National HIV Surveillance System', short.name= "NHSS") #parent
data.manager$register.parent.source('NSDUH', full.name = 'National Survey on Drug Use and Health', short.name= "NSDUH") #parent
data.manager$register.parent.source('NNDSS', full.name = 'National Notifiable Disease Surveillance System', short.name= "NNDSS") #parent

data.manager$register.source('cdc.sti', parent.source= "NNDSS", full.name = "CDC STI Data", short.name='cdc.sti') #child
data.manager$register.source('nsduh', parent.source= "NSDUH", full.name = "National Survey on Drug Use and Health", short.name='nsduh') #child 
data.manager$register.source('cdc.aggregated.county', parent.source= "NHSS", full.name = 'CDC Aggregated County', short.name = 'cdc aggd county') #child #Note this is for the aggregated county data being used to represent MSAs


#Register Ontologies:
data.manager$register.ontology(   #This is for the Atlas Plus STI data, creating a separate ontology bc age groups are different#
  'cdc.sti', 
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('13-14 years', '15-19 years', '20-24 years', '25-29 years', '30-34 years', '35-39 years', '40-44 years', '45-54 years', '55-64 years', '65+ years'),
    race=c('american indian/alaska native', 'asian', 'black/african american', 'hispanic/latino', 'native hawaiian/other pacific islander', 'white'),
    sex=c('male','female'),
    risk=c('msm','idu','msm_idu','heterosexual','other')
  ))

data.manager$register.ontology(   #Create a separate ontology for early syphilis and for late duration or unknown
  'cdc.syphilis',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('13-14 years', '15-24 years', '25-34 years', '35-44 years', '45-54 years', '55-64 years', '65+ years'),
    race=c('american indian/alaska native', 'asian', 'black/african american', 'hispanic/latino', 'native hawaiian/other pacific islander', 'white'),
    sex=c('male','female'),
    risk=c('msm','idu','msm_idu','heterosexual','other')
  ))

data.manager$register.ontology(
  'nsduh',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('13-24', '25+')))

#Codes:
source('data_processing/sti_processing.R') #STI data
source('data_processing/nsduh_processing_new.R') #NSDUH processing

#Aggregate Outcomes:
source('data_processing/put_msa_data_as_new_source_script.R') #Sources function to aggregate county data to MSA
source('../jheem2/R/HELPERS_array_helpers.R') #Necessary array helpers
source('commoncode/locations_of_interest.R') #Creates MSAS.OF.INTEREST

surveillance.manager = data.manager

put.msa.data.as.new.source(outcome = 'gonorrhea',
                           from.source.name = 'cdc.sti',
                           to.source.name = 'cdc.aggregated.county',
                           to.locations =  MSAS.OF.INTEREST,
                           geographic.type.from = 'COUNTY',
                           geographic.type.to = 'CBSA',
                           details.for.new.data = 'estimated from county data',
                           data.manager = surveillance.manager)

put.msa.data.as.new.source(outcome = 'ps.syphilis',
                           from.source.name = 'cdc.sti',
                           to.source.name = 'cdc.aggregated.county',
                           to.locations =  MSAS.OF.INTEREST,
                           geographic.type.from = 'COUNTY',
                           geographic.type.to = 'CBSA',
                           details.for.new.data = 'estimated from county data',
                           data.manager = surveillance.manager)

source('data_processing/sti_ratio_calculation.R') #Calculates STI Ratio data

#Save:
save(surveillance.manager, file="Q:/data_managers/data.manager.merge/surveillance.manager_section4.rdata")