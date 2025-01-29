library(jheem2)
library(locations)
library(tidyverse)
library(readxl)
library(haven)

###############################################################################

#####SECTION 2#####

###############################################################################

data.manager = create.data.manager('syphilis', description='syphilis data manager')

#Register outcomes:
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

data.manager$register.outcome(
  'primary.syphilis',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Primary Syphilis',
    axis.name = 'Primary Syphilis',
    units = 'cases',
    description = "Primary Syphilis"))

data.manager$register.outcome(
  'secondary.syphilis',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Secondary Syphilis',
    axis.name = 'Secondary Syphilis',
    units = 'cases',
    description = "Secondary Syphilis"))

data.manager$register.outcome(
  'late.latent.syphilis',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Late Latent Syphilis',
    axis.name = 'Late Latent Syphilis',
    units = 'cases',
    description = "Late Latent Syphilis"))

data.manager$register.outcome(
  'all.syphilis.cases',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'All Syphilis Cases',
    axis.name = 'All Syphilis Cases',
    units = 'cases',
    description = "All Syphilis Cases"))

#Register Sources:
data.manager$register.parent.source('NHSS', full.name = 'National HIV Surveillance System', short.name= "NHSS") #parent
data.manager$register.parent.source('NNDSS', full.name = 'National Notifiable Disease Surveillance System', short.name= "NNDSS") #parent
data.manager$register.parent.source('DHHS', full.name = 'U.S. Department of Health and Human Services', short.name= "DHHS") #parent

data.manager$register.source('cdc.sti', parent.source= "NNDSS", full.name = "Atlas Plus STI Data", short.name='cdc.sti')
data.manager$register.source('cdc.aggregated.county', parent.source= "NHSS", full.name = 'CDC Aggregated County', short.name = 'cdc aggd county') #Note this is for the aggregated county data being used to represent MSAs
data.manager$register.source('cdc.sti.surveillance.reports', parent.source= "DHHS", full.name = "CDC Sexually Transmitted Disease Surveillance", short.name='cdc.sti.surveillance.reports')

#Register Ontologies:
data.manager$register.ontology(
  'cdc.sti',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('13-14 years', '15-19 years', '20-24 years', '25-29 years', '30-34 years', '35-39 years', '40-44 years', '45-54 years', '55-64 years', '65+ years'),
    race=c('american indian/alaska native', 'asian', 'black/african american', 'hispanic/latino', 'native hawaiian/other pacific islander', 'white'),
    sex=c('male','female'),
    risk=c('msm','idu','msm_idu','heterosexual','other')
  ))
data.manager$register.ontology(  #Create a separate ontology for early syphilis

  'cdc.syphilis',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('13-14 years', '15-24 years', '25-34 years', '35-44 years', '45-54 years', '55-64 years', '65+ years', "Unknown"),
    race=c('american indian/alaska native', 'asian', 'black/african american', 'hispanic/latino', 'native hawaiian/other pacific islander', 'white'),
    sex=c('male','female'),
    risk=c('msm','idu','msm_idu','heterosexual','other')
  ))

#Codes:
source('data_processing/syphilis.manager/syphilis.data.R')
source('data_processing/syphilis.manager/cdc.sti.surveillance.reports.processing.2003.2017.R') #This pulls one table from 2003-2017 reports, Parastu wanted this report where primary and secondary are reported separate
source('data_processing/syphilis.manager/cdc.pdf.reports.1941.2022.R') #This pulls one table from a 2022 report that reports cases back to 1941
source('data_processing/syphilis.manager/cdc.pdf.reports.1997.2003.R') #This pulls syphilis data from the 1990s

# Aggregate Outcomes to MSA 
syphilis.manager = data.manager

source('commoncode/locations_of_interest.R') #Source locations of interest to create MSA vectors
source('commoncode/additional_locations_of_interest.R') #Additional locations of interest
source('../jheem2/R/HELPERS_array_helpers.R') 
source('data_processing/put_msa_data_as_new_source_script.R') #This aggregates county level data to other locations

put.msa.data.as.new.source(outcome = 'ps.syphilis',
                           from.source.name = 'cdc.sti',
                           to.source.name = 'cdc.aggregated.county',
                           to.locations =  MSAS.OF.INTEREST,  #Think of this as containing location 
                           geographic.type.from = 'COUNTY',
                           geographic.type.to = 'CBSA',
                           details.for.new.data = 'estimated from county data',
                           data.manager = syphilis.manager)

put.msa.data.as.new.source(outcome = 'early.syphilis',
                           from.source.name = 'cdc.sti',
                           to.source.name = 'cdc.aggregated.county',
                           to.locations =  MSAS.OF.INTEREST,  #Think of this as containing location 
                           geographic.type.from = 'COUNTY',
                           geographic.type.to = 'CBSA',
                           details.for.new.data = 'estimated from county data',
                           data.manager = syphilis.manager)

#Save:
save(syphilis.manager, file="Q:/data_managers/data.manager.merge/syphilis.manager_section2.rdata")
