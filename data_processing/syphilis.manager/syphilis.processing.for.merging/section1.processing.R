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
data.manager$register.parent.source('IQVIA', full.name = 'IQVIA', short.name= "IQVIA") #parent
data.manager$register.parent.source('NHSS', full.name = 'National HIV Surveillance System', short.name= "NHSS") #parent
data.manager$register.parent.source('ACS', full.name = 'American Community Survey', short.name= "ACS") #parent
data.manager$register.parent.source('census', full.name = 'United States Census Bureau', short.name= "census") #parent
data.manager$register.parent.source('NHANES', full.name = 'National Health and Nutrition Examination Survey', short.name= "NHANES") #parent

data.manager$register.source('aidsvu', parent.source= "IQVIA", full.name = "AIDS Vu", short.name='aidsvu') #children
data.manager$register.source('cdc.aggregated.county', parent.source= "NHSS", full.name = 'CDC Aggregated County', short.name = 'cdc aggd county') #Note this is for the aggregated county data being used to represent MSAs
data.manager$register.source('cdc.aggregated.proportion', parent.source= "NHSS", full.name = 'CDC Aggregated Proportion', short.name = 'cdc agg prop')
data.manager$register.source('cdc.hiv', parent.source= "NHSS", full.name = "CDC HIV Outcomes Data", short.name='cdc.hiv')
data.manager$register.source('cdc.prep', parent.source= "IQVIA", full.name = "CDC PrEP Data", short.name='cdc.prep')
data.manager$register.source('cdc.prep.indications', parent.source= "NHANES", full.name = "CDC PrEP Indications Data", short.name='cdc.prep.indications')
data.manager$register.source('cdc.sdh', parent.source= "ACS", full.name = "Atlas Plus SDH Data", short.name='cdc.sdh')
data.manager$register.source('cdc.rural', parent.source= "census", full.name = "Atlas Plus Rural Area Data", short.name='cdc.rural')
data.manager$register.source('prep.aidsvu.aggregated.county', parent.source= "IQVIA", full.name = 'PrEP AIDS Vu Aggregated County', short.name = 'prep aidsvu aggd county') #For aggregated prep data from AIDS Vu
data.manager$register.source('prep.cdc.aggregated.county', parent.source= "IQVIA", full.name = 'PrEP CDC Aggregated County', short.name = 'prep cdc aggd county') #For aggregated prep data from Atlas Plus (CDC)
data.manager$register.source('prep.indications.aggregated.county', parent.source= "NHANES", full.name = 'PrEP Indications Aggregated County', short.name = 'prep indications aggd county') #Note this is for the aggregated county data being used to represent MSAs

#Register Ontologies:
data.manager$register.ontology( #This is for the county level SDH data (excluding rural.area)
  'cdc.sdh',
  ont = ontology(
    year= c("2018-2022"),
    location= NULL,
    incomplete.dimensions = c("year", "location") 
  ))
data.manager$register.ontology( #This is for the county level rural area data; and the national level SDH data
  'cdc.sdh.two',
  ont = ontology(
    year= NULL,
    location= NULL
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
data.manager$register.ontology(
  'cdc.new',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55-64 years', "65+ years"),
    race=c('american indian/alaska native', 'asian', 'black/african american', 'hispanic/latino', 'native hawaiian/other pacific islander', 'white'),
    sex=c('male','female'),
    risk=c('msm','idu','msm_idu','heterosexual','other')
  ))
data.manager$register.ontology(
  'aidsvu',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('>24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
    race=c('black', 'hispanic', 'white'),
    sex=c('male','female')
  ))

#Codes:
surveillance.manager = load.data.manager(name="surveillance.manager", file="../../cached/surveillance.manager.rdata")
source('data_processing/syphilis.manager/social.determinants.of.health.R')
source('data_processing/syphilis.manager/hiv.data.for.syphilis.manager.R')
source('data_processing/syphilis.manager/prep.data.R')

syphilis.manager = data.manager

#Aggregate Outcomes to MSA:
source('commoncode/locations_of_interest.R') #Source locations of interest to create MSA vectors
source('commoncode/additional_locations_of_interest.R') #Additional locations of interest
source('../jheem2/R/HELPERS_array_helpers.R') 
source('data_processing/put_msa_data_as_new_source_script.R') #This aggregates county level data to other locations

put.msa.data.as.new.source(outcome = 'hiv.diagnosed.prevalence',
                           from.source.name = 'cdc.hiv',
                           to.source.name = 'cdc.aggregated.county',
                           to.locations =  MSAS.OF.INTEREST,  #Think of this as containing location 
                           geographic.type.from = 'COUNTY',
                           geographic.type.to = 'CBSA',
                           details.for.new.data = 'estimated from county data',
                           data.manager = syphilis.manager )

put.msa.data.as.new.source(outcome = 'hiv.diagnoses',
                           from.source.name = 'cdc.hiv',
                           to.source.name = 'cdc.aggregated.county',
                           to.locations =  MSAS.OF.INTEREST,  #Think of this as containing location 
                           geographic.type.from = 'COUNTY',
                           geographic.type.to = 'CBSA',
                           details.for.new.data = 'estimated from county data',
                           data.manager = syphilis.manager)

put.msa.data.as.new.source(outcome = 'prep',
                           from.source.name = 'cdc.prep',
                           to.source.name = 'prep.cdc.aggregated.county',
                           to.locations =  MSAS.OF.INTEREST,
                           geographic.type.from = 'COUNTY',
                           geographic.type.to = 'CBSA',
                           details.for.new.data = 'estimated from county data',
                           data.manager = syphilis.manager)

put.msa.data.as.new.source(outcome = 'prep',
                           from.source.name = 'aidsvu',
                           to.source.name = 'prep.aidsvu.aggregated.county',
                           to.locations =  MSAS.OF.INTEREST,
                           geographic.type.from = 'COUNTY',
                           geographic.type.to = 'CBSA',
                           details.for.new.data = 'estimated from county data',
                           data.manager = syphilis.manager)

put.msa.data.as.new.source(outcome = 'prep.indications',
                           from.source.name = 'cdc.prep.indications',
                           to.source.name = 'prep.indications.aggregated.county',
                           to.locations =  MSAS.OF.INTEREST,
                           geographic.type.from = 'COUNTY',
                           geographic.type.to = 'CBSA',
                           details.for.new.data = 'estimated from county data',
                           data.manager = syphilis.manager)

put.msa.data.as.new.source(outcome = 'hiv.suppression',
                           from.source.name= 'cdc.hiv',
                           to.source.name = 'cdc.aggregated.proportion',
                           to.locations = MSAS.OF.INTEREST,
                           geographic.type.from = 'COUNTY',
                           geographic.type.to = 'CBSA',
                           details.for.new.data = 'estimated from county data',
                           data.manager= syphilis.manager,
                           source.for.denominator= 'cdc.hiv',
                           ontology.for.denominator= 'cdc') 


#Save:
save(syphilis.manager, file="Q:/data_managers/data.manager.merge/syphilis.manager_section1.rdata")
