
library(jheem2)
library(tidyverse)
library(readxl)
library(stringr)
library(haven)
library(locations)
library(tools)

###############################################################################

#####SECTION 2#####
#HIV related outcomes (retention, suppression, diagnoses, etc.)
###############################################################################

data.manager = create.data.manager('surveillance', description='surveillance data manager')

#Register outcomes:
data.manager$register.outcome(
  'diagnosed.prevalence', #Changing this from prevalence to diagnosed.prevalence bc CDC's prevalence only includes people who know their status#
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Diganosed Prevalence',
    axis.name = 'Diganosed Prevalence (n)',
    units = 'cases',
    description = "Diagnosed HIV Prevalence"))

data.manager$register.outcome(
  'diagnoses',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'New Diagnoses',
    axis.name = 'New Diagnoses (n)',
    units = 'cases',
    description = "New HIV Cases Diagnosed in a Year"))

data.manager$register.outcome(
  'adult.population',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Adult Population',
    axis.name = 'Adult Population',
    units = 'population',
    description = "Adult Population Estimate, Ages 13 and over"))

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

data.manager$register.outcome(
  'hiv.tests',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'HIV Tests',
    axis.name = 'HIV Tests (n)',
    units = 'cases',
    description = "Count of CDC Funded HIV Tests"))

data.manager$register.outcome(
  'cdc.hiv.test.positivity', #This was newly.diagnosed.positives, changing it to hiv.test.positivity put this in as a percentage not a count#
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'HIV Test Positivity',
    axis.name = 'HIV Test Positivity',
    units = '%',
    description = "HIV Test Positivity"), denominator.outcome = 'hiv.tests')

data.manager$register.outcome(
  'linkage_1mo',
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Linkage (1 month)',
    axis.name = 'Proportion Linked (1 month)',
    units = '%',
    description = "Linkage to HIV care within 1 Month"), denominator.outcome = 'diagnoses')

data.manager$register.outcome(
  'linkage_3mo',
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Linkage (3 months)',
    axis.name = 'Proportion Linked (3 months)',
    units = '%',
    description = "Linkage to HIV care within 3 Months"), denominator.outcome = 'diagnoses')

data.manager$register.outcome(
  'engagement', #changed from receipt to engagement
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Engagement in Care',
    axis.name = 'Proportion of Engaged in Care',
    units = '%',
    description = "Engagement in  HIV medical care"), denominator.outcome = 'diagnosed.prevalence')

data.manager$register.outcome(
  'retention',  #Defined as:Individuals with ≥2 tests (CD4 or VL) ≥3 months apart#
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Retention',
    axis.name = 'Proportion Retained in Care',
    units = '%',
    description = "Retention in Care"), denominator.outcome = 'diagnosed.prevalence')

data.manager$register.outcome(
  'suppression', 
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Viral Suppression',
    axis.name = 'Proportion Virally Suppressed',
    units = '%',
    description = "HIV Viral Suppression"), denominator.outcome = 'diagnosed.prevalence')

data.manager$register.outcome(
  'retention.of.engaged', #Defined as >=2 tests (CD4 or VL) divided by >=1 test
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Retention of Engaged',
    axis.name = 'Retention of Engaged',
    units = '%',
    description = "Retention of Engaged in Care"), denominator.outcome = 'diagnosed.prevalence')

data.manager$register.outcome(  #Adding this as a denominator value for awareness/knowledge of status- bc the cdc denominator is an estimated prevalence value of both known and unknown status#
  'total.prevalence',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Total Prevalence',
    axis.name = 'Total Prevalence',
    units = 'cases',
    description = "Estimated Prevalence of Known and Unknown Status"))

data.manager$register.outcome(
  'awareness', #changed from knowledge to awareness
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'Awareness of HIV Status',
    axis.name = 'Proportion Aware of HIV Status',
    units = '%',
    description = "Awareness of HIV Status"), denominator.outcome = 'total.prevalence') #creating outcome that is pop values for awareness#

data.manager$register.outcome(
  'hiv.deaths',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'HIV Deaths',
    axis.name = 'HIV Deaths (n)',
    units = 'cases',
    description = "HIV Deaths"))

data.manager$register.outcome(
  'hiv.tests.per.population',
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'HIV Tests per Adult Population',
    axis.name = 'HIV Tests per Adult Population',
    units =  '%',
    description = "HIV Tests per Adult Population"), denominator.outcome = 'adult.population')

#Register Sources:
data.manager$register.parent.source('NHSS', full.name = 'National HIV Surveillance System', short.name= "NHSS") #parent
data.manager$register.parent.source('NHM&E', full.name = 'National HIV Prevention Program Monitoring and Evaluation', short.name= "NHM&E") #parent
data.manager$register.parent.source('LHD', full.name = 'Local Health Department', short.name= "LHD") #parent
data.manager$register.parent.source('IQVIA', full.name = 'IQVIA', short.name= "IQVIA")
data.manager$register.parent.source('NHANES', full.name = 'National Health and Nutrition Examination Survey', short.name= "NHANES")
data.manager$register.parent.source('cdc.retention.report.pdf', full.name = 'CDC HIV Surveillance PDF Reports', short.name= "cdc.retention.report.pdf") #These are really similar

data.manager$register.source('cdc.hiv', parent.source= "NHSS", full.name = "CDC HIV Outcomes Data", short.name='cdc.hiv') #child
data.manager$register.source('cdc.aggregated.county', parent.source= "NHSS", full.name = 'CDC Aggregated County', short.name = 'cdc aggd county') #Note this is for the aggregated county data being used to represent MSAs
data.manager$register.source('lhd', parent.source= "LHD", full.name = "Local Health Department", short.name='lhd') #child
data.manager$register.source('cdc.aggregated.proportion', parent.source= "NHSS", full.name = 'CDC Aggregated Proportion', short.name = 'cdc agg prop') #child
data.manager$register.source('cdc.testing', parent.source= "NHM&E", full.name = "CDC Annual HIV Testing Report", short.name='cdc.testing') #child
data.manager$register.source('aidsvu', parent.source= "IQVIA", full.name = "AIDS Vu", short.name='aidsvu')
data.manager$register.source('cdc.prep', parent.source= "IQVIA", full.name = "CDC PrEP Data", short.name='cdc.prep')
data.manager$register.source('cdc.prep.indications', parent.source= "NHANES", full.name = "CDC PrEP Indications Data", short.name='cdc.prep.indications')
data.manager$register.source('cdc.retention.reports', parent.source= "cdc.retention.report.pdf", full.name = "CDC HIV Surveillance Reports", short.name='cdc.retention.reports') #These are really similar
data.manager$register.source('cdc.testing', parent.source= "NHM&E", full.name = "CDC Annual HIV Testing Report", short.name='cdc.testing')
data.manager$register.source('prep.aidsvu.aggregated.county', parent.source= "IQVIA", full.name = 'PrEP AIDS Vu Aggregated County', short.name = 'prep aidsvu aggd county') #For aggregated prep data from AIDS Vu
data.manager$register.source('prep.cdc.aggregated.county', parent.source= "IQVIA", full.name = 'PrEP CDC Aggregated County', short.name = 'prep cdc aggd county') #For aggregated prep data from Atlas Plus (CDC)
data.manager$register.source('prep.indications.aggregated.county', parent.source= "NHANES", full.name = 'PrEP Indications Aggregated County', short.name = 'prep indications aggd county') #Note this is for the aggregated county data being used to represent MSAs
data.manager$register.source('cdc.supplemental.reports', parent.source= "NHSS", full.name = "HIV Surveillance Supplemental Report", short.name='cdc.supplemental.reports')


#Register Ontologies:

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
  'lhd',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
    race=c('black', 'hispanic', 'other'),
    sex=c('male','female'),
    risk=c('msm','idu','msm_idu','heterosexual')
  ))

data.manager$register.ontology(
  'cdc.national',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('13-19 years', '20-24 years', '25-34 years', '35-44 years', '45-54 years','55-59 years', '60-64 years', '65-69 years', '70-74 years', '75-79 years', '80-84 years','85+ years'),
    race=c('american indian/alaska native', 'asian', 'black/african american', 'hispanic/latino', 'native hawaiian/other pacific islander', 'white'),
    sex=c('male','female'),
    risk=c('msm','idu','msm_idu','heterosexual','other')
  ))

data.manager$register.ontology(
  'aidsvu',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
    race=c('black', 'hispanic', 'white'),
    sex=c('male','female')
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

#Codes:
source('data_processing/atlas_plus_hiv_processing.R')
source('data_processing/aids_vu_processing.R')
source('data_processing/cdc_test_count_processing.R')
source('data_processing/lhd_msa_processing.R')
source('data_processing/state_retention.R')
source('data_processing/older.suppression.data.R') #Older National Level Suppression Data from Todd's PDFs
source('data_processing/national_data.R') #National Atlas Plus data on diagnosis and prevalence
 
#Aggregate Outcomes:

source('data_processing/put_msa_data_as_new_source_script.R') #Sources function to aggregate county data to MSA
source('../jheem2/R/HELPERS_array_helpers.R') #Necessary array helpers
source('commoncode/locations_of_interest.R') #Creates MSAS.OF.INTEREST

surveillance.manager=data.manager

put.msa.data.as.new.source(outcome = 'diagnosed.prevalence',
                           from.source.name = 'cdc.hiv',
                           to.source.name = 'cdc.aggregated.county',
                           to.locations =  MSAS.OF.INTEREST,  #Think of this as containing location 
                           geographic.type.from = 'COUNTY',
                           geographic.type.to = 'CBSA',
                           details.for.new.data = 'estimated from county data',
                           data.manager = surveillance.manager)

put.msa.data.as.new.source(outcome = 'diagnoses',
                           from.source.name = 'cdc.hiv',
                           to.source.name = 'cdc.aggregated.county',
                           to.locations =  MSAS.OF.INTEREST, 
                           geographic.type.from = 'COUNTY',
                           geographic.type.to = 'CBSA',
                           details.for.new.data = 'estimated from county data',
                           data.manager = surveillance.manager)

put.msa.data.as.new.source(outcome = 'total.prevalence',
                           from.source.name = 'cdc.hiv',
                           to.source.name = 'cdc.aggregated.county',
                           to.locations =  MSAS.OF.INTEREST, 
                           geographic.type.from = 'COUNTY',
                           geographic.type.to = 'CBSA',
                           details.for.new.data = 'estimated from county data',
                           data.manager = surveillance.manager)

#Prep data
put.msa.data.as.new.source(outcome = 'prep',
                           from.source.name = 'cdc.prep',
                           to.source.name = 'prep.cdc.aggregated.county',
                           to.locations =  MSAS.OF.INTEREST,
                           geographic.type.from = 'COUNTY',
                           geographic.type.to = 'CBSA',
                           details.for.new.data = 'estimated from county data',
                           data.manager = surveillance.manager)

put.msa.data.as.new.source(outcome = 'prep',
                           from.source.name = 'aidsvu',
                           to.source.name = 'prep.aidsvu.aggregated.county',
                           to.locations =  MSAS.OF.INTEREST,
                           geographic.type.from = 'COUNTY',
                           geographic.type.to = 'CBSA',
                           details.for.new.data = 'estimated from county data',
                           data.manager = surveillance.manager)

put.msa.data.as.new.source(outcome = 'prep.indications',
                           from.source.name = 'cdc.prep.indications',
                           to.source.name = 'prep.indications.aggregated.county',
                           to.locations =  MSAS.OF.INTEREST,
                           geographic.type.from = 'COUNTY',
                           geographic.type.to = 'CBSA',
                           details.for.new.data = 'estimated from county data',
                           data.manager = surveillance.manager)

#Put proportion outcomes (awareness and suppression)
put.msa.data.as.new.source(outcome = 'awareness',
                           from.source.name= 'cdc.hiv',
                           to.source.name = 'cdc.aggregated.proportion',
                           to.locations = MSAS.OF.INTEREST,
                           geographic.type.from = 'COUNTY',
                           geographic.type.to = 'CBSA',
                           details.for.new.data = 'estimated from county data',
                           data.manager= surveillance.manager,
                           source.for.denominator= 'cdc.hiv',
                           ontology.for.denominator= 'cdc') 

put.msa.data.as.new.source(outcome = 'suppression',
                           from.source.name= 'cdc.hiv',
                           to.source.name = 'cdc.aggregated.proportion',
                           to.locations = MSAS.OF.INTEREST,
                           geographic.type.from = 'COUNTY',
                           geographic.type.to = 'CBSA',
                           details.for.new.data = 'estimated from county data',
                           data.manager= surveillance.manager,
                           source.for.denominator= 'cdc.hiv',
                           ontology.for.denominator= 'cdc') 

#Update for 2-11-25: Andrew/Todd requested the Coefficient of Variance for Awareness to be aggregated from County to MSA for Riverside C.40140 only since we have data for both counties:
#update for 2-18-25: Adding the same for Miami
Riverside_Counties <- locations::get.contained.locations("C.40140", "COUNTY") #06065, 06071

riverside.msa.coefficient.of.variance = surveillance.manager$pull(outcome = "awareness",
                                                                  metric = "coefficient.of.variance",
                                                                  dimension.values=list(location=Riverside_Counties),
                                                                  keep.dimensions='year')

riverside.msa.coefficient.of.variance <- as.data.frame.table(riverside.msa.coefficient.of.variance)%>%
  mutate(outcome = 'awareness')%>%
  mutate(metric = "coefficient.of.variance")%>%
  mutate(location = "C.40140")%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))

surveillance.manager$put.long.form(
  data = riverside.msa.coefficient.of.variance,
  ontology.name = 'cdc',
  source = 'cdc.aggregated.proportion',
  metric = 'coefficient.of.variance',
  dimension.values = list(),
  url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
  details = 'CDC Atlas Plus data')

# MIAMI: C.33100

miami.counties <- locations::get.contained.locations("C.33100", "COUNTY") #12011, 12086, 12099

miami.msa.coefficient.of.variance = surveillance.manager$pull(outcome = "awareness",
                                                              metric = "coefficient.of.variance",
                                                              dimension.values=list(location=miami.counties),
                                                              keep.dimensions='year')

miami.msa.coefficient.of.variance <- as.data.frame.table(miami.msa.coefficient.of.variance)%>%
  mutate(outcome = 'awareness')%>%
  mutate(metric = "coefficient.of.variance")%>%
  mutate(location = "C.33100")%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))

surveillance.manager$put.long.form(
  data = miami.msa.coefficient.of.variance,
  ontology.name = 'cdc',
  source = 'cdc.aggregated.proportion',
  metric = 'coefficient.of.variance',
  dimension.values = list(),
  url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
  details = 'CDC Atlas Plus data')


#Save:
save(surveillance.manager, file="Q:/data_managers/data.manager.merge/surveillance.manager_section2.rdata")
