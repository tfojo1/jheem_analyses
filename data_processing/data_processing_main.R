
library(jheem2)
library(tidyverse)
library(readxl)
library(stringr)
library(haven)
library(locations)
library(tools)

###Initialize data manager (surveillance manager) and establish ontology###

###CDC Atlas Plus Data###

data.manager = create.data.manager('surveillance', description='surveillance data manager')

data.manager$register.outcome(
    'adult.population',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Adult Population',
        axis.name = 'Adult Population',
        units = 'population',
        description = "Adult Population Estimate, Ages 13 and over"))

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

data.manager$register.outcome(
    'linkage_1mo',
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Linkage (1 month)',
        axis.name = 'Proportion Linked (1 month)',
        units = '%',
        description = "Linkage to HIV care within 1 Month"), denominator.outcome = 'diagnoses')

data.manager$register.outcome(
    'engagement', #changed from receipt to engagement
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Engagement in Care',
        axis.name = 'Proportion of Engaged in Care',
        units = '%',
        description = "Engagement in  HIV medical care"), denominator.outcome = 'diagnosed.prevalence')

data.manager$register.outcome(
    'suppression', 
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Viral Suppression',
        axis.name = 'Proportion Virally Suppressed',
        units = '%',
        description = "HIV Viral Suppression"), denominator.outcome = 'diagnosed.prevalence')

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
    'aids.diagnosed.prevalence', #changed from aids.diagnosis to aids.diagnosed.prevalence
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'AIDS Diagnosed Prevalence',
        axis.name = 'AIDS Diagnosed Prevalence',
        units = 'cases',
        description = "AIDS Diagnosed Prevalence"))

data.manager$register.outcome(
    'aids.diagnoses',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'AIDS Diagnoses',
        axis.name = 'AIDS Diagnoses',
        units = 'cases',
        description = "AIDS Diagnoses"))

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
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Gonorrhea Ratio',
        axis.name = 'Gonorrhea Ratio',
        units = 'cases',
        description = "Year Over Year Ratio of Gonorrhea Cases"))

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
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Syphilis Ratio',
        axis.name = 'Syphilis Ratio',
        units = 'cases',
        description = "Year Over Year Ratio of Primary and Secondary Syphilis Cases"))

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
    'linkage_3mo',
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Linkage (3 months)',
        axis.name = 'Proportion Linked (3 months)',
        units = '%',
        description = "Linkage to HIV care within 3 Months"), denominator.outcome = 'diagnoses')

data.manager$register.outcome(
    'retention',  #Defined as:Individuals with ≥2 tests (CD4 or VL) ≥3 months apart#
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Retention',
        axis.name = 'Proportion Retained in Care',
        units = '%',
        description = "Retention in Care"), denominator.outcome = 'diagnosed.prevalence')

data.manager$register.outcome(
    'retention.of.engaged', #Defined as >=2 tests (CD4 or VL) divided by >=1 test
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Retention of Engaged',
        axis.name = 'Retention of Engaged',
        units = '%',
        description = "Retention of Engaged in Care"), denominator.outcome = 'diagnosed.prevalence')

data.manager$register.outcome(
    'proportion.tested.n',           #Will have option in code to make this only people at risk or everyone#
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Denominator value for proportion tested in past year',
        axis.name = 'Denominator value for proportion tested in past year',
        units = '%',
        description = "Denominator value for proportion tested in past year"))

data.manager$register.outcome(
    'proportion.tested', 
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Proportion Tested in Past Year',
        axis.name = 'Proportion Tested in Past Year',
        units = '%',
        description = "Proportion of People who have received an HIV test in the last year"), denominator.outcome = 'proportion.tested.n')

data.manager$register.outcome(
    'proportion.msm', 
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Proportion of MSM',
        axis.name = 'Proportion of MSM',
        units = '%',
        description = "Proportion of Men who have sex with Men"), denominator.outcome = 'adult.population')

data.manager$register.outcome(
    'unweighted.denominator',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'unweighted.denominator',
        axis.name = 'unweighted.denominator)',
        units = 'cases',
        description = "BRFSS Unweighted Denominator Value"))

data.manager$register.outcome(
    'immigration',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Immigration',
        axis.name = 'Immigration',
        units = 'population',
        description = "Metro Immigration"))

data.manager$register.outcome(
    'emigration',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Emigration',
        axis.name = 'Emigration',
        units = 'population',
        description = "Metro Emigration"))

data.manager$register.outcome(
    'adult.immigration',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Adult Immigration',
        axis.name = 'Adult Immigration',
        units = 'population',
        description = "Metro Immigration Ages 13+"))

data.manager$register.outcome(
    'adult.emigration',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Emigration',
        axis.name = 'Emigration',
        units = 'population',
        description = "Metro Emigration Ages 13+"))

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

#Registering 'deaths' as an outcome for the deaths from the census manager aggregated from county to MSA in the surveillance manager
#this is general mortality (NOT hiv specific deaths)
data.manager$register.outcome(
    'deaths',
    metadata = create.outcome.metadata(
        scale = 'non.negative.number',
        display.name = 'Deaths',
        axis.name = 'Deaths',
        units = 'deaths',
        description = "Deaths"))

data.manager$register.outcome(
    'depression',
    metadata = create.outcome.metadata(
        scale = 'proportion',
        display.name = 'Major Depressive Episode in the Past Year',
        axis.name = 'Depression',
        units =  '%',
        description = "Major Depressive Episode in the Past Year"), denominator.outcome = 'adult.population')

data.manager$register.outcome(
  'hiv.tests.per.population',
  metadata = create.outcome.metadata(
    scale = 'proportion',
    display.name = 'HIV Tests per Adult Population',
    axis.name = 'HIV Tests per Adult Population',
    units =  '%',
    description = "HIV Tests per Adult Population"), denominator.outcome = 'adult.population')

#Register "Parent" Sources
data.manager$register.parent.source('IQVIA', full.name = 'IQVIA', short.name= "IQVIA")
data.manager$register.parent.source('NSDUH', full.name = 'National Survey on Drug Use and Health', short.name= "NSDUH")
data.manager$register.parent.source('LHD', full.name = 'Local Health Department', short.name= "LHD")
data.manager$register.parent.source('BRFSS', full.name = 'Behavioral Risk Factor Surveillance System', short.name= "BRFSS")
data.manager$register.parent.source('ACS', full.name = 'American Community Survey', short.name= "ACS")
data.manager$register.parent.source('NCHS', full.name = 'National Center for Health Statistics', short.name= "NCHS")
data.manager$register.parent.source('NNHSS', full.name = 'National Notifiable Disease Surveillance System', short.name= "NNHSS")
data.manager$register.parent.source('NHSS', full.name = 'National HIV Surveillance System', short.name= "NHSS")
data.manager$register.parent.source('NHANES', full.name = 'National Health and Nutrition Examination Survey', short.name= "NHANES")
data.manager$register.parent.source('cdc.retention.report.pdf', full.name = 'CDC HIV Surveillance PDF Reports', short.name= "cdc.retention.report.pdf") #These are really similar
data.manager$register.parent.source('NHM&E', full.name = 'National HIV Prevention Program Monitoring and Evaluation', short.name= "NHM&E")
data.manager$register.parent.source('census', full.name = 'United States Census Bureau', short.name= "census")


#Register Data Sources ('children')
data.manager$register.source('aidsvu', parent.source= "IQVIA", full.name = "AIDS Vu", short.name='aidsvu')
data.manager$register.source('nsduh', parent.source= "NSDUH", full.name = "National Survey on Drug Use and Health", short.name='nsduh')
data.manager$register.source('lhd', parent.source= "LHD", full.name = "Local Health Department", short.name='lhd')
data.manager$register.source('brfss', parent.source= "BRFSS", full.name = "Behavioral Risk Factor Surveillance System", short.name='brfss')
data.manager$register.source('emory', parent.source= "ACS", full.name = "Emory University", short.name='emory')
data.manager$register.source('cdc.aggregated.county', parent.source= "NHSS", full.name = 'CDC Aggregated County', short.name = 'cdc aggd county') #Note this is for the aggregated county data being used to represent MSAs
data.manager$register.source('census.population', parent.source= "ACS", full.name = "US Census Bureau Population Data", short.name='census.population')
data.manager$register.source('census.deaths', parent.source= "NCHS", full.name = "US Census Bureau Death Data", short.name='census.deaths')
data.manager$register.source('cdc.sti', parent.source= "NNHSS", full.name = "CDC STI Data", short.name='cdc.sti')
data.manager$register.source('cdc.prep', parent.source= "IQVIA", full.name = "CDC PrEP Data", short.name='cdc.prep')
data.manager$register.source('cdc.prep.indications', parent.source= "NHANES", full.name = "CDC PrEP Indications Data", short.name='cdc.prep.indications')
data.manager$register.source('cdc.hiv', parent.source= "NHSS", full.name = "CDC HIV Outcomes Data", short.name='cdc.hiv')
data.manager$register.source('cdc.retention.reports', parent.source= "cdc.retention.report.pdf", full.name = "CDC HIV Surveillance Reports", short.name='cdc.retention.reports') #These are really similar
data.manager$register.source('cdc.testing', parent.source= "NHM&E", full.name = "CDC Annual HIV Testing Report", short.name='cdc.testing')
data.manager$register.source('cdc.surveillance.reports', parent.source= "NHSS", full.name = "CDC HIV Surveillance Report", short.name='cdc.surveillance.reports')
data.manager$register.source('cdc.aids', parent.source= "NHSS", full.name = "CDC Wonder AIDS Public Information Data", short.name='cdc.aids')
data.manager$register.source('cdc.supplemental.reports', parent.source= "NHSS", full.name = "HIV Surveillance Supplemental Report", short.name='cdc.supplemental.reports')


#Creating these separately bc they have separate parent sources
data.manager$register.source('prep.aidsvu.aggregated.county', parent.source= "IQVIA", full.name = 'PrEP AIDS Vu Aggregated County', short.name = 'prep aidsvu aggd county') #For aggregated prep data from AIDS Vu
data.manager$register.source('prep.cdc.aggregated.county', parent.source= "IQVIA", full.name = 'PrEP CDC Aggregated County', short.name = 'prep cdc aggd county') #For aggregated prep data from Atlas Plus (CDC)
data.manager$register.source('prep.indications.aggregated.county', parent.source= "NHANES", full.name = 'PrEP Indications Aggregated County', short.name = 'prep indications aggd county') #Note this is for the aggregated county data being used to represent MSAs
data.manager$register.source('census.deaths.aggregated', parent.source= "NCHS", full.name = 'Census Deaths Aggregated', short.name = 'census deaths aggregated')
data.manager$register.source('cdc.aggregated.proportion', parent.source= "NHSS", full.name = 'CDC Aggregated Proportion', short.name = 'cdc agg prop')
data.manager$register.source('census.aggregated.adult.population', parent.source= "census", full.name = 'Census Aggregated Adult Population', short.name = 'census.agg.pop')

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

data.manager$register.ontology(
  'cdc.new',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55-64 years', "65+ years"),
    race=c('American Indian/Alaska Native', 'Asian', 'Black/African American', 'Hispanic/Latino', 'Multiracial', 'Native Hawaiian/Other Pacific Islander', 'White'),
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
    'cdc.msa.reports',
    ont = ontology(
        year= NULL,
        location= NULL,
        age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years', '55+ years'),
        race=c('American Indian/Alaska Native', 'Asian', 'Black/African American', 'Hispanic/Latino', 'White'),
        sex=c('male','female'),
        risk=c('msm','idu','msm_idu','heterosexual','other')
        
    ))

#This is for the Atlas Plus STI data, creating a separate ontology bc age groups are different#
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

#Create a separate ontology for early syphilis
data.manager$register.ontology(
    'cdc.syphilis',
    ont = ontology(
        year= NULL,
        location= NULL,
        age=c('13-14 years', '15-24 years', '25-34 years', '35-44 years', '45-54 years', '55-64 years', '65+ years'), 
        race=c('American Indian/Alaska Native', 'Asian', 'Black/African American', 'Hispanic/Latino', 'Multiracial', 'Native Hawaiian/Other Pacific Islander', 'White'),
        sex=c('male','female'),
        risk=c('msm','idu','msm_idu','heterosexual','other')
    ))

data.manager$register.ontology(
    'nsduh',
    ont = ontology(
        year= NULL,
        location= NULL,
        age=c('13-24', '25+')))


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
    'brfss',
    ont = ontology(
        year= NULL,
        location= NULL,
        age=c('18-24 years', '25-29 years', '30-34 years', '35-39 years', '40-44 years', '45-49 years', '50-54 years', '55-59 years', '60-64 years', '65-69 years', '70-74 years', '75-79 years', '80+ years'),
        race=c('White', 'Black', 'American Indian/Alaska Native', 'Asian', 'Native Hawaiian/Other Pacific Islander', 'Other race', 'Multiracial', 'Hispanic'),
        sex=c('male','female'),
        risk=c('msm', 'not_msm')
    ))

data.manager$register.ontology(
    'emory',
    ont = ontology(
        year= NULL,
        location= NULL,
        sex=c('male', 'female') #needs to have male and female here to represent all possible outcomes
    ))

data.manager$register.ontology(
    'census.immigration',
    ont = ontology(
        year= c("2006-2010", "2011-2015", "2016-2020"),
        location= NULL,
        age = c("1-4 years", "5-17 years", "18-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years",
                "65-69 years", "70-74 years", "75+ years"),
        race=c("Hispanic or Latino", "White, Non-Hispanic", "Black", "Other"),
        sex=c('male','female'),
        incomplete.dimensions = c("year", "location")
    ))

data.manager$register.ontology(
    'census.immigration.adults',
    ont = ontology(
        year= c("2011-2015", "2012-2016", "2013-2017", "2014-2018", "2015-2019", "2016-2020"),
        location= NULL,
        age = c("1-12 years", "13-17 years", "18-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years",
                "65-69 years", "70-74 years", "75+ years"),
        race=c("Hispanic or Latino", "White, Non-Hispanic", "Black", "Other"),
        sex=c('male','female'),
        incomplete.dimensions = c("year", "location")
    ))

data.manager$register.ontology(
    'cdc.national',
    ont = ontology(
        year= NULL,
        location= NULL,
        age=c('13-19 years', '20-24 years', '25-34 years', '35-44 years', '45-54 years','55-59 years', '60-64 years', '65-69 years', '70-74 years', '75-79 years', '80-84 years','85+ years'),
        race=c('American Indian/Alaska Native', 'Asian', 'Black/African American', 'Hispanic/Latino', 'Multiracial', 'Native Hawaiian/Other Pacific Islander', 'White'),
        sex=c('male','female'),
        risk=c('msm','idu','msm_idu','heterosexual','other')
    ))

data.manager$register.ontology(
    'cdc.aids',
    ont = ontology(
        year= NULL,
        location= NULL,
        age=c( '< 1 year', '1-12 years', '13-19 years', '20-24 years', '25-29 years', '30-34 years', '35-39 years', '40-44 years', '45-49 years', '50-54 years', '55-59 years', '60-64 years','65+ years'),
        race=c('American Indian/Alaska Native', 'Asian', 'Black', 'Hispanic', 'White', 'Unknown'),
        sex=c('male','female'),
        risk=c('msm','idu','msm_idu','heterosexual','other')
    ))

data.manager$register.ontology(
    'cdc.aids.deaths',
    ont = ontology(
        year= ("1981-2001"),
        location= NULL,
        age=c( '< 1 year', '1-12 years', '13-19 years', '20-24 years', '25-29 years', '30-34 years', '35-39 years', '40-44 years', '45-49 years', '50-54 years', '55-59 years', '60-64 years','65+ years'),
        race=c('American Indian/Alaska Native', 'Asian', 'Black', 'Hispanic', 'White', 'Unknown'),
        sex=c('male','female'),
        risk=c('msm','idu','msm_idu','heterosexual','other'),
        incomplete.dimensions = c("year", "location")
    ))

data.manager$register.ontology(
    'census',
    ont = ontology(
        year= NULL,
        location= NULL,
        age=c('< 1 year', '1 year', '2 years', '3 years', '4 years', '5 years', '6 years', '7 years', '8 years', '9 years', '10 years',
              '11 years', '12 years', '13 years', '14 years', '15 years', '16 years', '17 years', '18 years', '19 years', '20 years',
              '21 years', '22 years', '23 years', '24 years', '25 years', '26 years', '27 years', '28 years', '29 years', '30 years',
              '31 years', '32 years', '33 years', '34 years', '35 years', '36 years', '37 years', '38 years', '39 years', '40 years',
              '41 years', '42 years', '43 years', '44 years', '45 years', '46 years', '47 years', '48 years', '49 years', '50 years',
              '51 years', '52 years', '53 years', '54 years', '55 years', '56 years', '57 years', '58 years', '59 years', '60 years',
              '61 years', '62 years', '63 years', '64 years', '65 years', '66 years', '67 years', '68 years', '69 years', '70 years',
              '71 years', '72 years', '73 years', '74 years', '75 years', '76 years', '77 years', '78 years', '79 years', '80 years',
              '81 years', '82 years', '83 years', '84 years', '85+ years'),
        race=c('white', 'black', 'american indian or alaska native', 'asian or pacific islander'),
        ethnicity=c('hispanic', 'not hispanic'),
        sex=c('male','female')
    ))
################################################################################
###Source locations of interest to create MSA vector
source('commoncode/locations_of_interest.R')
source('commoncode/additional_locations_of_interest.R')
###############################################################################
###Source in File that reads .csvs and removes headers
source('data_processing/fix_cdc_headers.R')

###Source in AIDS Vu data and cleaning
source('data_processing/aids_vu_processing.R')

###Source CDC Test Count Data
source('data_processing/cdc_test_count_processing.R')

###Source LHD MSA data
source('data_processing/lhd_msa_processing.R')

###Source in STI data
source('data_processing/sti_processing.R')

###Source in NSDUH substance data
source('data_processing/nsduh_processing_new.R')

###Source in state retention data
source('data_processing/state_retention.R')

###Source in CDC MSA PDF Reports data and cleaning
source('data_processing/msa_reports_processing.R')

##Source in Code to use the county prevalence data from Atlas Plus in 2017
## To sum counties to estimate MSA data
##Andrew is writing a function to do this so use that once it's done and update this
# source('data_processing/msa_prevalence_totals_2017.R')

#Source BRFSS testing data
#source('data_processing/brfss_state.R')
#source('data_processing/brfss_msa.R')
#OR source the weighted BRFSS data
source('data_processing/brfss_state_weighted.R')
source('data_processing/brfss_msa_weighted.R')

#Source msm
source('data_processing/msm.R')

#Source BRFSS Unweighted Denominator values
source('data_processing/brfss_unweighted_denominators.R')

#Source immigration data
source('data_processing/immigration.R')
source('data_processing/immigration_age_calculations.R')

#National Atlas Plus data on diagnosis and prevalence
source('data_processing/national_data.R')

#CDC Wonder AIDS data from 1981-2001
source('data_processing/aids_data_1981_2001.R')

#Older National Level Suppression Data from Todd's PDFs
source('data_processing/older.suppression.data.R')

################################################################################
###Define the 'mappings' for Atlas plus data###

outcome.mappings = c('HIV diagnoses'='diagnoses',
                     'HIV deaths' = 'hiv.deaths',
                     'HIV prevalence' = 'diagnosed.prevalence',
                     'Linkage to HIV care' = 'linkage_1mo',
                     'Receipt of HIV medical care' = 'engagement',
                     'HIV viral suppression' = 'suppression',
                     'Knowledge of Status' = 'awareness')

risk.mappings = c('Heterosexual contact' = 'heterosexual',
                  'Injection drug use' = 'idu',
                  'Other' = 'other',
                  'Male-to-male sexual contact' = 'msm',
                  'Male-to-male sexual contact and injection drug use' = 'msm_idu')

age.mappings = c('13-24' = '13-24 years',
                 '25-34' = '25-34 years',
                 '35-44' = '35-44 years',
                 '45-54' = '45-54 years',
                 '55+' = '55+ years')

national.age.mappings = c('13-24' = '13-24 years',
                         '25-34' = '25-34 years',
                         '35-44' = '35-44 years',
                         '45-54' = '45-54 years',
                         '55-64' = '55-64 years',
                         '65+' = '65+ years')

#record possible values for the incomplete dimensions, year and location
locations = c()
years = c()

################################################################################
#---Clean Diagnoses---#

data.list.clean.diagnoses = lapply(data.list.diagnoses, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    #Universal Cleaning#
    data$outcome = outcome.mappings[data$Indicator]
    data = data[!is.na(data$outcome),]
    
    names(data)[names(data)=='Year'] = 'year'   
    data$year = substring(data$year,1, 4)                                          
    data$year = as.character(data$year)
    
    data$Cases[data$Cases %in% c("Data suppressed")] = NA    
    data$Cases[data$Cases %in% c("Data not available")] = NA  
    data$value = as.numeric(gsub(",", '', data$Cases))   
    
    #Location conditionals#
    if(grepl("state", filename)) {
        names(state.abb) <- state.name 
        data$Geography= gsub('[^[:alnum:] ]',"",data$Geography) #some states have ^ for preliminary data#
        names(data)[names(data)=='Geography'] = 'state'
        data$location =ifelse (data$state == "District of Columbia", "DC", state.abb[data$state]) 
    }
    if(grepl("ehe", filename)) {
        data$location = as.character(data$FIPS)
    }
    
    if(grepl("msa", filename)) {
        data$msa_indicator= substring(data$FIPS, 6, 10)
        
        data$msa_keep = ifelse(data$msa_indicator == "00000", "keep", "drop")
        data = subset(data,data$msa_keep == "keep")   #Can make this shorter once you check over everything#
        
        data$cbsa = substring(data$FIPS, 1, 5)
        data$location = paste("C", data$cbsa, sep=".")
    }
    
    if(grepl("allcounty", filename)) {
        data$location = as.character(data$FIPS)
    }
    
    ##Demographic conditionals##
    
    if(grepl("age", filename)) {
        data$age = age.mappings[data$Age.Group]
    }
    if(grepl("race", filename)) {
        names(data)[names(data)=='Race.Ethnicity'] = 'race'
    }
    if(grepl("sex", filename)) {
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
    }
    if(grepl("male", filename)) {
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
    }
    if(grepl("female", filename)) {
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
    }
    if(grepl("risk", filename)) {
        data$risk = risk.mappings[data$Transmission.Category]
    }
    
    list(filename, data) #what to return#
    
} )

#---Clean Deaths---#

data.list.clean.deaths = lapply(data.list.deaths, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    data$outcome = outcome.mappings[data$Indicator]
    data = data[!is.na(data$outcome),]
    
    names(data)[names(data)=='Year'] = 'year'   
    data$year = substring(data$year,1, 4)                                          
    data$year = as.character(data$year)
    
    data$Cases[data$Cases %in% c("Data suppressed")] = NA    
    data$Cases[data$Cases %in% c("Data not available")] = NA  
    data$value = as.numeric(gsub(",", '', data$Cases))   
    
    if(grepl("state", filename)) {
        names(state.abb) <- state.name 
        data$Geography= gsub('[^[:alnum:] ]',"",data$Geography) #some states have ^ for preliminary data#
        names(data)[names(data)=='Geography'] = 'state'
        data$location =ifelse (data$state == "District of Columbia", "DC", state.abb[data$state]) 
    }
    if(grepl("ehe", filename)) {
        data$location = as.character(data$FIPS)
    }
    if(grepl("msa", filename)) {
        data$msa_indicator= substring(data$FIPS, 6, 10)
        
        data$msa_keep = ifelse(data$msa_indicator == "00000", "keep", "drop")
        data = subset(data,data$msa_keep == "keep")   #Can make this shorter once you check over everything#
        
        data$cbsa = substring(data$FIPS, 1, 5)
        data$location = paste("C", data$cbsa, sep=".")
    }
    if(grepl("allcounty", filename)) {
        data$location = as.character(data$FIPS)
    }
    
    if(grepl("age", filename)) {
        data$age = age.mappings[data$Age.Group]
    }
    if(grepl("race", filename)) {
        names(data)[names(data)=='Race.Ethnicity'] = 'race'
    }
    if(grepl("sex", filename)) {
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
    }
    if(grepl("male", filename)) {
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
    }
    if(grepl("female", filename)) {
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
    }
    if(grepl("risk", filename)) {
        data$risk = risk.mappings[data$Transmission.Category]
        
    }
    list(filename, data) 
    
} )

##############################################################################
##Adding a new section to deaths to differentiate issues in death reporting###
##############################################################################
#Certain locations have notes on reporting issues with under reported death values
#Creating a separate list of these to create a separate put with specific details

data.list.clean.deaths.notes = lapply(data.list.deaths, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    data$outcome = outcome.mappings[data$Indicator]
    data = data[!is.na(data$outcome),]
    
    names(data)[names(data)=='Year'] = 'year'   
    data$year = substring(data$year,1, 4)                                          
    data$year = as.character(data$year)
    
    data$Cases[data$Cases %in% c("Data suppressed")] = NA    
    data$Cases[data$Cases %in% c("Data not available")] = NA  
    data$value = as.numeric(gsub(",", '', data$Cases))   
    
    if(grepl("state", filename)) {
        names(state.abb) <- state.name 
        
        ##use this to select only states with missing reporting##
        data <- data %>%
            filter(str_detect(Geography, "[^[:alnum:] ]"))  
        
        data$Geography= gsub('[^[:alnum:] ]',"",data$Geography) #some states have ^ for preliminary data#
        names(data)[names(data)=='Geography'] = 'state'
        data$location =ifelse (data$state == "District of Columbia", "DC", state.abb[data$state]) 
    }
    
    if(grepl("msa", filename)) {
        data$Geography= gsub('[^[:alnum:] ]',"",data$Geography)
        
        data <- data %>%
            filter(str_detect(Geography, "[^[:alnum:] ]"))  
    }
    
    if(grepl("age", filename)) {
        data$age = age.mappings[data$Age.Group]
    }
    if(grepl("race", filename)) {
        names(data)[names(data)=='Race.Ethnicity'] = 'race'
    }
    if(grepl("sex", filename)) {
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
    }
    if(grepl("male", filename)) {
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
    }
    if(grepl("female", filename)) {
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
    }
    if(grepl("risk", filename)) {
        data$risk = risk.mappings[data$Transmission.Category]
    }
    list(filename, data)
})


#---Clean Prevalence---#

data.list.clean.prevalence = lapply(data.list.prevalence, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    data$outcome = outcome.mappings[data$Indicator]
    data = data[!is.na(data$outcome),]
    
    names(data)[names(data)=='Year'] = 'year'   
    data$year = substring(data$year,1, 4)                                          
    data$year = as.character(data$year)
    
    data$Cases[data$Cases %in% c("Data suppressed")] = NA    
    data$Cases[data$Cases %in% c("Data not available")] = NA  
    data$value = as.numeric(gsub(",", '', data$Cases))   
    
    if(grepl("state", filename)) {
        names(state.abb) <- state.name 
        data$Geography= gsub('[^[:alnum:] ]',"",data$Geography) #some states have ^ for preliminary data#
        names(data)[names(data)=='Geography'] = 'state'
        data$location =ifelse (data$state == "District of Columbia", "DC", state.abb[data$state]) 
    }
    if(grepl("ehe", filename)) {
        data$location = as.character(data$FIPS)
    }
    if(grepl("msa", filename)) {
        data$msa_indicator= substring(data$FIPS, 6, 10)
        
        data$msa_keep = ifelse(data$msa_indicator == "00000", "keep", "drop")
        data = subset(data,data$msa_keep == "keep")   #Can make this shorter once you check over everything#
        
        data$cbsa = substring(data$FIPS, 1, 5)
        data$location = paste("C", data$cbsa, sep=".")
    }
    if(grepl("allcounty", filename)) {
        data$location = as.character(data$FIPS)
    } 
    
    if(grepl("age", filename)) {
        data$age = age.mappings[data$Age.Group]
    }
    if(grepl("race", filename)) {
        names(data)[names(data)=='Race.Ethnicity'] = 'race'
    }
    if(grepl("sex", filename)) {
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
    }
    if(grepl("male", filename)) {
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
    }
    if(grepl("female", filename)) {
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
    }
    if(grepl("risk", filename)) {
        data$risk = risk.mappings[data$Transmission.Category]
    }
    
    list(filename, data) 
    
} )

#---Clean SLE---#

data.list.clean.sle = lapply(data.list.sle, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    data$outcome = outcome.mappings[data$Indicator]
    data = data[!is.na(data$outcome),]
    
    names(data)[names(data)=='Year'] = 'year'   
    data$year = substring(data$year,1, 4)                                          
    data$year = as.character(data$year)
    
    #Pulling from 'percent' variable
    data$Percent[data$Percent %in% c("Data suppressed")] = NA    
    data$Percent[data$Percent %in% c("Data not available")] = NA 
    data$Percent = as.numeric(data$Percent)
    data$value = (data$Percent/100) 
    
    if(grepl("state", filename)) {
        names(state.abb) <- state.name 
        data$Geography= gsub('[^[:alnum:] ]',"",data$Geography) #some states have ^ for preliminary data#
        names(data)[names(data)=='Geography'] = 'state'
        data$location =ifelse (data$state == "District of Columbia", "DC", state.abb[data$state]) 
    }
    if(grepl("ehe", filename)) {
        data$location = as.character(data$FIPS)
    }
    if(grepl("msa", filename)) {
        data$msa_indicator= substring(data$FIPS, 6, 10)
        
        data$msa_keep = ifelse(data$msa_indicator == "00000", "keep", "drop")
        data = subset(data,data$msa_keep == "keep")   #Can make this shorter once you check over everything#
        
        data$cbsa = substring(data$FIPS, 1, 5)
        data$location = paste("C", data$cbsa, sep=".")
    }
    if(grepl("allcounty", filename)) {
        data$location = as.character(data$FIPS)
    }  
    
    if(grepl("age", filename)) {
        data$age = age.mappings[data$Age.Group]
    }
    if(grepl("race", filename)) {
        names(data)[names(data)=='Race.Ethnicity'] = 'race'
    }
    if(grepl("sex", filename)) {
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
    }
    if(grepl("male", filename)) {
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
    }
    if(grepl("female", filename)) {
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
    }
    if(grepl("risk", filename)) {
        data$risk = risk.mappings[data$Transmission.Category]
    }
    
    list(filename, data) 
    
})

#---Clean National Suppression---#

national.suppression = lapply(data.list.national.suppression , function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    data$outcome = outcome.mappings[data$Indicator]
    data = data[!is.na(data$outcome),]
    
    names(data)[names(data)=='Year'] = 'year'   
    data$year = substring(data$year,1, 4)                                          
    data$year = as.character(data$year)
    
    #Pulling from 'percent' variable
    data$Percent[data$Percent %in% c("Data suppressed")] = NA    
    data$Percent[data$Percent %in% c("Data not available")] = NA 
    data$Percent = as.numeric(data$Percent)
    data$value = (data$Percent/100) 
    
    data$location = "US"
    
    if(grepl("age", filename)) {
        data$age = national.age.mappings[data$Age.Group]
    }
    if(grepl("race", filename)) {
        names(data)[names(data)=='Race.Ethnicity'] = 'race'
    }
    if(grepl("sex", filename)) {
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
    }
    if(grepl("risk", filename)) {
        data$risk = risk.mappings[data$Transmission.Category]
    }
    if(grepl("male", filename)) {
      names(data)[names(data)=='Sex'] = 'sex'
      data$sex = tolower(data$sex)
    }
    if(grepl("female", filename)) {
      names(data)[names(data)=='Sex'] = 'sex'
      data$sex = tolower(data$sex)
    }
    
    list(filename, data) 
    
})

#---Clean Knowledge---#

data.list.clean.knowledge = lapply(data.list.knowledge, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    data$outcome = outcome.mappings[data$Indicator]
    data = data[!is.na(data$outcome),]
    
    names(data)[names(data)=='Year'] = 'year'   
    data$year = substring(data$year,1, 4)                                          
    data$year = as.character(data$year)
    
    #Pulling from 'percent' variable
    data$Percent[data$Percent %in% c("Data suppressed")] = NA    
    data$Percent[data$Percent %in% c("Data not available")] = NA 
    data$Percent = as.numeric(data$Percent)
    data$value = (data$Percent/100)   
    
    
    if(grepl("state", filename)) {
        names(state.abb) <- state.name 
        data$Geography= gsub('[^[:alnum:] ]',"",data$Geography) #some states have ^ for preliminary data#
        names(data)[names(data)=='Geography'] = 'state'
        data$location =ifelse (data$state == "District of Columbia", "DC", state.abb[data$state]) 
    }
    if(grepl("ehe", filename)) {
        data$location = as.character(data$FIPS)
    }
    if(grepl("msa", filename)) {
        data$msa_indicator= substring(data$FIPS, 6, 10)
        
        data$msa_keep = ifelse(data$msa_indicator == "00000", "keep", "drop")
        data = subset(data,data$msa_keep == "keep")   #Can make this shorter once you check over everything#
        
        data$cbsa = substring(data$FIPS, 1, 5)
        data$location = paste("C", data$cbsa, sep=".")
    }
    if(grepl("allcounty", filename)) {
        data$location = as.character(data$FIPS)
    }  
    
    if(grepl("age", filename)) {
        data$age = age.mappings[data$Age.Group]
    }
    if(grepl("race", filename)) {
        names(data)[names(data)=='Race.Ethnicity'] = 'race'
    }
    if(grepl("sex", filename)) {
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
    }
    if(grepl("male", filename)) {
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
    }
    if(grepl("female", filename)) {
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
    }
    if(grepl("risk", filename)) {
        data$risk = risk.mappings[data$Transmission.Category]
    }
    
    list(filename, data) 
    
} )

#Create a new dataset to put in the 'population' column for the outcome=knowledge/awareness data
#this is because this proportion has a denominator value which is an estimation of both dx and un-dx hiv cases and we don't have this anywhere else
#Clean knowldge population
data.list.clean.awareness.population = lapply(data.list.knowledge, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    data$outcome = "total.prevalence"
    
    names(data)[names(data)=='Year'] = 'year'   
    data$year = substring(data$year,1, 4)                                          
    data$year = as.character(data$year)
    
    #Pulling from 'population' variable
    data$Population[data$Population %in% c("Data suppressed")] = NA    
    data$Population[data$Population %in% c("Data not available")] = NA 
    data$Population = as.numeric(gsub(",", '', data$Population))  
    data$value = data$Population
    
    
    if(grepl("state", filename)) {
        names(state.abb) <- state.name 
        data$Geography= gsub('[^[:alnum:] ]',"",data$Geography) #some states have ^ for preliminary data#
        names(data)[names(data)=='Geography'] = 'state'
        data$location =ifelse (data$state == "District of Columbia", "DC", state.abb[data$state]) 
    }
    if(grepl("ehe", filename)) {
        data$location = as.character(data$FIPS)
    }
    if(grepl("msa", filename)) {
        data$msa_indicator= substring(data$FIPS, 6, 10)
        
        data$msa_keep = ifelse(data$msa_indicator == "00000", "keep", "drop")
        data = subset(data,data$msa_keep == "keep")   #Can make this shorter once you check over everything#
        
        data$cbsa = substring(data$FIPS, 1, 5)
        data$location = paste("C", data$cbsa, sep=".")
    }
    if(grepl("allcounty", filename)) {
        data$location = as.character(data$FIPS)
    }  
    
    if(grepl("age", filename)) {
        data$age = age.mappings[data$Age.Group]
    }
    if(grepl("race", filename)) {
        names(data)[names(data)=='Race.Ethnicity'] = 'race'
    }
    if(grepl("sex", filename)) {
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
    }
    if(grepl("male", filename)) {
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
    }
    if(grepl("female", filename)) {
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
    }
    if(grepl("risk", filename)) {
        data$risk = risk.mappings[data$Transmission.Category]
    }
    
    list(filename, data) 
} )

##Creating this to put the RSE associated with Knowledge; note the outcome is still knowledge/awareness but the value represents RSE

knowledge.rse = lapply(data.list.knowledge, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    data$outcome = outcome.mappings[data$Indicator]
    data = data[!is.na(data$outcome),]
    
    names(data)[names(data)=='Year'] = 'year'   
    data$year = substring(data$year,1, 4)                                          
    data$year = as.character(data$year)
    
    #Pulling from 'RSE' variable
    data$RSE[data$RSE %in% c("Data suppressed")] = NA    
    data$RSE[data$RSE %in% c("Data not available")] = NA 
    data$RSE = as.numeric(data$RSE)
    data$value = (data$RSE/100)   
    
    
    if(grepl("state", filename)) {
        names(state.abb) <- state.name 
        data$Geography= gsub('[^[:alnum:] ]',"",data$Geography) #some states have ^ for preliminary data#
        names(data)[names(data)=='Geography'] = 'state'
        data$location =ifelse (data$state == "District of Columbia", "DC", state.abb[data$state]) 
    }
    if(grepl("ehe", filename)) {
        data$location = as.character(data$FIPS)
    }
    
    data<- data%>%
        select(year, location, outcome, value)
    
    list(filename, data) 
    
})

#############################################################################
###Adding in Section for Atlas Plus PrEP data- outcomes are PrEP and
##PrEP indications
###########################################################################
#---Clean prep---#

data.list.clean.atlas.prep = lapply(data.list.atlas.prep, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    data$outcome = "prep"
    
    names(data)[names(data)=='Year'] = 'year'   
    data$year = substring(data$year,1, 4)                                          
    data$year = as.character(data$year)
    
    data$Cases[data$Cases %in% c("Data suppressed")] = NA    
    data$Cases[data$Cases %in% c("Data not available")] = NA  
    data$value = as.numeric(gsub(",", '', data$Cases))   
    
    
    if(grepl("state", filename)) {
        names(state.abb) <- state.name 
        data$Geography= gsub('[^[:alnum:] ]',"",data$Geography) #some states have ^ for preliminary data#
        names(data)[names(data)=='Geography'] = 'state'
        data$location =ifelse (data$state == "District of Columbia", "DC", state.abb[data$state]) 
    }
    if(grepl("ehe", filename)) {
        data$location = as.character(data$FIPS)
    }
    if(grepl("msa", filename)) {
        data$msa_indicator= substring(data$FIPS, 6, 10)
        
        data$msa_keep = ifelse(data$msa_indicator == "00000", "keep", "drop")
        data = subset(data,data$msa_keep == "keep")   #Can make this shorter once you check over everything#
        
        data$cbsa = substring(data$FIPS, 1, 5)
        data$location = paste("C", data$cbsa, sep=".")
    }
    if(grepl("allcounty", filename)) {
        data$location = as.character(data$FIPS)
    }  
    
    if(grepl("age", filename)) {
        data$age = age.mappings[data$Age.Group]
    }
    if(grepl("race", filename)) {
        names(data)[names(data)=='Race.Ethnicity'] = 'race'
    }
    if(grepl("sex", filename)) {
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
    }
    if(grepl("male", filename)) {
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
    }
    if(grepl("female", filename)) {
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
    }
    if(grepl("risk", filename)) {
        data$risk = risk.mappings[data$Transmission.Category]
    }
    
    list(filename, data) 
    
} )

#---Clean prep INIDCATIONS---#

data.list.clean.indications = lapply(data.list.atlas.prep, function(file){
    
    data=file[["data"]]
    filename = file[["filename"]]
    
    data$outcome = "prep.indications"
    
    names(data)[names(data)=='Year'] = 'year'   
    data$year = substring(data$year,1, 4)                                          
    data$year = as.character(data$year)
    
    data = subset(data, data$year=="2018" | data$year == "2017") #Decided 4-18-24 to remove any years after 2018 bc data is all the same
    
    data$Population[data$Population %in% c("Data suppressed")] = NA    
    data$Population[data$Population %in% c("Data not available")] = NA  
    data$value = as.numeric(gsub(",", '', data$Population))   
    
    if(grepl("state", filename)) {
        names(state.abb) <- state.name 
        data$Geography= gsub('[^[:alnum:] ]',"",data$Geography) #some states have ^ for preliminary data#
        names(data)[names(data)=='Geography'] = 'state'
        data$location =ifelse (data$state == "District of Columbia", "DC", state.abb[data$state]) 
    }
    if(grepl("ehe", filename)) {
        data$location = as.character(data$FIPS)
    }
    if(grepl("msa", filename)) {
        data$msa_indicator= substring(data$FIPS, 6, 10)
        
        data$msa_keep = ifelse(data$msa_indicator == "00000", "keep", "drop")
        data = subset(data,data$msa_keep == "keep")   #Can make this shorter once you check over everything#
        
        data$cbsa = substring(data$FIPS, 1, 5)
        data$location = paste("C", data$cbsa, sep=".")
    }
    if(grepl("allcounty", filename)) {
        data$location = as.character(data$FIPS)
    }  
    
    if(grepl("age", filename)) {
        data$age = age.mappings[data$Age.Group]
    }
    if(grepl("race", filename)) {
        names(data)[names(data)=='Race.Ethnicity'] = 'race'
    }
    if(grepl("sex", filename)) {
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
    }
    if(grepl("male", filename)) {
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
    }
    if(grepl("female", filename)) {
        names(data)[names(data)=='Sex'] = 'sex'
        data$sex = tolower(data$sex)
    }
    if(grepl("risk", filename)) {
        data$risk = risk.mappings[data$Transmission.Category]
    }
    
    list(filename, data) 
    
})


#########################################
##Put data into data manager###

##Outcome=diagnoses

diagnoses_all = lapply(data.list.clean.diagnoses, `[[`, 2)  

for (data in diagnoses_all) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cdc',
        source = 'cdc.hiv',
        dimension.values = list(),
        url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
        details = 'CDC Atlas Plus data')
}

##Outcome=prevalence
prevalence_all = lapply(data.list.clean.prevalence, `[[`, 2) 

for (data in prevalence_all) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cdc',
        source = 'cdc.hiv',
        dimension.values = list(),
        url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
        details = 'CDC Atlas Plus data')
}

##Outcome=deaths 
deaths_all = lapply(data.list.clean.deaths, `[[`, 2)  

for (data in deaths_all) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cdc',
        source = 'cdc.hiv',
        dimension.values = list(),
        url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
        details = 'CDC Atlas Plus data')
}

##Outcome=deaths; details= reporting issues
deaths_notes = lapply(data.list.clean.deaths.notes, `[[`, 2)  

for (data in deaths_notes) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cdc',
        source = 'cdc.hiv',
        dimension.values = list(),
        url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
        details = 'Jurisdiction with incomplete reporting of deaths for most recent year.')
}


##Outcome=SLE 
sle_all = lapply(data.list.clean.sle, `[[`, 2) 

for (data in sle_all) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cdc',
        source = 'cdc.hiv',
        dimension.values = list(),
        url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
        details = 'CDC Atlas Plus data')
}

##National Suppression 
national_suppression_all = lapply(national.suppression, `[[`, 2) 

for (data in national_suppression_all) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cdc.new',
        source = 'cdc.hiv',
        dimension.values = list(),
        url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
        details = 'CDC Atlas Plus data')
}


##Outcome=knowledge/awareness 
knowledge_all = lapply(data.list.clean.knowledge, `[[`, 2)  

for (data in knowledge_all) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cdc',
        source = 'cdc.hiv',
        dimension.values = list(),
        url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
        details = 'CDC Atlas Plus data')
}

#RSE for knowledge/awareness
awareness.rse= lapply(knowledge.rse, `[[`, 2)

for (data in awareness.rse) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cdc',
        source = 'cdc.hiv',
        metric = 'coefficient.of.variance',
        dimension.values = list(),
        url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
        details = 'CDC Atlas Plus data')
}

##outcome= total.prevalence (for population for knowledge
total_prev_all = lapply( data.list.clean.awareness.population, `[[`, 2)  

for (data in total_prev_all) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cdc',
        source = 'cdc.hiv',
        dimension.values = list(),
        url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
        details = 'CDC Atlas Plus data')
}

##Outcome=prep 
prep_atlas_all = lapply(data.list.clean.atlas.prep, `[[`, 2)  

for (data in prep_atlas_all) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cdc',
        source = 'cdc.prep',
        dimension.values = list(),
        url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
        details = 'CDC Atlas Plus data')
}

##Outcome=prep.INDICATIONS
indications_all = lapply(data.list.clean.indications, `[[`, 2)  

for (data in indications_all) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'cdc',
        source = 'cdc.prep.indications',
        dimension.values = list(),
        url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
        details = 'CDC Atlas Plus data')
}

################################################################################
###Rename surveillance manager####

surveillance.manager= data.manager #Add this here so you don't have to change data.manager throughout entire code#

################################################################################
#Decided to save a version of the surveillance manager at this point for trouble shooting in the future:
save(surveillance.manager, file="../../cached/surveillance.manager.temp.rdata")

##Put summation of census counties to create msa populations within the surveillance manager
#smaller.census.manager = load.data.manager("../../cached/smaller.census.manager.rdata")
census.manager = load.data.manager("../../cached/census.manager.rdata")

##Source code for function from Andrew to sum counties populations from census to create MSA populations for surveillance manager
#This code also adjusts the population to be the 'adult.population' ages 13 and over
source('data_processing/put_msa_data_without_estimation_script.R')
#source code to sum diagnosed prevalence and new diagnoses for counties to make up MSAs
source('data_processing/put_msa_data_as_new_source_script.R')

#this function allows data from the census manager to be transformed into 'adult' only outcomes, ages 13+ while maintaining the census manager data
#this function currently defaults to population, when using it for mortality you need to define mortality as the outome#
#adult.population
put.msa.data.strict(locations = MSAS.OF.INTEREST, 
                    data.manager = surveillance.manager, 
                    census.manager = census.manager)


#Put adult population for specific locations
put.msa.data.strict(locations= c(STATES.CONTAINING.LOCATIONS.OF.INTEREST, NSDUH.REGIONS.CONTAINING.LOCATIONS.OF.INTEREST, COUNTIES.CONTAINED.IN.LOCATIONS.OF.INTEREST, COUNTIES.FOR.LIMITED.POPULATION.DATA), 
                    contained.geographic.type = "county", #it will look for counties in the regions above
                    put.stratifications = list ('age', 'sex', 'race', 'ethnicity', c('race', 'ethnicity')),
                    data.manager = surveillance.manager,
                    census.manager = census.manager)

# CREATE NA DATA FRAMES FOR HISTORIC COUNTIES THAT NO LONGER EXIST--------
source('data_processing/dummy.data.frames.R')

# Adult population for recent census data ---------------------------------

#Source code that restructures census age groups to get adult.pop for 2020-2022
source('data_processing/restructure.recent.census.age.groups.R')

################################################################################
#Create aggregated outcomes 
#This is mainly county data being aggregated to MSA level
################################################################################
source('../jheem2/R/HELPERS_array_helpers.R')

#CDC Atlas Plus data
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
                           to.locations =  MSAS.OF.INTEREST,  #Think of this as containing location 
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

#Aggregating STI data
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

#This aggregates county level data to state level for the recent census years for adult.population (as well as county to MSAs of interest)
#where I wrote the restructure.recent.age.groups code to estimate for adult.pop
all.states = locations::get.all.for.type('state')
put.msa.data.as.new.source(outcome = 'adult.population',
                           from.source.name = 'census.population',
                           to.source.name = 'census.aggregated.adult.population',
                           to.locations =  all.states,   
                           geographic.type.from = 'COUNTY',
                           geographic.type.to = 'STATE',
                           details.for.new.data = 'estimated from county data',
                           data.manager = surveillance.manager)

put.msa.data.as.new.source(outcome = 'adult.population',
                           from.source.name = 'census.population',
                           to.source.name = 'census.aggregated.adult.population',
                           to.locations =  MSAS.OF.INTEREST,   
                           geographic.type.from = 'COUNTY',
                           geographic.type.to = 'CBSA',
                           details.for.new.data = 'estimated from county data',
                           data.manager = surveillance.manager)

# Source code to create hiv.tests.per.population --------------------------
source('data_processing/tests.per.population.R')

################################################################################
###Source code for the STI ratio calculation/put
################################################################################
source('data_processing/sti_ratio_calculation.R')

################################################################################
###Put- Sum deaths by county into deaths by MSA using this code/function
################################################################################
source('data_processing/simple_aggregate_county_to_msa_script.R')

get.msa.totals.from.county.simple(outcome= 'deaths', 
                                  metric='estimate',
                                  msas= MSAS.OF.INTEREST,
                                  source.from = 'census.deaths', 
                                  source.to='census.deaths.aggregated',
                                  details.for.put= 'estimated from county data',
                                  data.manager.from=census.manager,
                                  data.manager.to= surveillance.manager)


################################################################################
#Identify Potential Outliers
################################################################################
#TBD


# Save --------------------------------------------------------------------


###Save surveillance manager####
save(surveillance.manager, file="../../cached/surveillance.manager.rdata")

#Also save to Q drive
save(surveillance.manager, file="Q:/data_managers/surveillance.manager.rdata")




