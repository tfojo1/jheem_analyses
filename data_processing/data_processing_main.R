
library(jheem2)
library(tidyverse)
library(readxl)
library(stringr)
library(haven)
library(locations)

###Initialize data manager (surveillance manager) and establish ontology###

###CDC Atlas Plus Data###

data.manager = create.data.manager('surveillance', description='surveillance data manager')

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
    description = "Awareness of HIV Status"), denominator.outcome = 'total.prevalence') #creating outcome that is pop values for awarness#

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
  'ps.syphilis',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Primary and Secondary Syphilis',
    axis.name = 'Primary and Secondary Syphilis',
    units = 'cases',
    description = "Primary and Secondary Syphilis"))

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
    scale = 'non.negative.number',
    display.name = 'Heroin Use in the Past Year',
    axis.name = 'Heroin Use in the Past Year (n)',
    units = 'cases',
    description = "Heroin Use in the Past Year"))

data.manager$register.outcome(
  'cocaine', 
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Cocaine use in the Past Year',
    axis.name = 'Cocaine use in the Past Year (n)',
    units = 'cases',
    description = "Cocaine Use in the Past Year"))

data.manager$register.outcome(
  'hiv.tests',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'HIV Tests',
    axis.name = 'HIV Tests (n)',
    units = 'cases',
    description = "Count of CDC Funded HIV Tests"))

data.manager$register.outcome(
  'hiv.test.positivity', #This was newly.diagnosed.positives, changing it to hiv.test.positivity put this in as a percentage not a count#
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

data.manager$register.source('aidsvu', full.name = "AIDS Vu", short.name='aidsvu')

data.manager$register.source('cdc', full.name = "US Centers for Disease Control and Prevention", short.name='cdc')

data.manager$register.source('nsduh', full.name = "National Survey on Drug Use and Health", short.name='nsduh')

data.manager$register.source('lhd', full.name = "Local Health Department", short.name='lhd')

data.manager$register.source('brfss', full.name = "Behavioral Risk Factor Surveillance System", short.name='brfss')

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
  'aidsvu',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('under 25 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
    race=c('black', 'hispanic', 'white'),
    sex=c('male','female')
    
  ))

data.manager$register.ontology(
  'cdc msa reports',
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
  'cdc sti',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('0-14 years', '15-19 years', '20-24 years', '25-29 years', '30-34 years', '35-39 years', '40-44 years', '45-54 years', '55-64 years', '65+ years', 'unknown'),
    race=c('American Indian/Alaska Native', 'Asian', 'Black/African American', 'Hispanic/Latino', 'Multiracial', 'Native Hawaiian/Other Pacific Islander', 'White', 'Unknown'),
    sex=c('male','female'),
    risk=c('msm','idu','msm_idu','heterosexual','other')
  ))

#Create a separate ontology for early syphilis
data.manager$register.ontology(
  'cdc syphilis',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('0-14 years', '15-24 years', '25-34 years', '35-44 years', '45-54 years', '55-64 years', '65+ years', 'unknown'), 
    race=c('American Indian/Alaska Native', 'Asian', 'Black/African American', 'Hispanic/Latino', 'Multiracial', 'Native Hawaiian/Other Pacific Islander', 'White', 'Unknown'),
    sex=c('male','female'),
    risk=c('msm','idu','msm_idu','heterosexual','other')
  ))

data.manager$register.ontology(
  'nsduh',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('12 or Older', '12 to 17', '18 or Older', '18 to 25', '26 or Older'))
  )

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
    race=c('White', 'Black', 'American Indian/Alaska Native', 'Asian', 'Native Hawaiian/Other Pacific Islander', 'Other race', 'Multiracial', 'Hispanic', 'Unknown' ),
    sex=c('male','female'),
    risk=c('msm')
  ))

################################################################################

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
source('data_processing/nsduh_processing.R')

###Source in state retention data
source('data_processing/state_retention.R')

###Source in CDC MSA PDF Reports data and cleaning
##Pending location package updates
#source('data_processing/msa_reports_processing.R')

##Source in Code to use the county prevalence data from Atlas Plus in 2017
## To sum counties to estimate MSA data
# source('data_processing/msa_prevalence_totals_2017.R')

#Source BRFSS testing data
#source('data_processing/brfss.R')

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
    source = 'cdc',
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
    source = 'cdc',
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
    source = 'cdc',
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
    source = 'cdc',
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
     source = 'cdc',
     dimension.values = list(),
     url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
     details = 'CDC Atlas Plus data')
 }
 

 ##Outcome=knowledge 
 knowledge_all = lapply(data.list.clean.knowledge, `[[`, 2)  
 
 for (data in knowledge_all) {
   
   data.manager$put.long.form(
     data = data,
     ontology.name = 'cdc',
     source = 'cdc',
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
     source = 'cdc',
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
     source = 'cdc',
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
     source = 'cdc',
     dimension.values = list(),
     url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
     details = 'CDC Atlas Plus data')
 }
 
################################################################################
 ###Use various pull statements to check that data you put in is correct###
 
 x = (data.manager$pull(
   outcome = 'diagnoses',
   keep.dimensions = c('year', 'age'),
   dimension.values = list(sex=c('male', 'female'))))
 
 y =(data.manager$pull(
   outcome='hiv.deaths',
   keep.dimensions = c('location', 'year')))
 
 
################################################################################
###Save surveillance manager####
  
 surveillance.manager= data.manager #Add this here so you don't have to change data.manager throughout entire code#
  
  save(surveillance.manager, file="../../cached/surveillance.manager.rdata")


