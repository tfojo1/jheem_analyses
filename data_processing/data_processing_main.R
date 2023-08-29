

library(jheem2)
library(tidyverse)


###Initialize data manager (surveillance manager) and establish ontology###

###CDC Atlas Plus Data###

data.manager = create.data.manager('test', description='a data manager to test with')

data.manager$register.outcome(
  'diagnoses',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'New Diagnoses',
    axis.name = 'New Diagnoses (n)',
    units = 'cases',
    description = "New HIV Cases Diagnosed in a Year"))

data.manager$register.outcome(
'hiv deaths',
metadata = create.outcome.metadata(
  scale = 'non.negative.number',
  display.name = 'HIV Deaths',
  axis.name = 'HIV Deaths (n)',
  units = 'cases',
  description = "HIV Deaths"))

data.manager$register.outcome(
  'prevalence',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Prevalence',
    axis.name = 'Prevalence (n)',
    units = 'cases',
    description = "HIV Prevalence"))

data.manager$register.outcome(
  'linkage',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Linkage',
    axis.name = 'Linkage (n)',
    units = 'cases',
    description = "Linkage to HIV care"))

data.manager$register.outcome(
  'care',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Care',
    axis.name = 'Care (n)',
    units = 'cases',
    description = "Receipt of HIV medical care"))

data.manager$register.outcome(
  'suppression',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Suppression',
    axis.name = 'Suppression (n)',
    units = 'cases',
    description = "HIV Viral suppression"))

data.manager$register.outcome(
  'knowledge',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'knowledge',
    axis.name = 'knowledge (n)',
    units = 'cases',
    description = "Knowledge of Status"))

data.manager$register.outcome(
  'prep',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'prep',
    axis.name = 'prep (n)',
    units = 'cases',
    description = "PrEP Use"))

data.manager$register.outcome(
  'aids_prevalence',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'aids_prevalence',
    axis.name = 'aids_prevalence (n)',
    units = 'cases',
    description = "AIDS Prevalence"))

data.manager$register.outcome(
  'aids_diagnoses',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'aids_diagnoses',
    axis.name = 'aids_diagnoses (n)',
    units = 'cases',
    description = "AIDS Diagnoses"))

data.manager$register.source('aidsvu', full.name = "AIDS Vu", short.name='aidsvu')

data.manager$register.source('cdc', full.name = "US Centers for Disease Control and Prevention", short.name='CDC')

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
    age=c('10-14 years', '15-19 years', '20-24 years', '25-29 years', '30-34 years', '35-39 years',
          '40-44 years', '45-54 years', '55-64 years', '65+ years', 'Unknown'),
    race=c('American Indian/Alaska Native', 'Asian', 'Black/African American', 'Hispanic/Latino', 'White', 'Unknown'),
    sex=c('male','female'),
    risk=c('msm','idu','msm_idu','heterosexual','other')
  ))
################################################################################

###Source in File that reads .csvs and removes headers###

source('data_processing/fix_cdc_headers.R')

###Source in AIDS Vu data and cleaning###

source('data_processing/aids_vu_processing.R')

###Source in CDC MSA PDF Reports data and cleaning###

#source('data_processing/msa_reports_processing.R')

################################################################################

###Define the 'mappings' for Atlas plus data###

outcome.mappings = c('HIV diagnoses'='diagnoses',
                     'HIV deaths' = 'hiv deaths',
                     'HIV prevalence' = 'prevalence',
                     'Linkage to HIV care' = 'linkage',
                     'Receipt of HIV medical care' = 'care',
                     'HIV viral suppression' = 'suppression',
                     'Knowledge of Status' = 'knowledge')


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

#---Clean Knowledge---#

data.list.clean.knowledge = lapply(data.list.knowledge, function(file){
  
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
    details = 'CDC Reporting')
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
    details = 'CDC Reporting')
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
    details = 'CDC Reporting')
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
     details = 'CDC Reporting')
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
     details = 'CDC Reporting')
 }
 
 
################################################################################
 ###Use various pull statements to check that data you put in is correct###
 
 x = (data.manager$pull(
   outcome = 'diagnoses',
   keep.dimensions = c('year', 'age'),
   dimension.values = list(sex=c('male', 'female'))))
 
 y =(data.manager$pull(
   outcome='hiv deaths',
   keep.dimensions = c('location', 'year')))
 
 z =(data.manager$pull(
   outcome='prevalence',
   keep.dimensions = c('location', 'year')))

 xx = (data.manager$pull(
   outcome = 'linkage',
   keep.dimensions = c('location', 'year')))

yy = (data.manager$pull(
   outcome = 'care',
   keep.dimensions = c('location', 'year')))

zz = (data.manager$pull(
  outcome = 'suppression',
  keep.dimensions = c('location', 'year')))

zz = (data.manager$pull(
  outcome = 'knowledge',
  keep.dimensions = c('location', 'year')))

 
 
################################################################################
###Save surveillance manager####
  
 surveillance.manager= data.manager #Add this here so you don't have to change data.manager throughout entire code#
  
  save(surveillance.manager, file="../../cached/surveillance.manager.rdata")


