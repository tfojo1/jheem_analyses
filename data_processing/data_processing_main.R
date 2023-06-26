

library(jheem2)
library(tidyverse)


###Initialize data manager (surveillance manager) and establish ontology###

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
'deaths',
metadata = create.outcome.metadata(
  scale = 'non.negative.number',
  display.name = 'Deaths',
  axis.name = 'Deaths (n)',
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

data.manager$register.source('cdc', full.name = "US Centers for Disease Control and Prevention", short.name='CDC')

data.manager$register.ontology(
  'cdc',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
    race=c('American Indian/Alaska Native', 'Asian', 'Black/African American', 'Hispanic/Latino', 'Mutiracial', 'Native Hawaiian/Other Pacific Islander', 'White'),
    sex=c('male','female'),
    risk=c('msm','idu','msm_idu','heterosexual','other')
    ))


################################################################################

###Source in File that reads .csvs and removes headers###

source('data_processing/fix_headers.R')

################################################################################

###Define the 'mappings' for Atlas plus data###

outcome.mappings = c('HIV diagnoses'='diagnoses',
                     'HIV deaths' = 'deaths',
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
    data$location = data$County
  }
  if(grepl("msa", filename)) {
    data$location = data$Geography
  }
  
  # Add this back once you have county data saved#
  #  if(grepl("county", filename)) {
  #    #data$age = age.mappings[data$Age.Group]
  #  }
  #  
  
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
    data$location = data$County
  }
  if(grepl("msa", filename)) {
    data$location = data$Geography
  }
  
  # Add this back once you have county data saved#
  #  if(grepl("county", filename)) {
  #    #data$age = age.mappings[data$Age.Group]
  #  }
  #  

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
    data$location = data$County
  }
  if(grepl("msa", filename)) {
    data$location = data$Geography
  }
  
  # Add this back once you have county data saved#
  #  if(grepl("county", filename)) {
  #    #data$age = age.mappings[data$Age.Group]
  #  }
  #  
  
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
    data$location = data$County
  }
  if(grepl("msa", filename)) {
    data$location = data$Geography
  }
  
  # Add this back once you have county data saved#
  #  if(grepl("county", filename)) {
  #    #data$age = age.mappings[data$Age.Group]
  #  }
  #  
  
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

data.list.clean.Knowledge = lapply(data.list.knowledge, function(file){
  
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
    data$location = data$County
  }
  if(grepl("msa", filename)) {
    data$location = data$Geography
  }
  
  # Add this back once you have county data saved#
  #  if(grepl("county", filename)) {
  #    #data$age = age.mappings[data$Age.Group]
  #  }
  #  
  
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


################################################################################


###Put in data manager### 
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc',
    source = 'cdc',
    dimension.values = list(),                        #optional#
    url = 'https://gis.cdc.gov/grasp/nchhstpatlas/main.html',
    details = 'CDC Reporting')                                                  #Methodology footnotes here if there is substantial change#

  #Record years and locations
 locations = union(locations, unlist(unique(data['location'])))                 #Question: How is this section used?#
 years = union(years, unlist(unique(data['year'])))



################################################################################
###Save surveillance manager####
  
  surveilance.manager= data.manager #Add this here so you don't have to change data.manager throughout entire code#
  
  save(surveillance.manager, file="../../cached/surveillance.manager.rdata")


