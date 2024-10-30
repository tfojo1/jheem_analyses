#HIV New Diagnoses = diagnoses
#Prevalence = diagnosed.prevalence; total.prevalence
#Proportion Engaged in Care = engagement
#Proportion Suppressed = suppression

#County and MSA level

#I'm going to start with just the data from atlas plus and then I can add more in later if need.


# READ in HIV data --------------------------------------------------------

DATA.DIR.DIAGNOSES="../../data_raw/diagnoses"
DATA.DIR.PREVALENCE="../../data_raw/prevalence"
DATA.DIR.SLE="../../data_raw/sle"
DATA.DIR.NATIONAL.SUPPRESSION="../../data_raw/sle/national_suppression"
DATA.DIR.KNOWLEDGE="../../data_raw/knowledge" #This is used for total prevalence

#---HIV Diagnoses---#
diagnoses_files <- Sys.glob(paste0(DATA.DIR.DIAGNOSES, '/*.csv'))
#creating a list with sublists of filename, data#
data.list.diagnoses <- lapply(diagnoses_files, function(x){
  skip=8
  list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
})

#---HIV Prevalence---#
prevalence_files <- Sys.glob(paste0(DATA.DIR.PREVALENCE, '/*.csv'))
data.list.prevalence <- lapply(prevalence_files, function(x){
  skip=10
  list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
})

#---Suppression, Linkage, Receipt of Care---#
sle_files <- Sys.glob(paste0(DATA.DIR.SLE, '/*.csv'))
data.list.sle <- lapply(sle_files, function(x){
  if(grepl("allcounty", x)) {
    skip=9
    list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
  }
  else  skip=11
  list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
})

#---National Level Suppression--#
national_suppression_files <- Sys.glob(paste0(DATA.DIR.NATIONAL.SUPPRESSION, '/*.csv'))
data.list.national.suppression <- lapply(national_suppression_files, function(x){
  skip=9
  list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
})
#---Knowledge to Care---#
knowledge_files <- Sys.glob(paste0(DATA.DIR.KNOWLEDGE, '/*.csv'))
data.list.knowledge <- lapply(knowledge_files, function(x){
  skip=10
  list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
})

################################################################################
###Define the 'mappings' for Atlas plus data###

outcome.mappings = c('HIV diagnoses'='hiv.diagnoses',
                     'HIV prevalence' = 'hiv.diagnosed.prevalence',
                     'Linkage to HIV care' = 'hiv.linkage_1mo',
                     'Receipt of HIV medical care' = 'hiv.engagement',
                     'HIV viral suppression' = 'hiv.suppression',
                     'Knowledge of Status' = 'hiv.awareness')

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

#################################################################################

# Diagnoses ---------------------------------------------------------------
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
  
})

# Diagnosed Prevalence ----------------------------------------------------

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

# Total Prevalence --------------------------------------------------------
#Create a new dataset to put in the 'population' column for the outcome=knowledge/awareness data
#this is because this proportion has a denominator value which is an estimation of both dx and un-dx hiv cases and we don't have this anywhere else
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


data.list.clean.awareness.population = lapply(data.list.knowledge, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$outcome = "hiv.total.prevalence"
  
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

# Engagement and Suppression--------------------------------------------------------------

data.list.clean.sle = lapply(data.list.sle, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$outcome = outcome.mappings[data$Indicator]
  data = data[!is.na(data$outcome),]
  
  data= subset(data, data$outcome == "hiv.suppression" | data$outcome == "hiv.engagement")
  
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
    data <- data %>%
      filter(race != "Multiracial") #removing multiracial from the proportion outcomes
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
    data <- data %>%
      filter(race != "Multiracial") #removing multiracial from the proportion outcomes
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


# Put into Syphilis Manager -----------------------------------------------

##Outcome=diagnoses

diagnoses_all = lapply(data.list.clean.diagnoses, `[[`, 2)  

for (data in diagnoses_all) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc',
    source = 'cdc.hiv',
    dimension.values.to.distribute = list(race=c('Multiracial')),
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
    dimension.values.to.distribute = list(race=c('Multiracial')),
    url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
    details = 'CDC Atlas Plus data')
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

##outcome= total.prevalence (for population for knowledge
total_prev_all = lapply(data.list.clean.awareness.population, `[[`, 2)  

for (data in total_prev_all) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc',
    source = 'cdc.hiv',
    dimension.values.to.distribute = list(race=c('Multiracial')),
    url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
    details = 'CDC Atlas Plus data')
}

