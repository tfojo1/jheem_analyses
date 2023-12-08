# library(jheem2)
# library(tidyverse)

################################################################################
                ###Read in STI Data from Atlas Plus###
################################################################################ 

DATA.DIR.STI="../../data_raw/sti"

sti_files <- Sys.glob(paste0(DATA.DIR.STI, '/*.csv'))

#creating a list with sublists of filename, data#
data.list.sti <- lapply(sti_files, function(x){
  skip=7
  list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
})

################################################################################
              ###Read in early syphilis data###
################################################################################ 

DATA.DIR.EARLY="../../data_raw/sti/early_syphilis"

early_files <- Sys.glob(paste0(DATA.DIR.EARLY, '/*.csv'))

#creating a list with sublists of filename, data#
data.list.early <- lapply(early_files, function(x){
  skip=7
  list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
})

################################################################################
                    ###Clean STI Data from Atlas Plus###
                       ###Syphilis and Gonorrhea###
                ##Updating to add early and congenital syphilis##
################################################################################

#MAPPINGS#
outcome.mappings.sti = c('Primary and Secondary Syphilis'='ps.syphilis',
                     'Early Non-Primary, Non-Secondary Syphilis' = 'early.syphilis',
                     'Congenital Syphilis' = 'congenital.syphilis',
                     'Gonorrhea' = 'gonorrhea')

age.mappings.sti = c('0-14' = '0-14 years',
                 '15-19' = '15-19 years',
                 '20-24' = '20-24 years',
                 '25-29' = '25-29 years',
                 '30-34' = '30-34 years',
                 '35-39' = '35-39 years',
                 '40-44' = '40-44 years',
                 '45-54' = '45-54 years',
                 '55-64' = '55-64 years',
                 '65+' = '65+ years',
                 'Unknown' = 'unknown',
                 '15-24'= '15-24 years',
                 '25-34'= '25-34 years',
                 '35-44'= '35-44 years')

####
data.list.sti.clean = lapply(data.list.sti, function(file){

  data=file[["data"]]
  filename = file[["filename"]]

  data$year = substr(data$Year, 1, 4)
  data$Cases = (gsub(",", "", data$Cases))

  data$outcome = outcome.mappings.sti [data$Indicator]

  data <- data %>%
  mutate(value= ifelse(grepl("Data not available", Cases), NA,
                      ifelse(grepl("Data suppressed", Cases), NA, Cases)))
  data$value = as.numeric(data$value)

  if(grepl("state", filename)) {
    names(state.abb) <- state.name
    data$location =ifelse (data$Geography == "District of Columbia", "DC", state.abb[data$Geography])
  }

  if(grepl("county", filename)) {
    data$location = data$FIPS
  }
  
  ##Demographic conditionals##
  
  if(grepl("age", filename)) {
    data$age = age.mappings.sti[data$Age.Group]
  }
  if(grepl("race", filename)) {
    data$race= data$'Race.Ethnicity'
  }
  if(grepl("sex", filename)) {
    names(data)[names(data)=='Sex'] = 'sex'
    data$sex = tolower(data$sex)
  }

  data= as.data.frame(data)

  list(filename, data)
})

################################################################################
               ###Clean Early Syphilis Data###
################################################################################

data.list.early.syphilis = lapply(data.list.early, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$year = substr(data$Year, 1, 4)
  data$Cases = (gsub(",", "", data$Cases))
  
  data$outcome = outcome.mappings.sti [data$Indicator]
  
  data <- data %>%
    mutate(value= ifelse(grepl("Data not available", Cases), NA,
                         ifelse(grepl("Data suppressed", Cases), NA, Cases)))
  data$value = as.numeric(data$value)
  
  if(grepl("state", filename)) {
    names(state.abb) <- state.name
    data$location =ifelse (data$Geography == "District of Columbia", "DC", state.abb[data$Geography])
  }
  
  if(grepl("county", filename)) {
    data$location = data$FIPS
  }
  
  ##Demographic conditionals##
  
  if(grepl("age", filename)) {
    data$age = age.mappings.sti[data$Age.Group]
  }
  if(grepl("race", filename)) {
    data$race= data$'Race.Ethnicity'
  }
  if(grepl("sex", filename)) {
    names(data)[names(data)=='Sex'] = 'sex'
    data$sex = tolower(data$sex)
  }
  
  data= as.data.frame(data)
  
  list(filename, data)
})

################################################################################
                      ###Put Data into Data Manager###
################################################################################

#Create 2 put statements one for the majority of the STI data and then one separate put
#for the early.syphilis data bc the ontology is different bc of the slight change in age groups

sti_data = lapply(data.list.sti.clean, `[[`, 2)

 for (data in sti_data) {

   data.manager$put.long.form(
     data = data,
     ontology.name = 'cdc.sti',
     source = 'cdc',
     dimension.values = list(),
     url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
     details = 'CDC Reporting')
 }

early_syphilis_data = lapply(data.list.early.syphilis, `[[`, 2)

for (data in early_syphilis_data) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.syphilis',
    source = 'cdc',
    dimension.values = list(),
    url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
    details = 'CDC Reporting')
}
