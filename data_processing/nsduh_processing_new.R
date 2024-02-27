# library(jheem2)
# library(tidyverse)
# library(stringr)
# library(haven)

################################################################################
###Read in NSDUH Data###
################################################################################ 
DATA.DIR.NSDUH="../../data_raw/nsduh"

nsduh_files <- Sys.glob(paste0(DATA.DIR.NSDUH, '/*.csv'))

#creating a list with sublists of filename, data#
data.list.nsduh <- lapply(nsduh_files, function(x){
  list(filename=x, data=read.csv(x, header=TRUE))
})
################################################################################
#Create template for NSDUH data
################################################################################
nsduh.clean= lapply(data.list.nsduh, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data= subset(data, data$geography != "South")
  data= subset(data, data$geography != "West")
  data= subset(data, data$geography != "Midwest")
  data= subset(data, data$geography != "Northeast")
  
  data$estimate = if_else(data$estimate == "suppressed", NA, data$estimate)
  
  if(grepl("heroin", filename)) {
    data$outcome = "heroin"
  }
  if(grepl("cocaine", filename)) {
    data$outcome = "cocaine"
  }
  if(grepl("02.04", filename)) {
    data$year = "2002-2004"
  }
  if(grepl("04.06", filename)) {
    data$year = "2004-2006"
  }
  if(grepl("06.08", filename)) {
    data$year = "2006-2008"
  }
  if(grepl("08.10", filename)) {
    data$year = "2008-2010"
  }
  if(grepl("10.12", filename)) {
    data$year = "2010-2012"
  }
  if(grepl("12.14", filename)) {
    data$year = "2014-2016"
  }
  if(grepl("14.16", filename)) {
    data$year = "2014-2016"
  }
  if(grepl("16.18", filename)) {
    data$year = "2016-2018"
  }
  data= as.data.frame(data)
  
  list(filename, data)
})
################################################################################
###REGION### (total and age)
################################################################################
nsduh.region.total = lapply(nsduh.clean, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  data= subset(data, data$geography != "United States")
  
  #Create state abbreviations to get regions by filtering by those w/o a state abbrev
  data <- data%>%
    mutate(state_abbrev = state.abb[match(geography, state.name)])%>%
    mutate(state_abbrev = if_else(geography == "District of Columbia", "DC", state_abbrev ))%>%
    filter(is.na(state_abbrev))
  
  data$location = locations::get.location.code(data$geography, "NSDUH")
  data$location = as.character(data$location)
  
  #####Removing Invalid MSAs (instructed by Todd 11/9########
  data <- data %>%
    mutate(location_check = locations::is.location.valid(location))%>%
    filter(location_check == "TRUE")
  #I'm going to remove locations that are NA
  data = subset(data, !is.na(data$location))
  
  data$value = as.numeric(data$estimate)
  #I think you need to take out NA values from values col in order to put into manager#
  data = subset(data, !is.na(data$value))
  
  data$age = data$age_group
  #Decide on 2-27-24 to change age group ontology:
  data <- data%>%
    filter(age_group == "12 or Older")
  
  data <- data %>%
    select(year, outcome, value, location)
  
  data= as.data.frame(data)
  
  list(filename, data)
})

nsduh.region.age= lapply(nsduh.clean, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  data= subset(data, data$geography != "United States")
  
  #Create state abbreviations to get regions by filtering by those w/o a state abbrev
  data <- data%>%
    mutate(state_abbrev = state.abb[match(geography, state.name)])%>%
    mutate(state_abbrev = if_else(geography == "District of Columbia", "DC", state_abbrev ))%>%
    filter(is.na(state_abbrev))
  
  data$location = locations::get.location.code(data$geography, "NSDUH")
  data$location = as.character(data$location)
  
  #####Removing Invalid MSAs (instructed by Todd 11/9########
  data <- data %>%
    mutate(location_check = locations::is.location.valid(location))%>%
    filter(location_check == "TRUE")
  #I'm going to remove locations that are NA
  data = subset(data, !is.na(data$location))
  
  data$value = as.numeric(data$estimate)
  #I think you need to take out NA values from values col in order to put into manager#
  data = subset(data, !is.na(data$value))
  
  #Decide on 2-27-24 to change age group ontology:
  data <- data%>%
    filter(age_group == "26 or Older")
  data$age = data$age_group
  
  data <- data %>%
    select(year, outcome, value, location, age)
  
  data= as.data.frame(data)
  
  list(filename, data)
})
################################################################################
###STATE### (age and total)
################################################################################

nsduh.state.total = lapply(nsduh.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data= subset(data, data$geography != "United States")
  
  data <- data%>%
    mutate(state = state.abb[match(geography, state.name)])%>%
    mutate(location = ifelse(geography == "District of Columbia", "DC", state))
  
  data= subset(data, !is.na(data$location)) #remove any location that isn't a state
  
  data$value = as.numeric(data$estimate)

  #Decide on 2-27-24 to change age group ontology:
  data <- data%>%
    filter(age_group == "12 or Older")
  
  data$age = data$age_group
  
  data <- data %>%
    select(year, outcome, value, location)
  
  data= as.data.frame(data)
  
  list(filename, data) 
})

nsduh.state.age = lapply(nsduh.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data= subset(data, data$geography != "United States")
  
  data <- data%>%
    mutate(state = state.abb[match(geography, state.name)])%>%
    mutate(location = ifelse(geography == "District of Columbia", "DC", state))
  
  data= subset(data, !is.na(data$location)) #remove any location that isn't a state
  
  data$value = as.numeric(data$estimate)
  
  #Decide on 2-27-24 to change age group ontology:
  data <- data%>%
    filter(age_group == "26 or Older")
  
  data$age = data$age_group
  
  data <- data %>%
    select(year, outcome, value, location, age)
  
  data= as.data.frame(data)
  
  list(filename, data) 
})

################################################################################
###NATIONAL###
################################################################################

nsduh.national.total = lapply(nsduh.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data= subset(data, data$geography == "United States")
  
  data$value = as.numeric(data$estimate)
  data$location = "US"
  
  #Decide on 2-27-24 to change age group ontology:
  data <- data%>%
    filter(age_group == "12 or Older")
  
  data$age = data$age_group
  
  data <- data %>%
    select(year, outcome, value, location)
  
  data= as.data.frame(data)
  
  list(filename, data) 
})


nsduh.national.age = lapply(nsduh.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data= subset(data, data$geography == "United States")
  
  data$value = as.numeric(data$estimate)
  data$location = "US"

  #Decide on 2-27-24 to change age group ontology:
  data <- data%>%
    filter(age_group == "26 or Older")
  
  data$age = data$age_group
  
  data <- data %>%
    select(year, outcome, value, location, age)
  
  data= as.data.frame(data)
  
  list(filename, data) 
})
################################################################################
###Put Data into Data Manager###
################################################################################
nsduh_region = lapply(nsduh.region.total, `[[`, 2)

for (data in nsduh_region) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'nsduh',
    source = 'nsduh',
    dimension.values = list(),
    url = 'https://pdas.samhsa.gov/saes/substate',
    details = 'NSDUH Substate Estimates')
}
nsduh_region_age = lapply(nsduh.region.age, `[[`, 2)

for (data in nsduh_region_age) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'nsduh',
    source = 'nsduh',
    dimension.values = list(),
    url = 'https://pdas.samhsa.gov/saes/substate',
    details = 'NSDUH Substate Estimates')
}
##
nsduh_state = lapply(nsduh.state.total, `[[`, 2)

for (data in nsduh_state) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'nsduh',
    source = 'nsduh',
    dimension.values = list(),
    url = 'https://pdas.samhsa.gov/saes/substate',
    details = 'NSDUH Substate Estimates')
}
nsduh_state_age = lapply(nsduh.state.age, `[[`, 2)

for (data in nsduh_state_age) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'nsduh',
    source = 'nsduh',
    dimension.values = list(),
    url = 'https://pdas.samhsa.gov/saes/substate',
    details = 'NSDUH Substate Estimates')
}
##
nsduh_national_total = lapply(nsduh.national.total, `[[`, 2)

for (data in nsduh_national_total) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'nsduh',
    source = 'nsduh',
    dimension.values = list(),
    url = 'https://pdas.samhsa.gov/saes/substate',
    details = 'NSDUH Substate Estimates')
}
nsduh_national_age = lapply(nsduh.national.age, `[[`, 2)

for (data in nsduh_national_age) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'nsduh',
    source = 'nsduh',
    dimension.values = list(),
    url = 'https://pdas.samhsa.gov/saes/substate',
    details = 'NSDUH Substate Estimates')
}
