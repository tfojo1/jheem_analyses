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
  if(grepl("depression", filename)) {
    data$outcome = "depression"
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
    filter(outcome != "depression")%>% #depression does not have age group 12+ so we are not putting a total just age stratified data
    filter(age_group == "12 or Older")
  
  data <- data %>%
    select(year, outcome, value, location)
  
  data= as.data.frame(data)
  
  list(filename, data)
})

#Bring in file made up of census data to use to create new age groupings
load("data_processing/hiv.surveillance.manager/age_regroup_substate_region.RData")

nsduh.region.age.13.24= lapply(nsduh.clean, function(file){

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

  ##March 2024: Creating new age groupings
  data <- data %>% 
    filter(age_group != "12 or Older")%>% #remove age groups we don't need
    filter (age_group != "18 or Older")%>% #remove age groups we don't need
    filter(!is.na(estimate)) #remove estimate is missing

  data =left_join(data, age_regroup_substate_region, by=join_by("location", "year"), relationship = "many-to-many") #join with census data
  
  data$estimate = as.numeric(data$estimate)
  
  data$multiplied = ifelse(data$age_group == "12 to 17",
                            data$estimate * data$population.13.17,
                            NA)
  
  data$multiplied = ifelse(data$age_group == "18 to 25",
                            data$estimate * data$population.18.25,
                            data$multiplied)
  
  data <- data %>%
    filter(age_group != "26 or Older")%>%
    group_by(location, year)%>%
    mutate(numerator.sum = sum(multiplied))%>%
    mutate(denominator.sum = population.13.17 + population.18.25)%>%
    mutate(new.age.group.value = numerator.sum / denominator.sum)%>%
    mutate(new.age.group.name = "13-24")

    data <- data %>%
    rename(age = new.age.group.name)%>%
    rename(value = new.age.group.value)%>%
    filter(!is.na (value))%>% #there are some NAs I think because of regions that are in NSDUH data but not in our locations package
    select(outcome, location, year, age, value)

  data= as.data.frame(data)

  list(filename, data)
})

nsduh.region.age.25.and.older = lapply(nsduh.clean, function(file){
  
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
  
  ##This will only have 26+ (but we are calling it 25+)
  data <- data %>% 
    filter(age_group == "26 or Older")%>%
    mutate(age = "25+")%>%
    filter(!is.na(estimate)) #remove estimate is missing
  
  data$value = as.numeric(data$estimate)
  
  data <- data %>%
    select(year, outcome, value, location, age)
  
  data= as.data.frame(data)
  
  list(filename, data)
})

##Removing 2002-2004 years we don't have data
nsduh.region.age.13.24 = nsduh.region.age.13.24[c(F, T, T, T, T, T, T, T, T, T)]
nsduh.region.age.25.and.older = nsduh.region.age.25.and.older[c(F, T, T, T, T, T, T, T, T, T)]

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
    filter(outcome != "depression")%>% #depression does not have age group 12+ so we are not putting a total just age stratified data
    filter(age_group == "12 or Older")
  
  data$age = data$age_group
  
  data <- data %>%
    select(year, outcome, value, location)
  
  data= as.data.frame(data)
  
  list(filename, data) 
})

#Bring in file made up of census data to use to create new age groupings
load("data_processing/hiv.surveillance.manager/age_regroup_state.RData")

nsduh.state.age.13.24= lapply(nsduh.clean, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  data= subset(data, data$geography != "United States")
  
  data <- data%>%
    mutate(state = state.abb[match(geography, state.name)])%>%
    mutate(location = ifelse(geography == "District of Columbia", "DC", state))
  
  ##March 2024: Creating new age groupings
  data <- data %>% 
    filter(age_group != "12 or Older")%>% #remove age groups we don't need
    filter (age_group != "18 or Older")%>% #remove age groups we don't need
    filter(!is.na(estimate)) #remove estimate is missing
  
  data =left_join(data, age_regroup_state, by=join_by("location", "year"), relationship = "many-to-many") #join with census data
  
  data$estimate = as.numeric(data$estimate)
  
  data$multiplied = ifelse(data$age_group == "12 to 17",
                           data$estimate * data$population.13.17,
                           NA)
  
  data$multiplied = ifelse(data$age_group == "18 to 25",
                           data$estimate * data$population.18.25,
                           data$multiplied)
  
  data <- data %>%
    filter(age_group != "26 or Older")%>%
    group_by(location, year)%>%
    mutate(numerator.sum = sum(multiplied))%>%
    mutate(denominator.sum = population.13.17 + population.18.25)%>%
    mutate(new.age.group.value = numerator.sum / denominator.sum)%>%
    mutate(new.age.group.name = "13-24")
  
  data <- data %>%
    rename(age = new.age.group.name)%>%
    rename(value = new.age.group.value)%>%
    filter(!is.na (value))
  
  # %>% #there are some NAs I think because of regions that are in NSDUH data but not in our locations package
  #   select(outcome, location, year, age, value)
  
  data= as.data.frame(data)
  
  list(filename, data)
})

nsduh.state.age.25.and.older = lapply(nsduh.clean, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  data= subset(data, data$geography != "United States")
  
  data <- data%>%
    mutate(state = state.abb[match(geography, state.name)])%>%
    mutate(location = ifelse(geography == "District of Columbia", "DC", state))
  
  data= subset(data, !is.na(data$location)) #remove any location that isn't a state
  
  ##This will only have 26+ (but we are calling it 25+)
  data <- data %>% 
    filter(age_group == "26 or Older")%>%
    mutate(age = "25+")%>%
    filter(!is.na(estimate)) #remove estimate is missing
  
  data$value = as.numeric(data$estimate)
  
  # data <- data %>%
  #   select(year, outcome, value, location, age)
  
  data= as.data.frame(data)
  
  list(filename, data)
})


##Removing 2002-2004 years we don't have data
nsduh.state.age.13.24 = nsduh.state.age.13.24[c(F, T, T, T, T, T, T, T, T, T)]
nsduh.state.age.25.and.older = nsduh.state.age.25.and.older[c(F, T, T, T, T, T, T, T, T, T)]

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

# 
# nsduh.national.age = lapply(nsduh.clean, function(file){
#   
#   data=file[[2]] 
#   filename = file[[1]] 
#   
#   data= subset(data, data$geography == "United States")
#   
#   data$value = as.numeric(data$estimate)
#   data$location = "US"
# 
#   #Decide on 2-27-24 to change age group ontology:
#   data <- data%>%
#     filter(age_group == "26 or Older")
#   
#   data$age = data$age_group
#   
#   data <- data %>%
#     select(year, outcome, value, location, age)
#   
#   data= as.data.frame(data)
#   
#   list(filename, data) 
# })
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

nsduh_region_age1= lapply(nsduh.region.age.13.24, `[[`, 2)

for (data in nsduh_region_age1) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'nsduh',
    source = 'nsduh',
    dimension.values = list(),
    url = 'https://pdas.samhsa.gov/saes/substate',
    details = 'NSDUH Substate Estimates')
}

nsduh_region_age2 = lapply(nsduh.region.age.25.and.older, `[[`, 2)

for (data in nsduh_region_age2) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'nsduh',
    source = 'nsduh',
    dimension.values = list(),
    url = 'https://pdas.samhsa.gov/saes/substate',
    details = 'NSDUH Substate Estimates')
}


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

nsduh_state_13_24 = lapply(nsduh.state.age.13.24, `[[`, 2)

for (data in nsduh_state_13_24) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'nsduh',
    source = 'nsduh',
    dimension.values = list(),
    url = 'https://pdas.samhsa.gov/saes/substate',
    details = 'NSDUH Substate Estimates')
}


nsduh_state_25_up = lapply(nsduh.state.age.25.and.older, `[[`, 2)

for (data in nsduh_state_25_up) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'nsduh',
    source = 'nsduh',
    dimension.values = list(),
    url = 'https://pdas.samhsa.gov/saes/substate',
    details = 'NSDUH Substate Estimates')
}


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
# nsduh_national_age = lapply(nsduh.national.age, `[[`, 2)
# 
# for (data in nsduh_national_age) {
#   
#   data.manager$put.long.form(
#     data = data,
#     ontology.name = 'nsduh',
#     source = 'nsduh',
#     dimension.values = list(),
#     url = 'https://pdas.samhsa.gov/saes/substate',
#     details = 'NSDUH Substate Estimates')
# }


