#Pull birth and death data with demographics from CDC Wonder##
##THIS IS A PART OF THE CENSUS MANAGER##

################################################################################
                        ###BIRTH DATA###
################################################################################
DATA.DIR.BIRTH="../../data_raw/births_deaths/births"

birth_files <- list.files(DATA.DIR.BIRTH, pattern = ".txt", full.names = "TRUE")

data.list.births <- lapply(birth_files, function(x) {
  list(filename=x, data=read.delim2(x))
  
})
################################################################################
                      ###CLEAN BIRTH DATA###
################################################################################
data.list.births.clean = lapply(data.list.births, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]]
  
  data$year = as.character(data$Year)
  data$outcome= "births"
  data$ethnicity = data$`Mother.s.Hispanic.Origin`

   if(grepl("07.19", filename)) {
  data$race = data$`Mother.s.Bridged.Race`
   }
   if(grepl("20.22", filename)) {
  data$race = data$`Mother.s.Single.Race`
   }

  #Combining racial groups from bridged and single
  #Todd said for now to leave in Not reported, not available, and unknown and add to the ontology
  data$race = if_else(data$race == "Native Hawaiian or Other Pacific Islander" | data$race == "Asian", "Asian or Pacific Islander", data$race)

##Removing births = suppressed or Missing County##
##'Missing County' appears when county data is not available for a certain year. 
#This occurs because the county did not meet minimum population standards and thus the data for the county was recoded to the "Unidentified Counties"
   data = subset(data, data$Births != "Suppressed") 
   data = subset(data, data$Births != "Missing County")
   data$value = as.numeric(data$Births)
   
   #Fix Location Codes
   data$location= str_pad(data$`County.Code`, width=5, side="left", pad="0")
   data$location = as.character(data$location)
   #Remove locations that are invalid
   data$location_flag = locations::is.location.valid(data$location)
   data = subset(data, data$location_flag != 'FALSE')

  data <- data %>%
    select(outcome, year, location, value, race, ethnicity)
  
  data = as.data.frame(data)
  list(filename, data)  
  
})

################################################################################
                  ###Put BIRTHS into CENSUS MANAGER###
################################################################################
births_race_eth = lapply(data.list.births.clean , `[[`, 2)

for (data in births_race_eth ) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census.cdc.wonder.births.deaths',
    source = 'cdc_wonder',
    dimension.values = list(),
    url = 'https://wonder.cdc.gov/',
    details = 'CDC Wonder')
}

################################################################################
##Read in Death Data
#Note outcome = metro.deaths
################################################################################

DATA.DIR.DEATH="../../data_raw/births_deaths/deaths"

death_files <- list.files(DATA.DIR.DEATH, pattern = ".txt", full.names = "TRUE")

data.list.deaths <- lapply(death_files, function(x) {
  list(filename=x, data=read.delim2(x))
  
})

################################################################################
##CLEAN DEATH DATA
#Note outcome = metro.deaths
################################################################################

data.list.deaths.clean = lapply(data.list.deaths, function(file){
  
  data=file[["data"]]  
  filename = file[["filename"]]
  
  names(state.abb) <- state.name
  data$location =ifelse(data$State == "District of Columbia", "DC", state.abb[data$State]) 
  
  if(grepl("01.10", filename)) {
    data$year = "2001-2010"
  }
  if(grepl("11.20", filename)) {
    data$year = "2011-2020"
  }

  data$outcome= "metro.deaths"
  
  #Remove suppressed death values
  data = subset(data, data$Deaths != "Suppressed")
  data$value = as.numeric(data$'Deaths')
  
  data$age = data$"Ten.Year.Age.Groups"
  data$sex = tolower(data$Gender)
  data$ethnicity = data$'Hispanic.Origin'
  
   data = subset(data, data$ethnicity != "Not Stated")
   data = subset(data, data$age != "Not Stated")
  
  if(grepl("nh", filename)) {
    data$race = data$Race
    data <- data %>%
      select(outcome, year, location, value, sex, age, race, ethnicity)
  }
  if(grepl("HISP", filename)) {
    data <- data %>%
      select(outcome, year, location, value, sex, age, ethnicity)
  }
  
  data = as.data.frame(data)
  list(filename, data)  
  
})
################################################################################
#Note outcome = metro.deaths.denominator
################################################################################

data.list.deaths.denom = lapply(data.list.deaths, function(file){
  
  data=file[["data"]]  
  filename = file[["filename"]]
  
  names(state.abb) <- state.name
  data$location =ifelse(data$State == "District of Columbia", "DC", state.abb[data$State]) 
  
  if(grepl("01.10", filename)) {
    data$year = "2001-2010"
  }
  if(grepl("11.20", filename)) {
    data$year = "2011-2020"
  }
  
  data$outcome= "metro.deaths.denominator"
  
  #Remove suppressed  values
   data = subset(data, data$Population != "Not Applicable")
  data = subset(data, data$Population != "Suppressed")
   data$value = as.numeric(data$'Population')
  
  data$age = data$"Ten.Year.Age.Groups"
  data$sex = tolower(data$Gender)
  data$ethnicity = data$'Hispanic.Origin'
  
   data = subset(data, data$ethnicity != "Not Stated")
   data = subset(data, data$age != "Not Stated")

   if(grepl("nh", filename)) {
     data$race = data$Race
     data <- data %>%
       select(outcome, year, location, value, sex, age, race, ethnicity)
   }
   if(grepl("HISP", filename)) {
     data <- data %>%
       select(outcome, year, location, value, sex, age, ethnicity)
   }
  
  data = as.data.frame(data)
  list(filename, data)  
  
})

################################################################################
##Put DEATHS into CENSUS MANAGER
################################################################################

deaths_race_eth = lapply(data.list.deaths.clean, `[[`, 2)

for (data in deaths_race_eth ) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census.cdc.wonder.births.deaths',
    source = 'cdc_wonder',
    dimension.values = list(),
    url = 'https://wonder.cdc.gov/',
    details = 'CDC Wonder')
}

deaths_denom = lapply(data.list.deaths.denom, `[[`, 2)

for (data in deaths_denom ) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census.cdc.wonder.births.deaths',
    source = 'cdc_wonder',
    dimension.values = list(),
    url = 'https://wonder.cdc.gov/',
    details = 'CDC Wonder')
}
