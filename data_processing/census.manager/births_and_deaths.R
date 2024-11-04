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
#outcome= births
################################################################################
data.list.births.clean = lapply(data.list.births, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]]
  
  data$year = as.character(data$Year)
  data$outcome= "births"
  data$ethnicity = data$`Mother.s.Hispanic.Origin`
  
  #Fix Location Codes
  data$location= str_pad(data$`County.Code`, width=5, side="left", pad="0")
  data$location = as.character(data$location)
  #Remove locations that are invalid
  data$location_flag = locations::is.location.valid(data$location)
  data = subset(data, data$location_flag != 'FALSE')
  
  ##Removing births = suppressed or Missing County##
  ##'Missing County' appears when county data is not available for a certain year. 
  #This occurs because the county did not meet minimum population standards and thus the data for the county was recoded to the "Unidentified Counties"
  data = subset(data, data$Births != "Suppressed") 
  data = subset(data, data$Births != "Missing County")
  data$value = as.numeric(data$Births)
  

   if(grepl("bridged.race", filename)) {
  data$race = data$`Mother.s.Bridged.Race`
   }
  
   if(grepl("single.race", filename)) {
       data$race = data$`Mother.s.Single.Race`
       #Combining racial groups from bridged and single
       #Todd said for now to leave in Not reported, not available, and unknown and add to the ontology
       data$race = if_else(data$race == "Native Hawaiian or Other Pacific Islander" | data$race == "Asian", "Asian or Pacific Islander", data$race)
     }
     
      data <- data %>%
        select(outcome, year, location, value, race, ethnicity)
     
     if(grepl("single.race", filename)) {
       
       #Now re-calculate the rate for the combined asian + native hawaiian pacific islander group
       data <- data %>%
         group_by(year, location, ethnicity, race)%>%
         mutate(combined.asian.births = sum(value))%>%
         select(-value)%>%
         rename(value = combined.asian.births)%>%
         ungroup()
     }
      
      data$race = tolower(data$race)
      data$ethnicity = tolower(data$ethnicity)
     
  data = as.data.frame(data)
  list(filename, data)  
  
})
################################################################################
###CLEAN BIRTH DATA###
#outcome= births.denominator
################################################################################
data.list.births.denominator = lapply(data.list.births, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]]
  
  data$year = as.character(data$Year)
  data$outcome= "births.denominator"
  data$ethnicity = data$`Mother.s.Hispanic.Origin`
  
  #Fix Location Codes
  data$location= str_pad(data$`County.Code`, width=5, side="left", pad="0")
  data$location = as.character(data$location)
  #Remove locations that are invalid
  data$location_flag = locations::is.location.valid(data$location)
  data = subset(data, data$location_flag != 'FALSE')
  
  ##Removing births = suppressed or Missing County##
  ##'Missing County' appears when county data is not available for a certain year. 
  #This occurs because the county did not meet minimum population standards and thus the data for the county was recoded to the "Unidentified Counties"
  data = subset(data, data$`Total.Population` != "Suppressed") 
  data = subset(data, data$`Total.Population` != "Missing County")
  data = subset(data, data$`Total.Population` != "Not Available")
  data$value = as.numeric(data$`Total.Population`)
  data$`Total.Population` = as.numeric(data$`Total.Population`)
  
  if(grepl("bridged.race", filename)) {
    data$race = data$`Mother.s.Bridged.Race`
  }
  
  if(grepl("single.race", filename)) {
    data$race = data$`Mother.s.Single.Race`
    #Combining racial groups from bridged and single
    #Todd said for now to leave in Not reported, not available, and unknown and add to the ontology
    data$race = if_else(data$race == "Native Hawaiian or Other Pacific Islander" | data$race == "Asian", "Asian or Pacific Islander", data$race)
  }
  
  data <- data %>%
    select(outcome, year, location, value, race, ethnicity, Total.Population)
  
  if(grepl("single.race", filename)) {
    
    #Now re-calculate the rate for the combined asian + native hawaiian pacific islander group
    data <- data %>%
      group_by(year, location, ethnicity, race)%>%
      mutate(combined.asian.population = sum(Total.Population))%>%
      select(-value)%>%
      rename(value = combined.asian.population)%>%
      ungroup()
  }
  
  data <- data %>%
    select(outcome, year, location, value, race, ethnicity)
  
  data$race = tolower(data$race)
  data$ethnicity = tolower(data$ethnicity)
  
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
    dimension.values.to.distribute = list(race=c('more than one race', 'not reported', 'unknown or not stated', 'not available'), ethnicity = 'unknown or not stated'),
    url = 'https://wonder.cdc.gov/',
    details = 'CDC Wonder')
}

births_denom= lapply(data.list.births.denominator , `[[`, 2)

for (data in births_denom ) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census.cdc.wonder.births.deaths',
    source = 'cdc_wonder',
    dimension.values.to.distribute = list(race=c('more than one race', 'not reported', 'unknown or not stated', 'not available'), ethnicity = 'unknown or not stated'),
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
  data$race = data$Race
  
  #Remove suppressed death values
  data = subset(data, data$Deaths != "Suppressed")
  data$value = as.numeric(data$'Deaths')
  
  data$age = data$"Five.Year.Age.Groups"
  data$age = if_else(data$age == "60-64 years ", "60-64 years", data$age) #removing weird space formatting
  data$sex = tolower(data$Gender)
  data$ethnicity = data$'Hispanic.Origin'
  
   data = subset(data, data$ethnicity != "Not Stated")
   data = subset(data, data$age != "Not Stated")
   
     data <- data %>%
      select(outcome, year, location, value, sex, age, race, ethnicity)
  
     data$race = tolower(data$race)
     data$ethnicity = tolower(data$ethnicity)
     
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
  
  data$race = data$Race
  
  #Remove suppressed  values
   data = subset(data, data$Population != "Not Applicable")
  data = subset(data, data$Population != "Suppressed")
   data$value = as.numeric(data$'Population')
  
  data$age = data$"Five.Year.Age.Groups"
  data$age = if_else(data$age == "60-64 years ", "60-64 years", data$age) #removing weird space formatting
  data$sex = tolower(data$Gender)
  data$ethnicity = data$'Hispanic.Origin'
  
   data = subset(data, data$ethnicity != "Not Stated")
   data = subset(data, data$age != "Not Stated")

     data <- data %>%
       select(outcome, year, location, value, sex, age, race, ethnicity)
     
     data$race = tolower(data$race)
     data$ethnicity = tolower(data$ethnicity)
  
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
    ontology.name = 'metro.deaths',
    source = 'cdc_wonder',
    dimension.values.to.distribute = list(race=c('more than one race', 'not reported', 'unknown or not stated', 'not available'), ethnicity = 'unknown or not stated'),
    url = 'https://wonder.cdc.gov/',
    details = 'CDC Wonder')
}

deaths_denom = lapply(data.list.deaths.denom, `[[`, 2)

for (data in deaths_denom ) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'metro.deaths',
    source = 'cdc_wonder',
    dimension.values.to.distribute = list(race=c('more than one race', 'not reported', 'unknown or not stated', 'not available'), ethnicity = 'unknown or not stated'),
    url = 'https://wonder.cdc.gov/',
    details = 'CDC Wonder')
}


################################################################################
#Read in National Level Birth Data
################################################################################
DATA.DIR.BIRTH.NATIONAL="../../data_raw/births_deaths/births/national"

birth_files_national <- list.files(DATA.DIR.BIRTH.NATIONAL, pattern = ".txt", full.names = "TRUE")

data.list.births.national <- lapply(birth_files_national, function(x) {
  list(filename=x, data=read.delim2(x))
  
})
################################################################################
#Clean National Level Birth Data
################################################################################
births.national.clean = lapply(data.list.births.national, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]]
  
  data$year = as.character(data$Year)
  data$outcome= "births"
  data$ethnicity = data$`Mother.s.Hispanic.Origin`
  data$location = "US"
  
  ##Removing births = suppressed or Missing County##
  ##'Missing County' appears when county data is not available for a certain year. 
  #This occurs because the county did not meet minimum population standards and thus the data for the county was recoded to the "Unidentified Counties"
  data = subset(data, data$Births != "Suppressed") 
  data = subset(data, data$Births != "Missing County")
  
  data$value = as.numeric(data$Births)
  
  if(grepl("bridged.race", filename)) {
    data$race = data$`Mother.s.Bridged.Race`
  }
  if(grepl("single.race", filename)) {
    data$race = data$`Mother.s.Single.Race`
    #Combining racial groups from bridged and single
    #Todd said for now to leave in Not reported, not available, and unknown and add to the ontology
    data$race = if_else(data$race == "Native Hawaiian or Other Pacific Islander" | data$race == "Asian", "Asian or Pacific Islander", data$race)
  }
  
  data <- data %>%
    select(outcome, year, location, value, race, ethnicity, Births)
  
  if(grepl("single.race", filename)) {
    
    #Now re-calculate the rate for the combined asian + native hawaiian pacific islander group
    data <- data %>%
      group_by(year, location, ethnicity, race)%>%
      mutate(combined.asian.births = sum(Births))%>%
      select(-value)%>%
      rename(value = combined.asian.births)%>%
      ungroup()
  }
  
  data$race = tolower(data$race)
  data$ethnicity = tolower(data$ethnicity)
  
  data = as.data.frame(data)
  list(filename, data)  
  
})
################################################################################
#National Level Birth Denominator Cleaning
################################################################################
options(error=NULL)
births.denominator.national.clean = lapply(data.list.births.national, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]]
  
  data$year = as.character(data$Year)
  data$outcome= "births.denominator"
  data$ethnicity = data$`Mother.s.Hispanic.Origin`
  data$location = "US"
  
  ##Removing births = suppressed or Missing County##
  ##'Missing County' appears when county data is not available for a certain year. 
  #This occurs because the county did not meet minimum population standards and thus the data for the county was recoded to the "Unidentified Counties"
  data = subset(data, data$`Total.Population` != "Suppressed") 
  data = subset(data, data$`Total.Population` != "Missing County")
  data = subset(data, data$`Total.Population` != "Not Available")
  
  data$value = as.numeric(data$`Total.Population`)
  data$`Total.Population` = as.numeric(data$`Total.Population`)
  
  if(grepl("bridged.race", filename)) {
    data$race = data$`Mother.s.Bridged.Race`
  }
  if(grepl("single.race", filename)) {
    data$race = data$`Mother.s.Single.Race`
    #Combining racial groups from bridged and single
    #Todd said for now to leave in Not reported, not available, and unknown and add to the ontology
    data$race = if_else(data$race == "Native Hawaiian or Other Pacific Islander" | data$race == "Asian", "Asian or Pacific Islander", data$race)
  }
  
  #data <- data %>%
  #select(outcome, year, location, value, race, ethnicity, `Total.Population`)
  
  if(grepl("single.race", filename)) {
    
    #Now re-calculate the rate for the combined asian + native hawaiian pacific islander group
    data <- data %>%
      group_by(year, location, ethnicity, race)%>%
      mutate(combined.asian.population = sum(`Total.Population`))%>%
      select(-value)%>%
      rename(value = combined.asian.population)%>%
      ungroup()
  }
  
  data$race = tolower(data$race)
  data$ethnicity = tolower(data$ethnicity)
  
  data = as.data.frame(data)
  list(filename, data)  
  
})
################################################################################
#Put National Births into Census Manager#
################################################################################
births.national.clean.put = lapply(births.national.clean , `[[`, 2)

for (data in births.national.clean.put ) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census.cdc.wonder.births.deaths',
    source = 'cdc_wonder',
    dimension.values.to.distribute = list(race=c('more than one race', 'not reported', 'unknown or not stated', 'not available'), ethnicity = 'unknown or not stated'),
    url = 'https://wonder.cdc.gov/',
    details = 'CDC Wonder')
}

births_denom_national= lapply(births.denominator.national.clean , `[[`, 2)

for (data in births_denom_national ) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census.cdc.wonder.births.deaths',
    source = 'cdc_wonder',
    dimension.values.to.distribute = list(race=c('more than one race', 'not reported', 'unknown or not stated', 'not available'), ethnicity ='not stated', 'unknown or not stated'),
    url = 'https://wonder.cdc.gov/',
    details = 'CDC Wonder')
}