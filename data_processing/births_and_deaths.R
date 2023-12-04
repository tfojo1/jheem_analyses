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

  ##For right now I'm going to remove all the types of unk race/eth##
  data = subset(data, data$race != "Not Reported")
  data = subset(data, data$race != "Not Available")
  data = subset(data, data$race != "Unknown or Not Stated")
  data = subset(data, data$ethnicity != "Unknown or Not Stated")
  
  data$race = if_else(data$race == "Native Hawaiian or Other Pacific Islander" | data$race == "Asian", "Asian or Pacific Islander", data$race)

 ##For right now also going to remove births = suppressed or NA##
   data = subset(data, data$Births != "Suppressed") 
   data = subset(data, data$Births != "Missing County")
   data$value = as.numeric(data$Births)
   
   #Fix Location Codes
   data$location= str_pad(data$`County.Code`, width=5, side="left", pad="0")
   data$location = as.character(data$location)
   #Remove locations that are invalid
   data$location_flag = is.location.valid(data$location)
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
    ontology.name = 'census.cdc.wonder',
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
  
  if(grepl("06.10", filename)) {
    data$year = "2006-2010"
  }
  if(grepl("11.15", filename)) {
    data$year = "2011-2015"
  }
  if(grepl("16.20", filename)) {
    data$year = "2016-2020"
  }
  
  data$outcome= "metro.deaths"
  
  #Remove suppressed death values
  data = subset(data, data$Deaths != "Suppressed")
  data$value = as.numeric(data$'Deaths')
  
  data$age = data$"Single.Year.Ages"
  data$sex = tolower(data$Gender)
  data$race = data$Race
  data$ethnicity = data$'Hispanic.Origin'
  data = subset(data, data$ethnicity != "Not Stated")
  data = subset(data, data$age != "Not Stated")
   

  
   data <- data %>%
     select(outcome, year, location, value, sex, age, race, ethnicity)
  
  data = as.data.frame(data)
  list(filename, data)  
  
})
################################################################################
##Put DEATHS into CENSUS MANAGER
################################################################################

deaths_race_eth = lapply(data.list.deaths.clean , `[[`, 2)

for (data in deaths_race_eth ) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census.cdc.wonder',
    source = 'cdc_wonder',
    dimension.values = list(),
    url = 'https://wonder.cdc.gov/',
    details = 'CDC Wonder')
}

