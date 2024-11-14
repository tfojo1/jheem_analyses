#This code take the 'births' and 'female.population' columns from cdc wonder's fertility rate data then aggregates them to MSA level
#so that an MSA level fertility rate can be calculated

options(error=NULL)

DATA.DIR.FERTILITY="../../data_raw/syphilis.manager/fertility"

fertility.files <- list.files(DATA.DIR.FERTILITY, pattern = ".txt", full.names = "TRUE")

suppressWarnings(data.list.fertility <- lapply(fertility.files, function(x) {
  list(filename=x, data=read.delim2(x, colClasses = c('County.Code' = 'character')))
  
})) #Suppressing the warning that says that county.code is not available in the national level data

# Put Births ----------------------------------------------------

births.county = lapply(data.list.fertility, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$outcome = 'births'
  data$year = as.character(data$Year)
  data$age = data$'Age.of.Mother.9'
  data$ethnicity = data$'Mother.s.Hispanic.Origin'
  
  data$Births = ifelse(data$Births == "Not Available", NA, data$Births)
  data$value = as.numeric(data$Births)
  
  data$'Female.Population' = ifelse(data$'Female.Population' == "Not Available", NA, data$'Female.Population')
  data$'Female.Population' = as.numeric(data$'Female.Population')
  
  data <- data%>%
    filter(age != "50 years and over")%>% #These ages shouldn't be here bc fertility rate = 15-44
    filter(age != "45-49 years")%>%
    filter(age != "Under 15 years")
  
  #Location conditionals#
  if(grepl("county", filename)) {
    data$location = data$'County.Code'
  }
  if(grepl("national", filename)) {
    data$location = "US"
  }
  
  #Demographic conditionals#
  if(grepl("bridged.race", filename)) {
    data$race = data$'Mother.s.Bridged.Race'
  }
  
  if(grepl("single.race", filename)) {
    data$race = data$'Mother.s.Single.Race'
    
    #Single race data separates Asian from Native Hawaiian or other Pacific Islander (but for bridged they are combined, so I am combining them here)
    data$race.new = ifelse(data$race == "Asian", "Asian or Pacific Islander", data$race)
    data$race.new = ifelse(data$race.new == "Native Hawaiian or Other Pacific Islander", "Asian or Pacific Islander", data$race.new)
    
    data<-data %>%
      select(-race)%>%
      rename(race = race.new)
  }
  
  data <- data %>%
    select(outcome, year, location, age, race, ethnicity, Births, 'Female.Population', value)%>%    
    filter(race != 'More than one race') #Removing this from numerator and denominator for fertility- because this is a proportion, we'll remove this rather than redistributing it
  
  
  if(grepl("single.race", filename)) {
    
    #Now re-calculate the rate for the combined asian + native hawaiian pacific islander group
    data <- data %>%
      group_by(year, location, age, ethnicity, race)%>%
      mutate(combined.asian.births = sum(Births))%>%
      select(-value)%>%
      rename(value = combined.asian.births)%>%
      ungroup()
  }
  
  data = as.data.frame(data)
  
  data <- data %>%
    mutate(location.check = locations::is.location.valid(location))%>% #Removing counties coded as 'unidentified'
    filter(location.check == T)%>%
    filter(location != "US") #removing national
  
  data$race = tolower(data$race)
  data$ethnicity = tolower(data$ethnicity)
  
  list(filename, data) 
  
})


# Clean Female Population -------------------------------------------------

female.population.county = lapply(births.county, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  data <- data %>%
    select(-value)%>%
    mutate(value = Female.Population)

  if(grepl("single.race", filename)) {
    #Now re-calculate the rate for the combined asian + native hawaiian pacific islander group
    data <- data %>%
      group_by(year, location, age, ethnicity, race)%>%
      mutate(combined.asian.population = sum(Female.Population))%>%
      select(-value)%>%
      rename(value = combined.asian.population)%>% #This represents that single race groups for asian have been added together
      ungroup()
  }

   data <- data %>%
     select(-outcome)%>%
     select(year, location, age, race, ethnicity, value)%>%
     mutate(outcome = 'female.population')
  
   data = as.data.frame(data)
list(filename, data) 

})

# Put Births and Female Population (to be used to calc fertility rate) --------

births.county.put = lapply(births.county, `[[`, 2)

for (data in births.county.put) {
  
  syphilis.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.fertility',
    source = 'cdc.wonder.natality',
    dimension.values = list(),
    url = 'https://wonder.cdc.gov/natality.html',
    details = 'CDC Wonder Natality Data')
}

female.population.county.put = lapply(female.population.county, `[[`, 2)

for (data in female.population.county.put) {

  syphilis.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.fertility',
    source = 'cdc.wonder.natality',
    dimension.values = list(),
    url = 'https://wonder.cdc.gov/natality.html',
    details = 'CDC Wonder Natality Data')
}



# Aggregate to MSA --------------------------------------------------------

source('data_processing/syphilis.manager/aggregate.county.to.msa.R')

