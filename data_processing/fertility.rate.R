#CDC Wonder Fertility Rates Data (women ages 15-44)
options(error=NULL)

DATA.DIR.FERTILITY="../../data_raw/syphilis.manager/fertility"

fertility.files <- list.files(DATA.DIR.FERTILITY, pattern = ".txt", full.names = "TRUE")

suppressWarnings(data.list.fertility <- lapply(fertility.files, function(x) {
  list(filename=x, data=read.delim2(x, colClasses = c('County.Code' = 'character')))
 
})) #Suppressing the warning that says that county.code is not available in the national level data


# Clean Fertility Data ----------------------------------------------------

fertility.clean = lapply(data.list.fertility, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  #Universal Cleaning#
  data$outcome = 'fertility.rate'
  data$year = as.character(data$Year)
  data$age = data$'Age.of.Mother.9'
  data$ethnicity = data$'Mother.s.Hispanic.Origin'

  data$value = ifelse(data$'Fertility.Rate' == "Not Available", NA, data$'Fertility.Rate')
  data$value = as.numeric(data$value)
  
  data$Births = ifelse(data$Births == "Not Available", NA, data$Births)
  data$Births = as.numeric(data$Births)
  
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
    select(outcome, year, location, age, race, ethnicity, Births, 'Female.Population', value)
  
  
  if(grepl("single.race", filename)) {
  
    #Now re-calculate the rate for the combined asian + native hawaiian pacific islander group
  data <- data %>%
    group_by(year, location, age, ethnicity, race)%>%
     mutate(combined.asian.births = sum(Births))%>%
     mutate(combined.asian.female.pop = sum(Female.Population))%>%
     mutate(combined.asian.rate = round(((combined.asian.births/combined.asian.female.pop)*1000), digits = 2))%>%
    select(-value)%>%
    rename(value = combined.asian.rate)%>%
    ungroup()
  }
  
  data = as.data.frame(data)
  
  
  data$value = (data$value/1000) #changing format of rate
  data = subset(data, data$value < 1.0) 
  data <- data %>%
    mutate(location.check = locations::is.location.valid(location))%>% #Removing counties coded as 'unidentified'
    filter(location.check == T)
  
  list(filename, data) 
  
})

# Put female.population as denominator for fertility. ---------------------

female.population.clean = lapply(fertility.clean, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  data <- data %>%
    select(-outcome, -value)
  
  data$outcome = 'female.population'
  
  data$value = data$'Female.Population'
  
  data = as.data.frame(data)
  
  list(filename, data) 
  
})


# Put into Syphilis Data Manager ------------------------------------------

fertility.put = lapply(fertility.clean, `[[`, 2)

for (data in fertility.put) {

  census.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.fertility',
    source = 'cdc.wonder.natality',
    dimension.values = list(),
    url = 'https://wonder.cdc.gov/natality.html',
    details = 'CDC Wonder Natality Data')
}

female.population.put = lapply(female.population.clean, `[[`, 2)

for (data in female.population.put) {

  census.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.fertility',
    source = 'cdc.wonder.natality',
    dimension.values = list(),
    url = 'https://wonder.cdc.gov/natality.html',
    details = 'CDC Wonder Natality Data')
}
