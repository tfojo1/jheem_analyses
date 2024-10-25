
# Mortality Rate data from CDC Wonder -------------------------------------

DATA.DIR.MORTALITY="../../data_raw/mortality.cdc.wonder"

mortality.files <- list.files(DATA.DIR.MORTALITY, pattern = ".txt", full.names = "TRUE")

metro.deaths.national.raw <- lapply(mortality.files, function(x) {
  list(filename=x, data=read.delim2(x))
  
})

# Clean National Metro Deaths -------------------------------------------------------------------

national.metro.deaths = lapply(metro.deaths.national.raw, function(file){
  
  data=file[["data"]]  
  filename = file[["filename"]]
  
  data$location = "US" 
  data$year = as.character(data$Year)
  data$value = as.numeric(data$Deaths)
  data$outcome= "deaths" #Decided to change this from metro deaths to deaths on 10-12-24 (because this is really just deaths)
  
  data$sex = tolower(data$Gender)
  data$race = data$Race
  data$ethnicity = data$Hispanic.Origin
  data$age = data$`Five.Year.Age.Groups`
  data$age = if_else(data$age == "60-64 years ", "60-64 years", data$age) #removing weird space formatting
  
  data = subset(data, data$Deaths != "Suppressed") #Removing suppressed deaths
  data = subset(data, data$age != "Not Stated") #Removing not stated age
  
  data <- data %>%
    select(outcome, year, location, value, sex, age, race, ethnicity)
  
  data = as.data.frame(data)
  list(filename, data)  
})

# metro.deaths denominator, national --------------------------------------
national.metro.deaths.denominator  = lapply(metro.deaths.national.raw, function(file){
  
  data=file[["data"]]  
  filename = file[["filename"]]
  
  data$location = "US" 
  data$year = as.character(data$Year)
  data = subset(data, data$Population != "Not Applicable") #removing suppressed values
  data$value = as.numeric(data$Population)
  data$outcome= "deaths.denominator"
  
  data$sex = tolower(data$Gender)
  data$race = data$Race
  data$ethnicity = data$Hispanic.Origin
  data$age = data$`Five.Year.Age.Groups`
  data$age = if_else(data$age == "60-64 years ", "60-64 years", data$age) #removing weird space formatting
  
  data = subset(data, data$Population != "Suppressed") #Removing suppressed population
  data = subset(data, data$age != "Not Stated") #Removing not stated age
  
   data <- data %>%
     select(outcome, year, location, value, sex, age, race, ethnicity)
  
  data = as.data.frame(data)
  list(filename, data)  
  
})


# Put into Census Manager -----------------------------------------------

national.metro.deaths.put = lapply(national.metro.deaths, `[[`, 2)

for (data in national.metro.deaths.put ) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census.cdc.wonder.births.deaths',
    source = 'cdc_wonder',
    dimension.values.to.distribute = list(race=c('More than one race', 'Not reported', 'Unknown or Not Stated', 'Not Available'), ethnicity ='Not Stated'),
    url = 'https://wonder.cdc.gov/',
    details = 'CDC Wonder')
}

national.metro.deaths.denominator.put = lapply(national.metro.deaths.denominator, `[[`, 2)

for (data in national.metro.deaths.denominator.put ) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census.cdc.wonder.births.deaths',
    source = 'cdc_wonder',
    dimension.values.to.distribute = list(race=c('More than one race', 'Not reported', 'Unknown or Not Stated', 'Not Available'), ethnicity = 'Not Stated'),
    url = 'https://wonder.cdc.gov/',
    details = 'CDC Wonder')
}

