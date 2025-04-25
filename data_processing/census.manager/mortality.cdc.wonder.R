
# Mortality Rate data from CDC Wonder -------------------------------------

DATA.DIR.MORTALITY="Q:/data_raw/mortality.cdc.wonder"

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
  data$age.original = data$`Five.Year.Age.Groups`
  data$age.original = if_else(data$age.original == "60-64 years ", "60-64 years", data$age.original) #removing weird space formatting
  
  data = subset(data, data$Deaths != "Suppressed") #Removing suppressed deaths
  data = subset(data, data$age != "Not Stated") #Removing not stated age
  
  #Combined all of the over 85 age groups into one because it's causing ontology issues
  data$age = if_else(data$age.original == "85-89 years", "85+ years", data$age.original)
  data$age = if_else(data$age.original == "90-94 years", "85+ years", data$age)
  data$age = if_else(data$age.original == "95-99 years", "85+ years", data$age)
  data$age = if_else(data$age.original == "100+ years", "85+ years", data$age)
  
  data <- data %>%
    group_by(year, location, sex, age, race, ethnicity)%>%
    mutate(fixed.death.count = sum(value))%>%
    select(-value)%>%
    rename(value = fixed.death.count)
  
  data <- data %>%
    select(outcome, year, location, value, sex, age, race, ethnicity, age.original)%>%
    mutate(race = tolower(race))%>%
    mutate(ethnicity = tolower(ethnicity))
  
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
  data$age.original = data$`Five.Year.Age.Groups`
  data$age.original = if_else(data$age.original == "60-64 years ", "60-64 years", data$age.original) #removing weird space formatting
  
  data = subset(data, data$Population != "Suppressed") #Removing suppressed population
  data = subset(data, data$age != "Not Stated") #Removing not stated age
  
  #Combined all of the over 85 age groups into one because it's causing ontology issues
  #Turns out there's no population values for 85+
  data$age = if_else(data$age.original == "85-89 years", "85+ years", data$age.original)
  data$age = if_else(data$age.original == "90-94 years", "85+ years", data$age)
  data$age = if_else(data$age.original == "95-99 years", "85+ years", data$age)
  data$age = if_else(data$age.original == "100+ years", "85+ years", data$age)
  
  data <- data %>%
    group_by(year, location, sex, age, race, ethnicity)%>%
    mutate(fixed.denominator.count = sum(value))%>%
    select(-value)%>%
    rename(value = fixed.denominator.count)
  
   data <- data %>%
     select(outcome, year, location, value, sex, age, race, ethnicity)%>%
     mutate(race = tolower(race))%>%
     mutate(ethnicity = tolower(ethnicity))
  
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
    dimension.values.to.distribute = list(race=c('more than one race', 'not reported', 'unknown or not stated', 'not available'), ethnicity ='not stated'),
    url = 'https://wonder.cdc.gov/',
    details = 'CDC Wonder')
}

national.metro.deaths.denominator.put = lapply(national.metro.deaths.denominator, `[[`, 2)

for (data in national.metro.deaths.denominator.put ) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census.cdc.wonder.births.deaths',
    source = 'cdc_wonder',
    dimension.values.to.distribute = list(race=c('more than one race', 'not reported', 'unknown or not stated', 'not available'), ethnicity ='not stated'),
    url = 'https://wonder.cdc.gov/',
    details = 'CDC Wonder')
}

