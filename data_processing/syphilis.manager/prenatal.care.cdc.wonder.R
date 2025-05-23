#This code processing prenatal care data from CDC Wonder

DATA.DIR.PRENATAL="Q:/data_raw/syphilis.manager/prenatal.care.cdc.wonder/total"

cdc.prenatal.files <- list.files(DATA.DIR.PRENATAL, pattern = ".txt", full.names = "TRUE")

suppressWarnings(cdc.prenatal.data <- lapply(cdc.prenatal.files, function(x) {
  list(filename=x, 
       data=read.delim2(x, 
       colClasses = c('County.of.Residence.Code' = 'character')))
}))

#Total Level Data
#Note the data I pulled is only for ages 15-44

clean.total.prenatal = lapply(cdc.prenatal.data, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data = subset(data, data$`Trimester.Prenatal.Care.Began` != "Unknown or Not Stated")
  
  if(grepl("national", filename)) {
    data$location = "US"
  }
  
  if(grepl("county", filename)) {
    data$location = data$'County.of.Residence.Code'
  }
  
  data$year = as.character(data$Year)
  
  data <- data %>%
    select(-Year, -`Year.Code`, -Notes)%>%
    group_by(location, year)%>%
    mutate(total.births.that.year = sum(Births))%>%
    mutate(value = Births/total.births.that.year)%>%
    mutate(qa.check = sum(value))%>%
    mutate(outcome = case_when(`Trimester.Prenatal.Care.Began` == "1st to 3rd month" ~"prenatal.care.initiation.first.trimester",
                               `Trimester.Prenatal.Care.Began` == "4th to 6th month" ~"prenatal.care.initiation.second.trimester",
                               `Trimester.Prenatal.Care.Began` == "7th to final month" ~"prenatal.care.initiation.third.trimester",
                               `Trimester.Prenatal.Care.Began` == "No prenatal care" ~"no.prenatal.care"))%>%
    mutate(location.check = locations::is.location.valid(location))%>%
    filter(location.check == T) #Remove the 'unidentified counties'

  data= as.data.frame(data)
  
  list(filename, data)
})

#Put total level
total.prenatal.put = lapply(clean.total.prenatal, `[[`, 2)

for (data in total.prenatal.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.fertility',
    source = 'cdc.wonder.natality',
    dimension.values = list(),
    url = 'https://wonder.cdc.gov/natality-expanded-current.html',
    details = 'CDC Wonder Natality Data 2016-2023')
}

#Stratified by Age Only
DATA.DIR.PRENATAL.AGE="Q:/data_raw/syphilis.manager/prenatal.care.cdc.wonder/age"

prenatal.age.files <- Sys.glob(paste0(DATA.DIR.PRENATAL.AGE, '/*.csv'))

prenatal.age.data <- lapply(prenatal.age.files, function(x){
  list(filename=x, data=read.csv(x))
})

prenatal.age.data.clean = lapply(prenatal.age.data, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data = subset(data, data$`Trimester.Prenatal.Care.Began` != "Unknown or Not Stated")
  data$age = data$`Age.of.Mother.9`
  
  #Decided Feb 2025 that this will only include 15-44 so it aligns with births as the denominator:
  data <- data %>%
    filter(age != "50 years and over")%>% #These ages shouldn't be here bc fertility rate = 15-44
    filter(age != "45-49 years")%>%
    filter(age != "Under 15 years")
  
  if(grepl("national", filename)) {
    data$location = "US"
  }
  
  if(grepl("county", filename)) {
    data$location = as.character(data$'County.of.Residence.Code')
  }
  
  data$year = as.character(data$Year)
  
  data <- data %>%
    select(-Year, -`Year.Code`, -Notes)%>%
    group_by(location, year, age)%>%
    mutate(total.births.that.year = sum(Births))%>%
    mutate(value = Births/total.births.that.year)%>%
    mutate(qa.check = sum(value))%>%
    mutate(outcome = case_when(`Trimester.Prenatal.Care.Began` == "1st to 3rd month" ~"prenatal.care.initiation.first.trimester",
                               `Trimester.Prenatal.Care.Began` == "4th to 6th month" ~"prenatal.care.initiation.second.trimester",
                               `Trimester.Prenatal.Care.Began` == "7th to final month" ~"prenatal.care.initiation.third.trimester",
                               `Trimester.Prenatal.Care.Began` == "No prenatal care" ~"no.prenatal.care"))%>%
    mutate(location.check = locations::is.location.valid(location))%>%
    filter(location.check == T)%>% #Remove the 'unidentified counties'
    select(outcome, year, location, age, value, Births)%>%
    mutate(age = if_else(age == '50 years and over', '50+ years', age))%>%
    mutate(age = if_else(age == 'Under 15 years', '>15 years', age))
  
  data= as.data.frame(data)
  
  list(filename, data)
})

prenatal.age.data.clean.put = lapply(prenatal.age.data.clean, `[[`, 2)

for (data in prenatal.age.data.clean.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.fertility',
    source = 'cdc.wonder.natality',
    dimension.values = list(),
    url = 'https://wonder.cdc.gov/natality-expanded-current.html',
    details = 'CDC Wonder Natality Data 2016-2023')
}

#Stratified by Race/eth Only
#Note the data I pulled only includes age 15-44
DATA.DIR.PRENATAL.RACE="Q:/data_raw/syphilis.manager/prenatal.care.cdc.wonder/race.eth"

prenatal.race.files <- Sys.glob(paste0(DATA.DIR.PRENATAL.RACE, '/*.csv'))

prenatal.race.data <- lapply(prenatal.race.files, function(x){
  list(filename=x, data=read.csv(x))
})

prenatal.race.data.clean = lapply(prenatal.race.data, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data = subset(data, data$`Trimester.Prenatal.Care.Began` != "Unknown or Not Stated")
  data$race = tolower(data$`Mother.s.Single.Race.6`)
  data$ethnicity = tolower(data$`Mother.s.Hispanic.Origin`)

  if(grepl("national", filename)) {
    data$location = "US"
  }
  
  if(grepl("county", filename)) {
    data$location = as.character(data$'County.of.Residence.Code')
    data$location = str_pad(data$location, 5, pad="0") 
  }
  
  data$year = as.character(data$Year)
  
  #making race changes: removing unknown and more than one race, combining asian w native hawaiian or PI to align with existing ontology for births:
  data <- data %>%
    select(-Year, -`Year.Code`, -Notes)%>%
    filter(race != 'more than one race')%>% #Removing these because I don't think they get redistributed since this is a proportion
    filter(ethnicity != 'unknown or not stated')%>% #Removing these because I don't think they get redistributed since this is a proportion
    mutate(new.race = ifelse(race == "asian", "asian or pacific islander", race))%>%
    mutate(new.race = ifelse(race == "native hawaiian or other pacific islander", "asian or pacific islander", new.race))%>%
    group_by(year, location, `Trimester.Prenatal.Care.Began`, new.race, ethnicity)%>%
    mutate(new.value = sum(Births))%>%
    select(-race, -Births)%>%
    rename(race = new.race)%>%
    rename(Births = new.value)%>%
    ungroup()%>%
    select(location, year, race, ethnicity, `Trimester.Prenatal.Care.Began`, Births)
    
  
  data<- data[!duplicated(data), ]  #remove duplicates created by race change so births aren't summed twice 
  
  
    #Calculate proportion
  data <- data %>%
    group_by(location, year, race, ethnicity)%>%
    mutate(total.births.that.year = sum(Births))%>%
    mutate(value = Births/total.births.that.year)%>%
    mutate(qa.check = sum(value))%>%
    mutate(outcome = case_when(`Trimester.Prenatal.Care.Began` == "1st to 3rd month" ~"prenatal.care.initiation.first.trimester",
                               `Trimester.Prenatal.Care.Began` == "4th to 6th month" ~"prenatal.care.initiation.second.trimester",
                               `Trimester.Prenatal.Care.Began` == "7th to final month" ~"prenatal.care.initiation.third.trimester",
                               `Trimester.Prenatal.Care.Began` == "No prenatal care" ~"no.prenatal.care"))%>%
    mutate(location.check = locations::is.location.valid(location))%>%
    filter(location.check == T)%>% #Remove the 'unidentified counties'
   select(outcome, year, location, race, ethnicity, value, Births)

  data= as.data.frame(data)
  
  list(filename, data)
})

prenatal.race.data.clean.put = lapply(prenatal.race.data.clean, `[[`, 2)

for (data in prenatal.race.data.clean.put) {

  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.fertility',
    source = 'cdc.wonder.natality',
    dimension.values = list(),
    url = 'https://wonder.cdc.gov/natality-expanded-current.html',
    details = 'CDC Wonder Natality Data 2016-2023')
}

#Stratified by race, eth, age
DATA.DIR.PRENATAL.THREE="Q:/data_raw/syphilis.manager/prenatal.care.cdc.wonder/age.race.eth"

prenatal.race.files.three <- Sys.glob(paste0(DATA.DIR.PRENATAL.THREE, '/*.csv'))

prenatal.race.data.three <- lapply(prenatal.race.files.three, function(x){
  list(filename=x, data=read.csv(x))
})

prenatal.data.fully.stratified = lapply(prenatal.race.data.three, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data = subset(data, data$`Trimester.Prenatal.Care.Began` != "Unknown or Not Stated")
  data$race = tolower(data$`Mother.s.Single.Race.6`)
  data$ethnicity = tolower(data$`Mother.s.Hispanic.Origin`)
  data$age = data$`Age.of.Mother.9`
  
  #Decided Feb 2025 that this will only include 15-44 so it aligns with births as the denominator:
  data <- data %>%
    filter(age != "50 years and over")%>% #These ages shouldn't be here bc fertility rate = 15-44
    filter(age != "45-49 years")%>%
    filter(age != "Under 15 years")
  
  if(grepl("national", filename)) {
    data$location = "US"
  }
  
  if(grepl("county", filename)) {
    data$location = as.character(data$'County.of.Residence.Code')
  }
  
  if(grepl("2023", filename)) {
      data$year = "2023"  }
  if(grepl("2022", filename)) {
    data$year = "2022"  }
  if(grepl("2021", filename)) {
    data$year = "2021"  }
  if(grepl("2020", filename)) {
    data$year = "2020"  }
  if(grepl("2019", filename)) {
    data$year = "2019"  }
  if(grepl("2018", filename)) {
    data$year = "2018"  }
  if(grepl("2017", filename)) {
    data$year = "2017"  }
  if(grepl("2016", filename)) {
    data$year = "2016"  }
  if(grepl("16_23", filename)) {
    data$year = as.character(data$Year)  }

    #making race changes: removing unknown and more than one race, combining asian w native hawaiian or PI to align with existing ontology for births:
  data <- data %>%
    filter(race != 'more than one race')%>% #Removing these because I don't think they get redistributed since this is a proportion
    filter(ethnicity != 'unknown or not stated')%>% #Removing these because I don't think they get redistributed since this is a proportion
    mutate(new.race = ifelse(race == "asian", "asian or pacific islander", race))%>%
    mutate(new.race = ifelse(race == "native hawaiian or other pacific islander", "asian or pacific islander", new.race))%>%
    group_by(year, location, `Trimester.Prenatal.Care.Began`, new.race, ethnicity, age)%>%
    mutate(new.value = sum(Births))%>%
    select(-race, -Births)%>%
    rename(race = new.race)%>%
    rename(Births = new.value)%>%
    ungroup()%>%
    select(location, year, race, ethnicity, age, `Trimester.Prenatal.Care.Began`, Births)
    
  
  data<- data[!duplicated(data), ]  #remove duplicates created by race change so births aren't summed twice 
  
  data <- data %>%
    filter(race != 'more than one race')%>% #Removing these because I don't think they get redistributed since this is a proportion
    filter(ethnicity != 'unknown or not stated')%>% #Removing these because I don't think they get redistributed since this is a proportion
    group_by(location, year, age, race, ethnicity)%>%
    mutate(total.births.that.year = sum(Births))%>%
    mutate(value = Births/total.births.that.year)%>%
    mutate(qa.check = sum(value))%>%
    mutate(outcome = case_when(`Trimester.Prenatal.Care.Began` == "1st to 3rd month" ~"prenatal.care.initiation.first.trimester",
                               `Trimester.Prenatal.Care.Began` == "4th to 6th month" ~"prenatal.care.initiation.second.trimester",
                               `Trimester.Prenatal.Care.Began` == "7th to final month" ~"prenatal.care.initiation.third.trimester",
                               `Trimester.Prenatal.Care.Began` == "No prenatal care" ~"no.prenatal.care"))%>%
    mutate(location.check = locations::is.location.valid(location))%>%
    filter(location.check == T) %>% #Remove the 'unidentified counties'
    select(outcome, year, location, age, race, ethnicity, value, Births)

  data= as.data.frame(data)
  
  list(filename, data)
})


prenatal.data.fully.stratified.put = lapply(prenatal.data.fully.stratified, `[[`, 2)

for (data in prenatal.data.fully.stratified.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.fertility',
    source = 'cdc.wonder.natality',
    dimension.values = list(),
    url = 'https://wonder.cdc.gov/natality-expanded-current.html',
    details = 'CDC Wonder Natality Data 2016-2023')
}
