#Table 28a/b from Ryan White Reports- suppression/ambulatory
#we decided to pull the N from the viral suppression tables to represent ambulatory care
#Renaming the outcome 'oahs.clients' rather than 'ambulatory.care.past.year'

# Ambulatory (suppression total N) Data Total --------------------------------------------------------
DATA.DIR.RYAN.WHITE.AMBULATORY="Q:/data_raw/ryan.white.pdf.tables/ambulatory"

ambulatory.files <- Sys.glob(paste0(DATA.DIR.RYAN.WHITE.AMBULATORY, '/*.csv'))

ryan.white.ambulatory <- lapply(ambulatory.files, function(x){
  list(filename=x, data=read.csv(x, check.names = FALSE, colClasses = 'character'))
})

ryan.white.ambulatory.clean = lapply(ryan.white.ambulatory, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data <- data %>%
    pivot_longer(cols = contains("total"),
                 names_to = "year",
                 values_to = "value")
  
  data$value = gsub(",", "", data$value)
  data$value = as.numeric(data$value)

  if(grepl("state", filename)) {

    data <- data %>%
      select(state, year, value)%>%
      filter(state != 'Subtotal')

    data$location = locations::get.location.code(data$state, 'STATE')
    data$location = as.character(data$location)
  }

  if(grepl("_msa", filename)) {
    data$location = locations::get.location.code(data$`ema.tga`, 'CBSA')

    data<- data %>%
      mutate(location = ifelse(`ema.tga` == "Austin", "C.12420", location))%>%
      mutate(location = ifelse(`ema.tga` == "Charlotte", "C.16740", location))%>%
      mutate(location = ifelse(`ema.tga` == "Cleveland", "C.17460", location))%>%
      mutate(location = ifelse(`ema.tga` == "Norfolk", "C.47260", location))%>%
      mutate(location = ifelse(`ema.tga` == "Philadelphia", "C.37980", location))%>%
      mutate(location = ifelse(`ema.tga` == "Portland", "C.38900", location))%>%
      mutate(location = ifelse(`ema.tga` == "Washington", "C.47900", location))%>%
      mutate(location = ifelse(`ema.tga` == "Riverside San Bernardino", "C.40140", location))%>%
      mutate(location = ifelse(`ema.tga` == "Jacksonville", "C.27260", location))%>%
      mutate(location = ifelse(`ema.tga` == "Tampa St. Petersburg", "C.45300", location))%>%
      mutate(location = ifelse(`ema.tga` == "Minneapolis St. Paul", "C.33460", location))%>%
      mutate(location = ifelse(`ema.tga` == "Las Vegas", "C.29820", location))%>%
      mutate(location = ifelse(`ema.tga` == "Columbus", "C.18140", location))%>%

      #These are MSAs that need to be combined
      mutate(location = ifelse(`ema.tga` == "San Francisco", "C.41860", location))%>%
      mutate(location = ifelse(`ema.tga` == "Oakland", "C.41860", location))%>%

      mutate(location = ifelse(`ema.tga` == "Los Angeles", "C.31080", location))%>%
      mutate(location = ifelse(`ema.tga` == "Orange County", "C.31080", location))%>%

      mutate(location = ifelse(`ema.tga` == "Miami", "C.33100", location))%>%
      mutate(location = ifelse(`ema.tga` == "West Palm Beach", "C.33100", location))%>%
      mutate(location = ifelse(`ema.tga` == "Ft. Lauderdale", "C.33100", location))%>%

      mutate(location = ifelse(`ema.tga` == "Dallas", "C.19100", location))%>%
      mutate(location = ifelse(`ema.tga` == "Ft. Worth", "C.19100", location))%>%

      mutate(location = ifelse(`ema.tga` == "New York", "C.35620", location))%>%
      mutate(location = ifelse(`ema.tga` == "Middlesex", "C.35620", location))%>%
      mutate(location = ifelse(`ema.tga` == "Nassau Suffolk", "C.35620", location))%>%
      mutate(location = ifelse(`ema.tga` == "Bergen Passaic", "C.35620", location))%>%
      mutate(location = ifelse(`ema.tga` == "Jersey City", "C.35620", location))%>%
      mutate(location = ifelse(`ema.tga` == "Newark", "C.35620", location))%>%

      #Sum the combined MSAs
      group_by(location, year)%>%
      mutate(summed.value = sum(value))%>%
      select(-value)%>%
      rename(value = summed.value)
  }

  data$outcome = "oahs.clients"
  data$year = gsub("total count", "", data$year)
  data$location = as.character(data$location)
  data$year = trimws(data$year)

  ##Taking the most recent report of the oldest year:
  if(grepl("non.adap_2022", filename)) {
    data <- data %>%
      filter(year == "2018")
  }
  if(grepl("non.adap_2021", filename)) {
    data <- data %>%
      filter(year == "2017")
  }
  if(grepl("non.adap_2020", filename)) {
    data <- data %>%
      filter(year == "2016")
  }
  if(grepl("non.adap_2019", filename)) {
    data <- data %>%
      filter(year == "2015")
  }
  if(grepl("non.adap_2018", filename)) {
    data <- data %>%
      filter(year == "2014")
  }
  if(grepl("non.adap_2017", filename)) {
    data <- data %>%
      filter(year == "2013")
  }
  if(grepl("adap.clients_2021", filename)) {
    data <- data %>%
      filter(year == "2017")
  }
  if(grepl("adap.clients_2020", filename)) {
    data <- data %>%
      filter(year == "2016")
  }
  ####
  
  data= as.data.frame(data)
  list(filename, data)
})

ryan.white.ambulatory.clean.put = lapply(ryan.white.ambulatory.clean, `[[`, 2)

for (data in ryan.white.ambulatory.clean.put) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'ryan.white.pdfs',
    source = 'ryan.white.program',
    url = 'https://ryanwhite.hrsa.gov/data/reports',
    details = 'Ryan White Downloaded PDF Reports')
}


# Stratified Ambulatory ---------------------------------------------------

DATA.DIR.RYAN.WHITE.AMBULATORY.STRATA="Q:/data_raw/ryan.white.pdf.tables/ambulatory/stratified"

ambulatory.files.stratified <- Sys.glob(paste0(DATA.DIR.RYAN.WHITE.AMBULATORY.STRATA, '/*.csv'))

ryan.white.ambulatory.stratified <- lapply(ambulatory.files.stratified, function(x){
  list(filename=x, data=read.csv(x, check.names = FALSE, colClasses = 'character'))
})

ryan.white.ambulatory.stratified.clean = lapply(ryan.white.ambulatory.stratified, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  if(grepl("_age", filename)) {
    data <- data %>%
      pivot_longer(cols = contains("total"),
                   names_to = "age",
                   values_to = "value")%>%
      mutate(age = gsub("total", "", age))%>%
      mutate(age = gsub("n", "", age))%>%
      mutate(age = trimws(age))%>%
      filter(age != "<13 years")
  }
  if(grepl("race", filename)) {
    data <- data %>%
      pivot_longer(cols = contains("total"),
                   names_to = "race",
                   values_to = "value")%>%
      mutate(race = gsub("total", "", race))%>%
      mutate(race = trimws(race))
  }
  
  if(grepl("_state", filename)) {
    data <- data %>%
      filter(state != 'Subtotal')
    data$location = locations::get.location.code(data$state, 'STATE')
  }
  if(grepl("_msa", filename)) {
    data$location = locations::get.location.code(data$`ema.tga`, 'CBSA')

    data<- data %>%
      mutate(location = ifelse(`ema.tga` == "Austin", "C.12420", location))%>%
      mutate(location = ifelse(`ema.tga` == "Charlotte", "C.16740", location))%>%
      mutate(location = ifelse(`ema.tga` == "Cleveland", "C.17460", location))%>%
      mutate(location = ifelse(`ema.tga` == "Norfolk", "C.47260", location))%>%
      mutate(location = ifelse(`ema.tga` == "Philadelphia", "C.37980", location))%>%
      mutate(location = ifelse(`ema.tga` == "Portland", "C.38900", location))%>%
      mutate(location = ifelse(`ema.tga` == "Washington", "C.47900", location))%>%
      mutate(location = ifelse(`ema.tga` == "Riverside San Bernardino", "C.40140", location))%>%
      mutate(location = ifelse(`ema.tga` == "Jacksonville", "C.27260", location))%>%
      mutate(location = ifelse(`ema.tga` == "Tampa St. Petersburg", "C.45300", location))%>%
      mutate(location = ifelse(`ema.tga` == "Minneapolis St. Paul", "C.33460", location))%>%
      mutate(location = ifelse(`ema.tga` == "Las Vegas", "C.29820", location))%>%
      mutate(location = ifelse(`ema.tga` == "Columbus", "C.18140", location))%>%

      #These are MSAs that need to be combined
      mutate(location = ifelse(`ema.tga` == "San Francisco", "C.41860", location))%>%
      mutate(location = ifelse(`ema.tga` == "Oakland", "C.41860", location))%>%

      mutate(location = ifelse(`ema.tga` == "Los Angeles", "C.31080", location))%>%
      mutate(location = ifelse(`ema.tga` == "Orange County", "C.31080", location))%>%

      mutate(location = ifelse(`ema.tga` == "Miami", "C.33100", location))%>%
      mutate(location = ifelse(`ema.tga` == "West Palm Beach", "C.33100", location))%>%
      mutate(location = ifelse(`ema.tga` == "Ft. Lauderdale", "C.33100", location))%>%

      mutate(location = ifelse(`ema.tga` == "Dallas", "C.19100", location))%>%
      mutate(location = ifelse(`ema.tga` == "Ft. Worth", "C.19100", location))%>%

      mutate(location = ifelse(`ema.tga` == "New York", "C.35620", location))%>%
      mutate(location = ifelse(`ema.tga` == "Middlesex", "C.35620", location))%>%
      mutate(location = ifelse(`ema.tga` == "Nassau Suffolk", "C.35620", location))%>%
      mutate(location = ifelse(`ema.tga` == "Bergen Passaic", "C.35620", location))%>%
      mutate(location = ifelse(`ema.tga` == "Jersey City", "C.35620", location))%>%
      mutate(location = ifelse(`ema.tga` == "Newark", "C.35620", location))
  }

  if(grepl("2023", filename)) {
    data$year = "2023"
  }
  if(grepl("2022", filename)) {
    data$year = "2022"
  }
  if(grepl("2021", filename)) {
    data$year = "2021"
  }
  if(grepl("2020", filename)) {
    data$year = "2021"
  }
  if(grepl("2019", filename)) {
    data$year = "2019"
  }
  if(grepl("2018", filename)) {
    data$year = "2018"
  }
  if(grepl("2017", filename)) {
    data$year = "2017"
  }
  
  data$value = as.numeric(gsub(",", "", data$value))
  data$outcome = "oahs.clients"
  
  #Need to sum the MSAs that are grouped:
  if(grepl("race", filename)) {
  data <- data %>%
    group_by(location, race)%>%
    mutate(summed.value = sum(value, na.rm = T))%>%
    select(-value)%>%
    rename(value = summed.value)
  
  #Manually remove Dallas MSA (they did not report data for 2022 by race but Fort Worth did)
  data<-data%>%
    mutate(drop_var = ifelse(location == "C.19100" & year == "2022", "1", "0"))%>%
    filter(drop_var != "1")
  }
  if(grepl("age", filename)) {
    data <- data %>%
      group_by(location, age)%>%
      mutate(summed.value = sum(value, na.rm = T))%>%
      select(-value)%>%
      rename(value = summed.value)
  }
  data$location = as.character(data$location)
  
data= as.data.frame(data)
list(filename, data)
})

ryan.white.ambulatory.stratified.clean.put = lapply(ryan.white.ambulatory.stratified.clean, `[[`, 2)

for (data in ryan.white.ambulatory.stratified.clean.put) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'ryan.white.pdfs',
    source = 'ryan.white.program',
    dimension.values.to.distribute = list(race=c('multiple races')),
    url = 'https://ryanwhite.hrsa.gov/data/reports',
    details = 'Ryan White Downloaded PDF Reports')
}
