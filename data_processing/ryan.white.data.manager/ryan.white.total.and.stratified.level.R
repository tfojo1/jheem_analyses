# Total Level Data --------------------------------------------------------
DATA.DIR.RYAN.WHITE="../../data_raw/ryan.white.pdf.tables/total"

pdf.reports <- Sys.glob(paste0(DATA.DIR.RYAN.WHITE, '/*.csv'))

ryan.white.pdf.reports <- lapply(pdf.reports, function(x){
  list(filename=x, data=read.csv(x, check.names = FALSE))
})

###
ryan.white.totals = lapply(ryan.white.pdf.reports, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data <- data %>%
    pivot_longer(cols = contains("20"),
                 names_to = "year",
                 values_to = "value")
  
  data$value = gsub(",", '', data$value)
  data$value = as.numeric(data$value)
  
  if(grepl("non.adap", filename)) {
    data$outcome = 'non.adap.clients'
  }
  if(grepl("adap.clients", filename)) {
    data$outcome = 'adap.clients'
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
      mutate(location = ifelse(`ema.tga` == "Newark", "C.35620", location))%>%
      
      #Sum the combined MSAs
      group_by(location, year)%>%
      mutate(summed.value = sum(value))%>%
      select(-value)%>%
      rename(value = summed.value)
      
  }
  
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
  
  data$location = as.character(data$location)
  
  data= as.data.frame(data)
  list(filename, data)
})

###
ryan.white.totals.put = lapply(ryan.white.totals, `[[`, 2)

for (data in ryan.white.totals.put) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'ryan.white.pdfs',
    source = 'ryan.white.program',
    url = 'https://ryanwhite.hrsa.gov/data/reports',
    details = 'Ryan White Downloaded PDF Reports')
}


# Stratified Level Data - STATE ---------------------------------------------------
DATA.DIR.RYAN.WHITE.STRATIFIED="../../data_raw/ryan.white.pdf.tables/stratified.state"

pdf.reports.stratified <- Sys.glob(paste0(DATA.DIR.RYAN.WHITE.STRATIFIED, '/*.csv'))

ryan.white.pdf.reports.stratified <- lapply(pdf.reports.stratified, function(x){
  list(filename=x, data=read.csv(x, check.names = FALSE, colClasses = "character"))
})

###
ryan.white.stratified = lapply(ryan.white.pdf.reports.stratified, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  if(grepl("race.ethnicity", filename)) {
    data <- data %>%
      pivot_longer(cols = contains("count"),
                   names_to = "race",
                   values_to = "value")%>%
      select(state, race, value)%>%
      mutate(race = gsub("count", "", race))%>%
      mutate(race = trimws(race))%>%
      filter(race != 'total')
  }
  
  if(grepl("agegroup", filename)) {
    data <- data %>%
      pivot_longer(cols = contains("count"),
                   names_to = "age",
                   values_to = "value")%>%
      select(state, age, value)%>%
      mutate(age = gsub("count", "", age))%>%
      mutate(age = trimws(age))%>%
      filter(age != "<13 years")%>%
      filter(age != 'total')
  }
  
  if(grepl("fpl", filename)) {
    data <- data %>%
      pivot_longer(cols = contains("count"),
                   names_to = "fpl",
                   values_to = "value")%>%
      select(state, fpl, value)%>%
      mutate(fpl = gsub("count", "", fpl))%>%
      mutate(fpl = gsub("FPL", "", fpl))%>%
      mutate(fpl = trimws(fpl))%>%
      filter(fpl != 'total')
  }
  
  if(grepl("service.received", filename)) {
    data <- data %>%
      pivot_longer(cols = contains("count"),
                   names_to = "service.received",
                   values_to = "value")%>%
      select(state, service.received, value)%>%
      mutate(service.received = gsub("count", "", service.received))%>%
      mutate(service.received = trimws(service.received))%>%
      filter(service.received != 'total')
  }
  
  if(grepl("risk_sex", filename)) {
    data <- data %>%
      pivot_longer(cols = contains("count"),
                   names_to = "risk",
                   values_to = "value")%>%
      select(state, risk, value)%>%
      mutate(risk = gsub("count", "", risk))%>%
      mutate(risk = trimws(risk))%>%
      filter(risk != 'total')
  }
  
  if(grepl("state_sex_only", filename)) {
    data <- data %>%
      pivot_longer(cols = contains("count"),
                   names_to = "sex",
                   values_to = "value")%>%
      select(state, sex, value)%>%
      mutate(sex = gsub("count", "", sex))%>%
      mutate(sex = trimws(sex))%>%
      filter(sex == "male" | sex == 'female')
  }
  
  if(grepl("_state", filename)) {
    data <- data %>%
      filter(state != 'Subtotal')
    data$location = locations::get.location.code(data$state, 'STATE')
  }
  
  data$value = gsub(",", '', data$value)
  data$value = as.numeric(data$value)
  
  if(grepl("non.adap", filename)) {
    data$outcome = 'non.adap.clients'
  }
  if(grepl("adap.clients", filename)) {
    data$outcome = 'adap.clients'
  }
  
  if(grepl("2017", filename)) {
    data$year = "2017"
  }
  if(grepl("2018", filename)) {
    data$year = "2018"
  }
  if(grepl("2019", filename)) {
    data$year = "2019"
  }
  if(grepl("2020", filename)) {
    data$year = "2020"
  }
  if(grepl("2021", filename)) {
    data$year = "2021"
  }
  if(grepl("2022", filename)) {
    data$year = "2022"
  }
  if(grepl("2023", filename)) {
    data$year = "2023"
  }
  
  #Group and sum the 'other' risk category for risk_sex
  if(grepl("risk_sex", filename)) {
    data <- data %>%
      select(outcome, year, location, risk, value)%>%
      mutate(risk = trimws(risk))%>%
      mutate(risk = ifelse(risk == "perinatal", 'other', risk))%>%
      group_by(year, location, risk)%>%
      mutate(test = sum(value))%>%
      select(-value)%>%
      rename(value = test)
  }

  if(grepl("risk_sex_female", filename)) {
    data$sex = 'female'
  }
  if(grepl("risk_sex_male", filename)) {
    data$sex = 'male'
  }
  
  data$location = as.character(data$location)
  
  data= as.data.frame(data)
  list(filename, data)
})

ryan.white.stratified.put = lapply(ryan.white.stratified, `[[`, 2)

for (data in ryan.white.stratified.put) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'ryan.white.pdfs',
    source = 'ryan.white.program',
    dimension.values.to.distribute = list(race=c('multiple races')),
    url = 'https://ryanwhite.hrsa.gov/data/reports',
    details = 'Ryan White Downloaded PDF Reports')
}

# Stratified Level Data - MSA ---------------------------------------------------
DATA.DIR.RYAN.WHITE.STRATIFIED.MSA="../../data_raw/ryan.white.pdf.tables/stratified.msa"

pdf.reports.stratified.msa <- Sys.glob(paste0(DATA.DIR.RYAN.WHITE.STRATIFIED.MSA, '/*.csv'))

ryan.white.pdf.reports.stratified.msa <- lapply(pdf.reports.stratified.msa, function(x){
  list(filename=x, data=read.csv(x, check.names = FALSE, colClasses = "character"))
})

ryan.white.stratified.msa = lapply(ryan.white.pdf.reports.stratified.msa, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]

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

  if(grepl("race.ethnicity", filename)) {
    data <- data %>%
      pivot_longer(cols = contains("count"),
                   names_to = "race",
                   values_to = "value")%>%
      select('ema.tga', race, value, location)%>%
      mutate(value = as.numeric(gsub(",", "", value)))%>%
      mutate(race = gsub("count", "", race))%>%
      mutate(race = trimws(race))%>%
      filter(race != 'total')%>%
      group_by(location, race)%>%
      mutate(summed.value = sum(value))
  }

  if(grepl("agegroup", filename)) {
    data <- data %>%
      pivot_longer(cols = contains("count"),
                   names_to = "age",
                   values_to = "value")%>%
      select('ema.tga', age, value, location)%>%
        mutate(value = as.numeric(gsub(",", "", value)))%>%
      mutate(age = gsub("count", "", age))%>%
      mutate(age = trimws(age))%>%
      filter(age != "<13 years")%>%
      filter(age != 'total')%>%
      group_by(location, age)%>%
      mutate(summed.value = sum(value))
  }

  if(grepl("fpl", filename)) {
    data <- data %>%
      pivot_longer(cols = contains("count"),
                   names_to = "fpl",
                   values_to = "value")%>%
      select('ema.tga', fpl, value, location)%>%
        mutate(value = as.numeric(gsub(",", "", value)))%>%
      mutate(fpl = gsub("count", "", fpl))%>%
      mutate(fpl = gsub("FPL", "", fpl))%>%
      mutate(fpl = trimws(fpl))%>%
      filter(fpl != 'total')%>%
      group_by(location, fpl)%>%
      mutate(summed.value = sum(value))
  }

  if(grepl("service.received", filename)) {
    data <- data %>%
      pivot_longer(cols = contains("count"),
                   names_to = "service.received",
                   values_to = "value")%>%
      select('ema.tga', service.received, value, location)%>%
        mutate(value = as.numeric(gsub(",", "", value)))%>%
      mutate(service.received = gsub("count", "", service.received))%>%
      mutate(service.received = trimws(service.received))%>%
      filter(service.received != 'total')%>%
      group_by(location, service.received)%>%
      mutate(summed.value = sum(value))
  }

  if(grepl("risk_sex", filename)) {
    data <- data %>%
      pivot_longer(cols = contains("count"),
                   names_to = "risk",
                   values_to = "value")%>%
      select('ema.tga', risk, value, location)%>%
        mutate(value = as.numeric(gsub(",", "", value)))%>%
      mutate(risk = gsub("count", "", risk))%>%
      mutate(risk = trimws(risk))%>%
      filter(risk != 'total')
  }

  if(grepl("msa_sex_only", filename)) {
    data <- data %>%
      pivot_longer(cols = contains("count"),
                   names_to = "sex",
                   values_to = "value")%>%
      select('ema.tga', sex, value, location)%>%
        mutate(value = as.numeric(gsub(",", "", value)))%>%
      mutate(sex = gsub("count", "", sex))%>%
      mutate(sex = trimws(sex))%>%
      filter(sex == "male" | sex == 'female')%>%
      group_by(location, sex)%>%
      mutate(summed.value = sum(value))
  }

  if(grepl("non.adap", filename)) {
    data$outcome = 'non.adap.clients'
  }
  if(grepl("adap.clients", filename)) {
    data$outcome = 'adap.clients'
  }

  if(grepl("2017", filename)) {
    data$year = "2017"
  }
  if(grepl("2018", filename)) {
    data$year = "2018"
  }
  if(grepl("2019", filename)) {
    data$year = "2019"
  }
  if(grepl("2020", filename)) {
    data$year = "2020"
  }
  if(grepl("2021", filename)) {
    data$year = "2021"
  }
  if(grepl("2022", filename)) {
    data$year = "2022"
  }
  if(grepl("2023", filename)) {
    data$year = "2023"
  }

  # #Group and sum the 'other' risk category for risk_sex
  if(grepl("risk_sex", filename)) {
    data <- data %>%
      select('ema.tga', outcome, year, location, risk, value)%>%
      mutate(risk = trimws(risk))%>%
      mutate(risk = ifelse(risk == "perinatal", 'other', risk))%>%
      group_by(year, location, risk)%>%
      mutate(summed.value = sum(value))
  }

  if(grepl("risk_sex_female", filename)) {
    data$sex = 'female'
  }
  if(grepl("risk_sex_male", filename)) {
    data$sex = 'male'
  }

  data <- data %>%
    select(-value)%>%
    rename(value = summed.value)
  
  data$location = as.character(data$location)

  data= as.data.frame(data)
  list(filename, data)
})

ryan.white.stratified.msa.put = lapply(ryan.white.stratified.msa, `[[`, 2)

for (data in ryan.white.stratified.msa.put) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'ryan.white.pdfs',
    source = 'ryan.white.program',
    dimension.values.to.distribute = list(race=c('multiple races')),
    url = 'https://ryanwhite.hrsa.gov/data/reports',
    details = 'Ryan White Downloaded PDF Reports')
}