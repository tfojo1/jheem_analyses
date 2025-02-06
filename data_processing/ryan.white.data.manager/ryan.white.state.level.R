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
      filter(State != 'Subtotal')
    data$location = locations::get.location.code(data$State, 'STATE')
    data$location = as.character(data$location)
  }
  
  # if(grepl("_msa", filename)) {
  #   data$location = locations::get.location.code(data$`ema/tga`, 'CBSA')
  # }
  
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


# Stratified Level Data ---------------------------------------------------
DATA.DIR.RYAN.WHITE.STRATIFIED="../../data_raw/ryan.white.pdf.tables/stratified"

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
  
  # if(grepl("_msa", filename)) {
  #   data$location = locations::get.location.code(data$`ema/tga`, 'CBSA')
  # }
  
  data$value = gsub(",", '', data$value)
  data$value = as.numeric(data$value)
  
  if(grepl("non.adap", filename)) {
    data$outcome = 'non.adap.clients'
  }
  if(grepl("adap.clients", filename)) {
    data$outcome = 'adap.clients'
  }
  
  if(grepl("2022", filename)) {
    data$year = "2022"
  }
  if(grepl("2023", filename)) {
    data$year = "2023"
  }
  
  data$location = as.character(data$location)
  
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
