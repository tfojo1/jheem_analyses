#Table 28a from Ryan White Reports (denominator will be non.adap)

# Viral Suppression Data --------------------------------------------------------
DATA.DIR.RYAN.WHITE.SUPPRESSION="Q:/data_raw/ryan.white.pdf.tables/viral.suppression/state"

ryan.white.suppression.files <- Sys.glob(paste0(DATA.DIR.RYAN.WHITE.SUPPRESSION, '/*.csv'))

ryan.white.suppression <- lapply(ryan.white.suppression.files, function(x){
  list(filename=x, data=read.csv(x, check.names = FALSE, colClasses = 'character'))
})

#Viral Suppression - State Level
ryan.white.suppression.clean = lapply(ryan.white.suppression, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data <- data %>%
    pivot_longer(cols = contains("percent"),
                 names_to = "year",
                 values_to = "value")
  
  data$value = as.numeric(data$value)
  
  if(grepl("state", filename)) {
    
    data <- data %>%
      select(state, year, value)%>%
     filter(state != 'Subtotal')
    
      data$location = locations::get.location.code(data$state, 'STATE')
      data$location = as.character(data$location)
  }
  
 data$outcome = "oahs.suppression"
 data$value = (data$value/100)
 data$year = gsub("suppression percent", "", data$year)
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
 
 
 # if(grepl("adap.clients_2021", filename)) {
 #   data <- data %>%
 #     filter(year == "2017")
 # }
 # if(grepl("adap.clients_2020", filename)) {
 #   data <- data %>%
 #     filter(year == "2016")
 # }
 ####
  
  data= as.data.frame(data)
  list(filename, data)
})

ryan.white.suppression.clean.put = lapply(ryan.white.suppression.clean, `[[`, 2)

for (data in ryan.white.suppression.clean.put) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'ryan.white.pdfs',
    source = 'ryan.white.program',
    url = 'https://ryanwhite.hrsa.gov/data/reports',
    details = 'Ryan White Downloaded PDF Reports')
}


# Viral Suppression - MSA Level -------------------------------------------
#For the MSA, because they need to be regrouped, I'm going to put the N values and sum and then calculate a new proportion
DATA.DIR.RYAN.WHITE.SUPPRESSION.MSA="Q:/data_raw/ryan.white.pdf.tables/viral.suppression/msa"

ryan.white.suppression.files.msa <- Sys.glob(paste0(DATA.DIR.RYAN.WHITE.SUPPRESSION.MSA, '/*.csv'))

ryan.white.suppression.msa <- lapply(ryan.white.suppression.files.msa, function(x){
  list(filename=x, data=read.csv(x, check.names = FALSE, colClasses = 'character'))
})

ryan.white.suppression.clean.msa = lapply(ryan.white.suppression.msa, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data <- data[,!grepl("percent", colnames(data))]
  
  data <- data %>%
    pivot_longer(cols = contains("count"),
                 names_to = "outcome",
                 values_to = "value")

  data$value = as.numeric(gsub(",", "", data$value))
  data$year = as.character(str_sub(data$outcome, 1, 4))
  data$outcome = gsub('[0-9.]', '', data$outcome)
  
  if(grepl("msa", filename)) {
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
  
  data <- data %>%
    pivot_wider(names_from = "outcome", values_from = "value", values_fill = 0)%>%
    select(year, location, ` total count`, ` suppression count`)%>%
    group_by(year, location)%>%
    mutate(denominator = sum(` total count`))%>%
    mutate(numerator = sum(` suppression count`))%>%
    select(year, location, numerator, denominator)
  
  data<- data[!duplicated(data), ]
  
  data<-data %>%
    mutate(value=(numerator/denominator))%>%
    mutate(outcome = "oahs.suppression")%>%
    select(-numerator, -denominator)
  
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

  # if(grepl("adap.clients_2021", filename)) {
  #   data <- data %>%
  #     filter(year == "2017")
  # }
  # 
  # if(grepl("adap.clients_2020", filename)) {
  #   data <- data %>%
  #     filter(year == "2016")
  # }
  ####

data= as.data.frame(data)
list(filename, data)
})

ryan.white.suppression.clean.msa.put = lapply(ryan.white.suppression.clean.msa, `[[`, 2)

for (data in ryan.white.suppression.clean.msa.put) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'ryan.white.pdfs',
    source = 'ryan.white.program',
    url = 'https://ryanwhite.hrsa.gov/data/reports',
    details = 'Ryan White Downloaded PDF Reports')
}


# Stratified Data for Suppression -----------------------------------------
#Separating by State and MSA- because we are grouping together MSAs the proportion will need to be re-calculated


# STATE - stratified ------------------------------------------------------
DATA.DIR.RYAN.WHITE.SUPPRESSION.STATE.STRATA="Q:/data_raw/ryan.white.pdf.tables/viral.suppression/stratified/state"

ryan.white.suppression.files.state.strata <- Sys.glob(paste0(DATA.DIR.RYAN.WHITE.SUPPRESSION.STATE.STRATA, '/*.csv'))

ryan.white.suppression.state.strata <- lapply(ryan.white.suppression.files.state.strata, function(x){
  list(filename=x, data=read.csv(x, check.names = FALSE, colClasses = 'character'))
})

state.stratified.suppression.clean = lapply(ryan.white.suppression.state.strata, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  if(grepl("_age", filename)) {
    data <- data %>%
      pivot_longer(cols = contains("percent"),
                   names_to = "age",
                   values_to = "value")%>%
      mutate(age = gsub("percent", "", age))%>%
      mutate(age = gsub("viral suppression", "", age))%>%
      mutate(age = trimws(age))%>%
      filter(age != "<13 years")
  }
  if(grepl("race", filename)) {
    data <- data %>%
      pivot_longer(cols = contains("percent"),
                   names_to = "race",
                   values_to = "value")%>%
      mutate(race = gsub("percent", "", race))%>%
      mutate(race = gsub("viral suppression", "", race))%>%
      mutate(race = trimws(race))%>%
      filter(race != 'multiple races') #Removing this because we cannot redistribute race for proportion outcome
   }
  
  if(grepl("_state", filename)) {
    data <- data %>%
      filter(state != 'Subtotal')
    data$location = locations::get.location.code(data$state, 'STATE')
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
  data$value = (data$value/100)
  data$outcome = "oahs.suppression"
  
  data$location = as.character(data$location)
  
  data= as.data.frame(data)
  list(filename, data)
})

state.stratified.suppression.clean.put = lapply(state.stratified.suppression.clean, `[[`, 2)

for (data in state.stratified.suppression.clean.put) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'ryan.white.pdfs',
    source = 'ryan.white.program',
    url = 'https://ryanwhite.hrsa.gov/data/reports',
    details = 'Ryan White Downloaded PDF Reports')
}

# MSA- stratified ---------------------------------------------------------
DATA.DIR.RYAN.WHITE.SUPPRESSION.MSA.STRATA="Q:/data_raw/ryan.white.pdf.tables/viral.suppression/stratified/msa"

ryan.white.suppression.files.msa.strata <- Sys.glob(paste0(DATA.DIR.RYAN.WHITE.SUPPRESSION.MSA.STRATA, '/*.csv'))

ryan.white.suppression.msa.strata <- lapply(ryan.white.suppression.files.msa.strata, function(x){
  list(filename=x, data=read.csv(x, check.names = FALSE, colClasses = 'character'))
})

ryan.white.suppression.msa.strata.clean = lapply(ryan.white.suppression.msa.strata, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data <- data[,!grepl("percent", colnames(data))]
  
  data <- data %>%
    pivot_longer(cols = contains("count"),
                 names_to = "strata",
                 values_to = "value")
  
   data$value = as.numeric(gsub(",", "", data$value))
   
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
   
    data$outcome = str_extract(data$strata, "suppression count|total count")
    data$strata = gsub("viral suppression count|total count", "", data$strata)
  
  if(grepl("msa", filename)) {
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

  data <- data %>%
    pivot_wider(names_from = "outcome", values_from = "value", values_fill = 0)%>%
    select(year, location, `total count`, `suppression count`, strata)%>%
    group_by(year, location, strata)%>%
    mutate(denominator = sum(`total count`, na.rm=T))%>%
    mutate(numerator = sum(`suppression count`, na.rm=T))%>%
    select(year, location, numerator, denominator)

  data<- data[!duplicated(data), ]

  data<-data %>%
    mutate(value=(numerator/denominator))%>%
    mutate(outcome = "oahs.suppression")%>%
    select(-numerator, -denominator)

  data$location = as.character(data$location)
  data$year = trimws(data$year)
  
  if(grepl("_race", filename)) {
    data<-data %>%
      rename(race=strata)
    data$race = trimws(data$race)
  }
  if(grepl("_age", filename)) {
    data<-data %>%
      rename(age=strata)%>%
      mutate(age = gsub("years ", "years", age))%>%
      filter(age != "<13 years")
  }
  
  data= as.data.frame(data)
  list(filename, data)
})

ryan.white.suppression.msa.strata.clean.put = lapply(ryan.white.suppression.msa.strata.clean, `[[`, 2)

for (data in ryan.white.suppression.msa.strata.clean.put) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'ryan.white.pdfs',
    source = 'ryan.white.program',
    url = 'https://ryanwhite.hrsa.gov/data/reports',
    details = 'Ryan White Downloaded PDF Reports')
}