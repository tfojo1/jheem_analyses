#Table 28a from Ryan White Reports (denoinator will be non.adap)

# Viral Suppression Data --------------------------------------------------------
DATA.DIR.RYAN.WHITE.SUPPRESSION="../../data_raw/ryan.white.pdf.tables/viral.suppression/state"

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
  
 data$outcome = "non.adap.viral.suppression"
 data$value = (data$value/100)
 data$year = gsub("suppression percent", "", data$year)
  
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
DATA.DIR.RYAN.WHITE.SUPPRESSION.MSA="../../data_raw/ryan.white.pdf.tables/viral.suppression/msa"

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
    select(year, location, numerator, denominator)%>%
    filter(!is.na(numerator))%>%
    filter(!is.na(denominator))
  
  data<- data[!duplicated(data), ]
  
  data<-data %>%
    mutate(value=(numerator/denominator))%>%
    mutate(outcome = "non.adap.viral.suppression")%>%
    select(-numerator, -denominator)
  
  data$location = as.character(data$location)

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

