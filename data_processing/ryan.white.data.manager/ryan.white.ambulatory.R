#Table 27 from Ryan White Reports- Retention/ambulatory

# Retention/Ambulatory  Data --------------------------------------------------------
DATA.DIR.RYAN.WHITE.AMBULATORY="../../data_raw/ryan.white.pdf.tables/ambulatory"

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
      mutate(location = ifelse(`ema.tga` == "Miami", "C.33100", location))%>%
      mutate(location = ifelse(`ema.tga` == "Norfolk", "C.47260", location))%>%
      mutate(location = ifelse(`ema.tga` == "Philadelphia", "C.37980", location))%>%
      mutate(location = ifelse(`ema.tga` == "Portland", "C.38900", location))%>%
      mutate(location = ifelse(`ema.tga` == "Washington", "C.47900", location))%>%
      mutate(location = ifelse(`ema.tga` == "Riverside San Bernardino", "C.40140", location))%>%
      mutate(location = ifelse(`ema.tga` == "Jacksonville", "C.27260", location))%>%
      mutate(location = ifelse(`ema.tga` == "Tampa", "C.45300", location))%>%
      mutate(location = ifelse(`ema.tga` == "Minneapolis", "C.33460", location))%>%
      mutate(location = ifelse(`ema.tga` == "Las Vegas", "C.29820", location))%>%
      mutate(location = ifelse(`ema.tga` == "Columbus", "C.18140", location))%>%
      mutate(location = ifelse(`ema.tga` == "Ft. Worth", "C.49380", location))

    data$location.check = locations::is.location.valid(data$location)
    data = subset(data, data$location.check == "TRUE")
    data$location = as.character(data$location)
  }
  
  data$outcome = "ambulatory.care.past.year"
  data$year = gsub("total count", "", data$year)
  
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
