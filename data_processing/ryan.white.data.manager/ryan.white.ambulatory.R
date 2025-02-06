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
  
  data$outcome = "non.adap.retention"
  data$value = (data$value/100)
  data$year = gsub("retained percent", "", data$year)
  
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
