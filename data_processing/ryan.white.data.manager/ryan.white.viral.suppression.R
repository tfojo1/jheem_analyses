#Table 28a from Ryan White Reports (denoinator will be non.adap)

# Viral Suppression Data --------------------------------------------------------
DATA.DIR.RYAN.WHITE.SUPPRESSION="../../data_raw/ryan.white.pdf.tables/viral.suppression"

ryan.white.suppression.files <- Sys.glob(paste0(DATA.DIR.RYAN.WHITE.SUPPRESSION, '/*.csv'))

ryan.white.suppression <- lapply(ryan.white.suppression.files, function(x){
  list(filename=x, data=read.csv(x, check.names = FALSE, colClasses = 'character'))
})

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
