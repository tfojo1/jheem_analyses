DATA.DIR.SYPHILIS.DEATHS="../../data_raw/syphilis.manager/cdc.wonder.syphilis.deaths"

syphilis.deaths.files <- list.files(DATA.DIR.SYPHILIS.DEATHS, pattern = ".txt", full.names = "TRUE")

suppressWarnings(syphilis.deaths <- lapply(syphilis.deaths.files, function(x) {
  list(filename=x, 
       data=read.delim2(x))
}))


syphilis.deaths.clean = lapply(syphilis.deaths, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data <- data %>%
    mutate(year = as.character(Year))%>%
    mutate(value = Deaths)%>%
    mutate(location ="US")
  
  if(grepl("congenital", filename)) {
    data$outcome = 'congenital.syphilis.deaths'
  }
  if(grepl("total", filename)) {
    data$outcome = 'total.syphilis.deaths'
  }
    
  data= as.data.frame(data)
  
  list(filename, data)
})

syphilis.deaths.clean.put = lapply(syphilis.deaths.clean, `[[`, 2)

for (data in syphilis.deaths.clean.put) {

  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.syphilis',
    source = 'cdc_wonder',
    dimension.values = list(),
    url = 'https://wonder.cdc.gov/mortsql.html',
    details = 'CDC Wonder Mortality')
}