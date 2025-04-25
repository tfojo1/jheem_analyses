#This processing supression data taken from NASTAD PDF Reports

#add to details: https://nastad.org/adap-monitoring-project
#source = nastad.adap


DATA.DIR.RYAN.WHITE.NASTAD="Q:/data_raw/ryan.white.pdf.tables/viral.suppression/nastad"

nastad.files <- Sys.glob(paste0(DATA.DIR.RYAN.WHITE.NASTAD, '/*.csv'))

nastad.suppression <- lapply(nastad.files, function(x){
  list(filename=x, data=read.csv(x))
})

###
nastad.suppression.clean = lapply(nastad.suppression, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data <- data%>%
    filter(state != 'total')%>%
    mutate(location = as.character(locations::get.location.code(state, 'STATE')))%>%
    mutate(outcome = "adap.suppression")%>%
    rename(value = `viral.load..200`)%>%
    select(location, outcome, value)
    
    if(grepl("2022", filename)) {
        data$year = "2022"
    }
  if(grepl("2021", filename)) {
    data$year = "2021"
  }
  if(grepl("2020", filename)) {
    data$year = "2020"
  }
  if(grepl("2019", filename)) {
    data$year = "2019"
  }
  
  data= as.data.frame(data)
  list(filename, data)
})

###

nastad.suppression.put = lapply(nastad.suppression.clean, `[[`, 2)

for (data in nastad.suppression.put) {
  data.manager$put.long.form(
    data = data,
    ontology.name = 'ryan.white.pdfs',
    source = 'nastad.adap',
    url = 'https://nastad.org/adap-monitoring-project',
    details = 'NASTAD PDF Reports')
}