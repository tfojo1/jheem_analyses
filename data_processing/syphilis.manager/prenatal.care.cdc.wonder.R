DATA.DIR.PRENATAL="../../data_raw/syphilis.manager/prenatal.care.cdc.wonder/total"

cdc.prenatal.files <- list.files(DATA.DIR.PRENATAL, pattern = ".txt", full.names = "TRUE")

suppressWarnings(cdc.prenatal.data <- lapply(cdc.prenatal.files, function(x) {
  list(filename=x, 
       data=read.delim2(x, 
       colClasses = c('County.of.Residence.Code' = 'character')))
}))

#Total Level Data

clean.total.prenatal = lapply(cdc.prenatal.data, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data = subset(data, data$`Trimester.Prenatal.Care.Began` != "Unknown or Not Stated")
  
  if(grepl("national", filename)) {
    data$location = "US"
  }
  
  if(grepl("county", filename)) {
    data$location = data$'County.of.Residence.Code'
  }
  
  data$year = as.character(data$Year)
  
  data <- data %>%
    select(-Year, -`Year.Code`, -Notes)%>%
    group_by(location, year)%>%
    mutate(total.births.that.year = sum(Births))%>%
    mutate(value = Births/total.births.that.year)%>%
    mutate(qa.check = sum(value))%>%
    mutate(outcome = case_when(`Trimester.Prenatal.Care.Began` == "1st to 3rd month" ~"prenatal.care.initiation.first.trimester",
                               `Trimester.Prenatal.Care.Began` == "4th to 6th month" ~"prenatal.care.initiation.second.trimester",
                               `Trimester.Prenatal.Care.Began` == "7th to final month" ~"prenatal.care.initiation.third.trimester",
                               `Trimester.Prenatal.Care.Began` == "No prenatal care" ~"no.prenatal.care"))%>%
    mutate(location.check = locations::is.location.valid(location))%>%
    filter(location.check == T) #Remove the 'unidentified counties'

  data= as.data.frame(data)
  
  list(filename, data)
})

#Put total level
total.prenatal.put = lapply(clean.total.prenatal, `[[`, 2)

for (data in total.prenatal.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.fertility',
    source = 'cdc.wonder.natality',
    dimension.values = list(),
    url = 'https://wonder.cdc.gov/natality-expanded-current.html',
    details = 'CDC Wonder Natality Data 2016-2023')
}
