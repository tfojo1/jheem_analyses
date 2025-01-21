# State Level Total -------------------------------------------------------

DATA.DIR.PRENATAL="../../data_raw/syphilis.manager/inadequate.prenatal"

prenatal_files <- Sys.glob(paste0(DATA.DIR.PRENATAL, '/*.csv'))


inadequate.prenatal <- lapply(prenatal_files, function(x){
  list(filename=x, data=read.csv(x, header=TRUE))
})

inadequate.prenatal.clean = lapply(inadequate.prenatal, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]

  data$year = data$Year
  
data <- data %>%
  select(year, contains("..Percent"))%>%
  pivot_longer(cols = contains("..Percent"),
                names_to = "state",
               values_to = "value")

  data$state = sub("..Percent.", "", data$state)

  data$outcome = 'inadequate.prenatal.care'
  data$location = as.character(locations::get.location.code(data$state, "state"))
  data$value = as.numeric(data$value/100) #change to proportion
  data$year = as.character(data$year)

  data= as.data.frame(data)
  
  list(filename, data) 
  
})

inadequate.state = lapply(inadequate.prenatal.clean, `[[`, 2)  

for (data in inadequate.state) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'prenatal.care',
    source = 'march.of.dimes',
    dimension.values = list(),
    url = 'https://www.marchofdimes.org/peristats/data?top=5&lev=1&stop=34&reg=99&sreg=01&obj=8&slev=4',
    details = 'March of Dimes Peristats')
}


# National Level Total, Age, Race ----------------------------------------------------

DATA.DIR.PRENATAL.NATIONAL="../../data_raw/syphilis.manager/inadequate.prenatal/national"

national_prenatal_files <- Sys.glob(paste0(DATA.DIR.PRENATAL.NATIONAL, '/*.csv'))


inadequate.prenatal.national <- lapply(national_prenatal_files, function(x){
  list(filename=x, data=read.csv(x, header=TRUE))
})

national.inadequate.prenatal.clean = lapply(inadequate.prenatal.national, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$year = data$Year
  data$location = "US"
  data$outcome = 'inadequate.prenatal.care'

  data$value = as.numeric(data$'US..Percent.'/100) #change to proportion
  data$year = as.character(data$year)
  
  data= as.data.frame(data)
  
  list(filename, data) 
  
})

inadequate.national = lapply(national.inadequate.prenatal.clean, `[[`, 2)

for (data in inadequate.national) {

  data.manager$put.long.form(
    data = data,
    ontology.name = 'prenatal.care',
    source = 'march.of.dimes',
    dimension.values = list(),
    url = 'https://www.marchofdimes.org/peristats/data?top=5&lev=1&stop=34&reg=99&sreg=01&obj=8&slev=4',
    details = 'March of Dimes Peristats')
}
