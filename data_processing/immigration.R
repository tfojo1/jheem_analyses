library(readxl)
################################################################################
##read in immigration/emigration##
################################################################################

DATA.DIR.IMMIGRATION="../../data_raw/immigration"

immigration_files <- Sys.glob(paste0(DATA.DIR.IMMIGRATION, '/*.xlsx'))

data.list.immigration <- lapply(immigration_files, function(x){
  skip=1
  list(filename=x, data=read_excel(x, sheet= 1, skip=skip))
})

################################################################################
#Clean
#outcome=immigration
#outcome=emigration
################################################################################

data.list.immigration.clean = lapply(data.list.immigration, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$year = "2011-2015"
  data$sex = if_else(data$`Sex Code2` == "01", "male", "female")
  data$location = paste("C", data$`_MMSA`, sep=".")
  
  #remove invalid locations?
  data$location_test = locations::get.location.code(data$location)
  data = subset(data, data$location_test != "FALSE")
  
  if(grepl("immigration", filename)) {
   data$location = data$`Current Residence Metro Code1`
  }
  
  #Location conditional formatting
  if(grepl("emigration", filename)) {
    data$location = data$`Residence 1 Year Ago Metro Code1`
  }
  
  data= as.data.frame(data)
  
  list(filename, data)
  
})

################################################################################
#put
################################################################################

immigration_data = lapply(ddata.list.immigration.clean , `[[`, 2)  

for (data in immigration_data) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census',
    dimension.values = list(),
    url = 'https://www.census.gov/data/tables/2015/demo/geographic-mobility/metro-to-metro-migration.html',
    details = 'Census Metro Area to Metro Area Migration Flows')
}
