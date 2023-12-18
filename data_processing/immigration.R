library(readxl)
################################################################################
##read in immigration/emigration##
################################################################################

DATA.DIR.MOVEMENT="../../data_raw/movement"

movement_files <- Sys.glob(paste0(DATA.DIR.MOVEMENT, '/*.xlsx'))

data.list.move <- lapply(movement_files, function(x){
  skip=1
  list(filename=x, data=read_excel(x, sheet= 1, skip=skip))
})

################################################################################
#Clean
#outcome=immigration
#outcome=emigration
################################################################################

data.list.move.clean = lapply(data.list.move, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$year = "2011-2015"
  data$sex = if_else(data$`Sex Code2` == "01", "male", "female")
  
  if(grepl("immigration", filename)) {
   data$location = data$`Current Residence Metro Code1`
   data$location = paste("C", data$location, sep=".")
   data$outcome = "immigration"
   
     data <-data %>%
     mutate(value = (`Movers from Different Metropolitan Statistical Area3 Estimate` + `Movers from Elsewhere in the U.S. or Puerto Rico Estimate` + `Movers from Abroad4 Estimate`))%>%
     select(outcome, year, location, sex, value)
    
    data<- data[!duplicated(data), ]
    }
  
  #Location conditional formatting
  if(grepl("emigration", filename)) {
    data$location = data$`Residence 1 Year Ago Metro Code1`
    data$location = paste("C", data$location, sep=".")
    data$outcome = "emigration"
    
      data <-data %>%
        mutate(value = (`Movers to Different Metropolitan Statistical Area3 Estimate` + `Movers to Elsewhere in the U.S. or Puerto Rico Estimate` + `Movers in Metro-to-Metro Flow Estimate`))%>%
        select(outcome, year, location, sex, value)
  }
  
  #remove invalid locations?
  #data$location_test = locations::get.location.code(data$location, "CBSA")
  #data = subset(data, data$location_test != "FALSE")
  
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
