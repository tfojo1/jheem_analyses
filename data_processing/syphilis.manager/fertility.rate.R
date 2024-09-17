
options(error=NULL)

DATA.DIR.FERTILITY="../../data_raw/syphilis.manager/fertility"

fertility.files <- list.files(DATA.DIR.FERTILITY, pattern = ".txt", full.names = "TRUE")
data.list.fertility <- lapply(fertility.files, function(x) {
  list(filename=x, data=read.delim2(x, colClasses = c('County.Code' = 'character')))
 
})


# Clean Fertility Data ----------------------------------------------------

fertility.clean = lapply(data.list.fertility, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  #Universal Cleaning#
  data$outcome = 'fertility.rate'
  data$year = as.character(data$Year)
  data$value = as.numeric(data$'Fertility.Rate') #Is this the right format?#
  
  data <- data %>%
    select(-Notes, -Year.Code)
  
  #Location conditionals#
  if(grepl("county", filename)) {
    data$location = data$'County.Code'
  }
  if(grepl("national", filename)) {
      data$location = "US"
  }
   
   #Demographic conditionals#
   if(grepl("age", filename)) {
      data$age = data$'Age.of.Mother.9'
   }
  if(grepl("single.race", filename)) {
     data$race = data$'Mother.s.Single.Race'
  }
  if(grepl("bridged.race", filename)) {
     data$race = data$'Mother.s.Bridged.Race'
  }
  if(grepl("hispanic", filename)) {
    data$ethnicity = data$'Mother.s.Hispanic.Origin'
  }
  
  list(filename, data) 
  
})
# Put female.population as denominator for fertility. ---------------------

female.population.clean = lapply(fertility.clean, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  data <- data %>%
    select(-outcome, -value)
  
  data$outcome = 'female.population'
  
  data$value = data$'Female.Population' 
  
  list(filename, data) 
  
})


# Put into Syphilis Data Manager ------------------------------------------
#I think you need to add a source or a parent source and an ontology#
#You need to remove races that are not part of the ontology#
