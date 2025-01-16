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

  data$outcome = 'adequate.prenatal.care'
  data$location = locations::get.location.code(data$state, "state")
  data$value = as.numeric(data$value/100) #change to proportion
  
  list(filename, data) 
  
})
