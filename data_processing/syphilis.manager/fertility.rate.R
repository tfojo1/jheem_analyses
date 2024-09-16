DATA.DIR.FERTILITY="../../data_raw/syphilis.manager/fertility"

fertility.files <- Sys.glob(paste0(DATA.DIR.FERTILITY, '/*.csv'))
data.list.fertility <- lapply(fertility.files, function(x){
  skip=8
  list(filename=x, data=read.csv(x, skip=skip, header=TRUE, colClasses=c(FIPS="character")))
})


# Clean Fertility Data ----------------------------------------------------

fertility.clean = lapply(data.list.fertility, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  #Universal Cleaning#
  data$outcome = 'fertility.rate'
  data$year = as.character(data$Year)
  
  data$value = data$'Fertility Rate per 1,000' #CHECK HOW THIS SHOULD BE PUT- DIVIDE BY 1000?#
  
  #Figure out location and demographics#
  
  # #Location conditionals#
  # if(grepl("state", filename)) {
  #   names(state.abb) <- state.name 
  #   data$Geography= gsub('[^[:alnum:] ]',"",data$Geography) #some states have ^ for preliminary data#
  #   names(data)[names(data)=='Geography'] = 'state'
  #   data$location =ifelse (data$state == "District of Columbia", "DC", state.abb[data$state]) 
  # }
  # 
  # if(grepl("county", filename)) {
  #   data$location = as.character(data$FIPS)
  # }
  # 
  # ##Demographic conditionals##
  # 
  # if(grepl("age", filename)) {
  #   data$age = age.mappings[data$Age.Group]
  # }
  # if(grepl("race", filename)) {
  #   names(data)[names(data)=='Race.Ethnicity'] = 'race'
  # }
  
  list(filename, data) 
  
})


# Put female.population as denominator for fertility. ---------------------

female.population.clean = lapply(data.list.fertility, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  #Universal Cleaning#
  data$outcome = 'female.population'
  data$year = as.character(data$Year)
  
  data$value = data$'Female Population' #Remove commas?
  
  list(filename, data) 
  
})


# Put into Syphilis Data Manager ------------------------------------------


