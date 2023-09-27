# library(jheem2)
# library(readxl)
# library(tidyverse)

################################################################################
                  ###Read in CDC HIV Test Data###
################################################################################
DATA.DIR.TESTS="../../data_raw/tests"

test_files <- Sys.glob(paste0(DATA.DIR.TESTS, '/*.xlsx'))
                            
data.list.tests <- lapply(test_files, function(x){
  skip=1
  list(filename=x, data=read_excel(x, skip=skip))
})
################################################################################
                    ###Clean HIV Test Data###
################################################################################
data.list.tests.clean = lapply(data.list.tests, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]] 
  
  data= subset(data, data$`CDC Funded Jurisdiction` != "Total")
  data= subset(data, data$`CDC Funded Jurisdiction` != "U.S. Virgin Islands")
  
  data$location = state.abb[match(data$`CDC Funded Jurisdiction`, state.name)]

  data$location =ifelse(data$`CDC Funded Jurisdiction` == "District of Columbia", "DC", data$location) 
  data$location =ifelse(data$`CDC Funded Jurisdiction` == "Puerto Rico", "PR", data$location) 
  
  data$outcome = "hiv.tests"

  data$value = as.numeric(data$`Number of HIV tests conducted`)
  
  if(grepl("2018", filename)) {
    data$year = "2018"
  }
  if(grepl("2019", filename)) {
    data$year = "2019"
  }
  if(grepl("2020", filename)) {
      data$year = "2020"
  }
  if(grepl("2021", filename)) {
    data$year = "2021"
  }
  
  data <- data %>%
    select( year, location, value, outcome)
  
  data= as.data.frame(data)
  
  list(filename, data) 
})
################################################################################
              ###Clean HIV Test Positivity Data###
################################################################################
data.list.positives.clean = lapply(data.list.tests, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]] 
  
  data= subset(data, data$`CDC Funded Jurisdiction` != "Total")
  data= subset(data, data$`CDC Funded Jurisdiction` != "U.S. Virgin Islands")
  
  data$location = state.abb[match(data$`CDC Funded Jurisdiction`, state.name)]
  
  data$location =ifelse(data$`CDC Funded Jurisdiction` == "District of Columbia", "DC", data$location) 
  data$location =ifelse(data$`CDC Funded Jurisdiction` == "Puerto Rico", "PR", data$location) 
  
  if(grepl("2018", filename)) {
    data$year = "2018"
  }
  if(grepl("2019", filename)) {
    data$year = "2019"
  }
  if(grepl("2020", filename)) {
    data$year = "2020"
  }
  if(grepl("2021", filename)) {
    data$year = "2021"
  }

  data$outcome = "hiv.test.positivity"
  data$value = as.numeric(data$`Number of persons newly diagnosed with HIV`)
  
  data <- data %>%
    select( year, location, value, outcome)
  
  data= as.data.frame(data)
  
  list(filename, data) 
})
################################################################################
                    ###Add to Data Manager###
################################################################################
test_data = lapply(data.list.tests.clean, `[[`, 2)

for (data in test_data) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc',
    source = 'cdc',
    dimension.values = list(),
    url = 'https://www.cdc.gov/hiv/library/reports/testing/2021/index.html#:~:text=Among%20the%201%2C736%2C850%20CDC%2Dfunded,demographic%20characteristics%20and%20population%20groups.',
    details = 'CDC Annual HIV Testing Report')
}

positives_data = lapply(data.list.positives.clean, `[[`, 2)

for (data in positives_data) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc',
    source = 'cdc',
    dimension.values = list(),
    url = 'https://www.cdc.gov/hiv/library/reports/testing/2021/index.html#:~:text=Among%20the%201%2C736%2C850%20CDC%2Dfunded,demographic%20characteristics%20and%20population%20groups.',
    details = 'CDC Annual HIV Testing Report')
}
####################################
          ##Add in Cities##
####################################
city_tests <- lapply(test_files, function(x){
  skip=1
  list(filename=x, data=read_excel(x, sheet =2, skip=skip))
})
#################################
##Clean City Data- TEST COUNTS##
################################
data.list.city.test.clean = lapply(city_tests, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]] 

  
  data = subset(data, data$`CDC Funded Jurisdiction` == "Los Angeles" | data$`CDC Funded Jurisdiction` == "San Francisco" |
                  data$`CDC Funded Jurisdiction` == "Chicago" | data$`CDC Funded Jurisdiction` == "Baltimore" | 
                  data$`CDC Funded Jurisdiction` == "New York City" | data$`CDC Funded Jurisdiction` == "Philadelphia" |
                  data$`CDC Funded Jurisdiction` == "Houston"  )
  
  data$msa = ifelse (data$`CDC Funded Jurisdiction` == "Los Angeles", "Los Angeles-Long Beach-Anaheim, CA",
                     ifelse(data$`CDC Funded Jurisdiction` == "San Francisco",  "San Francisco-Oakland-Berkeley, CA",
                            ifelse(data$`CDC Funded Jurisdiction` == "Chicago", "Chicago-Naperville-Elgin, IL-IN-WI",
                                   ifelse (data$`CDC Funded Jurisdiction` == "Baltimore", "Baltimore-Columbia-Towson, MD",
                                   ifelse(data$`CDC Funded Jurisdiction` == "New York City", "New York-Newark-Jersey City, NY-NJ-PA",
                                          ifelse(data$`CDC Funded Jurisdiction` == "Philadelphia", "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD",
                                               ifelse(data$`CDC Funded Jurisdiction` == "Houston", "Houston-The Woodlands-Sugar Land, TX", "")))))))
  
  data$location =locations::get.cbsa.for.msa.name(data$msa)
  
  data$outcome = "hiv.tests"
  
  data$value = as.numeric(data$`Number of HIV tests conducted`)
  
  if(grepl("2018", filename)) {
    data$year = "2018"
  }
  if(grepl("2019", filename)) {
    data$year = "2019"
  }
  if(grepl("2020", filename)) {
    data$year = "2020"
  }
  if(grepl("2021", filename)) {
    data$year = "2021"
  }
  
   data <- data %>%
     select( year, location, value, outcome)
  
  data= as.data.frame(data)
  
  list(filename, data) 
})
#################################
##Clean City Data- POSITIVITY##
################################
data.list.city.positivity.clean = lapply(city_tests, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]] 
  
  
  data = subset(data, data$`CDC Funded Jurisdiction` == "Los Angeles" | data$`CDC Funded Jurisdiction` == "San Francisco" |
                  data$`CDC Funded Jurisdiction` == "Chicago" | data$`CDC Funded Jurisdiction` == "Baltimore" | 
                  data$`CDC Funded Jurisdiction` == "New York City" | data$`CDC Funded Jurisdiction` == "Philadelphia" |
                  data$`CDC Funded Jurisdiction` == "Houston"  )
  
  data$msa = ifelse (data$`CDC Funded Jurisdiction` == "Los Angeles", "Los Angeles-Long Beach-Anaheim, CA",
                     ifelse(data$`CDC Funded Jurisdiction` == "San Francisco",  "San Francisco-Oakland-Berkeley, CA",
                            ifelse(data$`CDC Funded Jurisdiction` == "Chicago", "Chicago-Naperville-Elgin, IL-IN-WI",
                                   ifelse (data$`CDC Funded Jurisdiction` == "Baltimore", "Baltimore-Columbia-Towson, MD",
                                           ifelse(data$`CDC Funded Jurisdiction` == "New York City", "New York-Newark-Jersey City, NY-NJ-PA",
                                                  ifelse(data$`CDC Funded Jurisdiction` == "Philadelphia", "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD",
                                                         ifelse(data$`CDC Funded Jurisdiction` == "Houston", "Houston-The Woodlands-Sugar Land, TX", "")))))))
  
  data$location =locations::get.cbsa.for.msa.name(data$msa)
  
  data$outcome = "hiv.test.positivity"
  
  data$value = as.numeric(data$`Number of persons newly diagnosed with HIV`)
  
  if(grepl("2018", filename)) {
    data$year = "2018"
  }
  if(grepl("2019", filename)) {
    data$year = "2019"
  }
  if(grepl("2020", filename)) {
    data$year = "2020"
  }
  if(grepl("2021", filename)) {
    data$year = "2021"
  }
  
  data <- data %>%
    select( year, location, value, outcome)
  
  data= as.data.frame(data)
  
  list(filename, data) 
})
#################################
##Add City Data into Manager##
################################
city_test_data = lapply(data.list.city.test.clean, `[[`, 2)

for (data in city_test_data) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc',
    source = 'cdc',
    dimension.values = list(),
    url = 'https://www.cdc.gov/hiv/library/reports/testing/2021/index.html#:~:text=Among%20the%201%2C736%2C850%20CDC%2Dfunded,demographic%20characteristics%20and%20population%20groups.',
    details = 'CDC Annual HIV Testing Report')
}

city_pos_data = lapply(data.list.city.positivity.clean, `[[`, 2)

for (data in city_pos_data) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc',
    source = 'cdc',
    dimension.values = list(),
    url = 'https://www.cdc.gov/hiv/library/reports/testing/2021/index.html#:~:text=Among%20the%201%2C736%2C850%20CDC%2Dfunded,demographic%20characteristics%20and%20population%20groups.',
    details = 'CDC Annual HIV Testing Report')
}
