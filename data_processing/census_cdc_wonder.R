#CDC Wonder Demographic data- single year population values

# library(readr)
# library(readxl)
# library(tidyverse)
################################################################################
              ###CDC Wonder Single Year Population Files###
################################################################################
DATA.DIR.CDC.WONDER="../../data_raw/cdc_wonder"

cdc_wonder_files <- list.files(DATA.DIR.CDC.WONDER, pattern = ".txt", full.names = "TRUE")

data.list.cdc.wonder <- lapply(cdc_wonder_files, function(x) {
  list(filename=x, data=read_delim(x,  delim = "\t", escape_double = FALSE, 
                                   col_types = cols(Notes = col_skip(), 
                                                    `Yearly July 1st Estimates` = col_character(), 
                                                    `Yearly July 1st Estimates Code` = col_character(), 
                                                    Population = col_character()), trim_ws = TRUE))
})

################################################################################
       ###Clean CDC Wonder Single Year Age Group Demographic Data###
################################################################################
data.list.cdc.wonder.clean = lapply(data.list.cdc.wonder  , function(file){
  
  data=file[["data"]] #apply the function to the data element#
  filename = file[["filename"]] #apply the function to the filename element#
  
  data$year = as.character(data$`Yearly July 1st Estimates`)
  data$location= as.character(data$`County Code`)
  data$sex = ifelse(grepl("female", filename), "female", "male")
  
  data$outcome= "population"

  data$value = ifelse(data$Population == "Missing", NA, data$Population) #Replacing 'missing' with NA
  data$value = as.numeric(data$value)
  
  data$race = data$Race
  data$ethnicity= data$Ethnicity
  data$age= data$Age
    
  data <- data %>%
    select(outcome, year, location, age, race, ethnicity, sex, value)
  
  data = as.data.frame(data)
  list(filename, data)  
  
})

################################################################################
                   ###Put into Census Manager###
################################################################################

county_single_year_age = lapply(data.list.cdc.wonder.clean, `[[`, 2)

for (data in county_single_year_age) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census.cdc.wonder.population',
    source = 'cdc_wonder',
    dimension.values = list(),
    url = 'https://wonder.cdc.gov/',
    details = 'CDC Wonder')
}

# Creating Dummy Counties -------------------------------------------------

#We noticed on 5-15-24 there was an issue with missing counties in dimnames
#This is because there are counties that were historically parts of states
#And then were removed as counties at some point.  We noticed this for FL, MO, and VA.
#we decided to put them here as NA so then we won't run into the dimnames
#related error

# need 9 data frames -----------------------------------------------------

#year_location
# dummy.counties.one<- data.frame(
#   outcome = c('population'),
#   year = c('2018'),
#   location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
#   value = as.numeric(NA)
# )
# #year_location_age
# dummy.counties.two <- data.frame(
#   outcome = c('population'),
#   year = c('2018'),
#   location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
#   value = as.numeric(NA),
#   age = c('< 1 year')
# )
# #year_location_race
# dummy.counties.three <- data.frame(
#   outcome = c('population'),
#   year = c('2018'),
#   location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
#   value = as.numeric(NA),
#   race = c('American Indian or Alaska Native')
# )
# #year_location_ethnicity
# dummy.counties.four  <- data.frame(
#   outcome = c('population'),
#   year = c('2018'),
#   location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
#   value = as.numeric(NA),
#   ethnicity = c('Hispanic or Latino')
# )
# 
# #year_location_race_ethnicity
# dummy.counties.five  <- data.frame(
#   outcome = c('population'),
#   year = c('2018'),
#   location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
#   value = as.numeric(NA),
#   race = c('American Indian or Alaska Native'),
#   ethnicity = c('Hispanic or Latino')
# )
# #year_location_sex
# dummy.counties.six <- data.frame(
#   outcome = c('population'),
#   year = c('2018'),
#   location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
#   value = as.numeric(NA),
#   sex = c('male')
# )
# #year_location_age_race_ethnicity
# dummy.counties.seven  <- data.frame(
#   outcome = c('population'),
#   year = c('2018'),
#   location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
#   value = as.numeric(NA),
#   race = c('American Indian or Alaska Native'),
#   ethnicity = c('Hispanic or Latino'),
#   age = c('< 1 year')
# )
# #year_location_age_sex
# dummy.counties.eight  <- data.frame(
#   outcome = c('population'),
#   year = c('2018'),
#   location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
#   value = as.numeric(NA),
#   sex = c('male'),
#   age = c('< 1 year')
# )
# #year_location_race_ethnicity_sex
# dummy.counties.nine  <- data.frame(
#   outcome = c('population'),
#   year = c('2018'),
#   location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
#   value = as.numeric(NA),
#   sex = c('male'),
#   race = c('American Indian or Alaska Native'),
#   ethnicity = c('Hispanic or Latino')
# )
# 
# #Put into list and put into data manage
# 
# dummy.data.all = list (dummy.counties.one, 
#                        dummy.counties.two,
#                        dummy.counties.three,
#                        dummy.counties.four,
#                        dummy.counties.five,
#                        dummy.counties.six,
#                        dummy.counties.seven,
#                        dummy.counties.eight,
#                        dummy.counties.nine)
# 
# for (data in dummy.data.all) {
#   
# census.manager$put.long.form(
#   data = data,
#   ontology.name = 'census.cdc.wonder.population',
#   source = 'cdc_wonder',
#   dimension.values = list(),
#   url ='https://wonder.cdc.gov/',
#   details = 'CDC Wonder',
#   allow.na.to.overwrite = T )
# }


# Update for 5-17, We only need 1 df --------------------------------------
#If this works you can remove the commented out section above

#year_location_age_race_ethnicity
dummy.counties.cdc.wonder  <- data.frame(
  outcome = c('population'),
  year = c('2018'),
  location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
  value = as.numeric(NA),
  race = c('American Indian or Alaska Native'),
  ethnicity = c('Hispanic or Latino'),
  age = c('< 1 year'),
  sex = c('male'))
  
  census.manager$put.long.form(
    data = dummy.counties.cdc.wonder,
    ontology.name = 'census.cdc.wonder.population',
    source = 'cdc_wonder',
    dimension.values = list(),
    url ='https://wonder.cdc.gov/',
    details = 'CDC Wonder',
    allow.na.to.overwrite = T )
  

