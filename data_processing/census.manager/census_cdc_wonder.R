#CDC Wonder Demographic data- single year population values

# library(readr)
# library(readxl)
# library(tidyverse)
################################################################################
              ###CDC Wonder Single Year Population Files###
################################################################################
DATA.DIR.CDC.WONDER="Q:/data_raw/cdc_wonder"

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
  
  data$race = tolower(data$Race)
  data$ethnicity= tolower(data$Ethnicity)
  data$age= data$Age
    
  data <- data %>%
    select(outcome, year, location, age, race, ethnicity, sex, value)%>%
    filter(year != "2020")  #Removign 2020 data now that we have census data for 2020 by single year ages
  
  data <- data %>% #Fixing Dade and St Geneive Counties
    mutate(location = ifelse(location == "12025", "12086", location))%>%
    mutate(location = ifelse(location == "29193", "29186", location))
  
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

#year_location_age_race_ethnicity
dummy.counties.cdc.wonder  <- data.frame(
  outcome = c('population'),
  year = c('2018'),
  location = c('51560', '51780', '51123'), #These are the counties that have changed#
  value = as.numeric(NA),
  race = c('american indian or alaska native'),
  ethnicity = c('hispanic or latino'),
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
  

