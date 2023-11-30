#Pull birth and death data with demographics from CDC Wonder
##THIS IS A PART OF THE CENSUS MANAGER

# library(readr)
# library(readxl)
# library(tidyverse)
################################################################################
                ###CDC Wonder Birth and Death data###
################################################################################
DATA.DIR.BIRTH.DEATH="../../data_raw/births_deaths"

birth_death_files <- list.files(DATA.DIR.BIRTH.DEATH, pattern = ".txt", full.names = "TRUE")

data.list.birth.death <- lapply(birth_death_files, function(x) {
  list(filename=x, data=read_delim(x,  delim = "\t", escape_double = FALSE, 
                                   col_types = cols(Notes = col_skip(), 
                                                    `Yearly July 1st Estimates` = col_character(), 
                                                    `Yearly July 1st Estimates Code` = col_character(), 
                                                    Population = col_character()), trim_ws = TRUE))
})

################################################################################
        ###Clean Birth and Death Data###
################################################################################
data.list.births.clean = lapply(data.list.birth.death, function(file){
  
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
            ###Put births and deaths into CENSUS MANAGER###
################################################################################
county_single_year_age = lapply(data.list.cdc.wonder.clean, `[[`, 2)

for (data in county_single_year_age) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census.cdc.wonder',
    source = 'cdc_wonder',
    dimension.values = list(),
    url = 'https://wonder.cdc.gov/',
    details = 'CDC Wonder')
}