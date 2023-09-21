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
    ontology.name = 'census.cdc.wonder',
    source = 'cdc_wonder',
    dimension.values = list(),
    url = 'https://wonder.cdc.gov/',
    details = 'CDC Wonder')
}
