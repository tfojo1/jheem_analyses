#CDC Wonder Demographic data- single year population values

library(readr)
library(readxl)
library(tidyverse)
################################################################################
              ###CDC Wonder Single Year Population Files###
################################################################################
DATA.DIR.CDC.WONDER="../../data_raw/cdc_wonder"

cdc_wonder_files <- list.files(DATA.DIR.CDC.WONDER, pattern = ".txt", full.names = "TRUE")


data.list.cdc.wonder.og <- lapply(cdc_wonder_files, function(x) {
  list(filename=x, data=read_delim(x,  delim = "\t", escape_double = FALSE,
                                   trim_ws = TRUE))
})


data.list.cdc.wonder <- lapply(cdc_wonder_files, function(x) {
  list(filename=x, data=read_delim(x,  delim = "\t", escape_double = FALSE, 
                                   col_types = cols(Population = col_character(), 
                                                    Notes = col_character(), Age = col_character(), 
                                                    `Age Code` = col_character(), Race = col_character(), 
                                                    `Race Code` = col_character(), Ethnicity = col_character(), 
                                                    `Ethnicity Code` = col_character(), 
                                                    County = col_character(), `County Code` = col_character(), 
                                                    `Yearly July 1st Estimates` = col_character(), 
                                                    `Yearly July 1st Estimates Code` = col_character()), 
                                   trim_ws = TRUE))
})


################################################################################
       ###Clean CDC Wonder Single Year Age Group Demographic Data###
################################################################################
data.list.cdc.wonder.clean = lapply(data.list.cdc.wonder  , function(file){
  
  data=file[["data"]] #apply the function to the data element#
  filename = file[["filename"]] #apply the function to the filename element#
  
  
  data$year = as.character(data$`Yearly July 1st Estimates`)
  data$location= data$`County Code`
  data$sex = ifelse(grepl("female", filename), "female", "male")
  
  data= subset(data, data$Population != "NA") #This removes the footers#
  
  data$outcome= "population"
  
#Figure out how to code poplation = missing then convert to a number#
  data$value = as.numeric(as.character(data$Population))
  
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
##Need to update the source here for CDC.  Do you need to add CDC Wonder as source for census manager?

county_demos = lapply(data.list.cdc.wonder.clean , `[[`, 2)

for (data in county_demos) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census.cdc.wonder',
    source = 'cdc_wonder',
    dimension.values = list(),
    url = 'https://wonder.cdc.gov/',
    details = 'CDC Wonder')
}
