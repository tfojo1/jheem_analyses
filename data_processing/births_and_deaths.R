#Pull birth and death data with demographics from CDC Wonder##
##THIS IS A PART OF THE CENSUS MANAGER##

# library(readr)
# library(readxl)
# library(tidyverse)
################################################################################
                        ###BIRTH DATA###
################################################################################
DATA.DIR.BIRTH="../../data_raw/births_deaths/births"

birth_files <- list.files(DATA.DIR.BIRTH, pattern = ".txt", full.names = "TRUE")

data.list.births <- lapply(birth_files, function(x) {
  list(filename=x, data=read_delim(x, delim = "\t", escape_double = FALSE, 
                                   col_types = cols(Notes = col_skip()), 
                                   trim_ws = TRUE))
})
################################################################################
                      ###CLEAN BIRTH DATA###
################################################################################
data.list.births.clean = lapply(data.list.births, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]]
  
  data$year = as.character(data$Year)
  data$outcome= "births"
  data$location= data$`County Code`
  data$race = data$`Mother's Bridged Race`
  data$ethnicity = data$`Mother's Hispanic Origin`
  
  ##For right now I'm going to remove all the types of unk race/eth##
  #data = subset(data, data$race != "Not Reported")
  #data = subset(data, !is.na(data$race))
  #data = subset(data, data$ethnicity != "Unknown or Not Stated")
  #data = subset(data, !is.na(data$ethnicity))
  
 ##For right now also going to remove births = suppressed or NA##
  #data = subset(data, data$Births != "Suppressed")
  #data = subset(data, !is.na(data$Births))
  #data$value = as.numeric(data$Births)
  
  # data <- data %>%
  #   select(outcome, year, location, race, ethnicity, value)
  
  data = as.data.frame(data)
  list(filename, data)  
  
})

################################################################################
                  ###Put BIRTHS into CENSUS MANAGER###
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


################################################################################
###outcoe is Metro Deaths##
################################################################################