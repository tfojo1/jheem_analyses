#This code is for the single year age data that Todd has for 2005-2017


################################################################################
              ###Read in SAS Census Files for 2005-2017###
################################################################################
library(haven)

DATA.DIR.CENSUS.SAS="../../data_raw/census_sas"

cdc_sas_files <- list.files(DATA.DIR.CENSUS.SAS, pattern = ".sas7bdat", full.names = "TRUE")

census_sas_data_list <- lapply(cdc_sas_files, function(x) {
  list(filename=x, data=read_sas(x))
})

################################################################################
                ###Clean SAS Census Files for 2005-2017###
################################################################################

data.list.census.sas = lapply(census_sas_data_list, function(file){
  
  ###update this whole section##
  
  # data=file[["data"]] #apply the function to the data element#
  # filename = file[["filename"]] #apply the function to the filename element#
  # 
  # 
  # data$year = as.character(data$`Yearly July 1st Estimates`)
  # data$location= data$`County Code`
  # data$sex = ifelse(grepl("female", filename), "female", "male")
  # 
  # data= subset(data, data$Population != "NA") #This removes the footers#
  # 
  # data$outcome= "population"
  # 
  # #Figure out how to code poplation = missing then convert to a number#
  # data$value = as.numeric(as.character(data$Population))
  # 
  # data$race = data$Race
  # data$ethnicity= data$Ethnicity
  # data$age= data$Age
  # 
  # data <- data %>%
  #   select(outcome, year, location, age, race, ethnicity, sex, value)
  # 
  # 
  # data = as.data.frame(data)
  list(filename, data)  
  
})



################################################################################
                  ###Put into Census Manager###
################################################################################


####UPDATE THIS SECTION####


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
