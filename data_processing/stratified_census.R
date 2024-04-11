library(tidyverse)
library(stringr)
library(tidyr)

#Ontology = stratified.census

#For race I'm going to use: white alone, black or african american alone, 
#american indian or alaska native alone, asian alone, 
#native hawaiian or pacific islander alone

#For ethnicity: total NH male + NH female (NH_MALE, NH_FEMALE); 
#then Hispanic male + Hispanic female (H-MALE, H_FEMALE)


# Read in Stratified Census Data ------------------------------------------

DATA.DIR.CENSUS.STRATIFIED="../../data_raw/population/stratified.census.data.20.22"

stratified_files <- Sys.glob(paste0(DATA.DIR.CENSUS.STRATIFIED, '/*.csv'))

#creating a list with sublists of filename, data#
stratified.census.data.20.22 <- lapply(stratified_files, function(x){
  list(filename=x, data=read.csv(x, header=TRUE))
})

# Mappings ----------------------------------------------------------------

year.mapping.20.22 = c('2' = '2020',
                       '3' = '2021',
                       '4' = '2022')

race.mapping.10.22 = c('WA' = 'White alone',
                       'BA' = 'Black alone',
                       'IA'= 'American Indian and Alaska Native alone',
                       'AA' = 'Asian alone',
                       'NA' = 'Native Hawaiian and Other Pacific Islander alone',
                       
                       'TOM' = 'Two or More Races',
                       
                       'WAC' = 'White alone or in combination',
                       'BAC' = 'Black or African American alone or in combination',
                       'IAC' = 'American Indian and Alaska Native alone or in combination',
                       'AAC' = 'Asian alone or in combination',
                       'NAC' = 'Native Hawaiian and Other Pacific Islander alone or in combination',
                       
                       'NH' = 'Not Hispanic',
                       
                       'NHWA' = 'Not Hispanic, White alone',
                       'NHBA' = 'Not Hispanic, Black or African American alone',
                       'NHIA' = 'Not Hispanic, American Indian and Alaska Native alone',
                       'NHAA' = 'Not Hispanic, Asian alone',
                       'NHNA' = 'Not Hispanic, Native Hawaiian and Other Pacific Islander alone',
                       
                       'NHTOM' = 'Not Hispanic, Two or More Races',
                       
                       'NHWAC' = 'Not Hispanic, White alone or in combination',
                       'NHBAC' = 'Not Hispanic, Black or African American alone or in combination',
                       'NHIAC' = 'Not Hispanic, American Indian and Alaska Native alone or in combination',
                       'NHAAC' = 'Not Hispanic, Asian alone or in combination',
                       'NHNAC' = 'Not Hispanic, Native Hawaiian and Other Pacific Islander alone or in combination',
                       
                       'H' = 'Hispanic',
                       
                       'HWA' = 'Hispanic, White alone',
                       'HBA' = 'Hispanic, Black or African American alone',
                       'HIA' = 'Hispanic, American Indian and Alaska Native alone',
                       'HAA' = 'Hispanic, Asian alone',
                       'HNA' = 'Hispanic, Native Hawaiian and Other Pacific Islander alone',
                       
                       'HTOM' = 'Hispanic, Two or More Races',
                       
                       'HWAC' = 'Hispanic, White alone or in combination',
                       'HBAC' = 'Hispanic, Black or African American alone or in combination',
                       'HIAC' = 'Hispanic, American Indian and Alaska Native alone or in combination',
                       'HAAC' = 'Hispanic, Asian alone or in combination',
                       'HNAC' = 'Hispanic, Native Hawaiian and Other Pacific Islander alone or in combination')

# Clean Demographic Data --------------------------------------------------

stratified.data.clean = lapply(stratified.census.data.20.22, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  #Fix Location
  data$state_code_clean= str_pad(data$STATE, width=2, side="left", pad="0")
  data$county_code_clean= str_pad(data$COUNTY, width=3, side="left", pad="0")
  data$FIPS= paste(data$state_code_clean, data$county_code_clean, sep="")
  data$location = data$FIPS

  #Add Year
  if(grepl("20.22", filename)) {
    data= subset(data, data$YEAR != 1)  #REMOVE CENSUS AND USE POP ESTIMATE#
    data$year = as.character(data$YEAR)
    data$year = year.mapping.20.22[data$year]
  }
  data$outcome = "population"
  data= as.data.frame(data)
  list(filename, data) 
})


# Format for Race ---------------------------------------------------------
census.by.race = lapply(stratified.data.clean, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
#Format Race Totals
if(grepl("race", filename)) {
  data= subset(data, data$AGEGRP == "0") #Filter for total age group so you can put total values for race
  
  data<- data %>%
    select(year, location, outcome, WA_MALE, WA_FEMALE, BA_MALE, BA_FEMALE, IA_MALE, IA_FEMALE, AA_MALE, AA_FEMALE,
          NA_MALE, NA_FEMALE)%>%
    pivot_longer(cols=c(one_of("WA_MALE", "WA_FEMALE", "BA_MALE", "BA_FEMALE", "IA_MALE", "IA_FEMALE", "AA_MALE", "AA_FEMALE",
                             "NA_MALE", "NA_FEMALE")), 
               names_to = c("race", "sex"),
               names_sep = "_",
               values_to = "value")
}

  data= as.data.frame(data)
  list(filename, data) 
})

# Format for Ethnicity ----------------------------------------------------





