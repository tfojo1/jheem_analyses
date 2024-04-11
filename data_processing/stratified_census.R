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

stratified.census.years = c('2' = '2020',
                       '3' = '2021',
                       '4' = '2022')

stratified.census.race = c('WA' = 'White',
                       'BA' = 'Black',
                       'IA'= 'American Indian and Alaska Native',
                       'AA' = 'Asian',
                       'NA' = 'Native Hawaiian and Other Pacific Islander',
                       'H' = 'Hispanic',
                       'NH' = 'Not Hispanic')
stratified.census.age = c(
 'under 5 years' = '0-4 years', 
  '5 to 9' = '5-9 years', 
  '10 to 14' = '10-14 years', 
 '15 to 19' = '15-19 years', 
  '20 to 24' = '20-24 years', 
 '25 to 29' = '25-29 years', 
 '30 to 34' =  '30-34 years', 
 '35 to 39' = '35-39 years',
 '40 to 44' = '40-44 years', 
  '45 to 49' = '45-49 years', 
 '50 to 54' = '50-54 years', 
 '55 to 59' = '55-59 years', 
 '60 to 64'= '60-64 years', 
 '65 to 69' = '65-69 years', 
 '70 to 74' = '70-74 years', 
  '75 to 79' = '75-79 years', 
  '80 to 84' = '80-84 years', 
  '85 years and over' = '85+ years'
)

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
    data$year = stratified.census.years[data$year]
  }
  data$outcome = "population"
  data= as.data.frame(data)
  list(filename, data) 
})


# ADD IN ONE WAY STRATIFICATIONS ------------------------------------------

#Race ---------------------------------------------------------
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
               values_to = "count.by.sex")
  
  data <- data %>%
    group_by(year, location, race)%>%
    mutate(value = sum(count.by.sex))%>%
    select(-sex, -count.by.sex)
  
  data$race = stratified.census.race[data$race]
}

  data= as.data.frame(data)
  list(filename, data) 
})

#Ethnicity ----------------------------------------------------
census.by.ethnicity = lapply(stratified.data.clean, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  #Format Ethnicity Totals
  if(grepl("race", filename)) {
    data= subset(data, data$AGEGRP == "0") #Filter for total age group so you can put total values for race
    
    data<- data %>%
      select(year, location, outcome, H_MALE, H_FEMALE, NH_MALE, NH_FEMALE)%>%
      pivot_longer(cols=c(one_of("H_MALE", "H_FEMALE", "NH_MALE", "NH_FEMALE")), 
                   names_to = c("ethnicity", "sex"),
                   names_sep = "_",
                   values_to = "count.by.sex")
    
    data <- data %>%
      group_by(year, location, ethnicity)%>%
      mutate(value = sum(count.by.sex))
    #%>%
     # select(-sex, -count.by.sex)
    
    data$ethnicity = stratified.census.race[data$ethnicity]
  }
  
  data= as.data.frame(data)
  list(filename, data) 
})



#Age ----------------------------------------------------------

census.by.age = lapply(stratified.data.clean, function(file){
  
  data=file[[2]]
  filename = file[[1]]

  if(grepl("age", filename)) {

     
     data<- data %>%
       select(year, location, outcome, UNDER5_TOT, AGE59_TOT, AGE1014_TOT, AGE1519_TOT, AGE2024_TOT, AGE2529_TOT, 
              AGE3034_TOT, AGE3539_TOT, AGE4044_TOT, AGE4549_TOT, AGE5054_TOT, AGE5559_TOT, AGE6064_TOT, 
              AGE6569_TOT, AGE7074_TOT, AGE7579_TOT, AGE8084_TOT, AGE85PLUS_TOT)%>%
       pivot_longer(cols=c(one_of("UNDER5_TOT", "AGE59_TOT", "AGE1014_TOT", "AGE1519_TOT", "AGE2024_TOT", 
                                  "AGE2529_TOT", "AGE3034_TOT", "AGE3539_TOT", "AGE4044_TOT", "AGE4549_TOT",
                                   "AGE5054_TOT", "AGE5559_TOT", "AGE6064_TOT", "AGE6569_TOT", "AGE7074_TOT", 
                                  "AGE7579_TOT", "AGE8084_TOT", "AGE85PLUS_TOT")), 
                    names_to = c("age"),
                    values_to = "value")
     
     data$age = stratified.census.age[data$age]
  }
  
  data= as.data.frame(data)
  list(filename, data) 
})

#Sex ----------------------------------------------------------

census.by.sex = lapply(stratified.data.clean, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  
  if(grepl("sex", filename)) {

    data<- data %>%
      select(year, location, outcome, POPEST_MALE, POPEST_FEMALE)%>%
      pivot_longer(cols=c(one_of("POPEST_MALE", "POPEST_FEMALE")),
                   names_to = c("sex"),
                   values_to = "value")
  }
  
  data= as.data.frame(data)
  list(filename, data) 
})
