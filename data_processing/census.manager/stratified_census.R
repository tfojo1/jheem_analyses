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
stratified.census.age = c('UNDER5' = '0-4 years', 
                          'AGE59' = '5-9 years', 
                          'AGE1014' = '10-14 years', 
                          'AGE1519' = '15-19 years', 
                          'AGE2024' = '20-24 years', 
                          'AGE2529' = '25-29 years', 
                          'AGE3034' =  '30-34 years', 
                          'AGE3539' = '35-39 years',
                          'AGE4044' = '40-44 years', 
                          'AGE4549' = '45-49 years', 
                          'AGE5054' = '50-54 years', 
                          'AGE5559' = '55-59 years', 
                          'AGE6064'= '60-64 years', 
                          'AGE6569' = '65-69 years', 
                          'AGE7074' = '70-74 years', 
                          'AGE7579' = '75-79 years', 
                          'AGE8084' = '80-84 years', 
                          'AGE85PLUS' = '85+ years')
stratified.census.age.two = c('1' = '0-4 years', 
                          '2' = '5-9 years', 
                          '3' = '10-14 years', 
                          '4' = '15-19 years', 
                          '5' = '20-24 years', 
                          '6' = '25-29 years', 
                          '7' =  '30-34 years', 
                          '8' = '35-39 years',
                          '9' = '40-44 years', 
                          '10' = '45-49 years', 
                          '11' = '50-54 years', 
                          '12' = '55-59 years', 
                          '13'= '60-64 years', 
                          '14' = '65-69 years', 
                          '15' = '70-74 years', 
                          '16' = '75-79 years', 
                          '17' = '80-84 years', 
                          '18' = '85+ years')
stratified.census.sex = c('POPEST_FEM' = 'female',
                          'POPEST_MALE' = 'male')
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
  data<- data[!duplicated(data), ]
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
      mutate(value = sum(count.by.sex))%>%
     select(-sex, -count.by.sex)
    
    data$ethnicity = stratified.census.race[data$ethnicity]
    data<- data[!duplicated(data), ]
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
     
     data$age = str_remove(data$age, "_TOT")
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
      select(year, location, outcome, POPEST_MALE, POPEST_FEM)%>%
      pivot_longer(cols=c(one_of("POPEST_MALE", "POPEST_FEM")),
                   names_to = c("sex"),
                   values_to = "value")
    data$sex = stratified.census.sex[data$sex]
  }
  
  data= as.data.frame(data)
  list(filename, data) 
})


# Put into Census Manager -------------------------------------------------
# Put this into one list --------------------------------------------------
stratified.data.for.put = list(
  census.by.race = census.by.race[[1]][[2]],
  census.by.ethnicity = census.by.ethnicity[[1]][[2]],
  census.by.sex = census.by.sex[[2]][[2]],
  census.by.age = census.by.age[[2]][[2]]
)

for (data in stratified.data.for.put) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'stratified.census',
    source = 'census.population',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}



# Adding Additional Layers of Stratification ------------------------------

# Age + Sex ---------------------------------------------------------------

census.by.age.sex = lapply(stratified.data.clean, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  if(grepl("sex_age", filename)) {
    
    data<- data %>%
      select(year, location, outcome, UNDER5_FEM, AGE59_FEM, AGE1014_FEM, AGE1519_FEM, AGE2024_FEM, AGE2529_FEM, 
             AGE3034_FEM, AGE3539_FEM, AGE4044_FEM, AGE4549_FEM, AGE5054_FEM, AGE5559_FEM, AGE6064_FEM, 
             AGE6569_FEM, AGE7074_FEM, AGE7579_FEM, AGE8084_FEM, AGE85PLUS_FEM, UNDER5_MALE, AGE59_MALE, 
             AGE1014_MALE, AGE1519_MALE, AGE2024_MALE, AGE2529_MALE, AGE3034_MALE, AGE3539_MALE, AGE4044_MALE, 
             AGE4549_MALE, AGE5054_MALE, AGE5559_MALE, AGE6064_MALE, AGE6569_MALE, AGE7074_MALE, AGE7579_MALE, 
             AGE8084_MALE, AGE85PLUS_MALE)%>%
      pivot_longer(cols=c(one_of("UNDER5_FEM", "AGE59_FEM", "AGE1014_FEM", "AGE1519_FEM", "AGE2024_FEM", "AGE2529_FEM", 
                                 "AGE3034_FEM", "AGE3539_FEM", "AGE4044_FEM", "AGE4549_FEM", "AGE5054_FEM", "AGE5559_FEM", "AGE6064_FEM", 
                                 "AGE6569_FEM", "AGE7074_FEM", "AGE7579_FEM", "AGE8084_FEM", "AGE85PLUS_FEM", "UNDER5_MALE", "AGE59_MALE", 
                                 "AGE1014_MALE", "AGE1519_MALE", "AGE2024_MALE", "AGE2529_MALE", "AGE3034_MALE", "AGE3539_MALE", "AGE4044_MALE", 
                                 "AGE4549_MALE", "AGE5054_MALE", "AGE5559_MALE", "AGE6064_MALE", "AGE6569_MALE", "AGE7074_MALE", "AGE7579_MALE", 
                                 "AGE8084_MALE", "AGE85PLUS_MALE")), 
                   names_to = c("age", "sex"),
                   names_sep = "_",
                   values_to = "value")
    
    data$age = stratified.census.age[data$age]
    data$sex = ifelse(data$sex == "MALE", 'male', data$sex)
    data$sex = ifelse(data$sex == "FEM", 'female', data$sex)
  }
  
  data= as.data.frame(data)
  list(filename, data) 
})


# Sex+Race ----------------------------------------------------------------
census.by.race.sex = lapply(stratified.data.clean, function(file){
  
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
    
    data$race = stratified.census.race[data$race]
    data<- data[!duplicated(data), ]
    data$sex = tolower(data$sex)
  }
  
  data= as.data.frame(data)
  list(filename, data) 
})

# Sex + Race + Age --------------------------------------------------------
census.by.sex.race.age = lapply(stratified.data.clean, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  #Format Race Totals
  if(grepl("race", filename)) {
    
    data= subset(data, data$AGEGRP != "0") #Remove total age group
    
    data<- data %>%
      select(year, location, outcome, AGEGRP, WA_MALE, WA_FEMALE, BA_MALE, BA_FEMALE, IA_MALE, IA_FEMALE, AA_MALE, AA_FEMALE,
             NA_MALE, NA_FEMALE)%>%
      pivot_longer(cols=c(one_of("WA_MALE", "WA_FEMALE", "BA_MALE", "BA_FEMALE", "IA_MALE", "IA_FEMALE", "AA_MALE", "AA_FEMALE",
                                 "NA_MALE", "NA_FEMALE")), 
                   names_to = c("race", "sex"),
                   names_sep = "_",
                   values_to = "value")
    
    data$race = stratified.census.race[data$race]
    data$age = stratified.census.age.two[data$AGEGRP]
    data$sex = tolower(data$sex)
  }
  
  data= as.data.frame(data)
  list(filename, data) 
})


# Put More levels of stratification into manager --------------------------

two.and.three.way.strata = list(
  census.by.age.sex = census.by.age.sex[[2]][[2]],
  census.by.race.sex = census.by.race.sex[[1]][[2]],
  census.by.sex.race.age = census.by.sex.race.age[[1]][[2]])

for (data in two.and.three.way.strata) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'stratified.census',
    source = 'census.population',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}
