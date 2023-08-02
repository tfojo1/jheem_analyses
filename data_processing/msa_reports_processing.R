###You'll need to add this as a source file into the main census processing code###
library(jheem2) #Remove this once you are sourcing the file#
library(tidyverse)
################################################################################
###Read in MSA Scraped PDF Files###
################################################################################
DATA.DIR.MSA.TOTAL="../../data_raw/msa_surveillance_reports/total"
DATA.DIR.MSA.DEATHS="../../data_raw/msa_surveillance_reports/deaths"
DATA.DIR.MSA.SEX="../../data_raw/msa_surveillance_reports/sex"
DATA.DIR.MSA.SEX.AGE="../../data_raw/msa_surveillance_reports/sex_age"
DATA.DIR.MSA.SEX.RACE="../../data_raw/msa_surveillance_reports/sex_race"
DATA.DIR.MSA.SEX.RISK="../../data_raw/msa_surveillance_reports/sex_risk"
DATA.DIR.MSA.RACE.RISK="../../data_raw/msa_surveillance_reports/race_risk"
DATA.DIR.MSA.2009="../../data_raw/msa_surveillance_reports/before_2009"

msa_total <- Sys.glob(paste0(DATA.DIR.MSA.TOTAL, '/*.csv'))
msa_deaths <- Sys.glob(paste0(DATA.DIR.MSA.DEATHS, '/*.csv'))
msa_sex <- Sys.glob(paste0(DATA.DIR.MSA.SEX, '/*.csv'))
msa_sex_age <- Sys.glob(paste0(DATA.DIR.MSA.SEX.AGE, '/*.csv'))
msa_sex_race <- Sys.glob(paste0(DATA.DIR.MSA.SEX.RACE, '/*.csv'))
msa_sex_risk <- Sys.glob(paste0(DATA.DIR.MSA.SEX.RISK, '/*.csv'))
msa_race_risk <- Sys.glob(paste0(DATA.DIR.MSA.RACE.RISK, '/*.csv'))
msa_2009 <- Sys.glob(paste0(DATA.DIR.MSA.2009, '/*.csv'))

#creating a list with sublists of filename, data#
data.list.msa_total <- lapply(msa_total, function(x){
  list(filename=x, data=read.csv(x, header=TRUE))
})
data.list.msa_deaths <- lapply(msa_deaths, function(x){
  list(filename=x, data=read.csv(x, header=TRUE))
})
data.list.msa_sex <- lapply(msa_sex, function(x){
  list(filename=x, data=read.csv(x, header=TRUE))
})
data.list.msa_sex_age <- lapply(msa_sex_age, function(x){
  list(filename=x, data=read.csv(x, header=TRUE))
})
data.list.msa_sex_race <- lapply(msa_sex_race, function(x){
  list(filename=x, data=read.csv(x, header=TRUE))
})
data.list.msa_sex_risk <- lapply(msa_sex_risk, function(x){
  list(filename=x, data=read.csv(x, header=TRUE))
})
data.list.msa_race_risk <- lapply(msa_race_risk, function(x){
  list(filename=x, data=read.csv(x, header=TRUE))
})
data.list.msa_2009 <- lapply(msa_2009, function(x){
  list(filename=x, data=read.csv(x, header=TRUE))
})

################################################################################
                            ###MSA DEATHS###
################################################################################
data.list.msa_deaths.clean = lapply(data.list.msa_deaths, function(file){
  
  data=file[["data"]] #apply the function to the data element#
  filename = file[["filename"]] #apply the function to the filename element#
   
  if(grepl("2009", filename)) {
    data$year = as.character("2009")
    # data$location = data$Area.of.residence
  }
   if(grepl("2010", filename)) {
     data$year = as.character("2010")
     data$location = data$Area.of.residence
   }
   if(grepl("2012", filename)) {
     data$year = as.character("2012")
     data$location = data$MSA.of.residence
   }
   if(grepl("2013", filename)) {
     data$year = as.character("2013")
     data$location = data$MSA.of.residence
   }
   if(grepl("2014", filename)) {
     data$year = as.character("2014")
     data$location = data$MSA.of.residence
   }
   if(grepl("2015", filename)) {
     data$year = as.character("2015")
     data$location = data$MSA.of.residence
   }
   if(grepl("2016", filename)) {
     data$year = as.character("2016")
     data$location = data$MSA.of.residence
   }
   if(grepl("2018", filename)) {
     data$year = as.character("2018")
     data$location = data$MSA.of.residence
   }

  ###Processing for values###
  if(grepl("2014", filename)){
    data$male_num = data$No.
    data$female_num = data$No..1
  }
  
  if(grepl(pattern= '2014|2015|2016|2018', x=filename)){

    data$male_num = as.numeric(data$male_num)
    
     data <- data %>%
       pivot_longer(cols=c(one_of("male_num", "female_num")),
                    names_to = "sex",
                    values_to = "value")
  }
  
  #Remove commas from values; Replace dashes with NAs#
 # data$value = as.numeric(gsub(",", '', data$value)) 
  
  list(filename, data)  
  
})

###I'm going to move on from this section but the pivot longer wont work and idk why
################################################################################
                         ###MSA PREVALENCE TOTAL###
################################################################################
data.list.msa_total.clean = lapply(data.list.msa_total, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]] 
  
  data$location = data$MSA
  
  # ifelse(data$prevalence_num == "-", NA, data$prevalence_num)
  # ifelse(data$diagnoses_num == "-", NA, data$diagnoses_num)

  data$diagnoses_num= str_replace_all(data$diagnoses_num, " ", "")  
  data$diagnoses_num= str_replace_all(data$diagnoses_num, ",", "")
  
  data$prevalence_num= str_replace_all(data$prevalence_num, ",", "")
  data$prevalence_num= str_replace_all(data$prevalence_num, " ", "")  

  
  data <- data%>%
    select(location, diagnoses_num, prevalence_num) %>%
    pivot_longer(cols=c(one_of('diagnoses_num', 'prevalence_num')),
                        names_to = "outcome",
                        values_to = "value")
  
  #I don't know how to handle the years in this case#
  data$value = as.numeric(data$value)
  data$outcome =(gsub("_num", '', data$outcome))
  
  data= as.data.frame(data)
  
  list(filename, data) 
  
  ###Pending changes: how to assign year values, which years to use. ####
  
})

################################################################################
                            ###MSA BY SEX ONLY###
################################################################################
data.list.msa_sex.clean = lapply(data.list.msa_sex, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]] 
  
  data$location = data$MSA
  
  if(grepl("male", filename)){
    data$sex="male"
  }
  
  if(grepl("female", filename)){
    data$sex="female"
  }
  
  ifelse(data$prevalence_num == "-", NA, data$prevalence_num)
  ifelse(data$diagnoses_num == "-", NA, data$diagnoses_num)
   
   data$diagnoses_num= str_replace_all(data$diagnoses_num, " ", "")  
   data$diagnoses_num= str_replace_all(data$diagnoses_num, ",", "")

  data$prevalence_num= str_replace_all(data$prevalence_num, ",", "")
  data$prevalence_num= str_replace_all(data$prevalence_num, " ", "")
  
  
  data <- data%>%
    select(location, sex, diagnoses_num, prevalence_num) %>%
    pivot_longer(cols=c(one_of('diagnoses_num', 'prevalence_num')),
                 names_to = "outcome",
                 values_to = "value")

  #I don't know how to handle the years in this case#
  data$value = as.numeric(data$value)
  data$outcome =(gsub("_num", '', data$outcome))
  
  data= as.data.frame(data)
  
  list(filename, data) 
  
  ###Pending changes: how to assign year values, which years to use. ####
  
})