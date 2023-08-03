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
                            ###MAPPINGS###
################################################################################
age.msa.mappings = c('1' = '13-24 years',
                        '2' = '25-34 years',
                        '3' = '35-44 years',
                        '4' = '45-54 years',
                        '5' = '55 years and older')

################################################################################
                            ###MSA DEATHS BY SEX###
################################################################################
data.list.msa_deaths.clean = lapply(data.list.msa_deaths, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]] 
   
  data$location = data$MSA
  
  #Remove commas from values#
  #Why is this causing NAs introduced by coercion warnings?#
  data$male = as.numeric(gsub(",",'', data$male_num)) 
  data$female = as.numeric(gsub(",",'', data$female_num)) 
  
  if(grepl("2009", filename)) {
    data$year = as.character("2009")
  }
   if(grepl("2010", filename)) {
     data$year = as.character("2010")
   }
   if(grepl("2012", filename)) {
     data$year = as.character("2012")
   }
   if(grepl("2013", filename)) {
     data$year = as.character("2013")
   }
   if(grepl("2014", filename)) {
     data$year = as.character("2014")
   }
   if(grepl("2015", filename)) {
     data$year = as.character("2015")
   }
   if(grepl("2016", filename)) {
     data$year = as.character("2016")
   }
   if(grepl("2018", filename)) {
     data$year = as.character("2018")
   }
      data <- data %>%
        select(year, location, male, female) %>%
       pivot_longer(cols=c(one_of("male", "female")),
                    names_to = "sex",
                    values_to = "value")

  data$outcome = "hiv deaths"
  list(filename, data)  
  
})
################################################################################
                         ###MSA PREVALENCE TOTAL###
             ###These files have both prevalence and diagnoses values###
################################################################################
data.list.msa_total.clean = lapply(data.list.msa_total, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]] 
  
  data$location = data$MSA

  data$diagnoses_num= str_replace_all(data$diagnoses_num, " ", "")  
  data$diagnoses_num= str_replace_all(data$diagnoses_num, ",", "")
  
  data$prevalence_num= str_replace_all(data$prevalence_num, ",", "")
  data$prevalence_num= str_replace_all(data$prevalence_num, " ", "")  

  
  data <- data%>%
    select(location, diagnoses_num, prevalence_num) %>%
    pivot_longer(cols=c(one_of('diagnoses_num', 'prevalence_num')),
                        names_to = "outcome",
                        values_to = "value")
  
  data$value = as.numeric(data$value)
  data$outcome =(gsub("_num", '', data$outcome))
  
  ##add year section
  if(grepl("2010 new 2009", filename)){
    data$year = ifelse(data$outcome == "diagnoses", "2010", "2009")
  }
  if(grepl("2011 new 2010", filename)){
    data$year = ifelse(data$outcome == "diagnoses", "2011", "2010")
  }
  if(grepl("2012 new 2011", filename)){
    data$year = ifelse(data$outcome == "diagnoses", "2012", "2011")
  }
  if(grepl("2013 new 2012", filename)){
    data$year = ifelse(data$outcome == "diagnoses", "2013", "2012")
  }
  if(grepl("2014 new 2013", filename)){
    data$year = ifelse(data$outcome == "diagnoses", "2014", "2013")
  }
  if(grepl("2015 new 2014", filename)){
    data$year = ifelse(data$outcome == "diagnoses", "2015", "2014")
  }
  if(grepl("2016 new 2015", filename)){
    data$year = ifelse(data$outcome == "diagnoses", "2016", "2015")
  }
  if(grepl("2017 new 2016", filename)){
    data$year = ifelse(data$outcome == "diagnoses", "2017", "2016")
  }
  
  data= as.data.frame(data)
  
  list(filename, data) 
  
  ###Pending changes: make sure this is the correct interpretation of years####
  
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

################################################################################
                        ###MSA BY SEX AND AGE###
################################################################################

data.list.msa_sex_age.clean = lapply(data.list.msa_sex_age, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]] 

  data$location = data$MSA
  
  # data$diagnoses_num_1= str_replace_all(data$diagnoses_num_1, ",", "")
  # 
  # data$test = as.numeric(gsub(data$diagnoses_num_2,",",''))
  
  
  # data$diagnoses_num_3= str_replace_all(data$diagnoses_num_3, ",", "")
  # data$diagnoses_num_4= str_replace_all(data$diagnoses_num_4, ",", "")
  # data$diagnoses_num_5= str_replace_all(data$diagnoses_num_5, ",", "")
  # 
  # data$diagnoses_num_1=as.numeric(data$diagnoses_num_1)
  # data$diagnoses_num_2=as.numeric(data$diagnoses_num_2)
  # data$diagnoses_num_3=as.numeric(data$diagnoses_num_3)
  # data$diagnoses_num_4=as.numeric(data$diagnoses_num_4)
  # data$diagnoses_num_5=as.numeric(data$diagnoses_num_5)
  # 
  # data$prevalence_num_1=as.numeric(data$prevalence_num_1)
  # data$prevalence_num_2=as.numeric(data$prevalence_num_2)
  # data$prevalence_num_3=as.numeric(data$prevalence_num_3)
  # data$prevalence_num_4=as.numeric(data$prevalence_num_4)
  # data$prevalence_num_5=as.numeric(data$prevalence_num_5)
  # 
  #Create Year#
if(grepl("2009", filename)) {
  data$year = as.character("2009")
}
if(grepl("2010", filename)) {
  data$year = as.character("2010")
}
  if(grepl("2011", filename)) {
    data$year = as.character("2011")
  }
if(grepl("2012", filename)) {
  data$year = as.character("2012")
}
if(grepl("2013", filename)) {
  data$year = as.character("2013")
}
if(grepl("2014", filename)) {
  data$year = as.character("2014")
}
if(grepl("2015", filename)) {
  data$year = as.character("2015")
}
if(grepl("2016", filename)) {
  data$year = as.character("2016")
}
  if(grepl("2017", filename)) {
    data$year = as.character("2017")
  }
if(grepl("2018", filename)) {
  data$year = as.character("2018")
}
  
  #Create Sex# 
if(grepl("male", filename)){
    data$sex="male"
  }
if(grepl("female", filename)){
    data$sex="female"
}
  
#   #Create Outcome#   
# if(grepl("new", filename)){
#     data$outcome="diagnoses"
#   }
# if(grepl("prevalence", filename)){
#     data$outcome="prevalence"
# }

  ###REMOVE COMMAS AND SPACES; CHANGE ALL TO NUMERIC###
  
  # ifelse(data$diagnoses_num_1 == "—", NA, data$diagnoses_num_1)
  # ifelse(data$diagnoses_num_2 == "—", NA, data$diagnoses_num_2)
  # ifelse(data$diagnoses_num_3 == "—", NA, data$diagnoses_num_3)
  # ifelse(data$diagnoses_num_4 == "—", NA, data$diagnoses_num_4)
  # ifelse(data$diagnoses_num_5 == "—", NA, data$diagnoses_num_5)
  
  # 
  # data$diagnoses_num_1= str_replace_all(data$diagnoses_num_1, " ", "")
  # data$diagnoses_num_2= str_replace_all(data$diagnoses_num_2, " ", "")  
  # data$diagnoses_num_3= str_replace_all(data$diagnoses_num_3, " ", "")  
  # data$diagnoses_num_4= str_replace_all(data$diagnoses_num_4, " ", "")  
  # data$diagnoses_num_5= str_replace_all(data$diagnoses_num_5, " ", "")


  ##############################################################

  data <- data %>%
     select(location, year, sex,(one_of("diagnoses_num_1", "diagnoses_num_2", "diagnoses_num_3",
  "diagnoses_num_4", "diagnoses_num_5", "prevalence_num_1", "prevalence_num_2", "prevalence_num_3",
  "prevalence_num_4", "prevalence_num_5")))

  # data<- data %>%
  # pivot_longer(cols=c(one_of("diagnoses_num_1", "diagnoses_num_2", "diagnoses_num_3",
  #                             "diagnoses_num_4", "diagnoses_num_5", "prevalence_num_1", "prevalence_num_2", "prevalence_num_3",
  #                             "prevalence_num_4", "prevalence_num_5")),
  #                     names_to = c("outcome", "age"),
  #                     names_sep = "_",
  #                     values_to = "value")

  #data$age = age.msa.mappings(data$WHATISTHISVAR)
  data= as.data.frame(data)
  
  list(filename, data) 
  
})
