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
DATA.DIR.MSA.BEFORE="../../data_raw/msa_surveillance_reports/before"

msa_total <- Sys.glob(paste0(DATA.DIR.MSA.TOTAL, '/*.csv'))
msa_deaths <- Sys.glob(paste0(DATA.DIR.MSA.DEATHS, '/*.csv'))
msa_sex <- Sys.glob(paste0(DATA.DIR.MSA.SEX, '/*.csv'))
msa_sex_age <- Sys.glob(paste0(DATA.DIR.MSA.SEX.AGE, '/*.csv'))
msa_sex_race <- Sys.glob(paste0(DATA.DIR.MSA.SEX.RACE, '/*.csv'))
msa_sex_risk <- Sys.glob(paste0(DATA.DIR.MSA.SEX.RISK, '/*.csv'))
msa_race_risk <- Sys.glob(paste0(DATA.DIR.MSA.RACE.RISK, '/*.csv'))
msa_before <- Sys.glob(paste0(DATA.DIR.MSA.BEFORE, '/*.csv'))

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
data.list.msa_2009 <- lapply(msa_before, function(x){
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

race.msa.mappings = c('1'= 'American Indian/Alaska Native',
                      '2'='Asian',
                      '3'= 'Black/African American',
                      '4'= 'Hispanic/Latino',
                      '5'= 'White')

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
            ###PENDING: is the correct interpretation of years? ####
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
  
  ##add year section##
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
  
})
################################################################################
                            ###MSA BY SEX ONLY###
      ###PENDING: is this the correct interpretation of year esp for 2018?###
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

  data$value = as.numeric(data$value)
  data$outcome =(gsub("_num", '', data$outcome))
  
  ##add year section
  if(grepl("2010 new 2009", filename)){
    data$year = ifelse(data$outcome == "diagnoses", "2010", "2009")
  }
  if(grepl("2011 new 2010", filename)){
    data$year = ifelse(data$outcome == "diagnoses", "2011", "2010")
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
  if(grepl("2018 new 2018", filename)){
    data$year = "2018"
  }
  
  data= as.data.frame(data)
  
  list(filename, data) 
  
})

################################################################################
                        ###MSA BY SEX AND AGE###
################################################################################
data.list.msa_sex_age.clean = lapply(data.list.msa_sex_age, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]] 

  data$location = data$MSA
  
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
  
  if(grepl("new", filename)){
    
    data$diagnoses_num_1 = (gsub("[[:punct:]]", NA, data$diagnoses_num_1))
    data$diagnoses_1 = as.numeric(data$diagnoses_num_1)
    
    data$diagnoses_num_2 = (gsub("[[:punct:]]", NA, data$diagnoses_num_2))
    data$diagnoses_2 = as.numeric(data$diagnoses_num_2)
    
    data$diagnoses_num_3 = (gsub("[[:punct:]]", NA, data$diagnoses_num_3))
    #data$diagnoses_num_3 = (gsub("[^[:alnum:] ]", NA, data$diagnoses_num_3))
    data$diagnoses_3 = as.numeric(data$diagnoses_num_3)
    
    data$diagnoses_num_4 = (gsub("[[:punct:]]", NA, data$diagnoses_num_4))
    data$diagnoses_4 = as.numeric(data$diagnoses_num_4)
    
    data$diagnoses_num_5 = (gsub("[[:punct:]]", NA, data$diagnoses_num_5))
    data$diagnoses_5 = as.numeric(data$diagnoses_num_5)
  }
  
  if(grepl("prevalence", filename)){
    data$prevalence_num_1 = (gsub("[[:punct:]]", NA, data$prevalence_num_1))
    data$prevalence_1 = as.numeric(data$prevalence_num_1)
    
    data$prevalence_num_2 = (gsub("[[:punct:]]", NA, data$prevalence_num_2))
    data$prevalence_2 = as.numeric(data$prevalence_num_2)
    
    data$prevalence_num_3 = (gsub("[[:punct:]]", NA, data$prevalence_num_3))
    data$prevalence_3 = as.numeric(data$prevalence_num_3)
    
    data$prevalence_num_4 = (gsub("[[:punct:]]", NA, data$prevalence_num_4))
    data$prevalence_4 = as.numeric(data$prevalence_num_4)
    
    data$prevalence_num_5 = (gsub("[[:punct:]]", NA, data$prevalence_num_5))
    data$prevalence_5 = as.numeric(data$prevalence_num_5)
  }
  
  data <- data %>%
    select(year, location, sex, one_of("diagnoses_1", "diagnoses_2", "diagnoses_3", "diagnoses_4", "diagnoses_5",
                                       "prevalence_1", "prevalence_2", "prevalence_3", "prevalence_4", "prevalence_5"))
  
  data <- data %>%
    pivot_longer(cols=c(one_of("diagnoses_1", "diagnoses_2", "diagnoses_3", "diagnoses_4", "diagnoses_5",
                               "prevalence_1", "prevalence_2", "prevalence_3", "prevalence_4", "prevalence_5")),
                 names_to = c("outcome", "age"),
                 names_sep = "_",
                 values_to = "value")

  data$age = age.msa.mappings[data$age]
  
  data= as.data.frame(data)
  
  list(filename, data) 
  
})

################################################################################
                        ###BEFORE 2009 FILES###
              ### PENDING:HOW TO DIFFERENTIATE AIDS VS HIV######
################################################################################
data.list.msa_2009.clean = lapply(data.list.msa_2009, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]] 
  
  data$location = data$msa
  
  ##add year section##
  ##problem is commas and dashes####
  if(grepl("1993 new 1992", filename)){
    data$diagnoses_1993 = data$new_no_1993
    data$prevalence_1992 = data$prev_total
    
    data$diagnoses_1993 = as.numeric(gsub(",",'', data$diagnoses_1993)) 
    data$prevalence_1992 = as.numeric(gsub(",",'', data$prevalence_1992)) 
  }
  if(grepl("1994 new 1993", filename)){
    data$diagnoses_1994 = data$new_no_1994
    data$prevalence_1993 = data$prev_total
    
    data$diagnoses_1994 = as.numeric(gsub(",",'', data$diagnoses_1994)) 
    data$prevalence_1993 = as.numeric(gsub(",",'', data$prevalence_1993)) 
  }
  if(grepl("1995 new 1994", filename)){
    data$diagnoses_1995 = data$new_no_1995
    data$prevalence_1994 = data$prev_total
    
    data$diagnoses_1995 = as.numeric(gsub(",",'', data$diagnoses_1995)) 
    data$prevalence_1994 = as.numeric(gsub(",",'', data$prevalence_1994)) 
  }
  if(grepl("1996 new 1995", filename)){
    data$diagnoses_1996 = data$new_no_1996
    data$prevalence_1995 = data$prev_total
    
    data$diagnoses_1996 = as.numeric(gsub(",",'', data$diagnoses_1996)) 
    data$prevalence_1995 = as.numeric(gsub(",",'', data$prevalence_1995)) 
  }
  if(grepl("1997 new 1996", filename)){
    data$diagnoses_1997 = data$new_no_1997
    data$prevalence_1996 = data$prev_total
    
    data$diagnoses_1997 = as.numeric(gsub(",",'', data$diagnoses_1997)) 
    data$prevalence_1996 = as.numeric(gsub(",",'', data$prevalence_1996)) 
  }
  if(grepl("1998 new 1997", filename)){
    data$diagnoses_1998 = data$new_no_1998
    data$prevalence_1997 = data$prev_total
    
    data$diagnoses_1998 = as.numeric(gsub(",",'', data$diagnoses_1998)) 
    data$prevalence_1997 = as.numeric(gsub(",",'', data$prevalence_1997)) 
  }
  if(grepl("1999 new 1998", filename)){
    data$diagnoses_1999 = data$new_no_1999
    data$prevalence_1998 = data$prev_total
    
    data$diagnoses_1999 = as.numeric(gsub(",",'', data$diagnoses_1999)) 
    data$prevalence_1998 = as.numeric(gsub(",",'', data$prevalence_1998)) 
  }
  if(grepl("2000 new 1999", filename)){
    data$diagnoses_2000 = data$new_no_2000
    data$prevalence_1999 = data$prev_total
    
    data$diagnoses_2000 = as.numeric(gsub(",",'', data$diagnoses_2000)) 
    data$prevalence_1999 = as.numeric(gsub(",",'', data$prevalence_1999)) 
  }
  if(grepl("2001 new 2000", filename)){
    data$diagnoses_2001 = data$new_no_2001
    data$prevalence_2000 = data$prev_total
    
    data$diagnoses_2001 = as.numeric(gsub(",",'', data$diagnoses_2001)) 
    data$prevalence_2000 = as.numeric(gsub(",",'', data$prevalence_2000)) 
  }
  if(grepl("2002 new 2001", filename)){
    data$diagnoses_2002 = data$new_no_2002
    data$prevalence_2001 = data$prev_total
    
    data$diagnoses_2002 = as.numeric(gsub(",",'', data$diagnoses_2002)) 
    data$prevalence_2001 = as.numeric(gsub(",",'', data$prevalence_2001)) 
  }
  if(grepl("2003 new 2002", filename)){
    data$diagnoses_2003 = data$new_no_2003
    data$prevalence_2002 = data$prev_total
    
    data$diagnoses_2003 = as.numeric(gsub(",",'', data$diagnoses_2003)) 
    data$prevalence_2002 = as.numeric(gsub(",",'', data$prevalence_2002)) 
  }
  if(grepl("2004 new 2003", filename)){
    data$diagnoses_2004 = data$new_no_2004
    data$prevalence_2003 = data$prev_total
    
    data$diagnoses_2004 = as.numeric(gsub(",",'', data$diagnoses_2004)) 
    data$prevalence_2003 = as.numeric(gsub(",",'', data$prevalence_2003)) 
  }
  if(grepl("2005 new 2004", filename)){
    data$diagnoses_2005 = data$new_no_2005
    data$prevalence_2004 = data$prev_total
    
    data$diagnoses_2005 = as.numeric(gsub(",",'', data$diagnoses_2005)) 
    data$prevalence_2004 = as.numeric(gsub(",",'', data$prevalence_2004)) 
  }
  if(grepl("2006 new 2005", filename)){
    data$diagnoses_2006 = data$new_no_2006
    data$prevalence_2005 = data$prev_total
    data$diagnoses_2006 = as.numeric(gsub(",",'', data$diagnoses_2006)) 
    data$prevalence_2005 = as.numeric(gsub(",",'', data$prevalence_2005)) 
  }
  if(grepl("2007 new 2006", filename)){
    data$diagnoses_2007 = data$new_no_2007
    data$prevalence_2006 = data$prev_total
    data$diagnoses_2007 = as.numeric(gsub(",",'', data$diagnoses_2007)) 
    data$prevalence_2006 = as.numeric(gsub(",",'', data$prevalence_2006)) 
  }
  
  if(grepl("2008 new 2007", filename)){
    data$diagnoses_2008 = data$new_num
    data$prevalence_2007 = data$prev_num
    data$diagnoses_2008 = as.numeric(gsub(",",'', data$diagnoses_2008)) 
    data$prevalence_2007 = as.numeric(gsub(",",'', data$prevalence_2007))
  }
  if(grepl("2009 new 2008", filename)){
    data$diagnoses_2009 = data$new_num
    data$prevalence_2008 = data$prev_num
    data$diagnoses_2009 = as.numeric(gsub(",",'', data$diagnoses_2009)) 
    data$prevalence_2008 = as.numeric(gsub(",",'', data$prevalence_2008))
  }
  
data <- data %>%
  select(location,(one_of("diagnoses_1993", "diagnoses_1994", "diagnoses_1995","diagnoses_1996", "diagnoses_1997", "diagnoses_1998", "diagnoses_1999",
                                 "diagnoses_2000", "diagnoses_2001", "diagnoses_2002", "diagnoses_2003", "diagnoses_2004", "diagnoses_2005",
                                 "diagnoses_2006", "diagnoses_2007", "diagnoses_2008", "diagnoses_2009", "prevalence_1992", "prevalence_1993", "prevalence_1994", "prevalence_1995",
                                 "prevalence_1996", "prevalence_1997", "prevalence_1998", "prevalence_1999", "prevalence_2000", "prevalence_2001", "prevalence_2002",
                                 "prevalence_2003", "prevalence_2004", "prevalence_2005","prevalence_2006", "prevalence_2007", "prevalence_2008")))

     data <- data %>%
       pivot_longer(cols=c(one_of("diagnoses_1993", "diagnoses_1994", "diagnoses_1995","diagnoses_1996", "diagnoses_1997", "diagnoses_1998", "diagnoses_1999",
                                 "diagnoses_2000", "diagnoses_2001", "diagnoses_2002", "diagnoses_2003", "diagnoses_2004", "diagnoses_2005",
                                 "diagnoses_2006", "diagnoses_2007", "diagnoses_2008", "diagnoses_2009", "prevalence_1992", "prevalence_1993", "prevalence_1994", "prevalence_1995",
                                 "prevalence_1996", "prevalence_1997", "prevalence_1998", "prevalence_1999", "prevalence_2000", "prevalence_2001", "prevalence_2002",
                                 "prevalence_2003", "prevalence_2004", "prevalence_2005","prevalence_2006", "prevalence_2007", "prevalence_2008")),
       names_to = c("outcome", "year"),
       names_sep = "_",
      values_to = "value")

  data= as.data.frame(data)
  
  list(filename, data) 
})

################################################################################
                          ###MSA BY SEX AND RACE###
################################################################################
data.list.msa_sex_race.clean = lapply(data.list.msa_sex_race, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]] 
  
  data$location = data$MSA
  
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

   if(grepl("new", filename)){

       data$diagnoses_num_1 = (gsub("[[:punct:]]", NA, data$diagnoses_num_1))
       data$diagnoses_1 = as.numeric(data$diagnoses_num_1)

        data$diagnoses_num_2 = (gsub("[[:punct:]]", NA, data$diagnoses_num_2))
        data$diagnoses_2 = as.numeric(data$diagnoses_num_2)
        
        data$diagnoses_num_3 = (gsub("[[:punct:]]", NA, data$diagnoses_num_3))
        #data$diagnoses_num_3 = (gsub("[^[:alnum:] ]", NA, data$diagnoses_num_3))
        data$diagnoses_3 = as.numeric(data$diagnoses_num_3)
        
        data$diagnoses_num_4 = (gsub("[[:punct:]]", NA, data$diagnoses_num_4))
        data$diagnoses_4 = as.numeric(data$diagnoses_num_4)
        
        data$diagnoses_num_5 = (gsub("[[:punct:]]", NA, data$diagnoses_num_5))
        data$diagnoses_5 = as.numeric(data$diagnoses_num_5)
   }

  if(grepl("prevalence", filename)){
    data$prevalence_num_1 = (gsub("[[:punct:]]", NA, data$prevalence_num_1))
    data$prevalence_1 = as.numeric(data$prevalence_num_1)
    
    data$prevalence_num_2 = (gsub("[[:punct:]]", NA, data$prevalence_num_2))
    data$prevalence_2 = as.numeric(data$prevalence_num_2)
    
    data$prevalence_num_3 = (gsub("[[:punct:]]", NA, data$prevalence_num_3))
    data$prevalence_3 = as.numeric(data$prevalence_num_3)
    
    data$prevalence_num_4 = (gsub("[[:punct:]]", NA, data$prevalence_num_4))
    data$prevalence_4 = as.numeric(data$prevalence_num_4)
    
    data$prevalence_num_5 = (gsub("[[:punct:]]", NA, data$prevalence_num_5))
    data$prevalence_5 = as.numeric(data$prevalence_num_5)
  }

   data <- data %>%
     select(year, location, sex, one_of("diagnoses_1", "diagnoses_2", "diagnoses_3", "diagnoses_4", "diagnoses_5",
                                        "prevalence_1", "prevalence_2", "prevalence_3", "prevalence_4", "prevalence_5"))

   data <- data %>%
     pivot_longer(cols=c(one_of("diagnoses_1", "diagnoses_2", "diagnoses_3", "diagnoses_4", "diagnoses_5",
                                "prevalence_1", "prevalence_2", "prevalence_3", "prevalence_4", "prevalence_5")),
                  names_to = c("outcome", "race"),
                  names_sep = "_",
                  values_to = "value")

     
data$race = race.msa.mappings[data$race]
   
data= as.data.frame(data)

list(filename, data) 

})


