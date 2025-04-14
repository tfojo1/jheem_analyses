###You'll need to add this as a source file into the main census processing code###
#library(jheem2) #Remove this once you are sourcing the file#
#library(tidyverse)
################################################################################
                ###Read in MSA Scraped PDF Files### 
################################################################################
DATA.DIR.MSA.TOTAL="Q:/data_raw/msa_surveillance_reports/total"
DATA.DIR.MSA.DEATHS="Q:/data_raw/msa_surveillance_reports/deaths"
DATA.DIR.MSA.SEX="Q:/data_raw/msa_surveillance_reports/sex"
DATA.DIR.MSA.SEX.AGE="Q:/data_raw/msa_surveillance_reports/sex_age"
DATA.DIR.MSA.SEX.RACE="Q:/data_raw/msa_surveillance_reports/sex_race"
DATA.DIR.MSA.SEX.RISK="Q:/data_raw/msa_surveillance_reports/sex_risk"
DATA.DIR.MSA.RACE.RISK="Q:/data_raw/msa_surveillance_reports/race_risk"
DATA.DIR.MSA.BEFORE="Q:/data_raw/msa_surveillance_reports/before"

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
                        '5' = '55+ years')

race.msa.mappings = c('1'= 'american indian/alaska native',
                      '2'='asian',
                      '3'= 'black/african american',
                      '4'= 'hispanic/latino',
                      '5'= 'white')

risk.msa.mappings = c('1'= 'msm',
                      '2'='idu',
                      '3'= 'msm_idu',
                      '4'= 'heterosexual',
                      '5'= 'other')

outcome.mappings.v2 = c('diagnoses'='diagnoses',
                     'prevalence' = 'diagnosed.prevalence',
                     "aids.diagnosed.prevalence" = "aids.diagnosed.prevalence",
                     "aids.diagnoses"= "aids.diagnoses")
################################################################################
                            ###MSA DEATHS BY SEX### 
################################################################################
data.list.msa_deaths.clean = lapply(data.list.msa_deaths, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]] 
  
data$division= ifelse(grepl("Division", data$MSA), "1", "0") #Remove MSA = division
data= subset(data, data$division != "1")

##Manually Remove MSAs Todd decided to take out bc they are out of date##
##The problem is these may have different symbols and things##
data= subset(data, data$MSA != "Bergen-Passaic, NJ")
data= subset(data, data$MSA != "Gary, IN")
data= subset(data, data$MSA != "Middlesex-Somerset-Hunterdon, NJ")
data= subset(data, data$MSA != "Middlesex, NJ")
data= subset(data, data$MSA != "Monmouth-Ocean City, NJ")
data= subset(data, data$MSA != "Monmouth-Ocean, NJ")
data= subset(data, data$MSA != "Nassau-Suffolk, NY")
data= subset(data, data$MSA != "Nassau-Suffolk, NJ")
data= subset(data, data$MSA != "West Palm Beach-Boca Raton, FL")
data= subset(data, data$MSA != "West Palm Beach, FL")

#Update 2-8-24
data= subset(data, data$MSA != "San Juan–Caguas–Guaynabo, PRc")
data= subset(data, data$MSA != "San Juan–Caguas–Guaynabo, PR")
data= subset(data, data$MSA != "San Juan–Carolina–Caguas, PR")

##Rename a few MSAs that are causing problems
data$MSA = if_else(data$MSA == "Charlotte–Gastonia–Concord, NC–SC", "Charlotte-Concord,NC-SC", data$MSA)
data$MSA = if_else(data$MSA == "Philadelphia, PA–NJ–DE–MD", "Philadelphia-Camden, PA-NJ-DE-MD", data$MSA)
data$MSA = if_else(data$MSA =="Portland–Vancouver–Beaverton, OR–WA", "Portland-Vancouver, OR-WA", data$MSA)
data$MSA = if_else(data$MSA == "Washington, DC–VA–MD–WV", "Washington-Arlington-Alexandria, DC-VA-MD-WV", data$MSA)

data$location = as.character(locations::get.cbsa.for.msa.name(data$MSA))
  
data$male = (gsub("[[:punct:]]", "", data$male_num))
data$female = (gsub("[[:punct:]]", "", data$female_num))

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
   #     select("year", "location", "male", "female") %>%
       pivot_longer(cols=c(one_of("male", "female")),
                    names_to = "sex",
                    values_to = "value")

  data$outcome = "hiv.deaths"
  
  data$value = as.numeric(data$value)
  
  data= as.data.frame(data)
  
  list(filename, data)  
  
})
################################################################################
                         ###MSA PREVALENCE TOTAL###
################################################################################
data.list.msa_total.clean = lapply(data.list.msa_total, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]] 
  
  data$division= ifelse(grepl("Division", data$MSA), "1", "0") #Remove MSA = division
  data= subset(data, data$division != "1")
  
  ##Manually Remove MSAs Todd decided to take out bc they are out of date##
  ##The problem is these may have different symbols and things##
  data= subset(data, data$MSA != "Bergen-Passaic, NJ")
  data= subset(data, data$MSA != "Gary, IN")
  data= subset(data, data$MSA != "Middlesex-Somerset-Hunterdon, NJ")
  data= subset(data, data$MSA != "Middlesex, NJ")
  data= subset(data, data$MSA != "Monmouth-Ocean City, NJ")
  data= subset(data, data$MSA != "Monmouth-Ocean, NJ")
  data= subset(data, data$MSA != "Nassau-Suffolk, NY")
  data= subset(data, data$MSA != "Nassau-Suffolk, NJ")
  data= subset(data, data$MSA != "West Palm Beach-Boca Raton, FL")
  data= subset(data, data$MSA != "West Palm Beach, FL")
  
  #Update 2-8-24
  data= subset(data, data$MSA != "San Juan–Caguas–Guaynabo, PRc")
  data= subset(data, data$MSA != "San Juan–Caguas–Guaynabo, PR")
  data= subset(data, data$MSA != "San Juan–Carolina–Caguas, PR")
  
  ##Rename a few MSAs that are causing problems
  data$MSA = if_else(data$MSA == "Charlotte–Gastonia–Concord, NC–SC", "Charlotte-Concord,NC-SC", data$MSA)
  data$MSA = if_else(data$MSA == "Philadelphia, PA–NJ–DE–MD", "Philadelphia-Camden, PA-NJ-DE-MD", data$MSA)
  data$MSA = if_else(data$MSA =="Portland–Vancouver–Beaverton, OR–WA", "Portland-Vancouver, OR-WA", data$MSA)
  data$MSA = if_else(data$MSA == "Washington, DC–VA–MD–WV", "Washington-Arlington-Alexandria, DC-VA-MD-WV", data$MSA)
  
  data$location = as.character(locations::get.cbsa.for.msa.name(data$MSA))

  data$diagnoses_num= str_replace_all(data$diagnoses_num, " ", "")  
  data$diagnoses_num= str_replace_all(data$diagnoses_num, ",", "")
  
  data$prevalence_num= str_replace_all(data$prevalence_num, ",", "")
  data$prevalence_num= str_replace_all(data$prevalence_num, " ", "")  

  
  data <- data%>%
    #select(location, diagnoses_num, prevalence_num) %>%
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
  if(grepl("2018 new 2018", filename)){
    data$year = ifelse(data$outcome == "diagnoses", "2018", "2018")
  }
  
  data$outcome = outcome.mappings.v2[data$outcome]
  
  #Update 2/20/24: Removing data prior to 2014 for outcome = diagnosed.prevalence; Decided this in the calibration process.  Keeping year=2014.
  data <- data %>%
    mutate(year_helper = as.numeric(year))%>%
    mutate(drop_var = if_else((outcome=='diagnosed.prevalence' & year_helper < 2014), "1", "0")) #drop 1
  
  data = subset(data, data$drop_var != "1")
  ##
  
  #Update for March 2025- want to remove all diagnosed.prevalence data for Riverside MSA in 2015 or before
  data <- data %>%
    mutate(drop_riverside = as.character(ifelse(location == "C.40140" & (year=="2014"|year == "2015") & outcome=='diagnosed.prevalence', "1", "0")))%>%
    filter(drop_riverside != "1")
    
  
  data= as.data.frame(data)
  list(filename, data) 
  
})
################################################################################
                            ###MSA BY SEX ONLY###
################################################################################
data.list.msa_sex.clean = lapply(data.list.msa_sex, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]] 
  
  data$division= ifelse(grepl("Division", data$MSA), "1", "0") #Remove MSA = division
  data= subset(data, data$division != "1")
  
  ##Manually Remove MSAs Todd decided to take out bc they are out of date##
  ##The problem is these may have different symbols and things##
  data= subset(data, data$MSA != "Bergen-Passaic, NJ")
  data= subset(data, data$MSA != "Gary, IN")
  data= subset(data, data$MSA != "Middlesex-Somerset-Hunterdon, NJ")
  data= subset(data, data$MSA != "Middlesex, NJ")
  data= subset(data, data$MSA != "Monmouth-Ocean City, NJ")
  data= subset(data, data$MSA != "Monmouth-Ocean, NJ")
  data= subset(data, data$MSA != "Nassau-Suffolk, NY")
  data= subset(data, data$MSA != "Nassau-Suffolk, NJ")
  data= subset(data, data$MSA != "West Palm Beach-Boca Raton, FL")
  data= subset(data, data$MSA != "West Palm Beach, FL")
  
  #Update 2-8-24
  data= subset(data, data$MSA != "San Juan–Caguas–Guaynabo, PRc")
  data= subset(data, data$MSA != "San Juan–Caguas–Guaynabo, PR")
  data= subset(data, data$MSA != "San Juan–Carolina–Caguas, PR")
  
  ##Rename a few MSAs that are causing problems
  data$MSA = if_else(data$MSA == "Charlotte–Gastonia–Concord, NC–SC", "Charlotte-Concord,NC-SC", data$MSA)
  data$MSA = if_else(data$MSA == "Philadelphia, PA–NJ–DE–MD", "Philadelphia-Camden, PA-NJ-DE-MD", data$MSA)
  data$MSA = if_else(data$MSA =="Portland–Vancouver–Beaverton, OR–WA", "Portland-Vancouver, OR-WA", data$MSA)
  data$MSA = if_else(data$MSA == "Washington, DC–VA–MD–WV", "Washington-Arlington-Alexandria, DC-VA-MD-WV", data$MSA)
  
  data$location = as.character(locations::get.cbsa.for.msa.name(data$MSA))
  
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
  
  data$outcome = outcome.mappings.v2[data$outcome]
  
  #Update 2/20/24: Removing data prior to 2014 for outcome = diagnosed.prevalence; Decided this in the calibration process.  Keeping year=2014.
  data <- data %>%
    mutate(year_helper = as.numeric(year))%>%
    mutate(drop_var = if_else((outcome=='diagnosed.prevalence' & year_helper < 2014), "1", "0")) #drop 1
  
  data = subset(data, data$drop_var != "1")
  ##
  
  #Update for March 2025- want to remove all diagnosed.prevalence data for Riverside MSA in 2015 or before
  data <- data %>%
    mutate(drop_riverside = as.character(ifelse(location == "C.40140" & (year=="2014"|year == "2015") & outcome=='diagnosed.prevalence', "1", "0")))%>%
    filter(drop_riverside != "1")
  
  
  data= as.data.frame(data)
  
  list(filename, data) 
})
################################################################################
                        ###MSA BY SEX AND AGE###
################################################################################
data.list.msa_sex_age.clean = lapply(data.list.msa_sex_age, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]] 
  
  data$division= ifelse(grepl("Division", data$MSA), "1", "0") #Remove MSA = division
  data= subset(data, data$division != "1")
  
  ##Manually Remove MSAs Todd decided to take out bc they are out of date##
  ##The problem is these may have different symbols and things##
  data= subset(data, data$MSA != "Bergen-Passaic, NJ")
  data= subset(data, data$MSA != "Gary, IN")
  data= subset(data, data$MSA != "Middlesex-Somerset-Hunterdon, NJ")
  data= subset(data, data$MSA != "Middlesex, NJ")
  data= subset(data, data$MSA != "Monmouth-Ocean City, NJ")
  data= subset(data, data$MSA != "Monmouth-Ocean, NJ")
  data= subset(data, data$MSA != "Nassau-Suffolk, NY")
  data= subset(data, data$MSA != "Nassau-Suffolk, NJ")
  data= subset(data, data$MSA != "West Palm Beach-Boca Raton, FL")
  data= subset(data, data$MSA != "West Palm Beach, FL")
  
  #Update 2-8-24
  data= subset(data, data$MSA != "San Juan–Caguas–Guaynabo, PRc")
  data= subset(data, data$MSA != "San Juan–Caguas–Guaynabo, PR")
  data= subset(data, data$MSA != "San Juan–Carolina–Caguas, PR")
  
  ##Rename a few MSAs that are causing problems
  data$MSA = if_else(data$MSA == "Charlotte–Gastonia–Concord, NC–SC", "Charlotte-Concord,NC-SC", data$MSA)
  data$MSA = if_else(data$MSA == "Philadelphia, PA–NJ–DE–MD", "Philadelphia-Camden, PA-NJ-DE-MD", data$MSA)
  data$MSA = if_else(data$MSA =="Portland–Vancouver–Beaverton, OR–WA", "Portland-Vancouver, OR-WA", data$MSA)
  data$MSA = if_else(data$MSA == "Washington, DC–VA–MD–WV", "Washington-Arlington-Alexandria, DC-VA-MD-WV", data$MSA)
  
  data$location = as.character(locations::get.cbsa.for.msa.name(data$MSA))
  
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
    
    data$diagnoses_num_1 = (gsub(",", "", data$diagnoses_num_1)) #replace comma with nothing
    data$diagnoses_num_1 = (gsub(" ", "", data$diagnoses_num_1)) #replace space with nothing
    data$diagnoses_num_1 = (gsub("[[:punct:]]", NA, data$diagnoses_num_1)) #if there's a dash replace with NA
    data$diagnoses_1 = as.numeric(data$diagnoses_num_1) #make it a number
    
    data$diagnoses_num_2 = (gsub(",", "", data$diagnoses_num_2)) 
    data$diagnoses_num_2 = (gsub(" ", "", data$diagnoses_num_2)) 
    data$diagnoses_num_2 = (gsub("[[:punct:]]", NA, data$diagnoses_num_2)) 
    data$diagnoses_2 = as.numeric(data$diagnoses_num_2)
    
    data$diagnoses_num_3 = (gsub(",", "", data$diagnoses_num_3)) 
    data$diagnoses_num_3 = (gsub(" ", "", data$diagnoses_num_3)) 
    data$diagnoses_num_3 = (gsub("[[:punct:]]", NA, data$diagnoses_num_3)) 
    data$diagnoses_3 = as.numeric(data$diagnoses_num_3)
    
    data$diagnoses_num_4 = (gsub(",", "", data$diagnoses_num_4)) 
    data$diagnoses_num_4 = (gsub(" ", "", data$diagnoses_num_4)) 
    data$diagnoses_num_4 = (gsub("[[:punct:]]", NA, data$diagnoses_num_4)) 
    data$diagnoses_4 = as.numeric(data$diagnoses_num_4)
    
    data$diagnoses_num_5 = (gsub(",", "", data$diagnoses_num_5)) 
    data$diagnoses_num_5 = (gsub(" ", "", data$diagnoses_num_5)) 
    data$diagnoses_num_5 = (gsub("[[:punct:]]", NA, data$diagnoses_num_5)) 
    data$diagnoses_5 = as.numeric(data$diagnoses_num_5)
  }
  
  if(grepl("prevalence", filename)){
    data$prevalence_num_1 = (gsub(",", "", data$prevalence_num_1)) #replace comma with nothing
    data$prevalence_num_1 = (gsub(" ", "", data$prevalence_num_1)) #replace space with nothing
    data$prevalence_num_1 = (gsub("[[:punct:]]", NA, data$prevalence_num_1)) #if there's a dash replace with NA
    data$prevalence_1 = as.numeric(data$prevalence_num_1)
    
    data$prevalence_num_2 = (gsub(",", "", data$prevalence_num_2)) 
    data$prevalence_num_2 = (gsub(" ", "", data$prevalence_num_2)) 
    data$prevalence_num_2 = (gsub("[[:punct:]]", NA, data$prevalence_num_2)) 
    data$prevalence_2 = as.numeric(data$prevalence_num_2)
    
    data$prevalence_num_3 = (gsub(",", "", data$prevalence_num_3)) 
    data$prevalence_num_3 = (gsub(" ", "", data$prevalence_num_3)) 
    data$prevalence_num_3 = (gsub("[[:punct:]]", NA, data$prevalence_num_3)) 
    data$prevalence_3 = as.numeric(data$prevalence_num_3)
    
    data$prevalence_num_4 = (gsub(",", "", data$prevalence_num_4)) 
    data$prevalence_num_4 = (gsub(" ", "", data$prevalence_num_4)) 
    data$prevalence_num_4 = (gsub("[[:punct:]]", NA, data$prevalence_num_4)) 
    data$prevalence_4 = as.numeric(data$prevalence_num_4)
    
    data$prevalence_num_5 = (gsub(",", "", data$prevalence_num_5)) 
    data$prevalence_num_5 = (gsub(" ", "", data$prevalence_num_5)) 
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
  data$outcome = outcome.mappings.v2[data$outcome]
  
  #Update 2/20/24: Removing data prior to 2014 for outcome = diagnosed.prevalence; Decided this in the calibration process.  Keeping year=2014.
  data <- data %>%
    mutate(year_helper = as.numeric(year))%>%
    mutate(drop_var = if_else((outcome=='diagnosed.prevalence' & year_helper < 2014), "1", "0")) #drop 1
  
  data = subset(data, data$drop_var != "1")
  ##
  
  #Update for March 2025- want to remove all diagnosed.prevalence data for Riverside MSA in 2015 or before
  data <- data %>%
    mutate(drop_riverside = as.character(ifelse(location == "C.40140" & (year=="2014"|year == "2015") & outcome=='diagnosed.prevalence', "1", "0")))%>%
    filter(drop_riverside != "1")
  
  
  data= as.data.frame(data)
  
  list(filename, data) 
  
})
################################################################################
                        ###BEFORE 2009 FILES###
      ###Note: these files have AIDS and HIV data differentiated######
################################################################################
data.list.msa_2009.clean = lapply(data.list.msa_2009, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]]
  
  if(grepl("state", filename)){
     data$location = state.abb[match(data$state, state.name)]
     data$location = ifelse(data$state == "District of Columbia", "DC", data$location)
  }
  
  if(grepl("cbsa", filename)){ #using cbsa bc msa is in the filename 2x so it causes an error
      
  data$division= ifelse(grepl("Division", data$msa), "1", "0") #Remove MSA = division
  data= subset(data, data$division != "1")
  
  #This might cause problems
  data$msa <- gsub("MSA", "", data$msa)
  
  #Update 2/6/24: Decided to add these old MSAs back in so that data isn't compromised
   data$msa<- gsub("Bergen-Passaic, N.J.", "New York, NY",
gsub("Bergen-Passaic, NJ", "New York, NY",
gsub("Middlesex, NJ", "New York, NY",
gsub("Middlesex, N.J.", "New York, NY",
gsub("Middlesex-Somerset-Hunterdon, NJ", "New York, NY",
gsub("Orange County, Calif.", "Los Angeles, CA",
gsub("Orange County, CA", "Los Angeles, CA",
gsub("West Palm Beach, Fla.", "Miami, FL",
gsub("West Palm Beach, FL", "Miami, FL",
gsub("West Palm Beach-Boca Raton, FL", "Miami, FL",
gsub("Nassau-Suffolk, N.Y.", "New York, NY",
gsub("Nassau-Suffolk, NJ", "New York, NY",
gsub("Nassau-Suffolk, NY", "New York, NY",
gsub("Nassau/Suffolk, N.Y.", "New York, NY",
gsub("Monmouth-Ocean City, N.J.", "New York, NY",
gsub("Monmouth-Ocean, N.J.", "New York, NY",
gsub("Monmouth-Ocean, NJ", "New York, NY",
gsub("Gary, Ind.", "Chicago, IL",
gsub("Gary, IN", "Chicago, IL",
gsub("Charlotte–Gastonia–Concord, NC–SC", "Charlotte, NC",
gsub("Charlotte-Gast.-Rock Hill, NC-SC", "Charlotte, NC",
gsub("Charlotte, N.C.", "Charlotte, NC",
gsub("Charlotte-Gastonia-Concord, NC-SC", "Charlotte, NC",
gsub("Philadelphia, PA–NJ–DE–MD", "Philadelphia, PA",
gsub("Philadelphia, Pa.", "Philadelphia, PA",
gsub("Philadelphia, PA-NJ", "Philadelphia, PA",
gsub("Philadelphia, Pa–NJ–Del–Md", "Philadelphia, PA",
gsub("Phil., PA-NJ-DE-MD", "Philadelphia, PA",
gsub("Portland–Vancouver–Beaverton, OR–WA", "Portland, OR",
gsub("Portland–Vancouver–Beaverton, Ore–Wash", "Portland, OR",
gsub("Portland-Vancouver-Beaverton, OR-WA", "Portland, OR",
gsub("Portland-Vancourver, OR-WA", "Portland, OR",
gsub("Portland, Oreg","Portland, OR",
gsub("Washington, DC-MD-VA-WV", "Washington, DC",
gsub("Washington, D.C.", "Washington, DC",
gsub("Washington, DC–VA–MD–WV", "Washington, DC",
gsub("Washington, DC–Va–Md–WV", "Washington, DC",
gsub("Washington, DC-VA-MD-WV ", "Washington, DC",
gsub("Las Vegas, NV-AZ", "Las Vegas, NV",
gsub("Wilmington, Del.", "Wilmington, DE",
gsub("Wilmington-Newark, DE-MD", "Wilmington, DE",
gsub("Norfolk, Va.", "Norfolk, VA",
gsub( "Springfield, MA Necma", "Springfield, MA",
gsub("Mc Allen-Edinburg-Mission, TX", "McAllen-Edinburg-Mission",
gsub("Detroit MI", "Detroit", 
gsub("Louisiville, KY-IN", "Louisville, KY",  data$msa))))))))))))))))))))))))))))))))))))))))))))))

  #Removing Puerto Rico
  data= subset(data, data$msa != "San Juan, P.R.")
  data= subset(data, data$msa != "San Juan–Caguas–Guaynabo, PR")
  data= subset(data, data$msa != "San Juan-Bayamon, PR")
 ##############################################################################
  data$location = as.character(locations::get.cbsa.for.msa.name(data$msa))
  }
  
  #Removing locations that do not work- they are just variations of those listed above#
  # data$check_loc = locations::is.location.valid(data$location)
  # data= subset(data, data$check_loc == "TRUE")

  ##add year section##
  if(grepl("1993 new 1992", filename)){
     data$diagnoses_1992 =  data$new_no_1992
      
    data$diagnoses_1993 = data$new_no_1993
    data$prevalence_1992 = data$prev_total

    data$diagnoses_1992 = as.numeric(gsub(",",'', data$diagnoses_1992))
    data$diagnoses_1993 = as.numeric(gsub(",",'', data$diagnoses_1993))
    data$prevalence_1992 = as.numeric(gsub(",",'', data$prevalence_1992))
    data$type = "aids"
  }
  if(grepl("1994 new 1993", filename)){
    data$diagnoses_1994 = data$new_no_1994
    data$prevalence_1993 = data$prev_total

    data$diagnoses_1994 = as.numeric(gsub(",",'', data$diagnoses_1994))
    data$prevalence_1993 = as.numeric(gsub(",",'', data$prevalence_1993))
    data$type = "aids"
  }
  if(grepl("1995 new 1994", filename)){
    data$diagnoses_1995 = data$new_no_1995
    data$prevalence_1994 = data$prev_total

    data$diagnoses_1995 = as.numeric(gsub(",",'', data$diagnoses_1995))
    data$prevalence_1994 = as.numeric(gsub(",",'', data$prevalence_1994))
    data$type = "aids"
  }
  if(grepl("1996 new 1995", filename)){
    data$diagnoses_1996 = data$new_no_1996
    data$prevalence_1995 = data$prev_total

    data$diagnoses_1996 = as.numeric(gsub(",",'', data$diagnoses_1996))
    data$prevalence_1995 = as.numeric(gsub(",",'', data$prevalence_1995))
    data$type = "aids"
  }
  if(grepl("1997 new 1996", filename)){
    data$diagnoses_1997 = data$new_no_1997
    data$prevalence_1996 = data$prev_total

    data$diagnoses_1997 = as.numeric(gsub(",",'', data$diagnoses_1997))
    data$prevalence_1996 = as.numeric(gsub(",",'', data$prevalence_1996))
    data$type = "aids"
  }
  if(grepl("1998 new 1997", filename)){
    data$diagnoses_1998 = data$new_no_1998
    data$prevalence_1997 = data$prev_total

    data$diagnoses_1998 = as.numeric(gsub(",",'', data$diagnoses_1998))
    data$prevalence_1997 = as.numeric(gsub(",",'', data$prevalence_1997))
    data$type = "aids"
  }
  if(grepl("1999 new 1998", filename)){
    data$diagnoses_1999 = data$new_no_1999
    data$prevalence_1998 = data$prev_total

    data$diagnoses_1999 = as.numeric(gsub(",",'', data$diagnoses_1999))
    data$prevalence_1998 = as.numeric(gsub(",",'', data$prevalence_1998))
    data$type = "aids"
  }
  if(grepl("2000 new 1999", filename)){
    data$diagnoses_2000 = data$new_no_2000
    data$prevalence_1999 = data$prev_total

    data$diagnoses_2000 = as.numeric(gsub(",",'', data$diagnoses_2000))
    data$prevalence_1999 = as.numeric(gsub(",",'', data$prevalence_1999))
    data$type = "aids"
  }
  if(grepl("2001 new 2000", filename)){
    data$diagnoses_2001 = data$new_no_2001
    data$prevalence_2000 = data$prev_total

    data$diagnoses_2001 = as.numeric(gsub(",",'', data$diagnoses_2001))
    data$prevalence_2000 = as.numeric(gsub(",",'', data$prevalence_2000))
    data$type = "aids"
  }
  if(grepl("2002 new 2001", filename)){
    data$diagnoses_2002 = data$new_no_2002
    data$prevalence_2001 = data$prev_total

    data$diagnoses_2002 = as.numeric(gsub(",",'', data$diagnoses_2002))
    data$prevalence_2001 = as.numeric(gsub(",",'', data$prevalence_2001))
    data$type = "aids"
  }
  if(grepl("2003 new 2002", filename)){
    data$diagnoses_2003 = data$new_no_2003
    data$prevalence_2002 = data$prev_total
    data$diagnoses_2003 = as.numeric(gsub(",",'', data$diagnoses_2003))
    data$prevalence_2002 = as.numeric(gsub(",",'', data$prevalence_2002))
    data$type = "aids"
  }
  if(grepl("2004 new 2003", filename)){
    data$diagnoses_2004 = data$new_no_2004
    data$prevalence_2003 = data$prev_total

    data$diagnoses_2004 = as.numeric(gsub(",",'', data$diagnoses_2004))
    data$prevalence_2003 = as.numeric(gsub(",",'', data$prevalence_2003))
    data$type = "aids"
  }
  if(grepl("2005 new 2004", filename)){
    data$diagnoses_2005 = data$new_no_2005
    data$prevalence_2004 = data$prev_total
    data$diagnoses_2005 = as.numeric(gsub(",",'', data$diagnoses_2005))
    data$prevalence_2004 = as.numeric(gsub(",",'', data$prevalence_2004))
    data$type = "aids"
  }
  if(grepl("2006 new 2005", filename)){
    data$diagnoses_2006 = data$new_no_2006
    data$prevalence_2005 = data$prev_total
    data$diagnoses_2006 = as.numeric(gsub(",",'', data$diagnoses_2006))
    data$prevalence_2005 = as.numeric(gsub(",",'', data$prevalence_2005))
    data$type = "aids"
  }
  if(grepl("2007 new 2006", filename)){
    data$diagnoses_2007 = data$new_no_2007
    data$prevalence_2006 = data$prev_total
    data$diagnoses_2007 = as.numeric(gsub(",",'', data$diagnoses_2007))
    data$prevalence_2006 = as.numeric(gsub(",",'', data$prevalence_2006))
    data$type = "aids"
  }

  if(grepl("2008 new 2007", filename)){
    data$diagnoses_2008 = data$new_num
    data$diagnoses_2008 = as.numeric(gsub(",","", data$diagnoses_2008))
    data$prevalence_2007 = data$prev_num
    data$prevalence_2007 = gsub(",","", data$prevalence_2007)
    data$prevalence_2007 = gsub("[[:punct:]]", NA, data$prevalence_2007)
    data$prevalence_2007 = as.numeric(data$prevalence_2007)
    data$type = "hiv"
  }
  if(grepl("2009 new 2008", filename)){
    data$diagnoses_2009 = data$new_num
    data$prevalence_2008 = data$prev_num
    data$diagnoses_2009 = as.numeric(gsub(",",'', data$diagnoses_2009))
    data$prevalence_2008 = as.numeric(gsub(",",'', data$prevalence_2008))
    data$type = "hiv"
  }

 data <- data %>%
   select(location,(one_of("diagnoses_1992", "diagnoses_1993", "diagnoses_1994", "diagnoses_1995","diagnoses_1996", "diagnoses_1997", "diagnoses_1998", "diagnoses_1999",
                                  "diagnoses_2000", "diagnoses_2001", "diagnoses_2002", "diagnoses_2003", "diagnoses_2004", "diagnoses_2005",
                                  "diagnoses_2006", "diagnoses_2007", "diagnoses_2008", "diagnoses_2009", "prevalence_1992", "prevalence_1993", "prevalence_1994", "prevalence_1995",
                                  "prevalence_1996", "prevalence_1997", "prevalence_1998", "prevalence_1999", "prevalence_2000", "prevalence_2001", "prevalence_2002",
                                  "prevalence_2003", "prevalence_2004", "prevalence_2005","prevalence_2006", "prevalence_2007", "prevalence_2008")))

     data <- data %>%
       pivot_longer(cols=c(one_of("diagnoses_1992", "diagnoses_1993", "diagnoses_1994", "diagnoses_1995","diagnoses_1996", "diagnoses_1997", "diagnoses_1998", "diagnoses_1999",
                                 "diagnoses_2000", "diagnoses_2001", "diagnoses_2002", "diagnoses_2003", "diagnoses_2004", "diagnoses_2005",
                                 "diagnoses_2006", "diagnoses_2007", "diagnoses_2008", "diagnoses_2009", "prevalence_1992", "prevalence_1993", "prevalence_1994", "prevalence_1995",
                                 "prevalence_1996", "prevalence_1997", "prevalence_1998", "prevalence_1999", "prevalence_2000", "prevalence_2001", "prevalence_2002",
                                 "prevalence_2003", "prevalence_2004", "prevalence_2005","prevalence_2006", "prevalence_2007", "prevalence_2008")),
       names_to = c("outcome", "year"),
       names_sep = "_",
      values_to = "value")


 #Differentiate AIDs as outcome for files before 2007-2008 change
     if(grepl("1993 new 1992", filename)){
      data$outcome_new = ifelse(data$outcome == "prevalence", "aids.diagnosed.prevalence", "aids.diagnoses")
      data <- data %>%
        select(-c(outcome))
      data$outcome= data$outcome_new

     }
     if(grepl("1994 new 1993", filename)){
       data$outcome_new = ifelse(data$outcome == "prevalence", "aids.diagnosed.prevalence", "aids.diagnoses")
       data <- data %>%
         select(-c(outcome))
       data$outcome= data$outcome_new
     }
     if(grepl("1995 new 1994", filename)){
       data$outcome_new = ifelse(data$outcome == "prevalence", "aids.diagnosed.prevalence", "aids.diagnoses")
       data <- data %>%
         select(-c(outcome))
       data$outcome= data$outcome_new
     }
     if(grepl("1996 new 1995", filename)){
       data$outcome_new = ifelse(data$outcome == "prevalence", "aids.diagnosed.prevalence", "aids.diagnoses")
       data <- data %>%
         select(-c(outcome))
       data$outcome= data$outcome_new
     }
     if(grepl("1997 new 1996", filename)){
       data$outcome_new = ifelse(data$outcome == "prevalence", "aids.diagnosed.prevalence", "aids.diagnoses")
       data <- data %>%
         select(-c(outcome))
       data$outcome= data$outcome_new
     }
     if(grepl("1998 new 1997", filename)){
       data$outcome_new = ifelse(data$outcome == "prevalence", "aids.diagnosed.prevalence", "aids.diagnoses")
       data <- data %>%
         select(-c(outcome))
       data$outcome= data$outcome_new
     }
     if(grepl("1999 new 1998", filename)){
       data$outcome_new = ifelse(data$outcome == "prevalence", "aids.diagnosed.prevalence", "aids.diagnoses")
       data <- data %>%
         select(-c(outcome))
       data$outcome= data$outcome_new
     }
     if(grepl("2000 new 1999", filename)){
       data$outcome_new = ifelse(data$outcome == "prevalence", "aids.diagnosed.prevalence", "aids.diagnoses")
       data <- data %>%
         select(-c(outcome))
       data$outcome= data$outcome_new
     }
     if(grepl("2001 new 2000", filename)){
       data$outcome_new = ifelse(data$outcome == "prevalence", "aids.diagnosed.prevalence", "aids.diagnoses")
       data <- data %>%
         select(-c(outcome))
       data$outcome= data$outcome_new
     }
     if(grepl("2002 new 2001", filename)){
       data$outcome_new = ifelse(data$outcome == "prevalence", "aids.diagnosed.prevalence", "aids.diagnoses")
       data <- data %>%
         select(-c(outcome))
       data$outcome= data$outcome_new
     }
     if(grepl("2003 new 2002", filename)){
       data$outcome_new = ifelse(data$outcome == "prevalence", "aids.diagnosed.prevalence", "aids.diagnoses")
       data <- data %>%
         select(-c(outcome))
       data$outcome= data$outcome_new
     }
     if(grepl("2004 new 2003", filename)){
       data$outcome_new = ifelse(data$outcome == "prevalence", "aids.diagnosed.prevalence", "aids.diagnoses")
       data <- data %>%
         select(-c(outcome))
       data$outcome= data$outcome_new
     }
     if(grepl("2005 new 2004", filename)){
       data$outcome_new = ifelse(data$outcome == "prevalence", "aids.diagnosed.prevalence", "aids.diagnoses")
       data <- data %>%
         select(-c(outcome))
       data$outcome= data$outcome_new
     }
     if(grepl("2006 new 2005", filename)){
       data$outcome_new = ifelse(data$outcome == "prevalence", "aids.diagnosed.prevalence", "aids.diagnoses")
       data <- data %>%
         select(-c(outcome))
       data$outcome= data$outcome_new
     }
     if(grepl("2007 new 2006", filename)){
       data$outcome_new = ifelse(data$outcome == "prevalence", "aids.diagnosed.prevalence", "aids.diagnoses")
       data <- data %>%
         select(-c(outcome))
       data$outcome= data$outcome_new
     }
     
  data$outcome = outcome.mappings.v2[data$outcome]
  
  #Update 2/20/24: Removing data prior to 2014 for outcome = diagnosed.prevalence; Decided this in the calibration process.  Keeping year=2014.
  data <- data %>%
    mutate(year_helper = as.numeric(year))%>%
    mutate(drop_var = if_else((outcome=='diagnosed.prevalence' & year_helper < 2014), "1", "0")) #drop 1
  
  data = subset(data, data$drop_var != "1")
  ##
  
  #6.3.24: Locations I had renamed above need to be summed together or else they are overwritten:
  data <- data %>%
    group_by(year, location, outcome)%>%
    mutate(value.adjusted = sum(value))%>% #Check that this is right and then rename
    select(year, outcome, location, value.adjusted)%>%
    rename(value = value.adjusted)
  
  data<- data[!duplicated(data), ] #This will take out the dup values
  
  #Update for March 2025- want to remove all diagnosed.prevalence data for Riverside MSA in 2015 or before
  data <- data %>%
    mutate(drop_riverside = as.character(ifelse(location == "C.40140" & (year=="2014"|year == "2015") & outcome=='diagnosed.prevalence', "1", "0")))%>%
    filter(drop_riverside != "1")
  
  
  
  data= as.data.frame(data)
  
  list(filename, data) 
})

################################################################################
                          ###MSA BY SEX AND RACE###
################################################################################
data.list.msa_sex_race.clean = lapply(data.list.msa_sex_race, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]] 
  
  data$division= ifelse(grepl("Division", data$MSA), "1", "0") #Remove MSA = division
  data= subset(data, data$division != "1")
  
  ##Manually Remove MSAs Todd decided to take out bc they are out of date##
  ##The problem is these may have different symbols and things##
  data= subset(data, data$MSA != "Bergen-Passaic, NJ")
  data= subset(data, data$MSA != "Gary, IN")
  data= subset(data, data$MSA != "Middlesex-Somerset-Hunterdon, NJ")
  data= subset(data, data$MSA != "Middlesex, NJ")
  data= subset(data, data$MSA != "Monmouth-Ocean City, NJ")
  data= subset(data, data$MSA != "Monmouth-Ocean, NJ")
  data= subset(data, data$MSA != "Nassau-Suffolk, NY")
  data= subset(data, data$MSA != "Nassau-Suffolk, NJ")
  data= subset(data, data$MSA != "West Palm Beach-Boca Raton, FL")
  data= subset(data, data$MSA != "West Palm Beach, FL")
  
  #Update 2-8-24
  data= subset(data, data$MSA != "San Juan–Caguas–Guaynabo, PRc")
  data= subset(data, data$MSA != "San Juan–Caguas–Guaynabo, PR")
  data= subset(data, data$MSA != "San Juan–Carolina–Caguas, PR")
  data= subset(data, data$MSA != "San Juan–Carolina–Caguas, PRd")
  data= subset(data, data$MSA != "San Juan–Carolina–Caguas, PRe")
  data= subset(data, data$MSA != "San Juan–Caguas–Guaynabo, PRd")
  data= subset(data, data$MSA != "San Juan–Caguas–Guaynabo, PRe")
  
  
  ##Rename a few MSAs that are causing problems
  data$MSA = if_else(data$MSA == "Charlotte–Gastonia–Concord, NC–SC", "Charlotte-Concord,NC-SC", data$MSA)
  data$MSA = if_else(data$MSA == "Philadelphia, PA–NJ–DE–MD", "Philadelphia-Camden, PA-NJ-DE-MD", data$MSA)
  data$MSA = if_else(data$MSA =="Portland–Vancouver–Beaverton, OR–WA", "Portland-Vancouver, OR-WA", data$MSA)
  data$MSA = if_else(data$MSA == "Washington, DC–VA–MD–WV", "Washington-Arlington-Alexandria, DC-VA-MD-WV", data$MSA)
  
  data$location = as.character(locations::get.cbsa.for.msa.name(data$MSA))
  
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
    
    data$diagnoses_num_1 = (gsub(",", "", data$diagnoses_num_1)) #replace comma with nothing
    data$diagnoses_num_1 = (gsub(" ", "", data$diagnoses_num_1)) #replace space with nothing
    data$diagnoses_num_1 = (gsub("[[:punct:]]", NA, data$diagnoses_num_1)) #if there's a dash replace with NA
    data$diagnoses_1 = as.numeric(data$diagnoses_num_1) #make it a number
    
    data$diagnoses_num_2 = (gsub(",", "", data$diagnoses_num_2)) 
    data$diagnoses_num_2 = (gsub(" ", "", data$diagnoses_num_2)) 
    data$diagnoses_num_2 = (gsub("[[:punct:]]", NA, data$diagnoses_num_2)) 
    data$diagnoses_2 = as.numeric(data$diagnoses_num_2)
    
    data$diagnoses_num_3 = (gsub(",", "", data$diagnoses_num_3)) 
    data$diagnoses_num_3 = (gsub(" ", "", data$diagnoses_num_3)) 
    data$diagnoses_num_3 = (gsub("[[:punct:]]", NA, data$diagnoses_num_3)) 
    data$diagnoses_3 = as.numeric(data$diagnoses_num_3)
    
    data$diagnoses_num_4 = (gsub(",", "", data$diagnoses_num_4)) 
    data$diagnoses_num_4 = (gsub(" ", "", data$diagnoses_num_4)) 
    data$diagnoses_num_4 = (gsub("[[:punct:]]", NA, data$diagnoses_num_4)) 
    data$diagnoses_4 = as.numeric(data$diagnoses_num_4)
    
    data$diagnoses_num_5 = (gsub(",", "", data$diagnoses_num_5)) 
    data$diagnoses_num_5 = (gsub(" ", "", data$diagnoses_num_5)) 
    data$diagnoses_num_5 = (gsub("[[:punct:]]", NA, data$diagnoses_num_5)) 
    data$diagnoses_5 = as.numeric(data$diagnoses_num_5)
  }
  
  if(grepl("prevalence", filename)){
    data$prevalence_num_1 = (gsub(",", "", data$prevalence_num_1)) #replace comma with nothing
    data$prevalence_num_1 = (gsub(" ", "", data$prevalence_num_1)) #replace space with nothing
    data$prevalence_num_1 = (gsub("[[:punct:]]", NA, data$prevalence_num_1)) #if there's a dash replace with NA
    data$prevalence_1 = as.numeric(data$prevalence_num_1)
    
    data$prevalence_num_2 = (gsub(",", "", data$prevalence_num_2)) 
    data$prevalence_num_2 = (gsub(" ", "", data$prevalence_num_2)) 
    data$prevalence_num_2 = (gsub("[[:punct:]]", NA, data$prevalence_num_2)) 
    data$prevalence_2 = as.numeric(data$prevalence_num_2)
    
    data$prevalence_num_3 = (gsub(",", "", data$prevalence_num_3)) 
    data$prevalence_num_3 = (gsub(" ", "", data$prevalence_num_3)) 
    data$prevalence_num_3 = (gsub("[[:punct:]]", NA, data$prevalence_num_3)) 
    data$prevalence_3 = as.numeric(data$prevalence_num_3)
    
    data$prevalence_num_4 = (gsub(",", "", data$prevalence_num_4)) 
    data$prevalence_num_4 = (gsub(" ", "", data$prevalence_num_4)) 
    data$prevalence_num_4 = (gsub("[[:punct:]]", NA, data$prevalence_num_4)) 
    data$prevalence_4 = as.numeric(data$prevalence_num_4)
    
    data$prevalence_num_5 = (gsub(",", "", data$prevalence_num_5)) 
    data$prevalence_num_5 = (gsub(" ", "", data$prevalence_num_5)) 
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
data$outcome = outcome.mappings.v2[data$outcome]

#Update 2/20/24: Removing data prior to 2014 for outcome = diagnosed.prevalence; Decided this in the calibration process.  Keeping year=2014.
data <- data %>%
  mutate(year_helper = as.numeric(year))%>%
  mutate(drop_var = if_else((outcome=='diagnosed.prevalence' & year_helper < 2014), "1", "0")) #drop 1

data = subset(data, data$drop_var != "1")
##

#Update for March 2025- want to remove all diagnosed.prevalence data for Riverside MSA in 2015 or before
data <- data %>%
  mutate(drop_riverside = as.character(ifelse(location == "C.40140" & (year=="2014"|year == "2015") & outcome=='diagnosed.prevalence', "1", "0")))%>%
  filter(drop_riverside != "1")

   
data= as.data.frame(data)

list(filename, data) 

})

################################################################################
                    ###MSA BY RACE AND RISK##
################################################################################
data.list.msa_race_risk.clean = lapply(data.list.msa_race_risk, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]] 
  
  data$division= ifelse(grepl("Division", data$MSA), "1", "0") #Remove MSA = division
  data= subset(data, data$division != "1")
  
  ##Manually Remove MSAs Todd decided to take out bc they are out of date##
  ##The problem is these may have different symbols and things##
  data= subset(data, data$MSA != "Bergen-Passaic, NJ")
  data= subset(data, data$MSA != "Gary, IN")
  data= subset(data, data$MSA != "Middlesex-Somerset-Hunterdon, NJ")
  data= subset(data, data$MSA != "Middlesex, NJ")
  data= subset(data, data$MSA != "Monmouth-Ocean City, NJ")
  data= subset(data, data$MSA != "Monmouth-Ocean, NJ")
  data= subset(data, data$MSA != "Nassau-Suffolk, NY")
  data= subset(data, data$MSA != "Nassau-Suffolk, NJ")
  data= subset(data, data$MSA != "West Palm Beach-Boca Raton, FL")
  data= subset(data, data$MSA != "West Palm Beach, FL")
  
  #Update 2-8-24
  data= subset(data, data$MSA != "San Juan–Caguas–Guaynabo, PRc")
  data= subset(data, data$MSA != "San Juan–Caguas–Guaynabo, PR")
  data= subset(data, data$MSA != "San Juan–Carolina–Caguas, PR")
  data= subset(data, data$MSA != "San Juan–Carolina–Caguas, PRd")
  data= subset(data, data$MSA != "San Juan–Carolina–Caguas, PRe")
  data= subset(data, data$MSA != "San Juan–Caguas–Guaynabo, PRd")
  data= subset(data, data$MSA != "San Juan–Caguas–Guaynabo, PRe")


  ##Rename a few MSAs that are causing problems
  data$MSA = if_else(data$MSA == "Charlotte–Gastonia–Concord, NC–SC", "Charlotte-Concord,NC-SC", data$MSA)
  data$MSA = if_else(data$MSA == "Philadelphia, PA–NJ–DE–MD", "Philadelphia-Camden, PA-NJ-DE-MD", data$MSA)
  data$MSA = if_else(data$MSA =="Portland–Vancouver–Beaverton, OR–WA", "Portland-Vancouver, OR-WA", data$MSA)
  data$MSA = if_else(data$MSA == "Washington, DC–VA–MD–WV", "Washington-Arlington-Alexandria, DC-VA-MD-WV", data$MSA)
  
  data$location = as.character(locations::get.cbsa.for.msa.name(data$MSA))


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

  #Create Race#
  if(grepl("black", filename)){
    data$race="black/african american"
  }
  if(grepl("white", filename)){
    data$race="white"
  }
  if(grepl("hispanic", filename)){
    data$race="hispanic/latino"
  }

  if(grepl("new", filename)){

    data$diagnoses_1 = (gsub(",", "", data$diagnoses_1)) #replace comma with nothing
    data$diagnoses_1 = (gsub(" ", "", data$diagnoses_1)) #replace space with nothing
    data$diagnoses_1 = (gsub("[[:punct:]]", NA, data$diagnoses_1)) #if there's a dash replace with NA
    data$diagnoses_1 = as.numeric(data$diagnoses_1) #make it a number

    data$diagnoses_2 = (gsub(",", "", data$diagnoses_2))
    data$diagnoses_2 = (gsub(" ", "", data$diagnoses_2))
    data$diagnoses_2 = (gsub("[[:punct:]]", NA, data$diagnoses_2))
    data$diagnoses_2 = as.numeric(data$diagnoses_2)

    data$diagnoses_3 = (gsub(",", "", data$diagnoses_3))
    data$diagnoses_3 = (gsub(" ", "", data$diagnoses_3))
    data$diagnoses_3 = (gsub("[[:punct:]]", NA, data$diagnoses_3))
    data$diagnoses_3 = as.numeric(data$diagnoses_3)

    data$diagnoses_4 = (gsub(",", "", data$diagnoses_4))
    data$diagnoses_4 = (gsub(" ", "", data$diagnoses_4))
    data$diagnoses_4 = (gsub("[[:punct:]]", NA, data$diagnoses_4))
    data$diagnoses_4 = as.numeric(data$diagnoses_4)

    data$diagnoses_5 = (gsub(",", "", data$diagnoses_5))
    data$diagnoses_5 = (gsub(" ", "", data$diagnoses_5))
    data$diagnoses_5 = (gsub("[[:punct:]]", NA, data$diagnoses_5))
    data$diagnoses_5 = as.numeric(data$diagnoses_5)
  }

  if(grepl("prevalence", filename)){

    data$prevalence_1 = (gsub(",", "", data$prevalence_1)) #replace comma with nothing
    data$prevalence_1 = (gsub(" ", "", data$prevalence_1)) #replace space with nothing
    data$prevalence_1 = (gsub("[[:punct:]]", NA, data$prevalence_1)) #if there's a dash replace with NA
    data$prevalence_1 = as.numeric(data$prevalence_1) #make it a number

    data$prevalence_2 = (gsub(",", "", data$prevalence_2))
    data$prevalence_2 = (gsub(" ", "", data$prevalence_2))
    data$prevalence_2 = (gsub("[[:punct:]]", NA, data$prevalence_2))
    data$prevalence_2 = as.numeric(data$prevalence_2)

    data$prevalence_3 = (gsub(",", "", data$prevalence_3))
    data$prevalence_3 = (gsub(" ", "", data$prevalence_3))
    data$prevalence_3 = (gsub("[[:punct:]]", NA, data$prevalence_3))
    data$prevalence_3 = as.numeric(data$prevalence_3)

    data$prevalence_4 = (gsub(",", "", data$prevalence_4))
    data$prevalence_4 = (gsub(" ", "", data$prevalence_4))
    data$prevalence_4 = (gsub("[[:punct:]]", NA, data$prevalence_4))
    data$prevalence_4 = as.numeric(data$prevalence_4)

    data$prevalence_5 = (gsub(",", "", data$prevalence_5))
    data$prevalence_5 = (gsub(" ", "", data$prevalence_5))
    data$prevalence_5 = (gsub("[[:punct:]]", NA, data$prevalence_5))
    data$prevalence_5 = as.numeric(data$prevalence_5)
  }

  data <- data %>%
    select(year, location, race, one_of("diagnoses_1", "diagnoses_2", "diagnoses_3", "diagnoses_4", "diagnoses_5",
                                        "prevalence_1", "prevalence_2", "prevalence_3", "prevalence_4", "prevalence_5"))
  data <- data %>%
    pivot_longer(cols=c(one_of("diagnoses_1", "diagnoses_2", "diagnoses_3", "diagnoses_4", "diagnoses_5",
                               "prevalence_1", "prevalence_2", "prevalence_3", "prevalence_4", "prevalence_5")),
                 names_to = c("outcome", "risk"),
                 names_sep = "_",
                 values_to = "value")

  data$risk= risk.msa.mappings[data$risk]
  data$outcome = outcome.mappings.v2[data$outcome]
  
  #update 2-13-24, manually removing outlier risk data for 2013
  data = subset(data, data$year != "2013")
  data = subset(data, data$year != "2012")
  data = subset(data, data$year != "2011")
  data = subset(data, data$year != "2010")
  data = subset(data, data$year != "2009")
  
  #Update 2/20/24: Removing data prior to 2014 for outcome = diagnosed.prevalence; Decided this in the calibration process.  Keeping year=2014.
  #Adjusting on 3/22/24- diagnoses not for 2014 or earlier; diagnosed.prevalence not for 2013 or earlier (for risk only)
  data <- data %>%
    mutate(year_helper = as.numeric(year))%>%
    mutate(drop_var = if_else((outcome=='diagnosed.prevalence' & year_helper <= 2013), "1", "0"))%>% #drop 1
    mutate(drop_var_2 = if_else((outcome == 'diagnoses' & year_helper <= 2014), "1", "0"))
  
  data = subset(data, data$drop_var != "1")
  data = subset(data, data$drop_var_2 != "1")
  ##
  
  #Update for March 2025- want to remove all diagnosed.prevalence data for Riverside MSA in 2015 or before
  data <- data %>%
    mutate(drop_riverside = as.character(ifelse(location == "C.40140" & (year=="2014"|year == "2015") & outcome=='diagnosed.prevalence', "1", "0")))%>%
    filter(drop_riverside != "1")
  
  
  data= as.data.frame(data)
  
  list(filename, data) 
  
})
################################################################################
                      ###MSA BY SEX AND RISK###
################################################################################
data.list.msa_sex_risk.clean = lapply(data.list.msa_sex_risk, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]] 
  
  data$division= ifelse(grepl("Division", data$MSA), "1", "0") #Remove MSA = division
  data= subset(data, data$division != "1")
  
  ##Manually Remove MSAs Todd decided to take out bc they are out of date##
  ##The problem is these may have different symbols and things##
  data= subset(data, data$MSA != "Bergen-Passaic, NJ")
  data= subset(data, data$MSA != "Gary, IN")
  data= subset(data, data$MSA != "Middlesex-Somerset-Hunterdon, NJ")
  data= subset(data, data$MSA != "Middlesex, NJ")
  data= subset(data, data$MSA != "Monmouth-Ocean City, NJ")
  data= subset(data, data$MSA != "Monmouth-Ocean, NJ")
  data= subset(data, data$MSA != "Nassau-Suffolk, NY")
  data= subset(data, data$MSA != "Nassau-Suffolk, NJ")
  data= subset(data, data$MSA != "West Palm Beach-Boca Raton, FL")
  data= subset(data, data$MSA != "West Palm Beach, FL")
  
  #Update 2-8-24
  data= subset(data, data$MSA != "San Juan–Caguas–Guaynabo, PRc")
  data= subset(data, data$MSA != "San Juan–Caguas–Guaynabo, PR")
  data= subset(data, data$MSA != "San Juan–Carolina–Caguas, PR")
  
  ##Rename a few MSAs that are causing problems
  data$MSA = if_else(data$MSA == "Charlotte–Gastonia–Concord, NC–SC", "Charlotte-Concord,NC-SC", data$MSA)
  data$MSA = if_else(data$MSA == "Philadelphia, PA–NJ–DE–MD", "Philadelphia-Camden, PA-NJ-DE-MD", data$MSA)
  data$MSA = if_else(data$MSA =="Portland–Vancouver–Beaverton, OR–WA", "Portland-Vancouver, OR-WA", data$MSA)
  data$MSA = if_else(data$MSA == "Washington, DC–VA–MD–WV", "Washington-Arlington-Alexandria, DC-VA-MD-WV", data$MSA)
  
  data$location = as.character(locations::get.cbsa.for.msa.name(data$MSA))
  
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
  
  data$sex = ifelse(grepl("female", filename), "female", "male")
  
  if(grepl("new male", filename)){
    
    data$diagnoses_num_1 = (gsub(",", "", data$diagnoses_num_1)) #replace comma with nothing
    data$diagnoses_num_1 = (gsub(" ", "", data$diagnoses_num_1)) #replace space with nothing
    data$diagnoses_num_1 = (gsub("[[:punct:]]", NA, data$diagnoses_num_1)) #if there's a dash replace with NA
    data$diagnoses_1 = as.numeric(data$diagnoses_num_1) #make it a number
    
    data$diagnoses_num_2 = (gsub(",", "", data$diagnoses_num_2)) 
    data$diagnoses_num_2 = (gsub(" ", "", data$diagnoses_num_2)) 
    data$diagnoses_num_2 = (gsub("[[:punct:]]", NA, data$diagnoses_num_2)) 
    data$diagnoses_2 = as.numeric(data$diagnoses_num_2)
    
    data$diagnoses_num_3 = (gsub(",", "", data$diagnoses_num_3)) 
    data$diagnoses_num_3 = (gsub(" ", "", data$diagnoses_num_3)) 
    data$diagnoses_num_3 = (gsub("[[:punct:]]", NA, data$diagnoses_num_3)) 
    data$diagnoses_3 = as.numeric(data$diagnoses_num_3)
    
    data$diagnoses_num_4 = (gsub(",", "", data$diagnoses_num_4)) 
    data$diagnoses_num_4 = (gsub(" ", "", data$diagnoses_num_4)) 
    data$diagnoses_num_4 = (gsub("[[:punct:]]", NA, data$diagnoses_num_4)) 
    data$diagnoses_4 = as.numeric(data$diagnoses_num_4)
    
    data$diagnoses_num_5 = (gsub(",", "", data$diagnoses_num_5)) 
    data$diagnoses_num_5 = (gsub(" ", "", data$diagnoses_num_5)) 
    data$diagnoses_num_5 = (gsub("[[:punct:]]", NA, data$diagnoses_num_5)) 
    data$diagnoses_5 = as.numeric(data$diagnoses_num_5)
  }
  
  if(grepl("prevalence male", filename)){
    
    data$prevalence_num_1 = (gsub(",", "", data$prevalence_num_1)) #replace comma with nothing
    data$prevalence_num_1 = (gsub(" ", "", data$prevalence_num_1)) #replace space with nothing
    data$prevalence_num_1 = (gsub("[[:punct:]]", NA, data$prevalence_num_1)) #if there's a dash replace with NA
    data$prevalence_1 = as.numeric(data$prevalence_num_1)
    
    data$prevalence_num_2 = (gsub(",", "", data$prevalence_num_2)) 
    data$prevalence_num_2 = (gsub(" ", "", data$prevalence_num_2)) 
    data$prevalence_num_2 = (gsub("[[:punct:]]", NA, data$prevalence_num_2)) 
    data$prevalence_2 = as.numeric(data$prevalence_num_2)
    
    data$prevalence_num_3 = (gsub(",", "", data$prevalence_num_3)) 
    data$prevalence_num_3 = (gsub(" ", "", data$prevalence_num_3)) 
    data$prevalence_num_3 = (gsub("[[:punct:]]", NA, data$prevalence_num_3)) 
    data$prevalence_3 = as.numeric(data$prevalence_num_3)
    
    data$prevalence_num_4 = (gsub(",", "", data$prevalence_num_4)) 
    data$prevalence_num_4 = (gsub(" ", "", data$prevalence_num_4)) 
    data$prevalence_num_4 = (gsub("[[:punct:]]", NA, data$prevalence_num_4)) 
    data$prevalence_4 = as.numeric(data$prevalence_num_4)
    
    data$prevalence_num_5 = (gsub(",", "", data$prevalence_num_5)) 
    data$prevalence_num_5 = (gsub(" ", "", data$prevalence_num_5)) 
    data$prevalence_num_5 = (gsub("[[:punct:]]", NA, data$prevalence_num_5)) 
    data$prevalence_5 = as.numeric(data$prevalence_num_5)
    
  }
  if(grepl("new female", filename)){
    
    data$diagnoses_num_2 = (gsub(",", "", data$diagnoses_num_2)) 
    data$diagnoses_num_2 = (gsub(" ", "", data$diagnoses_num_2)) 
    data$diagnoses_num_2 = (gsub("[[:punct:]]", NA, data$diagnoses_num_2)) 
    data$diagnoses_2 = as.numeric(data$diagnoses_num_2)
    
    data$diagnoses_num_4 = (gsub(",", "", data$diagnoses_num_4)) 
    data$diagnoses_num_4 = (gsub(" ", "", data$diagnoses_num_4)) 
    data$diagnoses_num_4 = (gsub("[[:punct:]]", NA, data$diagnoses_num_4)) 
    data$diagnoses_4 = as.numeric(data$diagnoses_num_4)
    
    data$diagnoses_num_5 = (gsub(",", "", data$diagnoses_num_5)) 
    data$diagnoses_num_5 = (gsub(" ", "", data$diagnoses_num_5)) 
    data$diagnoses_num_5 = (gsub("[[:punct:]]", NA, data$diagnoses_num_5)) 
    data$diagnoses_5 = as.numeric(data$diagnoses_num_5)
  }
  if(grepl("prevalence female", filename)){
    
    data$prevalence_num_2 = (gsub(",", "", data$prevalence_num_2)) 
    data$prevalence_num_2 = (gsub(" ", "", data$prevalence_num_2)) 
    data$prevalence_num_2 = (gsub("[[:punct:]]", NA, data$prevalence_num_2)) 
    data$prevalence_2 = as.numeric(data$prevalence_num_2)
    
    data$prevalence_num_4 = (gsub(",", "", data$prevalence_num_4)) 
    data$prevalence_num_4 = (gsub(" ", "", data$prevalence_num_4)) 
    data$prevalence_num_4 = (gsub("[[:punct:]]", NA, data$prevalence_num_4)) 
    data$prevalence_4 = as.numeric(data$prevalence_num_4)
    
    data$prevalence_num_5 = (gsub(",", "", data$prevalence_num_5)) 
    data$prevalence_num_5 = (gsub(" ", "", data$prevalence_num_5)) 
    data$prevalence_num_5 = (gsub("[[:punct:]]", NA, data$prevalence_num_5)) 
    data$prevalence_5 = as.numeric(data$prevalence_num_5)
  }
  data <- data %>%
    select(year, location, sex, one_of("diagnoses_1", "diagnoses_2", "diagnoses_3", "diagnoses_4", "diagnoses_5",
                                       "prevalence_1", "prevalence_2", "prevalence_3", "prevalence_4", "prevalence_5"))
  data <- data %>%
    pivot_longer(cols=c(one_of("diagnoses_1", "diagnoses_2", "diagnoses_3", "diagnoses_4", "diagnoses_5",
                               "prevalence_1", "prevalence_2", "prevalence_3", "prevalence_4", "prevalence_5")),
                 names_to = c("outcome", "risk"),
                 names_sep = "_",
                 values_to = "value")
  
  data$risk= risk.msa.mappings[data$risk]
  data$outcome = outcome.mappings.v2[data$outcome]
  
  #update 2-13-24, manually removing outlier risk data for 2013
  data = subset(data, data$year != "2013")
  data = subset(data, data$year != "2012")
  data = subset(data, data$year != "2011")
  data = subset(data, data$year != "2010")
  data = subset(data, data$year != "2009")
  
  #Update 2/20/24: Removing data prior to 2014 for outcome = diagnosed.prevalence; Decided this in the calibration process.  Keeping year=2014.
  #Adjusting on 3/22/24- diagnoses not for 2014 or earlier; diagnosed.prevalence not for 2013 or earlier (for risk only)
  data <- data %>%
    mutate(year_helper = as.numeric(year))%>%
    mutate(drop_var = if_else((outcome=='diagnosed.prevalence' & year_helper <= 2013), "1", "0"))%>% #drop 1
    mutate(drop_var_2 = if_else((outcome == 'diagnoses' & year_helper <= 2014), "1", "0"))
  
  data = subset(data, data$drop_var != "1")
  data = subset(data, data$drop_var_2 != "1")
  ##
  
  #Update for March 2025- want to remove all diagnosed.prevalence data for Riverside MSA in 2015 or before
  data <- data %>%
    mutate(drop_riverside = as.character(ifelse(location == "C.40140" & (year=="2014"|year == "2015") & outcome=='diagnosed.prevalence', "1", "0")))%>%
    filter(drop_riverside != "1")
  
  
  data= as.data.frame(data)
  
  list(filename, data) 
})
################################################################################
                  ###Put Data into Data Manager###
################################################################################

##MSA Deaths
msa_deaths = lapply(data.list.msa_deaths.clean, `[[`, 2)

for (data in msa_deaths) {

  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.msa.reports',
    source = 'cdc.surveillance.reports',
    dimension.values = list(),
    url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
    details = 'CDC Reporting')
}

##MSA total
msa_total = lapply(data.list.msa_total.clean, `[[`, 2)

for (data in msa_total) {

  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.msa.reports',
    source = 'cdc.surveillance.reports',
    dimension.values = list(),
    url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
    details = 'CDC Reporting')
}

##MSA by Sex
msa_sex = lapply(data.list.msa_sex.clean , `[[`, 2)

for (data in msa_sex) {

  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.msa.reports',
    source = 'cdc.surveillance.reports',
    dimension.values = list(),
    url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
    details = 'CDC MSA Reports')
}

##MSA by Sex and age
msa_sex_age = lapply(data.list.msa_sex_age.clean , `[[`, 2)

for (data in msa_sex_age) {

  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.msa.reports',
    source = 'cdc.surveillance.reports',
    dimension.values = list(),
    url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
    details = 'CDC MSA Reports')
}

##MSA Data Before 2009
msa_2009 = lapply(data.list.msa_2009.clean , `[[`, 2)

for (data in msa_2009) {

  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.msa.reports',
    source = 'cdc.surveillance.reports',
    dimension.values = list(),
    url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
    details = 'CDC MSA Reports')
}

##MSA by Sex and Race
msa_sex_race = lapply(data.list.msa_sex_race.clean , `[[`, 2)

for (data in msa_sex_race) {

  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.msa.reports',
    source = 'cdc.surveillance.reports',
    dimension.values = list(),
    url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
    details = 'CDC MSA Reports')
}

##MSA by Race and Risk
msa_race_risk = lapply(data.list.msa_race_risk.clean , `[[`, 2)

for (data in msa_race_risk) {

  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.msa.reports',
    source = 'cdc.surveillance.reports',
    dimension.values = list(),
    url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
    details = 'CDC MSA Reports')
}

##MSA by Sex and Risk
msa_sex_risk = lapply(data.list.msa_sex_risk.clean, `[[`, 2)

for (data in msa_sex_risk) {

  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.msa.reports',
    source = 'cdc.surveillance.reports',
    dimension.values = list(),
    url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
    details = 'CDC MSA Reports')
}


###############################################################################
#Addition for 1-26-24
#Sum sex_age, sex_race, and sex_risk to get totals for Age, Race, Risk
###############################################################################
#AGE TOTAL
sex.age.df = lapply(data.list.msa_sex_age.clean, function(file){
  data=file[[2]] 
  filename = file[[1]] 
  data= as.data.frame(data)
})
age.total <- bind_rows(sex.age.df)
age.total <- age.total %>%
  group_by(outcome, year, location, age)%>%
  mutate(total = sum(value))%>%
  select(-sex, -value)%>%
  rename(value = total)
age.total<- age.total[!duplicated(age.total), ]

#RACE TOTAL
sex.race.df = lapply(data.list.msa_sex_race.clean, function(file){
  data=file[[2]] 
  filename = file[[1]] 
  data= as.data.frame(data)
})
race.total <- bind_rows(sex.race.df)
race.total <- race.total %>%
  group_by(outcome, year, location, race)%>%
  mutate(total = sum(value))%>%
  select(-sex, -value)%>%
  rename(value = total)
race.total<- race.total[!duplicated(race.total), ]

#RISK TOTAL
sex.risk.df = lapply(data.list.msa_sex_risk.clean, function(file){
  data=file[[2]] 
  filename = file[[1]] 
  data= as.data.frame(data)
})
risk.total <- bind_rows(sex.risk.df)
risk.total <- risk.total %>%
  group_by(outcome, year, location, risk)%>%
  mutate(total = sum(value))%>%
  select(-sex, -value)%>%
  rename(value = total)
risk.total<- risk.total[!duplicated(risk.total), ]

###############################################################################
#Put Age, Race, Risk totals into data manager
###############################################################################

totals = list(
  "age.total" = as.data.frame(age.total), 
  "race.total"= as.data.frame(race.total), 
  "risk.total"=as.data.frame(risk.total))

for (data in totals) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.msa.reports',
    source = 'cdc.surveillance.reports',
    dimension.values = list(),
    url = 'https://www.cdc.gov/nchhstp/atlas/index.htm',
    details = 'CDC MSA Reports')
}
