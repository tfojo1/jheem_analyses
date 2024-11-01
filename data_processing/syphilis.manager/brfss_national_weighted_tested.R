#This is going into the Syphilis Manager for national proportion.msm data
#And for national proportion.tested

#library(haven)

################################################################################
###Read in BRFSS .xpt files
################################################################################

DATA.DIR.BRFSS.STATE="../../data_raw/brfss/brfss_state"

brfss_file_state <- list.files(DATA.DIR.BRFSS.STATE, pattern = ".XPT", full.names = "TRUE")

brfss_file_state_list <- lapply(brfss_file_state, function(x) {
  list(filename=x, data=read_xpt(x))
})

################################################################################
###Create mappings
################################################################################
brfss.sex.mappings = c('1' = 'male',
                       '2' = 'female',
                       '7' = NA,
                       '9' = NA)

brfss.race.mappings= c('1'= "white",
                       '2'= "black",
                       '3'= 'american indian/alaska native',
                       '4'= 'asian',
                       '5'= 'native hawaiian/other pacific islander',
                       '6'= 'other race',
                       '7'= 'multiracial', 
                       '8'= 'hispanic',
                       '9'= 'unknown')
brfss.age.mappings= c('1'= '18-24 years',
                      '2'= '25-29 years',
                      '3'='30-34 years',
                      '4'='35-39 years',
                      '5'='40-44 years',
                      '6'= '45-49 years',
                      '7'='50-54 years',
                      '8'='55-59 years',
                      '9'='60-64 years',
                      '10'='65-69 years',
                      '11'='70-74 years',
                      '12'= '75-79 years',
                      '13'= '80+ years',
                      '14'= 'Unknown')

################################################################################
##Creating clean template of BRFSS state data##
##Outcome = proportion.tested
################################################################################
data.list.brfss.national.clean = lapply(brfss_file_state_list, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]] 
  
  #Change _state to character#
  data$state_fips = as.character(data$`_STATE`)
  data= subset(data, data$state_fips != "66")  #Removing Guam#
  data= subset(data, data$state_fips != "78")  #Removing Virgin Islands#
  
  #Create year variable#
  if(grepl("2013", filename)) {
    data$year = as.numeric("2013")
    data$sex = as.character(data$SEX)
    data$race = data$`_RACE`
    data$age = data$`_AGEG5YR`  
    data$risk = NA #No sexual orientation data for 2013#
    data$ever.tested = data$HIVTST6
  }
  if(grepl("2014", filename)) {
    data$year = as.numeric("2014")
    data$sex = as.character(data$SEX)
    data$race = data$`_RACE`
    data$age = data$`_AGEG5YR`
    data <- data %>%
      mutate(risk = case_when(SEX== "1" & SXORIENT == "2" ~ "msm",
                              SEX== "1" & SXORIENT == "3" ~ "msm",
                              TRUE ~ NA ))
    data$ever.tested = data$HIVTST6
  }
  if(grepl("2015", filename)) {
    data$year = as.numeric("2015")
    data$sex = as.character(data$SEX)
    data$race = data$`_RACE`
    data$age = data$`_AGEG5YR`
    data <- data %>%
      mutate(risk = case_when(SEX== "1" & SXORIENT == "2" ~ "msm",
                              SEX== "1" & SXORIENT == "3" ~ "msm",
                              TRUE ~ NA ))
    data$ever.tested = data$HIVTST6
  }
  if(grepl("2016", filename)) {
    data$year = as.numeric("2016")
    data$sex = as.character(data$SEX)
    data$race = data$`_RACE`
    data$age = data$`_AGEG5YR`
    data <- data %>%
      mutate(risk = case_when(SEX== "1" & SXORIENT == "2" ~ "msm",
                              SEX== "1" & SXORIENT == "3" ~ "msm",
                              TRUE ~ NA )) %>%
      mutate(at_risk = if_else(HIVRISK4 =="1", '1', '0'))  #A value of 1 means individual is at risk; else 0; for denominator#
    data$ever.tested = data$HIVTST6
  }
  if(grepl("2017", filename)) {
    data$year = as.numeric("2017")
    data$sex = as.character(data$SEX)
    data$race = data$`_RACE`
    data$age = data$`_AGEG5YR`
    data <- data %>%
      mutate(risk = case_when(SEX== "1" & SXORIENT == "2" ~ "msm",
                              SEX== "1" & SXORIENT == "3" ~ "msm",
                              TRUE ~ NA )) %>%
      mutate(at_risk = if_else(HIVRISK5 =="1", '1', '0'))  #A value of 1 means individual is at risk; else 0; for denominator#
    data$ever.tested = data$HIVTST6
  }
  if(grepl("2018", filename)) {
    data$year = as.numeric("2018")
    data$sex = as.character(data$SEX1)
    data$race = data$`_RACE`
    data$age = data$`_AGEG5YR`
    data$risk = if_else(data$SOMALE == "1" | data$SOMALE == "3", "msm", NA) 
    data <- data %>%
      mutate(at_risk = if_else(HIVRISK5 =="1", '1', '0'))  #A value of 1 means individual is at risk; else 0; for denominator#
    data$ever.tested = data$HIVTST6
  }
  if(grepl("2019", filename)) {
    data$year = as.numeric("2019")
    data$sex = as.character(data$`_SEX`)
    data$race = data$`_RACE`
    data$age = data$`_AGEG5YR`
    data$risk = if_else(data$SOMALE == "1" | data$SOMALE == "3", "msm", NA)
    data <- data %>%
      mutate(at_risk = if_else(HIVRISK5 =="1", '1', '0'))  #A value of 1 means individual is at risk; else 0; for denominator#
    data$ever.tested = data$HIVTST7
  }
  if(grepl("2020", filename)) {
    data$year = as.numeric("2020")
    data$sex = as.character(data$`_SEX`)
    data$race = data$`_RACE`
    data$age = data$`_AGEG5YR`
    data$risk = if_else(data$SOMALE == "1" | data$SOMALE == "3", "msm", NA)
    data <- data %>%
      mutate(at_risk = if_else(HIVRISK5 =="1", '1', '0'))  #A value of 1 means individual is at risk; else 0; for denominator#
    data$ever.tested = data$HIVTST7
  }
  if(grepl("2021", filename)) {
    data$year = as.numeric("2021")
    data$sex = as.character(data$`_SEX`)
    data$race = data$`_RACE`
    data$age = data$`_AGEG5YR`
    data$risk = if_else(data$SOMALE == "1" | data$SOMALE == "3", "msm", NA)
    data$ever.tested = data$HIVTST7
  }
  if(grepl("2022", filename)) {
    data$year = as.numeric("2022")
    data$sex = as.character(data$`_SEX`)
    data$race = data$`_RACE1`
    data$age = data$`_AGEG5YR`
    data$risk = if_else(data$SOMALE == "1" | data$SOMALE == "3", "msm", NA) 
    data <- data %>%
      mutate(at_risk = if_else(HIVRISK5 =="1", '1', '0'))  #A value of 1 means individual is at risk; else 0; for denominator#
    data$ever.tested = data$HIVTST7
  }
  
  data$location = "US"
  data$outcome = "proportion.tested" 
  
  data$sex = brfss.sex.mappings[data$sex]
  data$age = brfss.age.mappings[data$age]
  data$race = brfss.race.mappings[data$race]
  
  data = subset(data, data$race != 'multiracial') #Removing multiracial, unknown 1-14-24
  data = subset(data, data$race != 'unknown')
  
  # Estimate Who Was Tested in the Past Year ------------------------------------
  #HIVTSTD3 = date of last test
  #HIVTST7 (renamed 'ever.tested') = ever tested for HIV
  
  data = subset(data, is.na(data$HIVTSTD3) | data$HIVTSTD3 != 999999) #Remove date of last HIV is refused
  
  # Create 'tested' variable (used to determine if test is in past year)------------------------------------------------
  
  #Do calculation to determine if test was in fact in the past year:
  data$test_year = as.numeric(str_sub(data$HIVTSTD3, -4))
  data$test_month = as.numeric(substring(data$HIVTSTD3, 1, nchar(data$HIVTSTD3)-4))
  
  #Copying from Todd's code#
  data$test_month = if_else((!is.na(data$test_month) & data$test_month==77), 6, data$test_month) #If month is missing but we have year- assuming june bc 50/50 prob#
  
  data$tested = as.numeric(!is.na(data$test_year) & data$test_year >= data$year) #create probabilities of testing in past year
  mask = !is.na(data$test_year) & data$test_year==(data$year-1)
  data$tested[mask] = data$test_month[mask] / 12
  
  #Correct any dates that are years in the future
  data$current_year = "2024" #YOU WILL NEED TO UPDATE THIS WHEN YOU GET NEW DATA but for now tested date should not be beyond BRFSS year
  data$tested = if_else(data$test_year > data$current_year, 0, data$tested) #If they reported a test date in the future, keep in the denominator
  
  
  # This is to address NAs in the 'tested' variable -----------------------------------------------------
  data$HIVTSTD3 = as.character(data$HIVTSTD3)
  data$tested = ifelse(is.na(data$HIVTSTD3) & data$ever.tested == '1', "0", data$tested)
  data$tested = ifelse(is.na(data$HIVTSTD3) & data$ever.tested == '2', "0", data$tested)
  data$tested = ifelse(is.na(data$HIVTSTD3) & data$ever.tested == '7', "0", data$tested)
  
  data$tested = ifelse(is.na(data$HIVTSTD3) & data$ever.tested == '9', "drop", data$tested)
  data$tested = ifelse(is.na(data$HIVTSTD3) & is.na(data$ever.tested), "drop", data$tested)
  
  data <- data %>%
    filter(tested != "drop")
  
  data$tested = as.numeric(data$tested)
  
  list(filename, data) 
})
################################################################################
##Total-by NATIONAL##
#WEIGHTED#
################################################################################
data.list.brfss.national.totals = lapply(data.list.brfss.national.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data<- data %>%
    group_by(location) %>%
    mutate(n_weighted = sum(`_LLCPWT`)) %>% #denominator should be the sum of weights#
    ungroup()
  
  data<- data %>%
    group_by(location) %>%
    mutate(sum_tested = sum(tested*`_LLCPWT`)) %>% #multiply numerator value by the weight value#
    ungroup()%>%
    mutate(proportion_tested = (sum_tested/n_weighted))
  
  data$proportion_tested = round(data$proportion_tested, digits=4)
  
  data$year = as.character(data$year)
  data$value = data$proportion_tested
  
  data <- data %>%
    select(outcome, year, location, sum_tested, n_weighted, proportion_tested, value, `_LLCPWT`)
  
  data= as.data.frame(data)
  
  list(filename, data) 
})

################################################################################
##Sex-by state##
#WEIGHTED#
################################################################################
data.list.brfss.national.sex = lapply(data.list.brfss.national.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data= subset(data, !is.na(data$sex)) #Remove sex is NA
  
  data<- data %>%
    group_by(location, sex) %>%
    mutate(n_weighted = sum(`_LLCPWT`)) %>% #denominator should be the sum of weights#
    ungroup()
  
  data<- data %>%
    group_by(location, sex) %>%
    mutate(sum_tested = sum(tested*`_LLCPWT`)) %>% #multiply numerator value by the weight value#
    ungroup()%>%
    mutate(proportion_tested = (sum_tested/n_weighted)) #denominators needs to be sum of weights by location by sex#
  
  data$proportion_tested = round(data$proportion_tested, digits=2)
  
  data$year = as.character(data$year)
  data$value = data$proportion_tested
  
  data <- data %>%
    select(outcome, year, location, sum_tested, n_weighted, value, sex, `_LLCPWT`)
  
  data= as.data.frame(data)
  
  list(filename, data) 
})
################################################################################
##Age-by state##
#WEIGHTED#
################################################################################
data.list.brfss.national.age = lapply(data.list.brfss.national.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data= subset(data, !is.na(data$age)) #Remove missing age
  data= subset(data, data$age != 'Unknown')
  
  data<- data %>%
    group_by(location, age) %>%
    mutate(n_weighted = sum(`_LLCPWT`)) %>% #denominator should be the sum of weights#
    ungroup()
  
  data<- data %>%
    group_by(location, age) %>%
    mutate(sum_tested = sum(tested*`_LLCPWT`)) %>% #multiply numerator value by the weight value#
    ungroup()%>%
    mutate(proportion_tested = (sum_tested/n_weighted)) #denominators needs to be sum of weights by location by age#
  
  data$proportion_tested = round(data$proportion_tested, digits=2)
  
  data$year = as.character(data$year)
  data$value = data$proportion_tested
  
  data <- data %>%
    select(outcome, year, location, sum_tested, n_weighted, value, age, `_LLCPWT`)
  
  data= as.data.frame(data)
  
  list(filename, data) 
})
################################################################################
##Race-by state##
#WEIGHTED#
################################################################################
data.list.brfss.national.race = lapply(data.list.brfss.national.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data= subset(data, !is.na(data$race)) #Remove unknown race
  data= subset(data, data$race != 'Unknown')
  
  data<- data %>%
    group_by(location, race) %>%
    mutate(n_weighted = sum(`_LLCPWT`)) %>% #denominator should be the sum of weights#
    ungroup()
  
  data<- data %>%
    group_by(location, race) %>%
    mutate(sum_tested = sum(tested*`_LLCPWT`)) %>% #multiply numerator value by the weight value#
    ungroup()%>%
    mutate(proportion_tested = (sum_tested/n_weighted)) #denominators needs to be sum of weights by location by race#
  
  data$proportion_tested = round(data$proportion_tested, digits=2)
  
  data$year = as.character(data$year)
  data$value = data$proportion_tested
  
  data <- data %>%
    select(outcome, year, location, sum_tested, n_weighted, value, race, tested, `_LLCPWT`)
  
  data= as.data.frame(data)
  
  list(filename, data) 
})
################################################################################
##Risk-by state-MSM Only##
#WEIGHTED#
################################################################################
data.list.brfss.national.risk = lapply(data.list.brfss.national.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data<- data %>%
    group_by(location, risk) %>%
    mutate(n_weighted = sum(`_LLCPWT`)) %>% #denominator should be the sum of weights#
    ungroup()
  
  data<- data %>%
    group_by(location, risk) %>%
    mutate(sum_tested = sum(tested*`_LLCPWT`)) %>% #multiply numerator value by the weight value#
    ungroup()%>%
    mutate(proportion_tested = (sum_tested/n_weighted)) #denominators needs to be sum of weights by location by risk#
  
  data$proportion_tested = round(data$proportion_tested, digits=2)
  
  data$year = as.character(data$year)
  data$value = data$proportion_tested
  
  data <- data %>%
    select(outcome, year, location, sum_tested, n_weighted, value, risk, `_LLCPWT`)
  
  data= as.data.frame(data)
  
  list(filename, data) 
})

#Make a separate dataset for risk - this is what will get put into the manager#
#Need to remove NAs for the put statement but need them in for the proportion calc below#
data.list.brfss.national.risk.put = lapply(data.list.brfss.national.risk, function(file){
  
  data=file[[2]] 
  filename = file[[1]]
  
  data <- data %>%
    filter(risk == "msm")
  
  data= as.data.frame(data)
  list(filename, data) 
})
################################################################################
################################################################################

##Create outcome for the denominator value -> proportion.tested.n##
#WEIGHTED


################################################################################
##Outcome = proportion.tested.n
#WEIGHTED
################################################################################
data.list.brfss.national.n = lapply(data.list.brfss.national.totals, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data$outcome = "proportion.tested.n"
  data$value = data$n_weighted #replace the "population" calculated above as the outcome value
  
  data <- data %>%
    select(outcome, year, location, value)
  
  data= as.data.frame(data)
  list(filename, data) 
})
################################################################################
##Sex
##Outcome = proportion.tested.n
#WEIGHTED
################################################################################
data.list.brfss.national.sex.n = lapply(data.list.brfss.national.sex, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data$outcome = "proportion.tested.n"
  data$value = data$n_weighted #replace the "population" calculated above as the outcome value
  
  data <- data %>%
    select(outcome, year, location, sex, value)
  
  data= as.data.frame(data)
  list(filename, data) 
})
################################################################################
##age
##Outcome = proportion.tested.n
#WEIGHTED
################################################################################
data.list.brfss.national.age.n = lapply(data.list.brfss.national.age, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data$outcome = "proportion.tested.n"
  data$value = data$n_weighted #replace the "population" calculated above as the outcome value
  
  data <- data %>%
    select(outcome, year, location, age, value)
  
  data= as.data.frame(data)
  list(filename, data) 
})
################################################################################
##race
##Outcome = proportion.tested.n
#WEIGHTED
################################################################################
data.list.brfss.national.race.n = lapply(data.list.brfss.national.race, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data$outcome = "proportion.tested.n"
  data$value = data$n_weighted #replace the "population" calculated above as the outcome value
  
  data <- data %>%
    select(outcome, year, location, race, value)
  
  data= as.data.frame(data)
  list(filename, data) 
})

################################################################################
##risk
##Outcome = proportion.tested.n
#WEIGHTED
################################################################################
data.list.brfss.national.risk.n = lapply(data.list.brfss.national.risk, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data$outcome = "proportion.tested.n"
  data$value = data$n_weighted #replace the "population" calculated above as the outcome value
  
  data <- data %>%
    select(outcome, year, location, risk, value)%>% 
    filter(risk == "msm")
  
  data= as.data.frame(data)
  list(filename, data) 
})
################################################################################
##PUT INTO THE DATA MANAGER###
#10 statements#
################################################################################
#National-TOTAL-proportion.tested
national.total.num = lapply(data.list.brfss.national.totals, `[[`, 2)  

for (data in national.total.num) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}

##national-SEX-proportion.tested
national.sex.num = lapply(data.list.brfss.national.sex, `[[`, 2)  

for (data in national.sex.num) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}
##national-AGE-proportion.tested
national.age.num = lapply(data.list.brfss.national.age, `[[`, 2)  

for (data in national.age.num) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}
##national-RACE-proportion.tested
national.race.num = lapply(data.list.brfss.national.race, `[[`, 2)  

for (data in national.race.num) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}
##national-RISK-proportion.tested
national.risk.num = lapply(data.list.brfss.national.risk.put, `[[`, 2)  

for (data in national.risk.num) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}

#####national-TOTAL-proportion.tested.N
national.total.denom = lapply(data.list.brfss.national.n, `[[`, 2)  

for (data in national.total.denom) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}
######national-SEX-proportion.tested.N
national.sex.denom = lapply(data.list.brfss.national.sex.n, `[[`, 2)  

for (data in national.sex.denom) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}
######national-AGE-proportion.tested.n
national.age.denom = lapply(data.list.brfss.national.age.n, `[[`, 2)  

for (data in national.age.denom) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}
######national-RACE-proportion.tested.n
national.race.denom = lapply(data.list.brfss.national.race.n, `[[`, 2)  

for (data in national.race.denom) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}
######national-RISK-proportion.tested.n
national.risk.denom = lapply(data.list.brfss.national.risk.n, `[[`, 2)  

for (data in national.risk.denom) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}



# Pulling MSA Level Proportion.Tested data from the surveillance manager --------
#No risk data because risk data isn't by MSA

syphilis.manager= load.data.manager(name="syphilis.manager", file="../../cached/syphilis.manager.rdata")
state = locations::get.all.for.type("state")
msas = locations::get.all.for.type("CBSA")

proportion.tested.msa <- as.data.frame.table(surveillance.manager$data$proportion.tested$estimate$brfss$brfss$year__location)%>%
  filter(location %in% msas)%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(outcome = "proportion.tested")
  
proportion.tested.msa.sex <- as.data.frame.table(surveillance.manager$data$proportion.tested$estimate$brfss$brfss$year__location__sex)%>%
  filter(location %in% msas)%>%
    rename(value = Freq)%>%
    mutate(year = as.character(year))%>%
    mutate(location = as.character(location))%>%
    mutate(value = as.numeric(value))%>%
    mutate(sex = as.character(sex))%>%
    mutate(outcome = "proportion.tested")
  
proportion.tested.msa.age <- as.data.frame.table(surveillance.manager$data$proportion.tested$estimate$brfss$brfss$year__location__age)%>%
  filter(location %in% msas)%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(age = as.character(age))%>%
  mutate(outcome = "proportion.tested")
  
proportion.tested.msa.race <- as.data.frame.table(surveillance.manager$data$proportion.tested$estimate$brfss$brfss$year__location__race)%>%
  filter(location %in% msas)%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(race = as.character(race))%>%
  mutate(outcome = "proportion.tested")%>%
  mutate(race = tolower(race))

# MSA Level proportion.tested.n -------------------------------------------
proportion.tested.denom.msa <- as.data.frame.table(surveillance.manager$data$proportion.tested.n$estimate$brfss$brfss$year__location)%>%
  filter(location %in% msas)%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(outcome = "proportion.tested.n")

proportion.tested.denom.msa.sex <- as.data.frame.table(surveillance.manager$data$proportion.tested.n$estimate$brfss$brfss$year__location__sex)%>%
  filter(location %in% msas)%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(sex = as.character(sex))%>%
  mutate(outcome = "proportion.tested.n")

proportion.tested.denom.msa.age <- as.data.frame.table(surveillance.manager$data$proportion.tested.n$estimate$brfss$brfss$year__location__age)%>%
  filter(location %in% msas)%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(age = as.character(age))%>%
  mutate(outcome = "proportion.tested.n")

proportion.tested.denom.msa.race <- as.data.frame.table(surveillance.manager$data$proportion.tested.n$estimate$brfss$brfss$year__location__race)%>%
  filter(location %in% msas)%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(race = as.character(race))%>%
  mutate(outcome = "proportion.tested.n")%>%
  mutate(race = tolower(race))

# Put ----------------------------------------------------------------------

proportion.tested.from.surveillance.manager = list(
  proportion.tested.msa,
  proportion.tested.msa.sex,
  proportion.tested.msa.age,
  proportion.tested.msa.race,
  proportion.tested.denom.msa,
  proportion.tested.denom.msa.sex,
  proportion.tested.denom.msa.age,
  proportion.tested.denom.msa.race
)

for (data in proportion.tested.from.surveillance.manager) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}
  