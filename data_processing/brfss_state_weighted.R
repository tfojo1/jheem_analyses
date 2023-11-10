#library(haven)

################################################################################
###Read in BRFSS .xpt files
################################################################################

DATA.DIR.BRFSS.STATE="../../data_raw/brfss/brfss_state"

brfss_file_state <- list.files(DATA.DIR.BRFSS.STATE, pattern = ".XPT", full.names = "TRUE")

#\\\\\\To show only individuals at risk of HIV in the denominator///////#
#Un-comment line 14, comment out line 9
#Un-comment line 259-262
#brfss_file_state <- list.files(DATA.DIR.BRFSS.STATE, pattern = "risk", full.names = "TRUE")

brfss_file_state_list <- lapply(brfss_file_state, function(x) {
  list(filename=x, data=read_xpt(x))
})

################################################################################
###Create state mapping bc the function isnt working but maybe there's 
#another way in the locations package that I don't know
################################################################################
state.to.fips.mappings = c('1' = 'AL',
                           '2'='AK',
                           '4'='AZ',
                           '5'='AR',
                           '6'='CA',
                           '8'='CO',
                           '9'='CT',
                           '10'='DE',
                           '11'='DC',
                           '12'='FL',
                           '13'='GA',
                           '15'='HI',
                           '16'='ID',
                           '17'='IL',
                           '18'='IN',
                           '19'='IA',
                           '20'='KS',
                           '21'='KY',
                           '22'='LA',
                           '23'='ME',
                           '24'='MD',
                           '25'='MA',
                           '26'='MI',
                           '27'='MN',
                           '28'='MS',
                           '29'='MO',
                           '30'='MT',
                           '31'='NE',
                           '32'='NV',
                           '33'='NH',
                           '34'='NJ',
                           '35'='NM',
                           '36'='NY',
                           '37'='NC',
                           '38'='ND',
                           '39'='OH',
                           '40'='OK',
                           '41'='OR',
                           '42'='PA',
                           '44'='RI',
                           '45'='SC',
                           '46'='SD',
                           '47'='TN',
                           '48'='TX',
                           '49'='UT',
                           '50'='VT',
                           '51'='VA',
                           '53'='WA',
                           '54'='WV',
                           '55'='WI',
                           '56'='WY',
                           '72'= 'PR')

brfss.sex.mappings = c('1' = 'male',
                       '2' = 'female',
                       '7' = NA,
                       '9' = NA)

brfss.race.mappings= c('1'= "White",
                       '2'= "Black",
                       '3'= 'American Indian/Alaska Native',
                       '4'= 'Asian',
                       '5'= 'Native Hawaiian/Other Pacific Islander',
                       '6'= 'Other race',
                       '7'= 'Multiracial', 
                       '8'= 'Hispanic',
                       '9'= 'Unknown')
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
data.list.brfss.state.clean = lapply(brfss_file_state_list, function(file){
  
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
  }
  if(grepl("2014", filename)) {
    data$year = as.numeric("2014")
    data$sex = as.character(data$SEX)
    data$race = data$`_RACE`
    data$age = data$`_AGEG5YR`
    data$test=data$SXORIENT
    data <- data %>%
      mutate(risk = case_when(SEX== "1" & SXORIENT == "2" ~ "msm",
                              SEX== "1" & SXORIENT == "3" ~ "msm",
                              TRUE ~ NA ))
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
  }
  if(grepl("2018", filename)) {
    data$year = as.numeric("2018")
    data$sex = as.character(data$SEX1)
    data$race = data$`_RACE`
    data$age = data$`_AGEG5YR`
    data$risk = if_else(data$SOMALE == "1" | data$SOMALE == "3", "msm", NA) 
    data <- data %>%
      mutate(at_risk = if_else(HIVRISK5 =="1", '1', '0'))  #A value of 1 means individual is at risk; else 0; for denominator#
  }
  if(grepl("2019", filename)) {
    data$year = as.numeric("2019")
    data$sex = as.character(data$`_SEX`)
    data$race = data$`_RACE`
    data$age = data$`_AGEG5YR`
    data$risk = if_else(data$SOMALE == "1" | data$SOMALE == "3", "msm", NA)
    data <- data %>%
      mutate(at_risk = if_else(HIVRISK5 =="1", '1', '0'))  #A value of 1 means individual is at risk; else 0; for denominator#
  }
  if(grepl("2020", filename)) {
    data$year = as.numeric("2020")
    data$sex = as.character(data$`_SEX`)
    data$race = data$`_RACE`
    data$age = data$`_AGEG5YR`
    data$risk = if_else(data$SOMALE == "1" | data$SOMALE == "3", "msm", NA)
    data <- data %>%
      mutate(at_risk = if_else(HIVRISK5 =="1", '1', '0'))  #A value of 1 means individual is at risk; else 0; for denominator#
  }
  if(grepl("2021", filename)) {
    data$year = as.numeric("2021")
    data$sex = as.character(data$`_SEX`)
    data$race = data$`_RACE`
    data$age = data$`_AGEG5YR`
    data$risk = if_else(data$SOMALE == "1" | data$SOMALE == "3", "msm", NA)
  }
  if(grepl("2022", filename)) {
    data$year = as.numeric("2022")
    data$sex = as.character(data$`_SEX`)
    data$race = data$`_RACE1`
    data$age = data$`_AGEG5YR`
    data$risk = if_else(data$SOMALE == "1" | data$SOMALE == "3", "msm", NA) 
    data <- data %>%
      mutate(at_risk = if_else(HIVRISK5 =="1", '1', '0'))  #A value of 1 means individual is at risk; else 0; for denominator#
  }
  
  #Create location#
  data$location = state.to.fips.mappings[data$state_fips]
  
  #Create outcome#
  data$outcome = "proportion.tested"  
  
  #Removing missing values for HIVTSTD3  
  data= subset(data, data$HIVTSTD3 != "777777")
  data= subset(data, data$HIVTSTD3 != "999999")
  
  #Do calculation to determine if test was in fact in the past year:
  data$test_year = as.numeric(str_sub(data$HIVTSTD3, -4))
  data$test_month = as.numeric(substring(data$HIVTSTD3, 1, nchar(data$HIVTSTD3)-4))
  
  #Copying from Todd's code#
  data$test_month = if_else((!is.na(data$test_month) & data$test_month==77), 6, data$test_month) #If month is missing but we have year- assuming june bc 50/50 prob#
  data$test_month = if_else(is.na(data$test_year), NA, data$test_month) #If year is missing, then it's NA altogether
  
  data$tested = as.numeric(!is.na(data$test_year) & data$test_year >= data$year) #create probabilities of testing in past year
  mask = !is.na(data$test_year) & data$test_year==(data$year-1)
  data$tested[mask] = data$test_month[mask] / 12
  
  #Correct any dates that are years in the future
  data$today = Sys.Date()
  data$current_year = substr(data$today, 1, 4)
  data$tested = if_else(data$test_year > data$current_year, NA, data$tested) #mark as NA for those with future test years
  
  data=subset(data, !is.na(data$tested)) #Remove people who were not tested or had incorrect data##
  
  data$sex = brfss.sex.mappings[data$sex]
  data$age = brfss.age.mappings[data$age]
  data$race = brfss.race.mappings[data$race]
  
  #Remove values that are NA
  data= subset(data, !is.na(data$sex))
  data= subset(data, !is.na(data$age))
  data= subset(data, !is.na(data$race))
  
  #Remove unknown race and age
  data= subset(data, data$age != 'Unknown')
  data= subset(data, data$race != 'Unknown')
  
  #\\\\\\To show only individuals at risk of HIV in the denominator///////#
  #Un-comment line 259-262
  # brfss_risk_var = c(HIVRISK5= "HIVRISK4")
  # data <- data %>%
  #    rename(any_of(brfss_risk_var))
  # data = subset(data, HIVRISK5 == "1" ) #select only those at risk#
  
  list(filename, data) 
})
################################################################################
                            ##Total-by state##
                                #WEIGHTED#
################################################################################
data.list.brfss.state.totals = lapply(data.list.brfss.state.clean, function(file){
  
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
    select(outcome, year, location, value, proportion_tested, n_weighted, `_LLCPWT`)
  data= as.data.frame(data)
  
  list(filename, data) 
})

################################################################################
                                ##Sex-by state##
                                    #WEIGHTED#
################################################################################
data.list.brfss.state.sex = lapply(data.list.brfss.state.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
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
    select(outcome, year, location, value, sex, proportion_tested, n_weighted, `_LLCPWT`)
  data= as.data.frame(data)
  
  list(filename, data) 
})
################################################################################
                              ##Age-by state##
                                  #WEIGHTED#
################################################################################
data.list.brfss.state.age = lapply(data.list.brfss.state.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
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
    select(outcome, year, location, value, age, proportion_tested, n_weighted, `_LLCPWT`)
  data= as.data.frame(data)
  
  list(filename, data) 
})
################################################################################
                            ##Race-by state##
                                #WEIGHTED#
################################################################################
data.list.brfss.state.race = lapply(data.list.brfss.state.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
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
    select(outcome, year, location, value, race, proportion_tested, n_weighted, `_LLCPWT`)
  data= as.data.frame(data)
  
  list(filename, data) 
})
################################################################################
                      ##Risk-by state-MSM Only##
                              #WEIGHTED#
################################################################################
data.list.brfss.state.risk = lapply(data.list.brfss.state.clean, function(file){
  
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
    select(outcome, year, location, value, risk, proportion_tested, n_weighted, `_LLCPWT`)
  data= as.data.frame(data)
  
  list(filename, data) 
})

#Make a separate dataset for risk - this is what will get put into the manager#
#Need to remove NAs for the put statement but need them in for the prportion calc below#
data.list.brfss.state.risk.put = lapply(data.list.brfss.state.risk, function(file){
  
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
data.list.brfss.state.n = lapply(data.list.brfss.state.totals, function(file){
  
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
data.list.brfss.state.sex.n = lapply(data.list.brfss.state.sex, function(file){
  
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
data.list.brfss.state.age.n = lapply(data.list.brfss.state.age, function(file){
  
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
data.list.brfss.state.race.n = lapply(data.list.brfss.state.race, function(file){
  
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
data.list.brfss.state.risk.n = lapply(data.list.brfss.state.risk, function(file){
  
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
##State-TOTAL-proportion.tested
state.total.num = lapply(data.list.brfss.state.totals, `[[`, 2)  

for (data in state.total.num) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}

##State-SEX-proportion.tested
state.sex.num = lapply(data.list.brfss.state.sex, `[[`, 2)  

for (data in state.sex.num) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}
##State-AGE-proportion.tested
state.age.num = lapply(data.list.brfss.state.age, `[[`, 2)  

for (data in state.age.num) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}
##State-RACE-proportion.tested
state.race.num = lapply(data.list.brfss.state.race, `[[`, 2)  

for (data in state.race.num) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}
##State-RISK-proportion.tested
state.risk.num = lapply(data.list.brfss.state.risk.put, `[[`, 2)  

for (data in state.risk.num) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}

#####State-TOTAL-proportion.tested.N
state.total.denom = lapply(data.list.brfss.state.n, `[[`, 2)  

for (data in state.total.denom) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}
######State-SEX-proportion.tested.N
state.sex.denom = lapply(data.list.brfss.state.sex.n, `[[`, 2)  

for (data in state.sex.denom) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}
######State-AGE-proportion.tested.n
state.age.denom = lapply(data.list.brfss.state.age.n, `[[`, 2)  

for (data in state.age.denom) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}
######State-RACE-proportion.tested.n
state.race.denom = lapply(data.list.brfss.state.race.n, `[[`, 2)  

for (data in state.race.denom) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}
######State-RISK-proportion.tested.n
state.risk.denom = lapply(data.list.brfss.state.risk.n, `[[`, 2)  

for (data in state.risk.denom) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}

