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
                           '20'='KA',
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
                           '35'='Nm',
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
                              TRUE ~ NA ))
  }
  if(grepl("2017", filename)) {
    data$year = as.numeric("2017")
    data$sex = as.character(data$SEX)
    data$race = data$`_RACE`
    data$age = data$`_AGEG5YR`
    data <- data %>%
      mutate(risk = case_when(SEX== "1" & SXORIENT == "2" ~ "msm",
                              SEX== "1" & SXORIENT == "3" ~ "msm",
                              TRUE ~ NA ))
  }
  if(grepl("2018", filename)) {
    data$year = as.numeric("2018")
    data$sex = as.character(data$SEX1)
    data$race = data$`_RACE`
    data$age = data$`_AGEG5YR`
    data$risk = if_else(data$SOMALE == "1" | data$SOMALE == "3", "msm", NA)
  }
  if(grepl("2019", filename)) {
    data$year = as.numeric("2019")
    data$sex = as.character(data$`_SEX`)
    data$race = data$`_RACE`
    data$age = data$`_AGEG5YR`
    data$risk = if_else(data$SOMALE == "1" | data$SOMALE == "3", "msm", NA)
  }
  if(grepl("2020", filename)) {
    data$year = as.numeric("2020")
    data$sex = as.character(data$`_SEX`)
    data$race = data$`_RACE`
    data$age = data$`_AGEG5YR`
    data$risk = if_else(data$SOMALE == "1" | data$SOMALE == "3", "msm", NA)
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
  }
  
  #Create location#
  data$location = state.to.fips.mappings[data$state_fips]
  
  #Create outcome#
  data$outcome = "proportion.tested"  #will need to create a second outcome for denominator#
  
  data$HIVTSTD3 = if_else(data$HIVTSTD3== "777777", NA, data$HIVTSTD3) #Chaging invalid to NA#
  data$HIVTSTD3 = if_else(data$HIVTSTD3== "999999", NA, data$HIVTSTD3) #Chaging invalid to NA#
  
  #data=subset(data, !is.na(data$HIVTSTD3)) #Need to remove NA values in order to create a sum of prob of tested#
  
  #Do calculation to determine if test was in fact in the past year:
  data$test_year = as.numeric(str_sub(data$HIVTSTD3, -4))
  data$test_month = substring(data$HIVTSTD3, 1, nchar(data$HIVTSTD3)-4)
  data$test_month = as.numeric(if_else(data$test_month == "77", NA, data$test_month))
  
  #Copying from Todd's code#
  data$test_month = if_else((!is.na(data$test_month) & data$test_month==77), 6, data$test_month) #Ask Todd to clarify this piece?#
  data$test_month = if_else(is.na(data$test_year), NA, data$test_month)
  
  data$tested = as.numeric(!is.na(data$test_year) & data$test_year >= data$year)
  mask = !is.na(data$test_year) & data$test_year==(data$year-1)
  data$tested[mask] = data$test_month[mask] / 12
  
  #data$tested = as.numeric(!is.na(data$test_year) & data$test_year >= (data$year-1))
  
  #Now calculate the proportion tested#
  data=subset(data, !is.na(data$tested)) #Remove people who were not tested##
  
  data$sex = brfss.sex.mappings[data$sex]
  data$age = brfss.age.mappings[data$age]
  data$race = brfss.race.mappings[data$race]
  
  list(filename, data) 
})
################################################################################
##Total-by state##
################################################################################
data.list.brfss.state.totals = lapply(data.list.brfss.state.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  #Create population variable-count of BRFSS responses by state#
  countspop <- table(data$location)
  data$n <-countspop[match(data$location, names(countspop))]
  
  data<- data %>%
    group_by(location) %>%
    mutate(sum_tested = sum(tested)) %>%
    ungroup()%>%
    mutate(proportion_tested = (sum_tested/n))
  
  data$proportion_tested = round(data$proportion_tested, digits=2)
  
  data$year = as.character(data$year)
  
  data <- data %>%
    select(outcome, year, location, proportion_tested, n)
  data= as.data.frame(data)
  
  list(filename, data) 
})

################################################################################
##Sex-by state##
################################################################################
data.list.brfss.state.sex = lapply(data.list.brfss.state.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data<- data %>%
    add_count(location, sex) %>%     #Create population variable-count of BRFSS responses by state#
    group_by(location, sex) %>%
    mutate(sum_tested = sum(tested)) %>%
    ungroup()%>%
    mutate(proportion_tested = (sum_tested/n))
  
  data$proportion_tested = round(data$proportion_tested, digits=2)
  
  data$year = as.character(data$year)
  
  data <- data %>%
    select(outcome, year, location, sex, proportion_tested, n)
  data= as.data.frame(data)
  
  list(filename, data) 
})
################################################################################
##Age-by state##
################################################################################
data.list.brfss.state.age = lapply(data.list.brfss.state.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data<- data %>%
    add_count(location, age) %>%     #Create population variable-count of BRFSS responses by state#
    group_by(location, age) %>%
    mutate(sum_tested = sum(tested)) %>%
    ungroup()%>%
    mutate(proportion_tested = (sum_tested/n))
  
  data$proportion_tested = round(data$proportion_tested, digits=2)
  
  data$year = as.character(data$year)
  
  data <- data %>%
    select(outcome, year, location, age, proportion_tested, n)
  data= as.data.frame(data)
  
  list(filename, data) 
})
################################################################################
##Race-by state##
################################################################################
data.list.brfss.state.race = lapply(data.list.brfss.state.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data<- data %>%
    add_count(location, race) %>%     #Create population variable-count of BRFSS responses by state#
    group_by(location, race) %>%
    mutate(sum_tested = sum(tested)) %>%
    ungroup()%>%
    mutate(proportion_tested = (sum_tested/n))
  
  data$proportion_tested = round(data$proportion_tested, digits=2)
  
  data$year = as.character(data$year)
  
  data <- data %>%
    select(outcome, year, location, race, proportion_tested, n)
  data= as.data.frame(data)
  
  list(filename, data) 
})
################################################################################
##Risk-by state-MSM Only##
################################################################################
data.list.brfss.state.risk = lapply(data.list.brfss.state.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data<- data %>%
    add_count(location, risk) %>%     #Create population variable-count of BRFSS responses by state#
    group_by(location, risk) %>%
    mutate(sum_tested = sum(tested)) %>%
    ungroup()%>%
    mutate(proportion_tested = (sum_tested/n))
  
  data$proportion_tested = round(data$proportion_tested, digits=2)
  
  data$year = as.character(data$year)
  
  data <- data %>%
    select(outcome, year, location, proportion_tested, risk, n)
  data= as.data.frame(data)
  
  list(filename, data) 
})
################################################################################
################################################################################
################################################################################


####2nd outcome###


################################################################################
##Outcome = proportion.tested.n
################################################################################
data.list.brfss.state.n = lapply(data.list.brfss.state.totals, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data$outcome = "proportion.tested.n"
  data$value = data$n #replace the "population" calculated above as the outcome value
  
  data <- data %>%
    select(outcome, year, location, value)
  
  data= as.data.frame(data)
  list(filename, data) 
})
################################################################################
##Sex
##Outcome = proportion.tested.n
################################################################################
data.list.brfss.state.sex.n = lapply(data.list.brfss.state.sex, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data$outcome = "proportion.tested.n"
  data$value = data$n #replace the "population" calculated above as the outcome value
  
  data <- data %>%
    select(outcome, year, location, value)
  
  data= as.data.frame(data)
  list(filename, data) 
})
################################################################################
##age
##Outcome = proportion.tested.n
################################################################################
data.list.brfss.state.age.n = lapply(data.list.brfss.state.age, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data$outcome = "proportion.tested.n"
  data$value = data$n #replace the "population" calculated above as the outcome value
  
  data <- data %>%
    select(outcome, year, location, value)
  
  data= as.data.frame(data)
  list(filename, data) 
})
################################################################################
##race
##Outcome = proportion.tested.n
################################################################################
data.list.brfss.state.race.n = lapply(data.list.brfss.state.race, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data$outcome = "proportion.tested.n"
  data$value = data$n #replace the "population" calculated above as the outcome value
  
  data <- data %>%
    select(outcome, year, location, value)
  
  data= as.data.frame(data)
  list(filename, data) 
})

################################################################################
##risk
##Outcome = proportion.tested.n
################################################################################
data.list.brfss.state.risk.n = lapply(data.list.brfss.state.risk, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data$outcome = "proportion.tested.n"
  data$value = data$n #replace the "population" calculated above as the outcome value
  
  data <- data %>%
    select(outcome, year, location, value)
  
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
state.risk.num = lapply(data.list.brfss.state.risk, `[[`, 2)  

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

