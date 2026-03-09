#library(haven)

################################################################################
###Read in BRFSS .xpt files
################################################################################

DATA.DIR.BRFSS.STATE="Q:/data_raw/brfss/brfss_state"

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
      data$race = data$`_RACE`
      data$age = data$`_AGEG5YR`  
      #(No sexual orientation data for 2013)#
      data$sex = data$SEX
      data$ever.tested = data$HIVTST6
  }
  if(grepl("2014", filename)) {
      data$year = as.numeric("2014")
      data$race = data$`_RACE`
      data$sex = data$SEX
      data$age = data$`_AGEG5YR`
      data$ever.tested = data$HIVTST6
      data <- data %>%
          mutate(msm = case_when((SEX== "1" & SXORIENT == "2") ~ "msm",
                                 (SEX== "1" & SXORIENT == "3") ~ "msm",
                                 TRUE ~ NA))
  }
  if(grepl("2015", filename)) {
      data$year = as.numeric("2015")
      data$race = data$`_RACE`
      data$sex = data$SEX
      data$age = data$`_AGEG5YR`
      data$ever.tested = data$HIVTST6
      data <- data %>%
          mutate(msm = case_when((SEX== "1" & SXORIENT == "2") ~ "msm",
                                 (SEX== "1" & SXORIENT == "3") ~ "msm",
                                 TRUE ~ NA))
  }
  if(grepl("2016", filename)) {
      data$year = as.numeric("2016")
      data$race = data$`_RACE`
      data$sex = data$SEX
      data$age = data$`_AGEG5YR`
      data$ever.tested = data$HIVTST6
      data <- data %>%
          mutate(msm = case_when((SEX== "1" & SXORIENT == "2") ~ "msm",
                                 (SEX== "1" & SXORIENT == "3") ~ "msm",
                                 TRUE ~ NA))
      
  }
  if(grepl("2017", filename)) {
      data$year = as.numeric("2017")
      data$race = data$`_RACE`
      data$sex = data$SEX
      data$age = data$`_AGEG5YR`
      data$ever.tested = data$HIVTST6
      data <- data %>%
          mutate(msm = case_when((SEX== "1" & SXORIENT == "2") ~ "msm",
                                 (SEX== "1" & SXORIENT == "3") ~ "msm",
                                 TRUE ~ NA))
      
  }
  if(grepl("2018", filename)) {
      data$year = as.numeric("2018")
      data$race = data$`_RACE`
      data$sex = data$SEX1
      data$age = data$`_AGEG5YR`
      data$ever.tested = data$HIVTST6
      
      data <- data %>%
          mutate(msm = case_when(
              SEX1 == "1" & SOMALE == "1" ~ "msm",
              SEX1 == "1" & SOMALE == "3" ~ 'msm',
              TRUE ~ NA)) 
      
  }
  if(grepl("2019", filename)) {
      data$year = as.numeric("2019")
      data$race = data$`_RACE`
      data$sex = data$`_SEX`
      data$age = data$`_AGEG5YR`
      data$ever.tested = data$HIVTST7
      
      data <- data %>%
          mutate(msm = case_when(
              `_SEX` == "1" & SOMALE == "1" ~ "msm",
              `_SEX` == "1" & SOMALE == "3" ~ 'msm',
              TRUE ~ NA)) 
  }
  if(grepl("2020", filename)) {
      data$year = as.numeric("2020")
      data$race = data$`_RACE`
      data$sex = data$`_SEX`
      data$age = data$`_AGEG5YR`
      data$ever.tested = data$HIVTST7
      
      data <- data %>%
          mutate(msm = case_when(
              `_SEX` == "1" & SOMALE == "1" ~ "msm",
              `_SEX` == "1" & SOMALE == "3" ~ 'msm',
              TRUE ~ NA)) 
      
  }
  if(grepl("2021", filename)) {
      data$year = as.numeric("2021")
      data$race = data$`_RACE`
      data$sex = data$`_SEX`
      data$age = data$`_AGEG5YR`
      data$ever.tested = data$HIVTST7
      
      data <- data%>%
          mutate(msm = case_when(
              `_SEX` == "1" & SOMALE == "1" ~ "msm",
              `_SEX` == "1" & SOMALE == "3" ~ 'msm',
              TRUE ~ NA))
      
  }
  if(grepl("2022", filename)) {
      data$year = as.numeric("2022")
      data$race = data$`_RACE1`
      data$sex = data$`_SEX`
      data$age = data$`_AGEG5YR`
      data$ever.tested = data$HIVTST7
      
      data <- data %>%
          mutate(msm = case_when(
              `_SEX` == "1" & SOMALE == "1" ~ "msm",
              `_SEX` == "1" & SOMALE == "3" ~ 'msm',
              TRUE ~ NA))
  }
  
  if(grepl("2023", filename)) {
      data$year = as.numeric("2023")
      data$race = data$`_RACE`
      data$sex = data$`_SEX`
      data$age = data$`_AGEG5YR`
      data$ever.tested = data$HIVTST7
      
      data <- data %>%
          mutate(msm = case_when(
              `_SEX` == "1" & SOMALE == "1" ~ "msm",
              `_SEX` == "1" & SOMALE == "3" ~ 'msm',
              TRUE ~ NA))
      
  }
  
  if(grepl("2024", filename)) {
      data$year = as.numeric("2024")
      data$race = data$`_RACE`
      data$sex = data$`_SEX`
      data$age = data$`_AGEG5YR`
      data$ever.tested = data$HIVTST7
      
      data <- data %>%
          mutate(msm = case_when(
              `_SEX` == "1" & SOMALE == "1" ~ "msm",
              `_SEX` == "1" & SOMALE == "3" ~ 'msm',
              TRUE ~ NA))
      
  }
  
  data$location = state.to.fips.mappings[data$state_fips]
  data$outcome = "proportion.tested.for.hiv" 
  
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
    select(outcome, year, location, sum_tested, n_weighted, proportion_tested, value, `_LLCPWT`)
  
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
  
  data= subset(data, !is.na(data$age)) #Remove missing age
  data= subset(data, data$age != 'Unknown')
  
  #Regroup age to meet SHIELD
  data$age = ifelse(data$age == '65-69 years', "65+ years", data$age)
  data$age = ifelse(data$age == '70-74 years', "65+ years", data$age)
  data$age = ifelse(data$age == '75-79 years', "65+ years", data$age)
  data$age = ifelse(data$age == '80+ years', "65+ years", data$age)
  
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
data.list.brfss.state.race = lapply(data.list.brfss.state.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data= subset(data, !is.na(data$race)) #Remove unknown race
  data= subset(data, data$race != 'unknown')
  
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
  
  data$outcome = "proportion.tested.for.hiv.n"
  data$value = data$n_weighted #replace the "population" calculated above as the outcome value
  
  data <- data %>%
    select(outcome, year, location, value)
  
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
  
  data$outcome = "proportion.tested.for.hiv.n"
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
  
  data$outcome = "proportion.tested.for.hiv.n"
  data$value = data$n_weighted #replace the "population" calculated above as the outcome value
  
  data <- data %>%
    select(outcome, year, location, race, value)
  
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

#===================================================================
#Updating for shield: creating sex dimension to be m,f,msm
#Removing risk dimension
#=====================================================================

#=============
#Male + Female Section
#=============


################################################################################
##Sex-by state##
#WEIGHTED#
################################################################################
data.list.brfss.state.sex = lapply(data.list.brfss.state.clean, function(file){
    
    data=file[[2]] 
    filename = file[[1]] 
    
    data= subset(data, !is.na(data$sex)) #Remove sex is NA
    
    data= subset(data, data$year != "2013") #Remove year != to 2013 bc this year does not have sexual orientation data that creates this variable
    
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

##state-SEX-proportion.tested
state.sex.num = lapply(data.list.brfss.state.sex, `[[`, 2)  

for (data in state.sex.num) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss.shield',
        source = 'brfss',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

################################################################################
##Sex
##Outcome = proportion.tested.n
#WEIGHTED
################################################################################
data.list.brfss.state.sex.fixed <- keep(data.list.brfss.state.sex, ~ nrow(.x[[2]]) > 0) #Remove 2013 without data

data.list.brfss.state.sex.n = lapply(data.list.brfss.state.sex.fixed, function(file){
    
    data=file[[2]] 
    filename = file[[1]] 
    
    data$outcome = "proportion.tested.for.hiv.n"
    data$value = data$n_weighted #replace the "population" calculated above as the outcome value
    
    data <- data %>%
        select(outcome, year, location, sex, value)
    
    data= as.data.frame(data)
    list(filename, data) 
})

######state-SEX-proportion.tested.N
state.sex.denom = lapply(data.list.brfss.state.sex.n, `[[`, 2)  

for (data in state.sex.denom) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss.shield',
        source = 'brfss',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}


#==============
#MSM Section
#=============
#This separately calculates the proportion tested for MSM which will be put as part of the sex dimension
data.list.brfss.state.msm.unclean <- data.list.brfss.state.clean[!sapply(data.list.brfss.state.clean, function(x) any(x[[2]]$year == 2013))]

data.list.brfss.state.msm = lapply(data.list.brfss.state.msm.unclean, function(file){
    
    data=file[[2]] 
    filename = file[[1]] 
    
    data= subset(data, data$year != "2013") #Remove year != to 2013 bc this year does not have sexual orientation data that creates this variable
    data= subset(data, !is.na(data$sex)) #Remove sex is NA
    data= subset(data, !is.na(data$msm)) #Remove sex is NA
    
    data<- data %>%
        group_by(location, msm) %>%
        mutate(n_weighted = sum(`_LLCPWT`)) %>% #denominator should be the sum of weights#
        ungroup()
    
    data<- data %>%
        group_by(location, msm) %>%
        mutate(sum_tested = sum(tested*`_LLCPWT`)) %>% #multiply numerator value by the weight value#
        ungroup()%>%
        mutate(proportion_tested = (sum_tested/n_weighted)) #denominators needs to be sum of weights by location by sex#
    
    data$proportion_tested = round(data$proportion_tested, digits=2)
    
    data$year = as.character(data$year)
    data$value = data$proportion_tested
    
    data <- data %>%
        select(-sex)%>%
        rename(sex = msm)%>%
        select(outcome, year, location, sum_tested, n_weighted, value, sex, `_LLCPWT`)
    
    data= as.data.frame(data)
    
    list(filename, data) 
})

##State-SEX-proportion.tested
state.msm.num = lapply(data.list.brfss.state.msm, `[[`, 2)  

for (data in state.msm.num) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss.shield',
        source = 'brfss',
        dimension.values = list(sex = 'msm'),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

#======================================
#Proportion.tested.n msm
#========================================

data.list.brfss.state.msm.n = lapply(data.list.brfss.state.msm, function(file){
    
    data=file[[2]] 
    filename = file[[1]] 
    
    data$outcome = "proportion.tested.for.hiv.n"
    data$value = data$n_weighted #replace the "population" calculated above as the outcome value
    
    data <- data %>%
        select(outcome, year, location, sex, value)
    
    data= as.data.frame(data)
    list(filename, data) 
})

######national-SEX-proportion.tested.N
national.msm.denom = lapply(data.list.brfss.state.msm.n, `[[`, 2)  

for (data in national.msm.denom) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss.shield',
        source = 'brfss',
        dimension.values = list(sex = 'msm'),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

#======================================================================
# Create Variance for proportion.tested.n (Added April 2024) --------
#======================================================================
#=====================================================================


#Variance- Total -----------------------------------------------------
variance.total.state = lapply(data.list.brfss.state.totals, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data <- data %>%
    mutate(weight_squared = ((`_LLCPWT`)^2))%>%
    group_by(year, location)%>%
    mutate(sum_each_sq_weight = sum(weight_squared))%>%
    ungroup()%>%
    mutate(variance = proportion_tested*(1-proportion_tested)*(sum_each_sq_weight)/ ((n_weighted)^2))%>% #n_weighted is the sum of the weights by strata
    select(year, location, outcome, variance)%>%
    rename(value = variance) #rename the old value to now be variance.  This now represents the variance metric for the proportion tested outcome
  
  data<- data[!duplicated(data), ]
  
  data= as.data.frame(data)
  list(filename, data) 
})
#Variance- Sex, M+F -----------------------------------------------------
variance.sex.state = lapply(data.list.brfss.state.sex, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data <- data %>%
    mutate(weight_squared = ((`_LLCPWT`)^2))%>%
    group_by(year, location, sex)%>%
    mutate(sum_each_sq_weight = sum(weight_squared))%>%
    ungroup()%>%
    mutate(variance = value*(1-value)*(sum_each_sq_weight)/ ((n_weighted)^2))%>% #n_weighted is the sum of the weights by strata
    select(year, location, outcome, variance, sex)%>%
    rename(value = variance)
  
  data<- data[!duplicated(data), ]
  
  data= as.data.frame(data)
  list(filename, data) 
})
#Variance- Sex, MSM -----------------------------------------------------

variance.sex.state.msm = lapply(data.list.brfss.state.msm, function(file){
    
    data=file[[2]] 
    filename = file[[1]] 
    
    data <- data %>%
        mutate(weight_squared = ((`_LLCPWT`)^2))%>%
        group_by(year, location, sex)%>%
        mutate(sum_each_sq_weight = sum(weight_squared))%>%
        ungroup()%>%
        mutate(variance = value*(1-value)*(sum_each_sq_weight)/ ((n_weighted)^2))%>% #n_weighted is the sum of the weights by strata
        select(year, location, outcome, variance, sex)%>%
        rename(value = variance)
    
    data<- data[!duplicated(data), ]
    
    data= as.data.frame(data)
    list(filename, data) 
})

#Variance- Age -----------------------------------------------------
variance.age.state = lapply(data.list.brfss.state.age, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data <- data %>%
    mutate(weight_squared = ((`_LLCPWT`)^2))%>%
    group_by(year, location, age)%>%
    mutate(sum_each_sq_weight = sum(weight_squared))%>%
    ungroup()%>%
    mutate(variance = value*(1-value)*(sum_each_sq_weight)/ ((n_weighted)^2))%>% #n_weighted is the sum of the weights by strata
    select(year, location, outcome, variance, age)%>%
    rename(value = variance)
  
  data<- data[!duplicated(data), ]
  
  data= as.data.frame(data)
  list(filename, data) 
})

#Variance - Race ---------------------------------------------------
variance.race.state = lapply(data.list.brfss.state.race, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data <- data %>%
    mutate(weight_squared = ((`_LLCPWT`)^2))%>%
    group_by(year, location, race)%>%
    mutate(sum_each_sq_weight = sum(weight_squared))%>%
    ungroup()%>%
    mutate(variance = value*(1-value)*(sum_each_sq_weight)/ ((n_weighted)^2))%>% #n_weighted is the sum of the weights by strata
    select(year, location, outcome, variance, race)%>%
    rename(value = variance)
  
  data<- data[!duplicated(data), ]
  
  data= as.data.frame(data)
  list(filename, data) 
})


# Put the variance data ---------------------------------------------------
prop.tested.variance.state= lapply(variance.total.state, `[[`, 2)

for (data in prop.tested.variance.state) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    metric = 'variance',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}

prop.tested.variance.sex.state= lapply(variance.sex.state, `[[`, 2)

for (data in prop.tested.variance.sex.state) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss.shield',
    source = 'brfss',
    metric = 'variance',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}

prop.tested.variance.sex.state.msm = lapply(variance.sex.state.msm, `[[`, 2)

for (data in prop.tested.variance.sex.state.msm) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss.shield',
        source = 'brfss',
        metric = 'variance',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

prop.tested.variance.age.state= lapply(variance.age.state, `[[`, 2)

for (data in prop.tested.variance.age.state) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    metric = 'variance',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}

prop.tested.variance.race.state= lapply(variance.race.state, `[[`, 2)

for (data in prop.tested.variance.race.state) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    metric = 'variance',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}
