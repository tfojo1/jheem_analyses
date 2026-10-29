#This code processes BRFSS data to create state level proportion.tested.for.hiv
#as well as proportion.tested.for.hiv.n


#This code also creates a dataframe that is later used for 'proportion.msm'

#This code was updated for SHIELD to create a mutually exclusive sex dimension
#that includes male, female, msm.

#==============================================================================
#Read in BRFSS Files
#==============================================================================

DATA.DIR.BRFSS.STATE=file.path(Q_ROOT, "data_raw/brfss/brfss_state")

brfss_file_state <- list.files(DATA.DIR.BRFSS.STATE, pattern = ".XPT", full.names = "TRUE")

brfss_file_state_list <- lapply(brfss_file_state, function(x) {
  list(filename=x, data=read_xpt(x))
})

#==============================================================================
#Create Mappings
#==============================================================================
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

#Mapping other, white, and multiracial to other to align with SHIELD:
brfss.race.mapping_RACE_G1= c('1'= 'other',
                              '2'= 'black',
                              '3'= 'hispanic',
                              '4'= 'other',
                              '5'= 'other') #blank is NA

brfss.race.mappings_RACEGR3= c('1'= 'other',
                               '2'= 'black',
                               '3'= 'other',
                               '4'= 'other',
                               '5'= 'hispanic',
                               '9'= NA)

brfss.race.mappings_RACEGR4= c('1'= 'other',
                               '2'= 'black',
                               '3'= 'other',
                               '4'= 'other',
                               '5'= 'hispanic',
                               '9'= NA)
#==============================================================================

#Creating a template of clean BRFSS Data for outcome proportion.tested

#==============================================================================

data.list.brfss.state.clean = lapply(brfss_file_state_list, function(file){
    
    data=file[["data"]] 
    filename = file[["filename"]] 
    
  
    data$state_fips = as.character(data$`_STATE`)
    data= subset(data, data$state_fips != "66")  #Removing Guam#
    data= subset(data, data$state_fips != "78")  #Removing Virgin Islands#
    
    if(grepl("2013", filename)) {
        data$year = as.numeric("2013")
        data$sex = as.character(data$SEX)
        data$race = data$`_RACE_G1`
        data$age = data$`_AGEG5YR`  
        data$risk = NA #No sexual orientation data for 2013#
        data$ever.tested = data$HIVTST6
        data$race = brfss.race.mapping_RACE_G1[data$race]
    }
    if(grepl("2014", filename)) {
        data$year = as.numeric("2014")
        data$sex = as.character(data$SEX)
        data$race = data$`_RACE_G1`
        data$age = data$`_AGEG5YR`
        data <- data %>%
            mutate(risk = case_when(SEX== "1" & SXORIENT == "2" ~ "msm",
                                    SEX== "1" & SXORIENT == "3" ~ "msm",
                                    TRUE ~ NA ))
        data$ever.tested = data$HIVTST6
        data$race = brfss.race.mapping_RACE_G1[data$race]
    }
    if(grepl("2015", filename)) {
        data$year = as.numeric("2015")
        data$sex = as.character(data$SEX)
        data$race = data$`_RACE_G1`
        data$age = data$`_AGEG5YR`
        data <- data %>%
            mutate(risk = case_when(SEX== "1" & SXORIENT == "2" ~ "msm",
                                    SEX== "1" & SXORIENT == "3" ~ "msm",
                                    TRUE ~ NA ))
        data$ever.tested = data$HIVTST6
        data$race = brfss.race.mapping_RACE_G1[data$race]
    }
    if(grepl("2016", filename)) {
        data$year = as.numeric("2016")
        data$sex = as.character(data$SEX)
        data$race = data$`_RACE_G1`
        data$age = data$`_AGEG5YR`
        data <- data %>%
            mutate(risk = case_when(SEX== "1" & SXORIENT == "2" ~ "msm",
                                    SEX== "1" & SXORIENT == "3" ~ "msm",
                                    TRUE ~ NA )) %>%
            mutate(at_risk = if_else(HIVRISK4 =="1", '1', '0'))  #A value of 1 means individual is at risk; else 0; for denominator#
        data$ever.tested = data$HIVTST6
        data$race = brfss.race.mapping_RACE_G1[data$race]
    }
    if(grepl("2017", filename)) {
        data$year = as.numeric("2017")
        data$sex = as.character(data$SEX)
        data$race = data$`_RACE_G1`
        data$age = data$`_AGEG5YR`
        data <- data %>%
            mutate(risk = case_when(SEX== "1" & SXORIENT == "2" ~ "msm",
                                    SEX== "1" & SXORIENT == "3" ~ "msm",
                                    TRUE ~ NA )) %>%
            mutate(at_risk = if_else(HIVRISK5 =="1", '1', '0'))  #A value of 1 means individual is at risk; else 0; for denominator#
        data$ever.tested = data$HIVTST6
        data$race = brfss.race.mapping_RACE_G1[data$race]
    }
    if(grepl("2018", filename)) {
        data$year = as.numeric("2018")
        data$sex = as.character(data$SEX1)
        data$race = data$`_RACE_G1`
        data$age = data$`_AGEG5YR`
        data$risk = if_else(data$SOMALE == "1" | data$SOMALE == "3", "msm", NA) 
        data <- data %>%
            mutate(at_risk = if_else(HIVRISK5 =="1", '1', '0'))  #A value of 1 means individual is at risk; else 0; for denominator#
        data$ever.tested = data$HIVTST6
        data$race = brfss.race.mapping_RACE_G1[data$race]
    }
    if(grepl("2019", filename)) {
        data$year = as.numeric("2019")
        data$sex = as.character(data$`_SEX`)
        data$race = data$`_RACE_G1`
        data$age = data$`_AGEG5YR`
        data$risk = if_else(data$SOMALE == "1" | data$SOMALE == "3", "msm", NA)
        data <- data %>%
            mutate(at_risk = if_else(HIVRISK5 =="1", '1', '0'))  #A value of 1 means individual is at risk; else 0; for denominator#
        data$ever.tested = data$HIVTST7
        data$race = brfss.race.mapping_RACE_G1[data$race]
    }
    if(grepl("2020", filename)) {
        data$year = as.numeric("2020")
        data$sex = as.character(data$`_SEX`)
        data$race = data$`_RACEGR3`
        data$age = data$`_AGEG5YR`
        data$risk = if_else(data$SOMALE == "1" | data$SOMALE == "3", "msm", NA)
        data <- data %>%
            mutate(at_risk = if_else(HIVRISK5 =="1", '1', '0'))  #A value of 1 means individual is at risk; else 0; for denominator#
        data$ever.tested = data$HIVTST7
        data$race = brfss.race.mappings_RACEGR3[data$race]
        
    }
    if(grepl("2021", filename)) {
        data$year = as.numeric("2021")
        data$sex = as.character(data$`_SEX`)
        data$race = data$`_RACEGR3`
        data$age = data$`_AGEG5YR`
        data$risk = if_else(data$SOMALE == "1" | data$SOMALE == "3", "msm", NA)
        data$ever.tested = data$HIVTST7
        data$race = brfss.race.mappings_RACEGR3[data$race]
        
    }
    if(grepl("2022", filename)) {
        data$year = as.numeric("2022")
        data$sex = as.character(data$`_SEX`)
        data$race = data$`_RACEGR4`
        data$age = data$`_AGEG5YR`
        data$risk = if_else(data$SOMALE == "1" | data$SOMALE == "3", "msm", NA) 
        data <- data %>%
            mutate(at_risk = if_else(HIVRISK5 =="1", '1', '0'))  #A value of 1 means individual is at risk; else 0; for denominator#
        data$ever.tested = data$HIVTST7
        data$race = brfss.race.mappings_RACEGR4[data$race]
    }
    
    if(grepl("2023", filename)) {
        data$year = as.numeric("2023")
        data$race = data$`_RACEGR3`
        data$sex = data$`_SEX`
        data$age = data$`_AGEG5YR`
        data$ever.tested = data$HIVTST7
        data$risk = if_else(data$SOMALE == "1" | data$SOMALE == "3", "msm", NA)
        data$race = brfss.race.mappings_RACEGR3[data$race]
    }
    
    if(grepl("2024", filename)) {
        data$year = as.numeric("2024")
        data$race = data$`_RACEGR3`
        data$sex = data$`_SEX`
        data$age = data$`_AGEG5YR`
        data$ever.tested = data$HIVTST7
        data$risk = if_else(data$SOMALE == "1" | data$SOMALE == "3", "msm", NA)
        data$race = brfss.race.mappings_RACEGR3[data$race]
    }
    
    data$location = state.to.fips.mappings[data$state_fips]
    data$outcome = "proportion.tested.for.hiv" 
    
    data$sex = brfss.sex.mappings[data$sex]
    data$age = brfss.age.mappings[data$age]
    
    data = subset(data, is.na(data$HIVTSTD3) | data$HIVTSTD3 != 999999) #Remove date of last HIV is refused
    
    # Create 'tested' variable (used to determine if HIV test is in past year)------------------------------------------------
    
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

#==============================================================================

#Outcome: proportion.tested
#Strata: Total (weighted)
#Location: State

#==============================================================================
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

#Put
state.total.num = lapply(data.list.brfss.state.totals, `[[`, 2)  

for (data in state.total.num) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss.shield',
        source = 'brfss',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

#===================================================================

#Outcome: proportion.tested
#Strata: Sex (weighted)
#Location: State

#Updating for SHIELD: Sex Dimension needs to be Male, Female, MSM 
#These are mutually exclusive

#Removing risk dimension
#=====================================================================

data.list.brfss.state.sex = lapply(data.list.brfss.state.clean, function(file){
    
    data=file[[2]] 
    filename = file[[1]] 
    
    data= subset(data, data$year != "2013") #Remove year != to 2013 bc this year does not have sexual orientation data that creates this variable
    
    data<- data %>%
        mutate(shield.sex = case_when(risk == "msm" ~ "msm",
                                      sex == "female" ~ "female",
                                      TRUE ~ "male"))%>%
        group_by(location, shield.sex) %>%
        mutate(n_weighted = sum(`_LLCPWT`)) %>% #denominator should be the sum of weights#
        ungroup()%>%
        rename(brfss.sex = sex)%>%
        rename(sex = shield.sex)
    
    data<- data %>%
        group_by(location, sex) %>%
        mutate(sum_tested = sum(tested*`_LLCPWT`)) %>% #multiply numerator value by the weight value#
        ungroup()%>%
        mutate(proportion_tested = (sum_tested/n_weighted))#denominators needs to be sum of weights by location by sex#
    
    data$proportion_tested = round(data$proportion_tested, digits=2)
    
    data$year = as.character(data$year)
    data$value = data$proportion_tested
    
    data <- data %>%
        select(outcome, year, location, sum_tested, n_weighted, value, sex, `_LLCPWT`)
    
    data= as.data.frame(data)
    
    list(filename, data) 
})

#Put
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

#==============================================================================

#Outcome: proportion.tested
#Strata: Age (weighted)
#Location: State

#==============================================================================
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

#Put
state.age.num = lapply(data.list.brfss.state.age, `[[`, 2)  

for (data in state.age.num) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss.shield',
        source = 'brfss',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}
#==============================================================================

#Outcome: proportion.tested
#Strata: Race (weighted)
#Location: State

#==============================================================================
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

#Put

state.race.num = lapply(data.list.brfss.state.race, `[[`, 2)  

for (data in state.race.num) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss.shield',
        source = 'brfss',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

#==============================================================================

#Outcome: proportion.tested.n
#Strata: Total (weighted)
#Location: State

#==============================================================================
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
#Put
state.total.denom = lapply(data.list.brfss.state.n, `[[`, 2)  

for (data in state.total.denom) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss.shield',
        source = 'brfss',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

#==============================================================================

#Outcome: proportion.tested.n
#Strata: Sex (weighted)
#Location: State

#==============================================================================
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

#Put
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
#==============================================================================

#Outcome: proportion.tested.n
#Strata: Age (weighted)
#Location: State

#==============================================================================
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

#Put  

state.age.denom = lapply(data.list.brfss.state.age.n, `[[`, 2)  

for (data in state.age.denom) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss.shield',
        source = 'brfss',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}
#==============================================================================

#Outcome: proportion.tested.n
#Strata: Race (weighted)
#Location: State

#==============================================================================
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

#Put:
state.race.denom = lapply(data.list.brfss.state.race.n, `[[`, 2)  

for (data in state.race.denom) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss.shield',
        source = 'brfss',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}


#======================================================================

#Add Variance for Outcome proportion.tested

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
#Variance- Sex, (male, female, msm) -----------------------------------------------------
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
    ontology.name = 'brfss.shield',
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

prop.tested.variance.age.state= lapply(variance.age.state, `[[`, 2)

for (data in prop.tested.variance.age.state) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss.shield',
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
    ontology.name = 'brfss.shield',
    source = 'brfss',
    metric = 'variance',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}
