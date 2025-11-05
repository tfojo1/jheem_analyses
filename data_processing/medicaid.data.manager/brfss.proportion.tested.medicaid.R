library(haven)

DATA.DIR.BRFSS.STATE="Q:/data_raw/brfss/brfss_state"

brfss_file_state <- list.files(DATA.DIR.BRFSS.STATE, pattern = ".XPT", full.names = "TRUE")

brfss_file_state_list <- lapply(brfss_file_state, function(x) {
    list(filename=x, data=read_xpt(x))
})

# Create BRFSS Mappings ---------------------------------------------------

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



# Create Template of BRFSS medicaid Data ---------------------------------

inconsistently_named_brfss_variables <- c('_HLTHPLN', 'HLTHPLN1', '_HLTHPL1', '_HLTHPL2',
                                          'HIVTST7', 'HIVTST6', 
                                          '_SEX', 'SEX1', 'SEX',
                                          '_RACE', '_RACE1',
                                          '_AGEG5YR',
                                          'SXORIENT', 'SOMALE',
                                          'PRIMINSR', 'HLTHCVR1', 'PRIMINS2', 'PRIMINS1',
                                          'HIVTSTD3')


brfss.medicaid.template = lapply(brfss_file_state_list, function(file){
    
    data=file[["data"]] 
    filename = file[["filename"]]
    
    #Clean location#
    data$state_fips = as.character(data$`_STATE`)
    data= subset(data, data$state_fips != "66")  #Removing Guam#
    data= subset(data, data$state_fips != "78")  #Removing Virgin Islands#
    
    #Create year
    data$year = str_sub(filename, 35, 38) 
    
    #Select vars of interest
    data <- data%>%
        select(year, state_fips, `_LLCPWT`, (any_of(inconsistently_named_brfss_variables)))
    
    #Rename BRFSS variables across years to have the same name:
    if("SEX1" %in% names(data)){
        data$sex = data$SEX1
    }
    if("SEX" %in% names(data)){
        data$sex = data$SEX
    }
    if("_SEX" %in% names(data)){
        data$sex = data$`_SEX`
    }
    if("_RACE" %in% names(data)){
        data$race = as.character(data$`_RACE`)
    }
    if("_RACE1" %in% names(data)){
        data$race = as.character(data$`_RACE1`)
    }
    if("_AGEG5YR" %in% names(data)){
        data$age = data$`_AGEG5YR`
    }
    if("HIVTST6" %in% names(data)){
        data$ever.tested = data$HIVTST6
    }
    if("HIVTST7" %in% names(data)){
        data$ever.tested = data$HIVTST7
    }
    if("_HLTHPLN" %in% names(data)){
        data$uninsured = data$`_HLTHPLN`
    }
    if("HLTHPLN1" %in% names(data)){
        data$uninsured = data$HLTHPLN1
    }
    if("_HLTHPL1" %in% names(data)){
        data$uninsured = data$`_HLTHPL1`
    }
    if("_HLTHPL2" %in% names(data)){
        data$uninsured = data$`_HLTHPL2`
    }
    if("PRIMINSR" %in% names(data)){
        data$medicaid = ifelse(data$PRIMINSR=="5", "1", "0") #5 is medicaid for PRIMINSR; for this var 1=medicaid
    }
    if("PRIMINS1" %in% names(data)){
        data$medicaid = ifelse(data$PRIMINS1=="5", "1", "0") #5 is medicaid for PRIMINSR1; for this var 1=medicaid
    }   
    if("PRIMINS2" %in% names(data)){
        data$medicaid = ifelse(data$PRIMINS2=="5", "1", "0") #5 is medicaid for PRIMINSR2; for this var 1=medicaid
    }       
    if("HLTHCVR1" %in% names(data)){
        data$medicaid = ifelse(data$HLTHCVR1=="4", "1", "0") #4 is medicaid for HLTHCVR1; for this var 1=medicaid
    }
    
    #Create a risk variable (different for different survey years depending on available variables):
    survey_years_no_risk <- c("2013")
    survey_years_with_sxorient<- c("2014", "2015", "2016", "2017")
    survey_years_with_somale<- c("2018", "2019", "2020", "2021", "2022", "2023", "2024")
    
    if(survey_years_no_risk %in% data$year){
        data$risk = NA #No sexual orientation data for 2013#
    }
    
    if(any(survey_years_with_sxorient %in% data$year)){
        data <- data %>%
            mutate(risk = case_when(SEX== "1" & SXORIENT == "2" ~ "msm",
                                    SEX== "1" & SXORIENT == "3" ~ "msm",
                                    TRUE ~ NA ))    }
    
    if(any(survey_years_with_somale %in% data$year)){
        data$risk = if_else(data$SOMALE == "1" | data$SOMALE == "3", "msm", NA) 
    }
    
    ###SELECT ONLY THOSE ON MEDICAID ###
    #Note: there is no medicaid variable in BRFSS for 2013 or 2015
    data<-data%>%
        mutate(medicaid = ifelse(year == "2013" | year == "2015", NA, medicaid))%>%
        filter(medicaid == "1")
    
    #Create outcome:
    data$outcome = "medicaid.total" 
    
    #Map stratifications:
    data$location = state.to.fips.mappings[data$state_fips]
    data$sex = brfss.sex.mappings[data$sex]
    data$age = brfss.age.mappings[data$age]
    data$race = brfss.race.mappings[data$race]
    data = subset(data, data$race != 'multiracial') #Decided to remove multiracial and unknown on 1-14-24
    data = subset(data, data$race != 'unknown')
    
    list(filename, data) 
})

# Calculate Total medicaid -----------------------------------------------

total.medicaid = lapply(brfss.medicaid.template, function(file){
    
    data=file[[2]] 
    filename = file[[1]] 
    
    data<- data %>%
        group_by(location) %>%
        mutate(n_weighted = sum(`_LLCPWT`)) %>% #denominator should be the sum of weights#
        ungroup()
    
    data$value = data$n_weighted
    
    data <- data %>%
        select(outcome, year, location, value)
    
    data<- data[!duplicated(data), ]
    data= as.data.frame(data)
    
    list(filename, data) 
})


# Create Template to Calculate Proportion Tested amongst medicaid ---------------------------

#HIVSTD3 = date of last test
#HIVTST6 and HIVTST7 = ever tested for hiv; renamed 'ever.tested'

medicaid.proportion.tested.template = lapply(brfss.medicaid.template, function(file){
    
    data=file[[2]] 
    filename = file[[1]] 
    
    data$year = as.numeric(data$year)
    
    #Remove date of last HIV is REFUSED:
    data = subset(data, is.na(data$HIVTSTD3) | data$HIVTSTD3 != 999999) 
    
    #Create variables to help determine if HIV test was in the past year:
    data$test_year = as.numeric(str_sub(data$HIVTSTD3, -4))
    data$test_month = as.numeric(substring(data$HIVTSTD3, 1, nchar(data$HIVTSTD3)-4))
    
    #If month is missing but year is available- we assign '6' (June) to give a 50/50 probability of test being within past year:
    data$test_month = if_else((!is.na(data$test_month) & data$test_month==77), 6, data$test_month)
    
    #Create 'tested' variable to represent probabilities of testing in the past year:
    data$tested = as.numeric(!is.na(data$test_year) & data$test_year >= data$year) 
    mask = !is.na(data$test_year) & data$test_year==(data$year-1)
    data$tested[mask] = data$test_month[mask] / 12
    
    #Correct any dates that are after the survey year:
    data$current_year = data$year
    data$tested = if_else(data$test_year > (data$current_year+1), 0, data$tested) #If they reported a test date in the future, keep in the denominator. Note: someone could be in the 2016 survey data and have gotten tested in 2017.
    
    #Address NAs in the 'tested' variable:
    data$HIVTSTD3 = as.character(data$HIVTSTD3)
    data$tested = ifelse(is.na(data$HIVTSTD3) & data$ever.tested == '1', "0", data$tested) #If date of last test is missing and ever.tested is 'yes'
    data$tested = ifelse(is.na(data$HIVTSTD3) & data$ever.tested == '2', "0", data$tested) #test date is missing and ever.tested is 'no'
    data$tested = ifelse(is.na(data$HIVTSTD3) & data$ever.tested == '7', "0", data$tested) #test date is missing and ever.tested is 'dont know'  ***CHECK THIS ONE***
    data$tested = ifelse(is.na(data$HIVTSTD3) & data$ever.tested == '9', "drop", data$tested) #test date is missing and ever.tested is 'refused'
    data$tested = ifelse(is.na(data$HIVTSTD3) & is.na(data$ever.tested), "drop", data$tested) #test date is missing and ever.tested is missing
    #Remove test date missing ever.tested refused or missing:
    data <- data %>%
        filter(tested != "drop")
    
    data$tested = as.numeric(data$tested)
    
    data$outcome = "proportion.tested.for.hiv.past.year.medicaid"
    
    data= as.data.frame(data)
    
    list(filename, data) 
})


# Calculate Total Proportion Tested medicaid -----------------------------
total.proportion.tested.medicaid = lapply(medicaid.proportion.tested.template, function(file){
    
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

###########################
#STRATIFIED DATA: Total medicaid
##########################

# Stratified: Total medicaid by Sex --------------------------------------
sex.medicaid = lapply(brfss.medicaid.template, function(file){
    
    data=file[[2]] 
    filename = file[[1]] 
    
    data= subset(data, !is.na(data$sex)) #Remove sex is NA
    
    data<- data %>%
        group_by(location, sex) %>%
        mutate(n_weighted = sum(`_LLCPWT`)) %>% #denominator should be the sum of weights#
        ungroup()
    
    data$value = data$n_weighted
    
    #Check the unweighted denominator value - if less than 50, suppress:
    data<-data%>%
        group_by(location, sex)%>%
        mutate(unweighted.denominator.check = n())%>%
        ungroup()%>%
        mutate(suppression.indicator = ifelse(unweighted.denominator.check < 50, "suppress", "keep")) %>%
        filter(suppression.indicator != "suppress")%>%
        select(outcome, year, location, value, sex)
    
    data<- data[!duplicated(data), ]
    data= as.data.frame(data)
    
    list(filename, data) 
})

# Stratified: Total medicaid by age --------------------------------------
age.medicaid = lapply(brfss.medicaid.template, function(file){
    
    data=file[[2]] 
    filename = file[[1]] 
    
    data= subset(data, !is.na(data$age)) #Remove missing age
    data= subset(data, data$age != 'Unknown')
    
    data<- data %>%
        group_by(location, age) %>%
        mutate(n_weighted = sum(`_LLCPWT`)) %>% #denominator should be the sum of weights#
        ungroup()
    
    data$value = data$n_weighted
    
    #Check the unweighted denominator value - if less than 50, suppress:
    data<-data%>%
        group_by(location, age)%>%
        mutate(unweighted.denominator.check = n())%>%
        ungroup()%>%
        mutate(suppression.indicator = ifelse(unweighted.denominator.check < 50, "suppress", "keep")) %>%
        filter(suppression.indicator != "suppress")%>%
      select(outcome, year, location, value, age)
    
    data<- data[!duplicated(data), ]
    data= as.data.frame(data)
    
    list(filename, data) 
})

# Stratified: Total medicaid by race --------------------------------------
race.medicaid = lapply(brfss.medicaid.template, function(file){
    
    data=file[[2]] 
    filename = file[[1]] 
    
    data= subset(data, !is.na(data$race)) #Remove unknown race
    data= subset(data, data$race != 'unknown')
    
    data<- data %>%
        group_by(location, race) %>%
        mutate(n_weighted = sum(`_LLCPWT`)) %>% #denominator should be the sum of weights#
        ungroup()
    
    data$value = data$n_weighted
    
    #Check the unweighted denominator value - if less than 50, suppress:
    data<-data%>%
        group_by(location, race)%>%
        mutate(unweighted.denominator.check = n())%>%
        ungroup()%>%
        mutate(suppression.indicator = ifelse(unweighted.denominator.check < 50, "suppress", "keep")) %>%
        filter(suppression.indicator != "suppress")%>%
       select(outcome, year, location, value, race)
    
    
    data<- data[!duplicated(data), ]
    data= as.data.frame(data)
    
    list(filename, data) 
})

# Stratified: Total medicaid by risk --------------------------------------
risk.medicaid = lapply(brfss.medicaid.template, function(file){
    
    data=file[[2]] 
    filename = file[[1]] 
    
    data<- data %>%
        group_by(location, risk) %>%
        mutate(n_weighted = sum(`_LLCPWT`)) %>% #denominator should be the sum of weights#
        ungroup()
    
    data$value = data$n_weighted
    
    #Check the unweighted denominator value - if less than 50, suppress:
    data<-data%>%
        group_by(location, risk)%>%
        mutate(unweighted.denominator.check = n())%>%
        ungroup()%>%
        mutate(suppression.indicator = ifelse(unweighted.denominator.check < 50, "suppress", "keep"))%>%
       filter(suppression.indicator != "suppress")%>%
       select(outcome, year, location, value, risk)
    
    data<- data[!duplicated(data), ]
    
    data <- data %>%
        filter(risk == "msm")
    
    data= as.data.frame(data)
    
    list(filename, data) 
})

##########################
#STRATIFIED DATA: Proportion.Tested
########################

# Calculate Sex Proportion Tested medicaid -----------------------------
sex.proportion.tested.medicaid = lapply(medicaid.proportion.tested.template, function(file){
    
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
        mutate(proportion_tested = (sum_tested/n_weighted))
    
    data$proportion_tested = round(data$proportion_tested, digits=4)
    
    data$year = as.character(data$year)
    data$value = data$proportion_tested
    
    #Check the unweighted denominator value - if less than 50, suppress:
    data<-data%>%
        group_by(location, sex)%>%
        mutate(unweighted.denominator.check = n())%>%
        ungroup()%>%
        mutate(suppression.indicator = ifelse(unweighted.denominator.check < 50, "suppress", "keep")) %>%
        filter(suppression.indicator != "suppress")%>%
        select(outcome, year, location, value, sex)
    
    data= as.data.frame(data)
    
    list(filename, data) 
})

# Calculate age Proportion Tested medicaid -----------------------------
age.proportion.tested.medicaid = lapply(medicaid.proportion.tested.template, function(file){
    
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
        mutate(proportion_tested = (sum_tested/n_weighted))
    
    data$proportion_tested = round(data$proportion_tested, digits=4)
    
    data$year = as.character(data$year)
    data$value = data$proportion_tested
    
    #Check the unweighted denominator value - if less than 50, suppress:
    data<-data%>%
        group_by(location, age)%>%
        mutate(unweighted.denominator.check = n())%>%
        ungroup()%>%
        mutate(suppression.indicator = ifelse(unweighted.denominator.check < 50, "suppress", "keep")) %>%
        filter(suppression.indicator != "suppress")%>%
        select(outcome, year, location, value, age)
    
    data= as.data.frame(data)
    
    list(filename, data) 
})

# Calculate race Proportion Tested medicaid -----------------------------
race.proportion.tested.medicaid = lapply(medicaid.proportion.tested.template, function(file){
    
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
        mutate(proportion_tested = (sum_tested/n_weighted))
    
    data$proportion_tested = round(data$proportion_tested, digits=4)
    
    data$year = as.character(data$year)
    data$value = data$proportion_tested
    
    #Check the unweighted denominator value - if less than 50, suppress:
    data<-data%>%
        group_by(location, race)%>%
        mutate(unweighted.denominator.check = n())%>%
        ungroup()%>%
        mutate(suppression.indicator = ifelse(unweighted.denominator.check < 50, "suppress", "keep")) %>%
        filter(suppression.indicator != "suppress")%>%
        select(outcome, year, location, value, race)
    
    data= as.data.frame(data)
    
    list(filename, data) 
})


# Calculate risk Proportion Tested medicaid -----------------------------
risk.proportion.tested.medicaid = lapply(medicaid.proportion.tested.template, function(file){
    
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
        mutate(proportion_tested = (sum_tested/n_weighted))
    
    data$proportion_tested = round(data$proportion_tested, digits=4)
    
    data$year = as.character(data$year)
    data$value = data$proportion_tested
    
    #Select only risk = MSM
    data<-data%>%
        filter(risk == "msm")
    
    #Check the unweighted denominator value - if less than 50, suppress:
    data<-data%>%
        group_by(location, risk)%>%
        mutate(unweighted.denominator.check = n())%>%
        ungroup()%>%
        mutate(suppression.indicator = ifelse(unweighted.denominator.check < 50, "suppress", "keep")) %>%
        filter(suppression.indicator != "suppress")%>%
        select(outcome, year, location, value, risk)
    
    data= as.data.frame(data)
    
    list(filename, data) 
})


# Put statements: ---------------------------------------------------------

total.medicaid.put = lapply(total.medicaid, `[[`, 2)  

for (data in total.medicaid.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss',
        source = 'brfss',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

total.proportion.tested.medicaid.put = lapply(total.proportion.tested.medicaid, `[[`, 2)  

for (data in total.proportion.tested.medicaid.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss',
        source = 'brfss',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

sex.medicaid.put = lapply(sex.medicaid, `[[`, 2)  

for (data in sex.medicaid.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss',
        source = 'brfss',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

sex.proportion.tested.medicaid.put = lapply(sex.proportion.tested.medicaid, `[[`, 2)  

for (data in sex.proportion.tested.medicaid.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss',
        source = 'brfss',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

age.medicaid.put = lapply(age.medicaid, `[[`, 2)  

for (data in age.medicaid.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss',
        source = 'brfss',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

age.proportion.tested.medicaid.put = lapply(age.proportion.tested.medicaid, `[[`, 2)  

for (data in age.proportion.tested.medicaid.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss',
        source = 'brfss',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

race.medicaid.put = lapply(race.medicaid, `[[`, 2)  

for (data in race.medicaid.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss',
        source = 'brfss',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

race.proportion.tested.medicaid.put = lapply(race.proportion.tested.medicaid, `[[`, 2)  

for (data in race.proportion.tested.medicaid.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss',
        source = 'brfss',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

risk.medicaid.put = lapply(risk.medicaid, `[[`, 2)  

for (data in risk.medicaid.put) {
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss',
        source = 'brfss',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}

risk.proportion.tested.medicaid.put = lapply(risk.proportion.tested.medicaid, `[[`, 2)  

for (data in risk.proportion.tested.medicaid.put) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss',
        source = 'brfss',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}
