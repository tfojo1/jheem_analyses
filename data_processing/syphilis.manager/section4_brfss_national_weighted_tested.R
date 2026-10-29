

#This codes uses data from BRFSS to create a proportion.msm and proportion.tested
#data at the national level.  It pulls a list of cleaned dataframes from a previous code:
#'data_processing/syphilis.manager/section4_proportion.tested.for.hiv_state_weighted.R'


#==============================================================================
#Read in BRFSS Files
#==============================================================================

DATA.DIR.BRFSS.STATE=file.path(Q_ROOT, "data_raw/brfss/brfss_state")

brfss_file_state <- list.files(DATA.DIR.BRFSS.STATE, pattern = ".XPT", full.names = "TRUE")

brfss_file_state_list <- lapply(brfss_file_state, function(x) {
  list(filename=x, data=read_xpt(x))
})

#==============================================================================
#Create a clean template of BRFSS State data

#Outcome: Proportion.tested
#==============================================================================

data.list.brfss.national.clean = lapply(data.list.brfss.state.clean, function(file){
  
    data=file[[2]] 
    filename = file[[1]]
  
  data$location = "US"
  data$outcome = "proportion.tested.for.hiv" 


  list(filename, data) 
})
#==============================================================================

#Outcome: proportion.tested
#Strata: Total (weighted)
#Location: National

#==============================================================================
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

#Put
national.total.num = lapply(data.list.brfss.national.totals, `[[`, 2)  

for (data in national.total.num) {
    
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
#Strata: Sex (weighted) (male, female, MSM)
#Location: National

#==============================================================================
data.list.brfss.national.sex = lapply(data.list.brfss.national.clean, function(file){
    
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
        mutate(proportion_tested = (sum_tested/n_weighted)) #denominators needs to be sum of weights by location by sex#
    
    data$proportion_tested = round(data$proportion_tested, digits=2)
    
    data$year = as.character(data$year)
    data$value = data$proportion_tested
    
    data <- data %>%
        select(outcome, year, location, sum_tested, n_weighted, value, sex, `_LLCPWT`)
    
    data= as.data.frame(data)
    
    list(filename, data) 
})

#Put
national.sex.num = lapply(data.list.brfss.national.sex, `[[`, 2)  

for (data in national.sex.num) {
    
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
#Location: National

#==============================================================================
data.list.brfss.national.age = lapply(data.list.brfss.national.clean, function(file){
  
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
national.age.num = lapply(data.list.brfss.national.age, `[[`, 2)  

for (data in national.age.num) {
    
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
#Location: National

#==============================================================================
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

#Put
national.race.num = lapply(data.list.brfss.national.race, `[[`, 2)  

for (data in national.race.num) {
    
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
#Location: National

#==============================================================================
data.list.brfss.national.n = lapply(data.list.brfss.national.totals, function(file){
  
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
national.total.denom = lapply(data.list.brfss.national.n, `[[`, 2)  

for (data in national.total.denom) {
    
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
#Strata: Sex (weighted) (male, female, msm)
#Location: National

#==============================================================================
data.list.brfss.national.sex.fixed <- keep(data.list.brfss.national.sex, ~ nrow(.x[[2]]) > 0) #Remove 2013 without data

data.list.brfss.national.sex.n = lapply(data.list.brfss.national.sex.fixed, function(file){
    
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
national.sex.denom = lapply(data.list.brfss.national.sex.n, `[[`, 2)  

for (data in national.sex.denom) {
    
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
#Location: National

#==============================================================================
data.list.brfss.national.age.n = lapply(data.list.brfss.national.age, function(file){
  
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
national.age.denom = lapply(data.list.brfss.national.age.n, `[[`, 2)  

for (data in national.age.denom) {
    
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
#Location: National

#==============================================================================
data.list.brfss.national.race.n = lapply(data.list.brfss.national.race, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data$outcome = "proportion.tested.for.hiv.n"
  data$value = data$n_weighted #replace the "population" calculated above as the outcome value
  
  data <- data %>%
    select(outcome, year, location, race, value)
  
  data= as.data.frame(data)
  list(filename, data) 
})

#Put
national.race.denom = lapply(data.list.brfss.national.race.n, `[[`, 2)  

for (data in national.race.denom) {
    
    data.manager$put.long.form(
        data = data,
        ontology.name = 'brfss.shield',
        source = 'brfss',
        dimension.values = list(),
        url = 'https://www.cdc.gov/brfss/index.html',
        details = 'Behavioral Risk Factor Surveillance System')
}


  
