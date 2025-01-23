################################################################################
#Weighted Proportion of MSM from BRFSS
#Note this pulls a dataset created within the brfss_national_weighted_tested file
################################################################################
##Create a version of the brfss.state.sex data list to use below##
data.list.brfss.national.sex.for.msm = lapply(data.list.brfss.national.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data$location = "US"
  
  data= subset(data, !is.na(data$sex)) #Need to remove sex is NA
  
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
    select(outcome, year, location, sum_tested, n_weighted, value, sex, `_LLCPWT`, race, risk, age)
  
  data= as.data.frame(data)
  
  list(filename, data) 
})

#######
##Total
#######
#Pulling from the brffs.state.sex data list so that denominator for msm proportion is males only
data.list.brfss.national.msm = lapply(data.list.brfss.national.sex.for.msm, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  #Create MSM proportion
  data$outcome= "proportion.msm"
  data$msm = as.numeric(if_else(data$risk == "msm", "1", "0"))
  
  data<- data %>%
    group_by(location, msm) %>%
    mutate(msm_total = sum(msm*`_LLCPWT`))%>%
    ungroup()
  
  data <- data %>%
    select(outcome, year, location, msm, msm_total, n_weighted)%>% #n_weighted here is the sum of the weights by sex#
    filter(!is.na(msm_total))%>%
    mutate(value = (msm_total/n_weighted))
  
  data$value = round(data$value, digits=2)
  
  data<- data[!duplicated(data), ]
  
  #Need to add sex column in for put statment dimensions
  data$sex = "male"
  
  data= as.data.frame(data)
  list(filename, data) 
})

################################################################################
##Prior to calculating MSM proportion for race and age, need to create
##Denominator values that are total males of each race and total males of each
#age group so that MSM proportion can have male only denominator and be stratified by race/age appropriately
################################################################################
data.list.race.male.denom = lapply(data.list.brfss.national.sex.for.msm, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data= subset(data, data$sex =="male")
  
  data<- data %>%
    group_by(location, race) %>%
    mutate(n_weighted = sum(`_LLCPWT`)) %>% #denominator should be the sum of weights#
    ungroup()
  
  data= as.data.frame(data)
  list(filename, data) 
})
data.list.age.male.denom = lapply(data.list.brfss.national.sex.for.msm, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data= subset(data, data$sex =="male")
  
  data<- data %>%
    group_by(location, age) %>%
    mutate(n_weighted = sum(`_LLCPWT`)) %>% #denominator should be the sum of weights#
    ungroup()
  
  data= as.data.frame(data)
  list(filename, data) 
})
################################################################################
################################################################################

#######
##Race
#######

data.list.brfss.national.msm.race = lapply(data.list.race.male.denom, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data= subset(data, data$race != "Unknown")
  
  #Create MSM proportion
  data$outcome= "proportion.msm"
  data$msm = as.numeric(if_else(data$risk == "msm", "1", "0"))
  
  data<- data %>%
    group_by(location,msm, race) %>%
    mutate(msm_total = sum(msm*`_LLCPWT`))%>% 
    ungroup()
  
  data <- data %>%
    select(outcome, year, location, race, msm, msm_total, n_weighted)%>%
    filter(!is.na(msm_total))%>%
    mutate(value = (msm_total/n_weighted))%>%
    mutate(race = tolower(race))
  
  data$value = round(data$value, digits=2)
  
  data<- data[!duplicated(data), ]
  
  #Need to add sex column in for put statment dimensions
  data$sex = "male"
  
  data= as.data.frame(data)
  list(filename, data) 
})

#######
##Age
#######

##this needs to be just males of the age group##

data.list.brfss.national.msm.age = lapply(data.list.age.male.denom, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data= subset(data, data$age != "Unknown")
  
  #Create MSM proportion
  data$outcome= "proportion.msm"
  data$msm = as.numeric(if_else(data$risk == "msm", "1", "0"))
  
  data<- data %>%
    group_by(location,msm,age) %>%
    mutate(msm_total = sum(msm*`_LLCPWT`))%>% 
    ungroup()
  
  data <- data %>%
    select(outcome, year, location, age, msm, msm_total, n_weighted)%>%
    filter(!is.na(msm_total))%>%
    mutate(value = (msm_total/n_weighted))
  
  data$value = round(data$value, digits=2)
  
  data<- data[!duplicated(data), ]
  
  #Need to add sex column in for put statment dimensions
  data$sex = "male"
  
  data= as.data.frame(data)
  list(filename, data) 
})
###############################################################################
##Put MSM Proportion into data manager- BRFSS##
###############################################################################
##BRFSS MSM State-Total
msm.state.total = lapply(data.list.brfss.national.msm, `[[`, 2)  

for (data in msm.state.total) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss.msm',
    source = 'brfss',
    dimension.values = list(sex = "male"),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}

##BRFSS MSM State- RACE
msm.state.race = lapply(data.list.brfss.national.msm.race, `[[`, 2)  

for (data in msm.state.race) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss.msm',
    source = 'brfss',
    dimension.values = list(sex = "male"),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}

##BRFSS MSM State-AGE
msm.state.age = lapply(data.list.brfss.national.msm.age, `[[`, 2)  

for (data in msm.state.age) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss.msm',
    source = 'brfss',
    dimension.values = list(sex = "male"),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}