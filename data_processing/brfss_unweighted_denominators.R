################################################################################


            ###STATE UNWEIGHTED DENOMINATORS##


################################################################################

################################################################################
##Total-by state## 
#UNWEIGHTED#
################################################################################
data.list.brfss.state.totals.n_unweighted = lapply(data.list.brfss.state.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  #Create population variable-count of BRFSS responses by state#
  countspop <- table(data$location)
  data$n <-countspop[match(data$location, names(countspop))]
  
  data$year = as.character(data$year)
  data$value = data$n
  
  data$outcome = "unweighted.denominator"
  
  data <- data %>%
    select(outcome, year, location, value, n)
  
  data<- data[!duplicated(data), ]
  
  data= as.data.frame(data)
  
  list(filename, data) 
})

################################################################################
##Sex-by state
#UNWEIGHTED#
################################################################################
data.list.brfss.state.sex.n_unweighted = lapply(data.list.brfss.state.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]]
  
  data = subset(data, !is.na(data$sex))
  
  data<- data %>%
    add_count(location, sex) %>%     #Create population variable-count of BRFSS responses by state#
    group_by(location, sex)
  
  data$year = as.character(data$year)
  data$value = data$n
  
  data$outcome = "unweighted.denominator"
  
  data <- data %>%
    select(outcome, year, location, value, sex, n)
  
  data<- data[!duplicated(data), ]
  
  data= as.data.frame(data)
  
  list(filename, data) 
})
################################################################################
##Age-by state#
#UNWEIGHTED#
################################################################################
data.list.brfss.state.age.n_unweighted = lapply(data.list.brfss.state.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data = subset(data, !is.na(data$age))
  
  data<- data %>%
    add_count(location, age) %>%     #Create population variable-count of BRFSS responses by state#
    group_by(location, age) 
  
  data$year = as.character(data$year)
  data$value = data$n
  
  data$outcome = "unweighted.denominator"
  
  data <- data %>%
    select(outcome, year, location, value, age, n)
  
  data<- data[!duplicated(data), ]
  
  data= as.data.frame(data)
  
  list(filename, data) 
})
################################################################################
##Race-by state#  
#UNWEIGHTED#
################################################################################
data.list.brfss.state.race.n_unweighted = lapply(data.list.brfss.state.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data = subset(data, !is.na(data$race))
  
  data<- data %>%
    add_count(location, race) %>%     #Create population variable-count of BRFSS responses by state#
    group_by(location, race) 
  
  data$year = as.character(data$year)
  data$value = data$n
  
  data$outcome = "unweighted.denominator"
  
  data <- data %>%
    select(outcome, year, location, value, race, n)
  
  data<- data[!duplicated(data), ]
  
  data= as.data.frame(data)
  
  list(filename, data) 
})
################################################################################
##Risk-by state-MSM Only
################################################################################
data.list.brfss.state.risk.n_unweighted = lapply(data.list.brfss.state.clean, function(file){

  data=file[[2]]
  filename = file[[1]]

  data<- data %>%
    add_count(location, risk) %>%     #Create population variable-count of BRFSS responses by state#
    group_by(location, risk)

  data$year = as.character(data$year)
  data$value = data$n

  data$outcome = "unweighted.denominator"

  data <- data %>%
    select(outcome, year, location, value, risk, n)%>%
    filter(!is.na(risk))

  data<- data[!duplicated(data), ]

  data= as.data.frame(data)

  list(filename, data)
})

################################################################################

##Put unweighted denominators for state###

################################################################################

total.unweighted = lapply(data.list.brfss.state.totals.n_unweighted, `[[`, 2)  

for (data in total.unweighted) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}

sex.unweighted = lapply(data.list.brfss.state.sex.n_unweighted, `[[`, 2)  

for (data in sex.unweighted) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}

age.unweighted = lapply(data.list.brfss.state.age.n_unweighted, `[[`, 2)  

for (data in age.unweighted) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}

race.unweighted = lapply(data.list.brfss.state.race.n_unweighted, `[[`, 2)  

for (data in race.unweighted) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}

risk.unweighted = lapply(data.list.brfss.state.risk.n_unweighted, `[[`, 2)  

for (data in risk.unweighted) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}


################################################################################


###MSA UNWEIGHTED DENOMINATORS##


################################################################################
##MSA - Total
data.list.brfss.msa.totals.n_unweighted = lapply(data.list.brfss.msa.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  #Create population variable-count of BRFSS responses by state#
  countspop <- table(data$location)
  data$n <-countspop[match(data$location, names(countspop))]
  
  data$year = as.character(data$year)
  data$value = data$n
  
  data$outcome = "unweighted.denominator"
  
  data <- data %>%
    select(outcome, year, location, value, n)
  
  data<- data[!duplicated(data), ]
  
  data= as.data.frame(data)
  
  list(filename, data) 
})

##MSA - SEX
data.list.brfss.msa.sex.n_unweighted = lapply(data.list.brfss.msa.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data<- data %>%
    add_count(location, sex) %>%     #Create population variable-count of BRFSS responses by state#
    group_by(location, sex)
  
  data$year = as.character(data$year)
  data$value = data$n
  
  data$outcome = "unweighted.denominator"
  
  data <- data %>%
    select(outcome, year, location, value, sex, n)
  
  data<- data[!duplicated(data), ]
  
  data= as.data.frame(data)
  
  list(filename, data) 
})

##MSA - Age
data.list.brfss.msa.age.n_unweighted = lapply(data.list.brfss.msa.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data<- data %>%
    add_count(location, age) %>%     #Create population variable-count of BRFSS responses by state#
    group_by(location, age) 
  
  data$year = as.character(data$year)
  data$value = data$n
  
  data$outcome = "unweighted.denominator"
  
  data <- data %>%
    select(outcome, year, location, value, age, n)
  
  data<- data[!duplicated(data), ]
  
  data= as.data.frame(data)
  
  list(filename, data) 
})

##MSA- RACE

data.list.brfss.msa.race.n_unweighted = lapply(data.list.brfss.msa.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data<- data %>%
    add_count(location, race) %>%     #Create population variable-count of BRFSS responses by state#
    group_by(location, race) 
  
  data$year = as.character(data$year)
  data$value = data$n
  
  data$outcome = "unweighted.denominator"
  
  data <- data %>%
    select(outcome, year, location, value, race, n)
  
  data<- data[!duplicated(data), ]
  
  data= as.data.frame(data)
  
  list(filename, data) 
})
################################################################################


##Put MSA Unweighted denominators


################################################################################
total.unweighted.msa = lapply(data.list.brfss.msa.totals.n_unweighted, `[[`, 2)  

for (data in total.unweighted.msa) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}

sex.unweighted.msa = lapply(data.list.brfss.msa.sex.n_unweighted, `[[`, 2)  

for (data in sex.unweighted.msa) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}

age.unweighted.msa = lapply(data.list.brfss.msa.age.n_unweighted, `[[`, 2)  

for (data in age.unweighted.msa) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}

race.unweighted.msa = lapply(data.list.brfss.msa.race.n_unweighted, `[[`, 2)  

for (data in race.unweighted.msa) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}

