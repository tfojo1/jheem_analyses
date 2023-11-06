#library(haven)

################################################################################
###Read in BRFSS .xpt files
################################################################################

DATA.DIR.BRFSS.MSA="../../data_raw/brfss/brfss_msa"

brfss_file_msa<- list.files(DATA.DIR.BRFSS.MSA, pattern = ".xpt", full.names = "TRUE")

#\\\\\\To show only individuals at risk of HIV in the denominator///////#
        #Un-comment line 14, comment out line 9
        #Un-comment line 161-164
#brfss_file_msa <- list.files(DATA.DIR.BRFSS.MSA, pattern = "risk", full.names = "TRUE")

brfss_file_msa_list <- lapply(brfss_file_msa, function(x) {
  list(filename=x, data=read_xpt(x))
})

################################################################################
###Create state mapping bc the function isnt working but maybe there's 
#another way in the locations package that I don't know
################################################################################
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
##Creating clean template of BRFSS msa data##
##Outcome = proportion.tested
################################################################################
data.list.brfss.msa.clean = lapply(brfss_file_msa_list, function(file){
  
  data=file[["data"]] 
  filename = file[["filename"]] 
  
  #Format MSA for data manager#
  data$location = paste("C", data$`_MMSA`, sep=".")
  #locations::is.location.valid(check$location) #there's a couple of these that are not valid#
  
  #Create year variable#
  if(grepl("2013", filename)) {
    data$year = as.numeric("2013")
    data$sex = as.character(data$SEX)
    data$race = data$`_RACE`
    data$age = data$`_AGEG5YR`  
  }
  if(grepl("2014", filename)) {
    data$year = as.numeric("2014")
    data$sex = as.character(data$SEX)
    data$race = data$`_RACE`
    data$age = data$`_AGEG5YR`
  }
  if(grepl("2015", filename)) {
    data$year = as.numeric("2015")
    data$sex = as.character(data$SEX)
    data$race = data$`_RACE`
    data$age = data$`_AGEG5YR`
  }
  if(grepl("2016", filename)) {
    data$year = as.numeric("2016")
    data$sex = as.character(data$SEX)
    data$race = data$`_RACE`
    data$age = data$`_AGEG5YR`
  }
  if(grepl("2017", filename)) {
    data$year = as.numeric("2017")
    data$sex = as.character(data$SEX)
    data$race = data$`_RACE`
    data$age = data$`_AGEG5YR`
  }
  if(grepl("2018", filename)) {
    data$year = as.numeric("2018")
    data$sex = as.character(data$SEX1)
    data$race = data$`_RACE`
    data$age = data$`_AGEG5YR`
  }
  if(grepl("2019", filename)) {
    data$year = as.numeric("2019")
    data$sex = as.character(data$`_SEX`)
    data$race = data$`_RACE`
    data$age = data$`_AGEG5YR`
  }
  if(grepl("2020", filename)) {
    data$year = as.numeric("2020")
    data$sex = as.character(data$`_SEX`)
    data$race = data$`_RACE`
    data$age = data$`_AGEG5YR`
  }
  if(grepl("2021", filename)) {
    data$year = as.numeric("2021")
    data$sex = as.character(data$`_SEX`)
    data$race = data$`_RACE`
    data$age = data$`_AGEG5YR`
  }
  if(grepl("2022", filename)) {
    data$year = as.numeric("2022")
    data$sex = as.character(data$`_SEX`)
    data$race = data$`_RACE1`
    data$age = data$`_AGEG5YR`
  }
  
  
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
  
  #\\\\\\To show only individuals at risk of HIV in the denominator///////#
  #Un-comment line 161-164
  # brfss_risk_var = c(HIVRISK5= "HIVRISK4")
  #  data <- data %>%
  #     rename(any_of(brfss_risk_var))
  #  data = subset(data, HIVRISK5 == "1" ) #select only those at risk#
  
  list(filename, data) 
})
################################################################################
##Total-by msa##
################################################################################
data.list.brfss.msa.totals = lapply(data.list.brfss.msa.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  #Create population variable-count of BRFSS responses by msa#
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
##Sex-by msa##
################################################################################
data.list.brfss.msa.sex = lapply(data.list.brfss.msa.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data<- data %>%
    add_count(location, sex) %>%     #Create population variable-count of BRFSS responses by msa#
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
##Age-by msa##
################################################################################
data.list.brfss.msa.age = lapply(data.list.brfss.msa.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data<- data %>%
    add_count(location, age) %>%     #Create population variable-count of BRFSS responses by msa#
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
##Race-by msa##
################################################################################
data.list.brfss.msa.race = lapply(data.list.brfss.msa.clean, function(file){
  
  data=file[[2]] 
  filename = file[[1]] 
  
  data<- data %>%
    add_count(location, race) %>%     #Create population variable-count of BRFSS responses by msa#
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
################################################################################
################################################################################


####2nd outcome###


################################################################################
##Outcome = proportion.tested.n
################################################################################
data.list.brfss.msa.n = lapply(data.list.brfss.msa.totals, function(file){
  
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
data.list.brfss.msa.sex.n = lapply(data.list.brfss.msa.sex, function(file){
  
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
data.list.brfss.msa.age.n = lapply(data.list.brfss.msa.age, function(file){
  
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
data.list.brfss.msa.race.n = lapply(data.list.brfss.msa.race, function(file){
  
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
#10 msaments#
################################################################################
##msa-TOTAL-proportion.tested
msa.total.num = lapply(data.list.brfss.msa.totals, `[[`, 2)  

for (data in msa.total.num) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}

##msa-SEX-proportion.tested
msa.sex.num = lapply(data.list.brfss.msa.sex, `[[`, 2)  

for (data in msa.sex.num) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}
##msa-AGE-proportion.tested
msa.age.num = lapply(data.list.brfss.msa.age, `[[`, 2)  

for (data in msa.age.num) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}
##msa-RACE-proportion.tested
msa.race.num = lapply(data.list.brfss.msa.race, `[[`, 2)  

for (data in msa.race.num) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}

#####msa-TOTAL-proportion.tested.N
msa.total.denom = lapply(data.list.brfss.msa.n, `[[`, 2)  

for (data in msa.total.denom) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}
######msa-SEX-proportion.tested.N
msa.sex.denom = lapply(data.list.brfss.msa.sex.n, `[[`, 2)  

for (data in msa.sex.denom) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}
######msa-AGE-proportion.tested.n
msa.age.denom = lapply(data.list.brfss.msa.age.n, `[[`, 2)  

for (data in msa.age.denom) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}
######msa-RACE-proportion.tested.n
msa.race.denom = lapply(data.list.brfss.msa.race.n, `[[`, 2)  

for (data in msa.race.denom) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'brfss',
    source = 'brfss',
    dimension.values = list(),
    url = 'https://www.cdc.gov/brfss/index.html',
    details = 'Behavioral Risk Factor Surveillance System')
}

