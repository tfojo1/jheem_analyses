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
################################################################################
