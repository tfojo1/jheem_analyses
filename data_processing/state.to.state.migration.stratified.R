#This code processes the stratified state to state migration data from Census
#The datasets give % values of folks who immigrated from each demographic, we are multiplying that value by the population
#To get a value that immigrated

#This is for immigration (not adult.immigration)

DATA.DIR.IMM.STATE.STRATIFIED="../../data_raw/movement/state.to.state/stratified"

state.stratified.migration <- Sys.glob(paste0(DATA.DIR.IMM.STATE.STRATIFIED, '/*.xlsx'))

#creating a list with sublists of filename, data#
state.stratified.migration.data <- lapply(state.stratified.migration, function(x){
  list(filename=x, data=read_excel(x, sheet='Data'))
})

####

#Immigration
state.stratified.migration.data.clean = lapply(state.stratified.migration.data, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data <- data %>%
    mutate(across(-c(Label), as.numeric))
  
data <- data %>%
  rename(strata = `Label`)%>%
  select(strata, contains("Total Estimate"), contains("Moved; from different  state Estimate"))%>%
  pivot_longer(
    cols = contains("estimate"),
    names_to = "state",
    values_to = "value"
  )%>%
  mutate(text = state)%>%
  mutate(state = gsub( "Moved; from different  state Estimate", "", state))%>%
  mutate(state = gsub( " Total Estimate", "", state))%>%
  mutate(metric = ifelse(grepl(" Total Estimate",text),'population','proportion'))%>%
  select(-text)%>%
  filter(strata == "1 to 4 years" | strata == "5 to 17 years" | strata == "18 to 24 years" | strata == "25 to 34 years" | strata == "35 to 44 years"| strata == "45 to 54 years" | strata == "55 to 64 years"
         | strata == "65 to 74 years" | strata == "75 years and over"
         | strata == 'Male'| strata == 'Female'
         | strata == 'White' | strata == 'Black or African American' | strata == 'American Indian and Alaska Native' | strata == 'Asian' | strata == 'Native Hawaiian and Other Pacific Islander'
         | strata == 'Some other race'| strata == 'Two or more races')%>%
  filter(state != "Total Estimate")%>%
  filter(!is.na(state))%>%
  filter(state != "")

data = as.data.frame(split(data, data$metric))

data <- data %>%
  mutate(population.value = as.numeric(ifelse(population.value == "N", NA, population.value)))%>%
  mutate(proportion.value = as.numeric(ifelse(proportion.value == "N", NA, proportion.value)))%>%
  mutate(immigration.value = round(population.value * proportion.value))%>%
  mutate(location = state.abb[match(population.state, state.name)])%>%
  mutate(location = ifelse(population.state  == "District of Columbia", "DC", location))%>%
  mutate(outcome = "immigration")%>%
  mutate(year= str_sub(filename, -9, -6))%>%
  rename(value = immigration.value)%>%
  select(outcome, year, location, population.strata, value)

  data = as.data.frame(data)
  
  list(filename, data) 
  
})

#Age
state.immigration.age = lapply(state.stratified.migration.data.clean, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  data <- data%>%
    filter(population.strata == "1 to 4 years" | population.strata == "5 to 17 years" | population.strata == "18 to 24 years" | population.strata == "25 to 34 years" | population.strata == "35 to 44 years"| population.strata == "45 to 54 years" | population.strata == "55 to 64 years" 
           | population.strata == "65 to 74 years" | population.strata == "75 years and over")%>%
    rename(age = population.strata)%>%
    mutate(age = ifelse(age == "75 years and over", "75+ years", age))%>%
    mutate(age = gsub(" to ", "-", age))
  
data = as.data.frame(data)
list(filename, data) 
})
#Sex

state.immigration.sex = lapply(state.stratified.migration.data.clean, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  data <- data%>%
    filter(population.strata == "Male" | population.strata == "Female" )%>%
    rename(sex = population.strata)%>%
    mutate(sex = tolower(sex))
  
  data = as.data.frame(data)
  list(filename, data) 
})

#Race
state.immigration.race = lapply(state.stratified.migration.data.clean, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  data <- data%>%
    filter( population.strata == 'White' | population.strata == 'Black or African American' | population.strata == 'American Indian and Alaska Native' | population.strata == 'Asian' | population.strata == 'Native Hawaiian and Other Pacific Islander' 
  | population.strata == 'Some other race'| population.strata == 'Two or more races')%>%
    rename(race = population.strata)%>%
    mutate(race = tolower(race))
  
  data = as.data.frame(data)
  list(filename, data) 
})


# PUT ---------------------------------------------------------------------
# re-distribute these two races -------------------------------------------
# some other race
# two or more races

state.immigration.race.put = lapply(state.immigration.race, `[[`, 2)  

for (data in state.immigration.race.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'census.immigration.state.to.state', 
    source = 'census.population',
    dimension.values.to.distribute = list(race=c('some other race', 'two or more races')),
    url = 'https://data.census.gov/table?q=state+migration&g=040XX00US02',
    details = 'Census State to State Migration Flows')
}

state.immigration.sex.put = lapply(state.immigration.sex, `[[`, 2)  

for (data in state.immigration.sex.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'census.immigration.state.to.state', 
    source = 'census.population',
    dimension.values = list(),
    url = 'https://data.census.gov/table?q=state+migration&g=040XX00US02',
    details = 'Census State to State Migration Flows')
}

state.immigration.age.put = lapply(state.immigration.age, `[[`, 2)  

for (data in state.immigration.age.put) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'census.immigration.state.to.state', 
    source = 'census.population',
    dimension.values = list(),
    url = 'https://data.census.gov/table?q=state+migration&g=040XX00US02',
    details = 'Census State to State Migration Flows')
}
