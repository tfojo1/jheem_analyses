#This code pulls population data from the census and aggregates the county level data to the national level 
#To be put into the syphilis manager as US population data.

# counties = dimnames(census.manager$data$population$estimate$census.population$census$year__location)$location
# 
# xy = census.manager$pull(
#   outcome = "population",
#   source = 'census.population', 
#   from.ontology.names = "census", 
#   dimension.values = list(location = counties), 
#   keep.dimensions = "year",
#   na.rm = T)  


# Race alone 2010-2023 ----------------------------------------------------

counties = dimnames(census.manager$data$population$estimate$census.population$stratified.census$year__location__race)$location #3133 counties

race.alone.20.23 = census.manager$pull(
  outcome = "population",
  source = 'census.population', 
  from.ontology.names = "stratified.census", 
  dimension.values = list(location = counties), 
  keep.dimensions = c('year', 'race'),
  na.rm = T) 

race.alone.20.23 = as.data.frame.table(race.alone.20.23)%>%
  mutate(year = as.character(year))%>%
  mutate(value = as.numeric(Freq))%>%
  mutate(outcome = "population")%>%
  mutate(location = "US")%>%
  mutate(race = as.character(race))%>%
  select(-Freq, -source)

# Ethnicity Alone 2010-2023 -----------------------------------------------

counties = dimnames(census.manager$data$population$estimate$census.population$stratified.census$year__location__ethnicity)$location #3133 counties

eth.alone.20.23 = census.manager$pull(
  outcome = "population",
  source = 'census.population', 
  from.ontology.names = "stratified.census", 
  dimension.values = list(location = counties), 
  keep.dimensions = c('year', 'ethnicity'),
  na.rm = T) 

eth.alone.20.23 = as.data.frame.table(eth.alone.20.23)%>%
  mutate(year = as.character(year))%>%
  mutate(value = as.numeric(Freq))%>%
  mutate(outcome = "population")%>%
  mutate(location = "US")%>%
  mutate(ethnicity = as.character(ethnicity))%>%
  select(-Freq, -source)

# Age + Race 2010-2023 -----------------------------------------------

counties = dimnames(census.manager$data$population$estimate$census.population$stratified.census$year__location__age__race)$location #3133 counties

age.race.20.23 = census.manager$pull(
  outcome = "population",
  source = 'census.population', 
  from.ontology.names = "stratified.census", 
  dimension.values = list(location = counties), 
  keep.dimensions = c('year', 'age', 'race'),
  na.rm = T) 

age.race.20.23 = as.data.frame.table(age.race.20.23)%>%
  mutate(year = as.character(year))%>%
  mutate(value = as.numeric(Freq))%>%
  mutate(outcome = "population")%>%
  mutate(location = "US")%>%
  mutate(age = as.character(age))%>%
  mutate(race = as.character(race))%>%
  select(-Freq, -source)

# Age + ethnicity 2010-2023 -----------------------------------------------

counties = dimnames(census.manager$data$population$estimate$census.population$stratified.census$year__location__age__ethnicity)$location #3133 counties

age.ethnicity.20.23 = census.manager$pull(
  outcome = "population",
  source = 'census.population', 
  from.ontology.names = "stratified.census", 
  dimension.values = list(location = counties), 
  keep.dimensions = c('year', 'age', 'ethnicity'),
  na.rm = T) 

age.ethnicity.20.23 = as.data.frame.table(age.ethnicity.20.23)%>%
  mutate(year = as.character(year))%>%
  mutate(value = as.numeric(Freq))%>%
  mutate(outcome = "population")%>%
  mutate(location = "US")%>%
  mutate(age = as.character(age))%>%
  mutate(ethnicity = as.character(ethnicity))%>%
  select(-Freq, -source)

# Race + Ethnicity 2010-2023 -----------------------------------------------

counties = dimnames(census.manager$data$population$estimate$census.population$stratified.census$year__location__race__ethnicity)$location #3133 counties

race.ethnicity.20.23 = census.manager$pull(
  outcome = "population",
  source = 'census.population', 
  from.ontology.names = "stratified.census", 
  dimension.values = list(location = counties), 
  keep.dimensions = c('year', 'race', 'ethnicity'),
  na.rm = T) 

race.ethnicity.20.23 = as.data.frame.table(race.ethnicity.20.23)%>%
  mutate(year = as.character(year))%>%
  mutate(value = as.numeric(Freq))%>%
  mutate(outcome = "population")%>%
  mutate(location = "US")%>%
  mutate(race = as.character(race))%>%
  mutate(ethnicity = as.character(ethnicity))%>%
  select(-Freq, -source)


# Race + Ethnicity + Sex 2010-2023 --------------------------------------------
counties = dimnames(census.manager$data$population$estimate$census.population$stratified.census$year__location__race__ethnicity__sex)$location #3133 counties

race.ethnicity.sex.20.23 = census.manager$pull(
  outcome = "population",
  source = 'census.population', 
  from.ontology.names = "stratified.census", 
  dimension.values = list(location = counties), 
  keep.dimensions = c('year', 'race', 'ethnicity', 'sex'),
  na.rm = T) 

race.ethnicity.sex.20.23 = as.data.frame.table(race.ethnicity.sex.20.23)%>%
  mutate(year = as.character(year))%>%
  mutate(value = as.numeric(Freq))%>%
  mutate(outcome = "population")%>%
  mutate(location = "US")%>%
  mutate(race = as.character(race))%>%
  mutate(ethnicity = as.character(ethnicity))%>%
  mutate(sex = as.character(sex))%>%
  select(-Freq, -source)

# Race + Ethnicity + Age 2010-2023 ----------------------------------------
counties = dimnames(census.manager$data$population$estimate$census.population$stratified.census$year__location__age__race__ethnicity)$location #3133 counties

race.ethnicity.age.20.23 = census.manager$pull(
  outcome = "population",
  source = 'census.population', 
  from.ontology.names = "stratified.census", 
  dimension.values = list(location = counties), 
  keep.dimensions = c('year', 'race', 'ethnicity', 'age'),
  na.rm = T) 

race.ethnicity.age.20.23 = as.data.frame.table(race.ethnicity.age.20.23)%>%
  mutate(year = as.character(year))%>%
  mutate(value = as.numeric(Freq))%>%
  mutate(outcome = "population")%>%
  mutate(location = "US")%>%
  mutate(race = as.character(race))%>%
  mutate(ethnicity = as.character(ethnicity))%>%
  mutate(age = as.character(age))%>%
  select(-Freq, -source)

# Age + Race + Ethnicity + Sex --------------------------------------------
counties = dimnames(census.manager$data$population$estimate$census.population$stratified.census$year__location__age__race__ethnicity__sex)$location #3133 counties

race.ethnicity.age.20.23 = census.manager$pull(
  outcome = "population",
  source = 'census.population', 
  from.ontology.names = "stratified.census", 
  dimension.values = list(location = counties), 
  keep.dimensions = c('year', 'race', 'ethnicity', 'age', 'sex'),
  na.rm = T) 

race.ethnicity.age.20.23 = as.data.frame.table(race.ethnicity.age.20.23)%>%
  mutate(year = as.character(year))%>%
  mutate(value = as.numeric(Freq))%>%
  mutate(outcome = "population")%>%
  mutate(location = "US")%>%
  mutate(race = as.character(race))%>%
  mutate(ethnicity = as.character(ethnicity))%>%
  mutate(age = as.character(age))%>%
  mutate(sex = as.character(sex))%>%
  select(-Freq, -source)


# Put ---------------------------------------------------------------------

national.population.stratified = list(
  race.alone.20.23,
  eth.alone.20.23,
  age.race.20.23,
  age.ethnicity.20.23,
  race.ethnicity.20.23,
  race.ethnicity.sex.20.23,
  race.ethnicity.age.20.23,
   race.ethnicity.age.20.23
)

for (data in national.population.stratified) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'stratified.census',
    source = 'census.population',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}

