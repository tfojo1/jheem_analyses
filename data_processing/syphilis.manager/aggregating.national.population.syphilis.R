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
counties = (counties[! counties%in% c("US")])

race.alone.10.23 = census.manager$pull(
  outcome = "population",
  source = 'census.population', 
  from.ontology.names = "stratified.census", 
  dimension.values = list(location = counties), 
  keep.dimensions = c('year', 'race'),
  na.rm = T) 

race.alone.10.23 = as.data.frame.table(race.alone.10.23)%>%
  mutate(year = as.character(year))%>%
  mutate(value = as.numeric(Freq))%>%
  mutate(outcome = "population")%>%
  mutate(location = "US")%>%
  mutate(race = as.character(race))%>%
  select(-Freq, -source)%>%
  mutate(race = tolower(race))

# Ethnicity Alone 2010-2023 -----------------------------------------------

counties = dimnames(census.manager$data$population$estimate$census.population$stratified.census$year__location__ethnicity)$location #3133 counties
counties = (counties[! counties%in% c("US")])

eth.alone.10.23 = census.manager$pull(
  outcome = "population",
  source = 'census.population', 
  from.ontology.names = "stratified.census", 
  dimension.values = list(location = counties), 
  keep.dimensions = c('year', 'ethnicity'),
  na.rm = T) 

eth.alone.10.23 = as.data.frame.table(eth.alone.10.23)%>%
  mutate(year = as.character(year))%>%
  mutate(value = as.numeric(Freq))%>%
  mutate(outcome = "population")%>%
  mutate(location = "US")%>%
  mutate(ethnicity = as.character(ethnicity))%>%
  select(-Freq, -source)%>%
  mutate(ethnicity = tolower(ethnicity))

# Age + Race 2010-2023 -----------------------------------------------

counties = dimnames(census.manager$data$population$estimate$census.population$stratified.census$year__location__age__race)$location #3133 counties
counties = (counties[! counties%in% c("US")])

age.race.10.23 = census.manager$pull(
  outcome = "population",
  source = 'census.population', 
  from.ontology.names = "stratified.census", 
  dimension.values = list(location = counties), 
  keep.dimensions = c('year', 'age', 'race'),
  na.rm = T) 

age.race.10.23 = as.data.frame.table(age.race.10.23)%>%
  mutate(year = as.character(year))%>%
  mutate(value = as.numeric(Freq))%>%
  mutate(outcome = "population")%>%
  mutate(location = "US")%>%
  mutate(age = as.character(age))%>%
  mutate(race = as.character(race))%>%
  select(-Freq, -source)%>%
  mutate(race = tolower(race))

# Age + ethnicity 2010-2023 -----------------------------------------------

counties = dimnames(census.manager$data$population$estimate$census.population$stratified.census$year__location__age__ethnicity)$location #3133 counties
counties = (counties[! counties%in% c("US")])


age.ethnicity.10.23 = census.manager$pull(
  outcome = "population",
  source = 'census.population', 
  from.ontology.names = "stratified.census", 
  dimension.values = list(location = counties), 
  keep.dimensions = c('year', 'age', 'ethnicity'),
  na.rm = T) 

age.ethnicity.10.23 = as.data.frame.table(age.ethnicity.10.23)%>%
  mutate(year = as.character(year))%>%
  mutate(value = as.numeric(Freq))%>%
  mutate(outcome = "population")%>%
  mutate(location = "US")%>%
  mutate(age = as.character(age))%>%
  mutate(ethnicity = as.character(ethnicity))%>%
  select(-Freq, -source)%>%
  mutate(ethnicity = tolower(ethnicity))

# Race + Ethnicity 2010-2023 -----------------------------------------------

counties = dimnames(census.manager$data$population$estimate$census.population$stratified.census$year__location__race__ethnicity)$location #3133 counties
counties = (counties[! counties%in% c("US")])

race.ethnicity.10.23 = census.manager$pull(
  outcome = "population",
  source = 'census.population', 
  from.ontology.names = "stratified.census", 
  dimension.values = list(location = counties), 
  keep.dimensions = c('year', 'race', 'ethnicity'),
  na.rm = T) 

race.ethnicity.10.23 = as.data.frame.table(race.ethnicity.10.23)%>%
  mutate(year = as.character(year))%>%
  mutate(value = as.numeric(Freq))%>%
  mutate(outcome = "population")%>%
  mutate(location = "US")%>%
  mutate(race = as.character(race))%>%
  mutate(ethnicity = as.character(ethnicity))%>%
  select(-Freq, -source)%>%
  mutate(race = tolower(race))%>%
  mutate(ethnicity = tolower(ethnicity))


# Race + Ethnicity + Sex 2010-2023 --------------------------------------------
counties = dimnames(census.manager$data$population$estimate$census.population$stratified.census$year__location__race__ethnicity__sex)$location #3133 counties
counties = (counties[! counties%in% c("US")])

race.ethnicity.sex.10.23 = census.manager$pull(
  outcome = "population",
  source = 'census.population', 
  from.ontology.names = "stratified.census", 
  dimension.values = list(location = counties), 
  keep.dimensions = c('year', 'race', 'ethnicity', 'sex'),
  na.rm = T) 

race.ethnicity.sex.10.23 = as.data.frame.table(race.ethnicity.sex.10.23)%>%
  mutate(year = as.character(year))%>%
  mutate(value = as.numeric(Freq))%>%
  mutate(outcome = "population")%>%
  mutate(location = "US")%>%
  mutate(race = as.character(race))%>%
  mutate(ethnicity = as.character(ethnicity))%>%
  mutate(sex = as.character(sex))%>%
  select(-Freq, -source)%>%
  mutate(race = tolower(race))%>%
  mutate(ethnicity = tolower(ethnicity))

# Race + Ethnicity + Age 2010-2023 ----------------------------------------
counties = dimnames(census.manager$data$population$estimate$census.population$stratified.census$year__location__age__race__ethnicity)$location #3133 counties
counties = (counties[! counties%in% c("US")])

race.ethnicity.age.10.23 = census.manager$pull(
  outcome = "population",
  source = 'census.population', 
  from.ontology.names = "stratified.census", 
  dimension.values = list(location = counties), 
  keep.dimensions = c('year', 'race', 'ethnicity', 'age'),
  na.rm = T) 

race.ethnicity.age.10.23 = as.data.frame.table(race.ethnicity.age.10.23)%>%
  mutate(year = as.character(year))%>%
  mutate(value = as.numeric(Freq))%>%
  mutate(outcome = "population")%>%
  mutate(location = "US")%>%
  mutate(race = as.character(race))%>%
  mutate(ethnicity = as.character(ethnicity))%>%
  mutate(age = as.character(age))%>%
  select(-Freq, -source)%>%
  mutate(race = tolower(race))%>%
  mutate(ethnicity = tolower(ethnicity))

# Age + Race + Ethnicity + Sex --------------------------------------------
counties = dimnames(census.manager$data$population$estimate$census.population$stratified.census$year__location__age__race__ethnicity__sex)$location #3133 counties
counties = (counties[! counties%in% c("US")])

race.ethnicity.age.sex.10.23 = census.manager$pull(
  outcome = "population",
  source = 'census.population', 
  from.ontology.names = "stratified.census", 
  dimension.values = list(location = counties), 
  keep.dimensions = c('year', 'race', 'ethnicity', 'age', 'sex'),
  na.rm = T) 

race.ethnicity.age.sex.10.23 = as.data.frame.table(race.ethnicity.age.sex.10.23)%>%
  mutate(year = as.character(year))%>%
  mutate(value = as.numeric(Freq))%>%
  mutate(outcome = "population")%>%
  mutate(location = "US")%>%
  mutate(race = as.character(race))%>%
  mutate(ethnicity = as.character(ethnicity))%>%
  mutate(age = as.character(age))%>%
  mutate(sex = as.character(sex))%>%
  select(-Freq, -source)%>%
  mutate(race = tolower(race))%>%
  mutate(ethnicity = tolower(ethnicity))

# Age 2010-2019 -----------------------------------------------------------

counties = dimnames(census.manager$data$population$estimate$census.population$stratified.census$year__location__age)$location #3134 counties
counties = (counties[! counties%in% c("US")])

age.alone.10.19 = census.manager$pull(
  outcome = "population",
  source = 'census.population', 
  from.ontology.names = "stratified.census", 
  dimension.values = list(location = counties), 
  keep.dimensions = c('year', 'age'),
  na.rm = T) 

age.alone.10.19 = as.data.frame.table(age.alone.10.19)%>% #NOTE: you are currently seeing data by age group for 2020-2023 here bc it has to be removed from the census manager
  mutate(year = as.character(year))%>%
  mutate(value = as.numeric(Freq))%>%
  mutate(outcome = "population")%>%
  mutate(location = "US")%>%
  mutate(age = as.character(age))%>%
  select(-Freq, -source)

# Sex 2010-2019 -----------------------------------------------------------
counties = dimnames(census.manager$data$population$estimate$census.population$stratified.census$year__location__sex)$location #3134 counties
counties = (counties[! counties%in% c("US")])

sex.alone.10.19 = census.manager$pull(
  outcome = "population",
  source = 'census.population', 
  from.ontology.names = "stratified.census", 
  dimension.values = list(location = counties), 
  keep.dimensions = c('year', 'sex'),
  na.rm = T) 

sex.alone.10.19 = as.data.frame.table(sex.alone.10.19)%>% #NOTE: you are currently seeing data by age group for 2020-2023 here bc it has to be removed from the census manager
  mutate(year = as.character(year))%>%
  mutate(value = as.numeric(Freq))%>%
  mutate(outcome = "population")%>%
  mutate(location = "US")%>%
  mutate(sex = as.character(sex))%>%
  select(-Freq, -source)

# Age + Sex  2010-2019 ----------------------------------------------------

counties = dimnames(census.manager$data$population$estimate$census.population$stratified.census$year__location__age__sex)$location #3134 counties
counties = (counties[! counties%in% c("US")])

age.sex.10.19 = census.manager$pull(
  outcome = "population",
  source = 'census.population', 
  from.ontology.names = "stratified.census", 
  dimension.values = list(location = counties), 
  keep.dimensions = c('year', 'age', 'sex'),
  na.rm = T) 

age.sex.10.19 = as.data.frame.table(age.sex.10.19)%>%
  mutate(year = as.character(year))%>%
  mutate(value = as.numeric(Freq))%>%
  mutate(outcome = "population")%>%
  mutate(location = "US")%>%
  mutate(age = as.character(age))%>%
  mutate(sex = as.character(sex))%>%
  select(-Freq, -source)

# Put ---------------------------------------------------------------------

national.population.stratified = list(
  race.alone.10.23,
  eth.alone.10.23,
  age.race.10.23,
  age.ethnicity.10.23,
  race.ethnicity.10.23,
  race.ethnicity.sex.10.23,
  race.ethnicity.age.10.23,
  race.ethnicity.age.sex.10.23,
  age.alone.10.19,
  sex.alone.10.19,
  age.sex.10.19
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



# These have a separate ontology (because of single year ages) ------------


# Age 2020-2023 -----------------------------------------------------------
counties = dimnames(census.manager$data$population$estimate$census.population$census$year__location__age)$location #3144 counties
counties = (counties[! counties%in% c("US")])

age.alone.20.23 = census.manager$pull(
  outcome = "population",
  source = 'census.population', 
  from.ontology.names = "census", 
  dimension.values = list(location = counties), 
  keep.dimensions = c('year', 'age'),
  na.rm = T) 

age.alone.20.23 = as.data.frame.table(age.alone.20.23)%>% 
  mutate(year = as.character(year))%>%
  mutate(value = as.numeric(Freq))%>%
  mutate(outcome = "population")%>%
  mutate(location = "US")%>%
  mutate(age = as.character(age))%>%
  select(-Freq, -source)

# Sex 2020-2023 -----------------------------------------------------------
counties = dimnames(census.manager$data$population$estimate$census.population$census$year__location__sex)$location #3144 counties
counties = (counties[! counties%in% c("US")])

sex.alone.20.23 = census.manager$pull(
  outcome = "population",
  source = 'census.population', 
  from.ontology.names = "census", 
  dimension.values = list(location = counties), 
  keep.dimensions = c('year', 'sex'),
  na.rm = T) 

sex.alone.20.23 = as.data.frame.table(sex.alone.20.23)%>% 
  mutate(year = as.character(year))%>%
  mutate(value = as.numeric(Freq))%>%
  mutate(outcome = "population")%>%
  mutate(location = "US")%>%
  mutate(sex = as.character(sex))%>%
  select(-Freq, -source)

# Age + Sex 2020-2023 -----------------------------------------------------
counties = dimnames(census.manager$data$population$estimate$census.population$census$year__location__age__sex)$location #3134 counties
counties = (counties[! counties%in% c("US")])

age.sex.20.23 = census.manager$pull(
  outcome = "population",
  source = 'census.population', 
  from.ontology.names = "census", 
  dimension.values = list(location = counties), 
  keep.dimensions = c('year', 'age', 'sex'),
  na.rm = T) 

age.sex.20.23 = as.data.frame.table(age.sex.20.23)%>%
  mutate(year = as.character(year))%>%
  mutate(value = as.numeric(Freq))%>%
  mutate(outcome = "population")%>%
  mutate(location = "US")%>%
  mutate(age = as.character(age))%>%
  mutate(sex = as.character(sex))%>%
  select(-Freq, -source)
# Put ---------------------------------------------------------------------



national.population.census = list(
  age.alone.20.23,
  sex.alone.20.23,
  age.sex.20.23
)

for (data in national.population.census) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census.population',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}
