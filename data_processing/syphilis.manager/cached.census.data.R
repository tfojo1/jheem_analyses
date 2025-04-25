#Note: This is data being pulled from the Census Manager (not exclusively data from the Census)

#Load the census manager to get population data that is clean and processed
census.manager = load.data.manager(name="census.manager", file="Q:/data_managers/census.manager.rdata")

# THESE ARE ONTOLOGY = CENSUS.DATA ----------------------------------------

#Population total 1970-2023
population.total = as.data.frame.table(census.manager$data$population$estimate$census.population$census$year__location)%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(outcome = "population")%>%
  filter(location != "51123" & location != "51515" & location != "51560" & location != "51780" & location != "46131")

#Age 2020-2023 (SINGLE YEAR)
population.age.20.23 = as.data.frame.table(census.manager$data$population$estimate$census.population$census$year__location__age)%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(age = as.character(age))%>%
  mutate(outcome = "population")

#Sex 2020-23
population.sex.20.23 = as.data.frame.table(census.manager$data$population$estimate$census.population$census$year__location__sex)%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(sex = as.character(sex))%>%
  mutate(outcome = "population")

#Age+Sex 20-23-ontology census data bc its single age 
age.sex.20.23 = as.data.frame.table(census.manager$data$population$estimate$census.population$census$year__location__age__sex)%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(age = as.character(age))%>%
  mutate(sex = as.character(sex))%>%
  mutate(outcome = "population")

#Combine
population.data.list = list(
  population.total,
  population.age.20.23,
  population.sex.20.23,
  age.sex.20.23
  
)

# Put from census into syphilis.manager -----------------------------------

for (data in population.data.list) {

  data.manager$put.long.form(
    data = data,
    ontology.name = 'census', #need to change the racial groups
    source = 'census.population', 
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}

# THESE ARE ONTOLOGY = STRATIFIED.CENSUS -----------------------

#Age 2010-2019 (AGE GROUP)
population.age.10.19 = as.data.frame.table(census.manager$data$population$estimate$census.population$stratified.census$year__location__age)%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(age = as.character(age))%>%
  mutate(outcome = "population")

#Sex 2010-19
population.sex.10.19 = as.data.frame.table(census.manager$data$population$estimate$census.population$stratified.census$year__location__sex)%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(sex = as.character(sex))%>%
  mutate(outcome = "population")

#Race 2010-2023
population.race = as.data.frame.table(census.manager$data$population$estimate$census.population$stratified.census$year__location__race)%>%
  rename(value = Freq)%>%
  mutate(race = tolower(race))%>%
  # mutate(race = ifelse(race == "asian", "asian or pacific islander", race))%>%
  # mutate(race = ifelse(race == "native hawaiian and other pacific islander", "asian or pacific islander", race))%>%
  #mutate(race = ifelse(race == "american indian and alaska native", "american indian or alaska native", race))%>%
  # group_by(year, location, race)%>%
  # mutate(new.value = sum(value))%>%
  # select(-value)%>%
  # rename(value = new.value)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(outcome = "population")%>%
  mutate(race = tolower(race))

population.race<- as.data.frame(population.race[!duplicated(population.race), ])

#Ethnicity 2010-2023
population.ethnicity = as.data.frame.table(census.manager$data$population$estimate$census.population$stratified.census$year__location__ethnicity)%>%
  rename(value = Freq)%>%
  mutate(ethnicity = as.character(ethnicity))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(outcome = "population")%>%
  mutate(ethnicity = tolower(ethnicity))

# Race+Ethnicity+Sex 2010-2023
population.race.eth.sex = as.data.frame.table(census.manager$data$population$estimate$census.population$stratified.census$year__location__race__ethnicity__sex)%>%
  rename(value = Freq)%>%
  mutate(ethnicity = as.character(ethnicity))%>%
  mutate(race = as.character(race))%>%
  mutate(sex = as.character(sex))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(outcome = "population")%>%
  mutate(race = tolower(race))%>%
  mutate(ethnicity = tolower(ethnicity))
  

#Race+Ethnicity 2010-2023
population.race.eth= as.data.frame.table(census.manager$data$population$estimate$census.population$stratified.census$year__location__race__ethnicity)%>%
  rename(value = Freq)%>%
  mutate(ethnicity = as.character(ethnicity))%>%
  mutate(race = as.character(race))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(outcome = "population")%>%
  mutate(race = tolower(race))%>%
  mutate(ethnicity = tolower(ethnicity))


#Age+Sex 2010-2019 (ontology= stratifed.census because this is age group data)
age.sex.10.19 = as.data.frame.table(census.manager$data$population$estimate$census.population$stratified.census$year__location__age__sex)%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(age = as.character(age))%>%
  mutate(sex = as.character(sex))%>%
  mutate(outcome = "population")

#Age+Race+Ethnicity 2010-2023
population.race.eth.age = as.data.frame.table(census.manager$data$population$estimate$census.population$stratified.census$year__location__age__race__ethnicity)%>%
  rename(value = Freq)%>%
  mutate(ethnicity = as.character(ethnicity))%>%
  mutate(race = as.character(race))%>%
  mutate(age = as.character(age))%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(outcome = "population")%>%
  mutate(race = tolower(race))%>%
  mutate(ethnicity = tolower(ethnicity))


#Age+Race 2010-2023
age.race = as.data.frame.table(census.manager$data$population$estimate$census.population$stratified.census$year__location__age__race)%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(age = as.character(age))%>%
  mutate(race = as.character(race))%>%
  mutate(outcome = "population")%>%
  mutate(race = tolower(race))

#Age+Ethnicity 2010-2023
age.eth = as.data.frame.table(census.manager$data$population$estimate$census.population$stratified.census$year__location__age__ethnicity)%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(age = as.character(age))%>%
  mutate(ethnicity = as.character(ethnicity))%>%
  mutate(outcome = "population")%>%
  mutate(ethnicity = tolower(ethnicity))

#Age+Race+Ethnicity+Sex 20102-2023
age.race.eth.sex = as.data.frame.table(census.manager$data$population$estimate$census.population$stratified.census$year__location__age__race__ethnicity__sex)%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(age = as.character(age))%>%
  mutate(sex = as.character(sex))%>%
  mutate(ethnicity = as.character(ethnicity))%>%
  mutate(race = as.character(race))%>%
  mutate(outcome = "population")%>%
  mutate(ethnicity = tolower(ethnicity))%>%
  mutate(race = tolower(race))


# Put for ontology = stratified census ------------------------------------
#Combine
population.stratified.census = list(
  population.age.10.19,
  population.sex.10.19,
  population.race,
  population.ethnicity,
  population.race.eth,
  age.race,
  age.eth,
  age.sex.10.19,
  population.race.eth.age,
  population.race.eth.sex,
  age.race.eth.sex
)

for (data in population.stratified.census) {
  
data.manager$put.long.form(
  data = data,
  ontology.name = 'stratified.census', 
  source = 'census.population', 
  dimension.values = list(),
  url = 'www.census.gov',
  details = 'Census Reporting')
}


# Census Deaths -----------------------------------------------------------
deaths.total = as.data.frame.table(census.manager$data$deaths$estimate$census.deaths$census$year__location)%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(outcome = "deaths")

# Put for Census Deaths ---------------------------------------------------
  data.manager$put.long.form(
    data = deaths.total,
    ontology.name = 'census',
    source = 'census.deaths',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')

# National Level Births and the Denominator -------------------------------
# 
# national.births = as.data.frame.table(census.manager$data$births$estimate$cdc_wonder$census.cdc.wonder.births.deaths$year__location__race__ethnicity)%>%
#   filter(location == "US")%>%
#   rename(value = Freq)%>%
#   mutate(year = as.character(year))%>%
#   mutate(location = as.character(location))%>%
#   mutate(race = as.character(race))%>%
#   mutate(ethnicity = as.character(ethnicity))%>%
#   mutate(value = as.numeric(value))%>%
#   mutate(outcome = "births")%>%
#   mutate(race = tolower(race))%>%
#   mutate(ethnicity = tolower(ethnicity))
# 
# 
# national.births.denominator = as.data.frame.table(census.manager$data$births.denominator$estimate$cdc_wonder$census.cdc.wonder.births.deaths$year__location__race__ethnicity)%>%
#   filter(location == "US")%>%
#   rename(value = Freq)%>%
#   mutate(year = as.character(year))%>%
#   mutate(location = as.character(location))%>%
#   mutate(race = as.character(race))%>%
#   mutate(ethnicity = as.character(ethnicity))%>%
#   mutate(value = as.numeric(value))%>%
#   mutate(outcome = "births.denominator")%>%
#   mutate(race = tolower(race))%>%
#   mutate(ethnicity = tolower(ethnicity))
# 
# 
# national.birth.data.combined = list(
#   national.births,
#   national.births.denominator
# )
# 
# 
# for (data in national.birth.data.combined ) {
#   
#   data.manager$put.long.form(
#     data = data,
#     ontology.name = 'census.cdc.wonder.births.deaths',
#     source = 'cdc.wonder.natality',
#     dimension.values.to.distribute = list(race=c('more than one race', 'not reported', 'unknown or not stated', 'not Available'), ethnicity ='unknown or not stated', 'not stated'),
#     url = 'https://wonder.cdc.gov/',
#     details = 'CDC Wonder')
# }


# National Population Data by Age, Sex, Age+Sex ---------------------------
#These are split up because different years have different ontologies bc of availability of single year age data

nat.sex = as.data.frame.table(census.manager$data$population$estimate$census.population$stratified.census$year__location__sex)%>%
  filter(location == "US")%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(sex = as.character(sex))%>%
  mutate(outcome = "population")

nat.age = as.data.frame.table(census.manager$data$population$estimate$census.population$stratified.census$year__location__age)%>%
  filter(location == "US")%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(age = as.character(age))%>%
  mutate(outcome = "population")

nat.age.sex = as.data.frame.table(census.manager$data$population$estimate$census.population$stratified.census$year__location__age__sex)%>%
  filter(location == "US")%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(age = as.character(age))%>%
  mutate(sex = as.character(sex))%>%
  mutate(outcome = "population")

nat.age.sex.combined = list(
  nat.sex,
  nat.age,
  nat.age.sex)

#national.sex.put = lapply(national.sex, `[[`, 2)

for (data in nat.age.sex.combined) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'stratified.census',
    source = 'census.population',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}


# National Level Death Data -----------------------------------------------

#from census
national.deaths.total <- as.data.frame.table(census.manager$data$deaths$estimate$census.population$census$year__location)%>%
  filter(location == "US")%>%
  mutate(outcome = "deaths")%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(Freq))
  
  #From cdc wonder
  national.deaths.stratified <- as.data.frame.table(census.manager$data$deaths$estimate$cdc_wonder$census.cdc.wonder.births.deaths$year__location__age__race__ethnicity__sex)%>%
  filter(location == "US")%>%
  mutate(outcome = "deaths")%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
    mutate(value = as.numeric(Freq))%>%
  mutate(age = as.character(age))%>%
  mutate(sex = as.character(sex))%>%
  mutate(race = as.character(race))%>%
  mutate(ethnicity = as.character(ethnicity))%>%
    mutate(race = tolower(race))%>%
    mutate(ethnicity = tolower(ethnicity))
  

#Need separate puts bc these are separate sources
  
  #census national death total data
  data.manager$put.long.form(
    data = national.deaths.total,
    ontology.name = 'census',
    source = 'census.population',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
  
  
    #cdc wonder national stratified death data
    data.manager$put.long.form(
      data = national.deaths.stratified,
      ontology.name = 'census.cdc.wonder.births.deaths',
      source = 'cdc_wonder',
      dimension.values.to.distribute = list(race=c('more than one race', 'not reported', 'unknown or not stated', 'not available'), ethnicity ='unknown or not stated', 'not stated'),
      url = 'https://wonder.cdc.gov/',
      details = 'CDC Wonder')


  
