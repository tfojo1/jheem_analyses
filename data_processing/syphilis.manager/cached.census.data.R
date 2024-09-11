
#Load the census manager to get population data that is clean and processed
census.manager = load.data.manager(name="census.manager", file="../../cached/census.manager.rdata")

#Population total 1970-2023
population.total = as.data.frame.table(census.manager$data$population$estimate$census.population$census$year__location)%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(outcome = "population")

#Age 2020-2023 (SINGLE YEAR)
population.age.20.23 = as.data.frame.table(census.manager$data$population$estimate$census.population$census$year__location__age)%>%
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

#Sex 2020-23
population.sex.20.23 = as.data.frame.table(census.manager$data$population$estimate$census.population$census$year__location__sex)%>%
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
                mutate(race = ifelse(race == "asian", "asian or pacific islander", race))%>%
                mutate(race = ifelse(race == "native hawaiian and other pacific islander", "asian or pacific islander", race))%>%
                 mutate(race = ifelse(race == "american indian and alaska native", "american indian or alaska native", race))%>%
                group_by(year, location, race)%>%
                mutate(new.value = sum(value))%>%
                select(-value)%>%
                rename(value = new.value)%>%
                mutate(year = as.character(year))%>%
                mutate(location = as.character(location))%>%
                mutate(value = as.numeric(value))%>%
                mutate(outcome = "population")
  

population.race<- as.data.frame(population.race[!duplicated(population.race), ])

#Combine
population.data.list = list(
  population.total,
  population.age.20.23,
  population.sex.20.23,
  population.sex.10.19,
  population.race
  
)

problem.locations = c("12025", "29193")

population.data.list = lapply(population.data.list, function(x) filter(x, !location %in% problem.locations))


# Put from census into syphilis.manager -----------------------------------

for (data in population.data.list) {

  data.manager$put.long.form(
    data = data,
    ontology.name = 'census.data', #need to change the racial groups
    source = 'census.population', 
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}

# Putting this separately with a different ontology -----------------------

#Age 2010-2019 (AGE GROUP)
population.age.10.19 = as.data.frame.table(census.manager$data$population$estimate$census.population$stratified.census$year__location__age)%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(age = as.character(age))%>%
  mutate(outcome = "population")

data.manager$put.long.form(
  data = population.age.10.19,
  ontology.name = 'stratified.census', 
  source = 'census.population', 
  dimension.values = list(),
  url = 'www.census.gov',
  details = 'Census Reporting')


