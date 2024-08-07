
#Load the census manager to get population data that is clean and processed
census.manager = load.data.manager(name="census.manager", file="../../cached/census.manager.rdata")

#original source=census.population; original ontology=census
population.total = as.data.frame.table(census.manager$data$population$estimate$census.population$census$year__location)%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(outcome = "population")

population.age = as.data.frame.table(census.manager$data$population$estimate$census.population$census$year__location__age)%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(age = as.character(age))%>%
  mutate(outcome = "population")

population.sex = as.data.frame.table(census.manager$data$population$estimate$census.population$census$year__location__sex)%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = as.numeric(value))%>%
  mutate(sex = as.character(sex))%>%
  mutate(outcome = "population")
  

#original source=census.population; original ontology= stratified.census
population.race = as.data.frame.table(census.manager$data$population$estimate$census.population$stratified.census$year__location__race)%>%
                rename(value = Freq)%>%
                mutate(race = tolower(race))%>%
                mutate(race = ifelse(race == "asian", "asian or pacific islander", race))%>%
                mutate(race = ifelse(race == "native hawaiian and other pacific islander", "asian or pacific islander", race))%>%
                group_by(year, location, race)%>%
                mutate(new.value = sum(value))%>%
                select(-value)%>%
                rename(value = new.value)%>%
                mutate(year = as.character(year))%>%
                mutate(location = as.character(location))%>%
                mutate(value = as.numeric(value))%>%
                mutate(outcome = "population")
  

population.race<- as.data.frame(population.race[!duplicated(population.race), ])
    

population.data.list = list(
  population.total,
  population.age,
  population.sex,
  population.race)

# Put from census into syphilis.manager -----------------------------------

population.put = lapply(population.data.list, `[[`, 2)

for (data in population.put) {

  data.manager$put.long.form(
    data = data,
    ontology.name = 'census.data', #need to change the racial groups
    source = 'census.population', 
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}