#Moved the main fertility processing file to the census manager
#So now pulling the fertility data from the census manager for the syphilis manager

#Fertility Rates
fertility.rate.data = as.data.frame.table(census.manager$data$fertility.rate$estimate$cdc.wonder.natality$cdc.fertility$year__location__age__race__ethnicity)%>%
  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(race = as.character(race))%>%
  mutate(ethnicity = as.character(ethnicity))%>%
  mutate(age = as.character(age))%>%
  mutate(value = as.numeric(value))%>%
  mutate(outcome = "fertility.rate")

#Female.Population
female.population.data <- as.data.frame.table(census.manager$data$female.population$estimate$cdc.wonder.natality$cdc.fertility$year__location__age__race__ethnicity)%>%  rename(value = Freq)%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(race = as.character(race))%>%
  mutate(ethnicity = as.character(ethnicity))%>%
  mutate(age = as.character(age))%>%
  mutate(value = as.numeric(value))%>%
  mutate(outcome = "female.population")


# Put into Syphilis Manager -----------------------------------------------

#Combine
fertility.data.list = list(
  fertility.rate.data,
  female.population.data)

for (data in fertility.data.list) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'cdc.fertility',
    source = 'cdc.wonder.natality',
    dimension.values = list(),
    url = 'https://wonder.cdc.gov/natality.html',
    details = 'CDC Wonder Natality Data')
}