###This is meant for recent population data to estimate the adult.population
#Census data for 2021-2023 is not stratified by single year age groups (or anything)
#So we are taking the proportion of adults from 2020 and using it to calculate
#adults for the next years

#Pull adult.population from surveillance manager
adult.population.pull = as.data.frame.table(surveillance.manager$data$adult.population$estimate$cdc_wonder$census.cdc.wonder.population$year__location)
adult.population.df <- adult.population.pull%>%
  filter(year == "2020")%>%
  rename("adult.population.2020" = Freq)

#Pull total population from census manager
#Note that this is a different size than the df above bc we had only put 
#adult.population into the data manager by certain loations
stratified.population = census.manager$data$population$estimate$cdc_wonder$census.cdc.wonder.population$year__location__age__race__ethnicity__sex
summed.population = as.data.frame.table(apply(stratified.population, c('year', 'location'), function(x){sum(x, na.rm=T)}))
total.population.df<- summed.population %>%
  filter(year == "2020")%>%
  rename("total.population.2020" = Freq)

#Join 
all.populations.2020 = inner_join(adult.population.df, total.population.df, by = "location")
all.populations.2020 <- all.populations.2020%>%
  rename(year = 'year.x')%>%
  select(-year.y)%>%
  mutate(proportion.adult.population.2020 = (adult.population.2020/total.population.2020))%>%
  select(location, proportion.adult.population.2020)

#Pull recent population years to multiply (2021, 2022, 2023)
years.of.interest <- c("2021", "2022", "2023")
recent.populations = as.data.frame.table(census.manager$data$population$estimate$census.population$census$year__location[years.of.interest,])
recent.populations<-recent.populations %>%
  rename(total.population.value = Freq)


#Join- need to join so all.populations.2020 matches to multiple lines of recent populations
recent.years.combined =left_join(recent.populations, all.populations.2020, by=join_by("location"), relationship = "many-to-many")

#This (above) gives a lot of NAs.  It looks like the issue is that there are counties 
#that are not included in the cdc_wonder data (that are in the census).  
#That pull from the surveillance manager has to be from wonder bc it was the 
#most recent data.  So if there isn't a population value in cdc wonder then 
#there won't be an adult.population value.

estimated.population.data <- recent.years.combined%>%
  filter(!is.na(proportion.adult.population.2020))%>%
  mutate(value = round(total.population.value*proportion.adult.population.2020))%>%
  mutate(outcome = 'adult.population')%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  select(year, location, outcome, value)

##Put into surveillance manager:
data.manager = surveillance.manager

estimated.population.data <- list(estimated.population.data)

for (data in estimated.population.data) {
  
  data.manager$put.long.form(
    data = data,
    ontology.name = 'census', 
    source = 'adult.population.estimate.census', #added- the other puts for adult.population happen in Andrew's script
    dimension.values = list(),
    url = 'https://www.census.gov/programs-surveys/popest/data/data-sets.html',
    details = 'Census Population Estimates multiplied by proportion of adults from 2020 from CDC Wonder')
}

surveillance.manager = data.manager
