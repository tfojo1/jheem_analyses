#Create new outcome = hiv.test.per.population
  # (hiv.tests/adult.population)

states = locations::get.all.for.type("state")

#Get hiv.test data from surveillance manager
#Select only years we have adult.population for
#Remove NA values
numerator = as.data.frame.table(surveillance.manager$data$hiv.tests$estimate$cdc.testing$cdc$year__location) %>% rename(hiv.tests.value = Freq)
  numerator.subset = subset(numerator, location %in% states)
  final.numerator <- numerator.subset%>%
    filter(year != "2020")%>%
    filter(year != "2021")%>%
    filter(hiv.tests.value != "NaN")

#Get adult.population data
#Subset so location only equals states
#Remove years we don't have hiv.test data for 
denominator.early.years = as.data.frame.table(surveillance.manager$data$adult.population$estimate$census.population$census$year__location) %>% rename(adult.population.value = Freq)
denominator.later.years = as.data.frame.table(surveillance.manager$data$adult.population$estimate$cdc_wonder$census.cdc.wonder.population$year__location) %>% rename(adult.population.value = Freq)
                                
  denominator.early.years = subset(denominator.early.years, location %in% states)
  denominator.later.years = subset(denominator.later.years, location %in% states)

denominator = rbind(denominator.early.years, denominator.later.years)

final.denominator <- denominator %>%
  filter(year != "2005")%>%
  filter(year != "2006")%>%
  filter(year != "2007")%>%
  filter(year != "2008")%>%
  filter(year != "2009")%>%
  filter(year != "2010")%>%
  filter(adult.population.value != "NaN")

#Figure out what states we have adult.pop for (bc it's only 30)
#Subset hiv.test data to match
states.we.have.pop.for = final.denominator$location
final.numerator = subset(final.numerator, location %in% states.we.have.pop.for)

#Combine adult pop and hiv tests
combined = left_join(final.numerator, final.denominator, join_by(location, year))

#Calculate the new outcome
tests.per.pop <- combined %>%
  mutate(value = hiv.tests.value/adult.population.value)%>%
  mutate(outcome = "hiv.tests.per.population")%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))

#Put into data manager (but at this point surveillance manager)
  surveillance.manager$put.long.form(
    data = tests.per.pop,
    ontology.name = 'cdc',
    source = 'cdc.testing',
    dimension.values = list(),
    url = 'https://www.cdc.gov/hiv/library/reports/testing/2021/index.html#:~:text=Among%20the%201%2C736%2C850%20CDC%2Dfunded,demographic%20characteristics%20and%20population%20groups.',
    details = 'CDC Annual HIV Testing Report')

  

# Adding in Cities --------------------------------------------------------

  #Add cities to hiv.tests.per.population
  
  cities <- c("C.12060", "C.12580", "C.16980" ,"C.26420", "C.31080", "C.35620", "C.37980", "C.41860")
  
  #hiv.tests
  cities.numerator = as.data.frame.table(surveillance.manager$data$hiv.tests$estimate$cdc.testing$cdc$year__location[, cities]) %>% rename(hiv.test.value = Freq)%>% filter(hiv.test.value != "NaN") #2011-2021
  
  
  #Adult.pop
  denominator.cities.1 = as.data.frame.table(surveillance.manager$data$adult.population$estimate$census.population$census$year__location[, cities]) %>% rename(adult.population.value = Freq)
  denominator.cities.2 = as.data.frame.table(surveillance.manager$data$adult.population$estimate$cdc_wonder$census.cdc.wonder.population$year__location[, cities]) %>% rename(adult.population.value = Freq)
  
  denominator = rbind(denominator.cities.1, denominator.cities.2)
  
  final.denominator <- denominator %>%
    filter(year != "2005")%>%
    filter(year != "2006")%>%
    filter(year != "2007")%>%
    filter(year != "2008")%>%
    filter(year != "2009")%>%
    filter(year != "2010")%>%
    filter(adult.population.value != "NaN")
  
  #combine
  combined.cities = left_join(cities.numerator, final.denominator, join_by(location, year))%>% filter(!is.na(adult.population.value))
  
  combined.cities <- combined.cities %>%
    mutate(value = (hiv.test.value/adult.population.value))%>%
    select(year, location, value)%>%
    mutate(outcome = 'hiv.tests.per.population')
  
  #Put into data manager (but at this point surveillance manager)
  surveillance.manager$put.long.form(
    data = combined.cities,
    ontology.name = 'cdc',
    source = 'cdc.testing',
    dimension.values = list(),
    url = 'https://www.cdc.gov/hiv/library/reports/testing/2021/index.html#:~:text=Among%20the%201%2C736%2C850%20CDC%2Dfunded,demographic%20characteristics%20and%20population%20groups.',
    details = 'CDC Annual HIV Testing Report')  
