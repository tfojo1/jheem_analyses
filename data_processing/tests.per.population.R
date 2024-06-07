
# Establish boundary values -----------------------------------------------

hiv.test.locations.of.interest <- c("NJ", "NY", "PA" ,"FL", "CA" ,"GA" ,"TX" ,"IL" ,"IN", "WI" ,"MD", "VA", "WV" ,"DC", "DE", "AZ", "MI", "NV" ,"MA",
                                    "NH", "NC" ,"SC", "LA", "AR" ,"MS", "TN", "WA", "KY", "OH" ,"MO", "C.12060", "C.12580", "C.16980" 
                                    ,"C.26420", "C.31080", "C.35620", "C.37980" ,"C.41860") #states for which we have adult pop + msas for which we have hiv.tests

hiv.tests.years.of.interest <- c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021") #all the years for which we have hiv.test data


# Get hiv.tests -----------------------------------------------------------

hiv.tests.raw = as.data.frame.table(surveillance.manager$data$hiv.tests$estimate$cdc.testing$cdc$year__location)%>% rename(hiv.test.value = Freq)

hiv.tests.clean = subset(hiv.tests.raw, location %in% hiv.test.locations.of.interest) %>% filter(hiv.test.value != "NaN") #now we only have years/locations of interest.  There's a new NAs-what do we do with that?


# get adult.population ----------------------------------------------------
#Pulling adult.population from 3 sources: 20117-2017 single year age  census data, 2018-2019 single year age cdc wonder data; 2020-2021 by estimated adult.population data

#census (2020-2022 here are estimated)
adult.pop.source.one = (as.data.frame.table(surveillance.manager$data$adult.population$estimate$census.aggregated.adult.population$census$year__location)) %>% rename(adult.pop.value = Freq) %>% mutate(year = as.character(year))
adult.pop.source.one.clean = subset(adult.pop.source.one,  location %in% hiv.test.locations.of.interest)
adult.pop.source.one.final = subset(adult.pop.source.one.clean,  year %in% hiv.tests.years.of.interest)

#CDC Wonder
adult.pop.source.two = (as.data.frame.table(surveillance.manager$data$adult.population$estimate$cdc_wonder$census.cdc.wonder.population$year__location)) %>% rename(adult.pop.value = Freq) %>% filter(year == 2018 | year == 2019) %>% mutate(year = as.character(year))
adult.pop.source.two.final  = subset(adult.pop.source.two,  location %in% hiv.test.locations.of.interest)


adult.pop.all = rbind(adult.pop.source.one.final , adult.pop.source.two.final )

# Combined hiv.test df with adult.pop df ----------------------------------

all.combined = left_join(hiv.tests.clean, adult.pop.all,join_by(location, year))

final.tests.per.pop <- all.combined%>%
  mutate(year = as.character(year))%>%
  mutate(location = as.character(location))%>%
  mutate(value = hiv.test.value/adult.pop.value)%>%
  mutate(outcome = "hiv.tests.per.population")%>%
  select(year, outcome, location, value)

# Put into SURVEILLANCE MANAGER (bc at this point in the code it's no longer the data manager)-------------------------------------------

surveillance.manager$put.long.form(
  data = final.tests.per.pop,
  ontology.name = 'cdc',
  source = 'cdc.testing',
  dimension.values = list(),
  url = 'https://www.cdc.gov/hiv/library/reports/testing/2021/index.html#:~:text=Among%20the%201%2C736%2C850%20CDC%2Dfunded,demographic%20characteristics%20and%20population%20groups.',
  details = 'CDC Annual HIV Testing Report')  



