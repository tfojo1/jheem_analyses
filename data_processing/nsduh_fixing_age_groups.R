#census.manager = load.data.manager(name="census.manager", file="../../cached/census.manager.rdata")

ages.of.interest.13.17 = c("13 years", "14 years", "15 years", "16 years", "17 years")
ages.of.interest.18.25 = c("18 years", "19 years", "20 years", "21 years", "22 years", "23 years", "24 years", "25 years")

#There is not single year age groups for 2013
#population.2003.12.17 = as.data.frame(census.manager$data$population$estimate$census.population$census$year__location__age__race__ethnicity__sex["2003", ,ages.of.interest.13.17,,,])
population.2005.12.17 = as.data.frame(rowSums(census.manager$data$population$estimate$census.population$census$year__location__age__race__ethnicity__sex["2005", ,ages.of.interest.13.17,,,]))
population.2007.12.17 = as.data.frame(rowSums(census.manager$data$population$estimate$census.population$census$year__location__age__race__ethnicity__sex["2007", ,ages.of.interest.13.17,,,]))
population.2009.12.17 = as.data.frame(rowSums(census.manager$data$population$estimate$census.population$census$year__location__age__race__ethnicity__sex["2009", ,ages.of.interest.13.17,,,]))
population.2011.12.17 = as.data.frame(rowSums(census.manager$data$population$estimate$census.population$census$year__location__age__race__ethnicity__sex["2011", ,ages.of.interest.13.17,,,]))
population.2013.12.17 = as.data.frame(rowSums(census.manager$data$population$estimate$census.population$census$year__location__age__race__ethnicity__sex["2013", ,ages.of.interest.13.17,,,]))
population.2015.12.17 = as.data.frame(rowSums(census.manager$data$population$estimate$census.population$census$year__location__age__race__ethnicity__sex["2015", ,ages.of.interest.13.17,,,]))
population.2017.12.17 = as.data.frame(rowSums(census.manager$data$population$estimate$census.population$census$year__location__age__race__ethnicity__sex["2017", ,ages.of.interest.13.17,,,]))

lower.age.list <- list(population.2005.12.17, population.2007.12.17, population.2009.12.17, population.2011.12.17, population.2013.12.17, population.2015.12.17, population.2017.12.17)
            
aids.data.clean = lapply(lower.age.list, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]    
  
  #data$population.age.12.17 = rowSums(data)

  data = rownames_to_column(data, var = 'location')
  
  data <- data %>%
  select(location, population.age.12.17)

list(filename, data) 
})


#NSDUH has national, states, and substate regions.  Census has county

counties.in.states = lapply(states, function(state){
  counties.in.this.state = locations::get.contained.locations(state, "county")
})

#Could you make a variable for eah county that corresponds to each 'state' or 'substate region' and then group_by + sum to get values for locations
#Then join to NSDUH data