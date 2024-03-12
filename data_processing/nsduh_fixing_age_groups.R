#census.manager = load.data.manager(name="census.manager", file="../../cached/census.manager.rdata")

ages.of.interest.13.17 = c("13 years", "14 years", "15 years", "16 years", "17 years")
ages.of.interest.18.25 = c("18 years", "19 years", "20 years", "21 years", "22 years", "23 years", "24 years", "25 years")

################################################################################
state.to.fips.mappings = c('01' = 'AL',
                           '02'='AK',
                           '04'='AZ',
                           '05'='AR',
                           '06'='CA',
                           '08'='CO',
                           '09'='CT',
                           '10'='DE',
                           '11'='DC',
                           '12'='FL',
                           '13'='GA',
                           '15'='HI',
                           '16'='ID',
                           '17'='IL',
                           '18'='IN',
                           '19'='IA',
                           '20'='KS',
                           '21'='KY',
                           '22'='LA',
                           '23'='ME',
                           '24'='MD',
                           '25'='MA',
                           '26'='MI',
                           '27'='MN',
                           '28'='MS',
                           '29'='MO',
                           '30'='MT',
                           '31'='NE',
                           '32'='NV',
                           '33'='NH',
                           '34'='NJ',
                           '35'='NM',
                           '36'='NY',
                           '37'='NC',
                           '38'='ND',
                           '39'='OH',
                           '40'='OK',
                           '41'='OR',
                           '42'='PA',
                           '44'='RI',
                           '45'='SC',
                           '46'='SD',
                           '47'='TN',
                           '48'='TX',
                           '49'='UT',
                           '50'='VT',
                           '51'='VA',
                           '53'='WA',
                           '54'='WV',
                           '55'='WI',
                           '56'='WY',
                           '72'= 'PR')
##################################################################################
#There is not single year age groups for 2013
#population.2003.12.17 = as.data.frame(census.manager$data$population$estimate$census.population$census$year__location__age__race__ethnicity__sex["2003", ,ages.of.interest.13.17,,,])
population.2005.12.17 = as.data.frame(rowSums(census.manager$data$population$estimate$census.population$census$year__location__age__race__ethnicity__sex["2005", ,ages.of.interest.13.17,,,]))
population.2007.12.17 = as.data.frame(rowSums(census.manager$data$population$estimate$census.population$census$year__location__age__race__ethnicity__sex["2007", ,ages.of.interest.13.17,,,]))
population.2009.12.17 = as.data.frame(rowSums(census.manager$data$population$estimate$census.population$census$year__location__age__race__ethnicity__sex["2009", ,ages.of.interest.13.17,,,]))
population.2011.12.17 = as.data.frame(rowSums(census.manager$data$population$estimate$census.population$census$year__location__age__race__ethnicity__sex["2011", ,ages.of.interest.13.17,,,]))
population.2013.12.17 = as.data.frame(rowSums(census.manager$data$population$estimate$census.population$census$year__location__age__race__ethnicity__sex["2013", ,ages.of.interest.13.17,,,]))
population.2015.12.17 = as.data.frame(rowSums(census.manager$data$population$estimate$census.population$census$year__location__age__race__ethnicity__sex["2015", ,ages.of.interest.13.17,,,]))
population.2017.12.17 = as.data.frame(rowSums(census.manager$data$population$estimate$census.population$census$year__location__age__race__ethnicity__sex["2017", ,ages.of.interest.13.17,,,]))

##Messy formatting bc idk how else to do this##
#####################################################################################################################
population.2005.12.17 <- population.2005.12.17%>%
  rename(population.13.17 = `rowSums(census.manager$data$population$estimate$census.population$census$year__location__age__race__ethnicity__sex["2005", , ages.of.interest.13.17, , , ])`)%>%
mutate(year = "2005")
  population.2007.12.17 <- population.2007.12.17%>%
  rename(population.13.17 = `rowSums(census.manager$data$population$estimate$census.population$census$year__location__age__race__ethnicity__sex["2007", , ages.of.interest.13.17, , , ])`)%>%
  mutate(year = "2007")
population.2009.12.17  <- population.2009.12.17 %>%
  rename(population.13.17 = `rowSums(census.manager$data$population$estimate$census.population$census$year__location__age__race__ethnicity__sex["2009", , ages.of.interest.13.17, , , ])`)%>%
  mutate(year = "2009")
population.2011.12.17 <- population.2011.12.17%>%
  rename(population.13.17 = `rowSums(census.manager$data$population$estimate$census.population$census$year__location__age__race__ethnicity__sex["2011", , ages.of.interest.13.17, , , ])`)%>%
  mutate(year = "2011")
population.2013.12.17 <- population.2013.12.17%>%
  rename(population.13.17 = `rowSums(census.manager$data$population$estimate$census.population$census$year__location__age__race__ethnicity__sex["2013", , ages.of.interest.13.17, , , ])`)%>%
  mutate(year = "2013")
population.2015.12.17  <- population.2015.12.17 %>%
  rename(population.13.17 = `rowSums(census.manager$data$population$estimate$census.population$census$year__location__age__race__ethnicity__sex["2015", , ages.of.interest.13.17, , , ])`)%>%
  mutate(year = "2015")
population.2017.12.17 <- population.2017.12.17%>%
  rename(population.13.17 = `rowSums(census.manager$data$population$estimate$census.population$census$year__location__age__race__ethnicity__sex["2017", , ages.of.interest.13.17, , , ])`)%>%
  mutate(year = "2017")
#####################################################################################################################
#This creates a list of census population values for ages 12-17 by county

lower.age.list <- list(population.2005.12.17, population.2007.12.17, population.2009.12.17, population.2011.12.17, population.2013.12.17, population.2015.12.17, population.2017.12.17)
            
younger.population = lapply(lower.age.list, function(file){
  
  data=file[1]
  
  data= as.data.frame(data)
  
   data = rownames_to_column(data, var = 'location.county')

   data$location.state.fips = substr(data$location.county, start = 1, stop = 2)
   data$location.state = state.to.fips.mappings[data$location.state.fips]
  
list(data) 
})

#########################################################################################################
#Now need to srt out locations
#NSDUH has national, states, and substate regions.  Census has county

counties.in.states = lapply(states, function(state){
  counties.in.this.state = locations::get.contained.locations(state, "county")
})

#Could you make a variable for each county that corresponds to each 'state' or 'substate region' and then group_by + sum to get values for locations
#Then join to NSDUH data