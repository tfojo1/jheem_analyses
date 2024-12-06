#This file is being used as a draft to adjust the census processing to save as several smaller data managers that can be merged to cut processing time

library(jheem2)
library(tidyverse)
library(readr)
library(readxl) 
library(haven)
library(locations)

###############################################################################

#####POPULATION 3#####

###############################################################################

census.manager = create.data.manager('census_manager', description='an additional data manager for census data')

#Register outcomes:
census.manager$register.outcome(
  'population',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Population',
    axis.name = 'Population',
    units = 'population',
    description = "Populaion Estimate"))

#Register Sources:
census.manager$register.parent.source('census', full.name = 'United States Census Bureau', short.name= "census") #parent
census.manager$register.source('census.population', parent.source= "census", full.name = "Census Population Data", short.name='census.population')#child

#Register Ontologies:
census.manager$register.ontology(
  'census',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('< 1 year', '1 year', '2 years', '3 years', '4 years', '5 years', '6 years', '7 years', '8 years', '9 years', '10 years',
          '11 years', '12 years', '13 years', '14 years', '15 years', '16 years', '17 years', '18 years', '19 years', '20 years',
          '21 years', '22 years', '23 years', '24 years', '25 years', '26 years', '27 years', '28 years', '29 years', '30 years',
          '31 years', '32 years', '33 years', '34 years', '35 years', '36 years', '37 years', '38 years', '39 years', '40 years',
          '41 years', '42 years', '43 years', '44 years', '45 years', '46 years', '47 years', '48 years', '49 years', '50 years',
          '51 years', '52 years', '53 years', '54 years', '55 years', '56 years', '57 years', '58 years', '59 years', '60 years',
          '61 years', '62 years', '63 years', '64 years', '65 years', '66 years', '67 years', '68 years', '69 years', '70 years',
          '71 years', '72 years', '73 years', '74 years', '75 years', '76 years', '77 years', '78 years', '79 years', '80 years',
          '81 years', '82 years', '83 years', '84 years', '85+ years'),
    race=c('white', 'black', 'american indian or alaska native', 'asian or pacific islander'),
    ethnicity=c('hispanic', 'not hispanic'),
    sex=c('male','female')
  ))

#Codes:


DATA.DIR.CENSUS.COUNTY="../../data_raw/population/county_00.23"

census_county_files <- Sys.glob(paste0(DATA.DIR.CENSUS.COUNTY, '/*.csv'))

#creating a list with sublists of filename, data#
data.list.county.pop <- lapply(census_county_files, function(x){
  list(filename=x, data=read.csv(x, header=TRUE))
})

################################################################################
###COUNTY POPULATION ESTIMATES 2000-2022###
################################################################################

data.list.county = lapply(data.list.county.pop, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  if(grepl("00.10", filename)) {
    data$POPESTIMATE2010= data$old_2010estimate  #REMOVE CENSUS AND USE POP ESTIMATE#   #Remove popestimate2010 from the 2000-2010 file so the more recent estimate from the
    #2010-2019 file can be used instead
    data$county_code = as.numeric(data$COUNTY)
    data$state_code = as.numeric(data$STATE)
    
    data= subset(data, data$COUNTY != "0")   #Remove county=0 (represents the county population)#
    
    data$state_code_clean= str_pad(data$state_code, width=2, side="left", pad="0")
    data$county_code_clean= str_pad(data$county_code, width=3, side="left", pad="0")
    
    #Combine county and county codes into FIPS- change FIPS to 'location'
    data$FIPS= paste(data$state_code_clean, data$county_code_clean, sep="")
    
    data$location = data$FIPS
  }
  
  if(grepl("10.19", filename)) { #Location is calculated differently here because FIPS is not available
    
    data$state = sub('.*,\\s*', '', data$COUNTY)
    data$county = gsub(",.*$", "", data$COUNTY)
    data$location.list = locations::get.location.code(data$county, 'COUNTY')
    data$state.abb = state.abb[match(data$state,state.name)]
    
    data$location.codes = sapply(1:nrow(data), function(i){
      intersect(unlist(get.location.code(data$county[i], 'COUNTY')), get.contained.locations((data$state.abb[i]), 'COUNTY')
      )})
    
    data$location = as.character(data$location.codes)
    data$location = ifelse(data$location.list == "11001", '11001', data$location)
    data$location = ifelse(data$county == "Yellowstone County", '30111', data$location) #Yellowstone county is associated with 30111 and 30113.  It looks like 30113 stopped being used in 1989.
    data$location.check = locations::is.location.valid(data$location)
    
    data <- data %>%
      filter(location.check == "TRUE")#Manually removing these counties until I know otherwise
    
  }
  
  if(grepl("20.23", filename)) {
    data$county_code = as.numeric(data$COUNTY)
    data$state_code = as.numeric(data$STATE)
    
    data= subset(data, data$COUNTY != "0")   #Remove county=0 (represents the county population)#
    
    data$state_code_clean= str_pad(data$state_code, width=2, side="left", pad="0")
    data$county_code_clean= str_pad(data$county_code, width=3, side="left", pad="0")
    
    #Combine county and county codes into FIPS- change FIPS to 'location'
    data$FIPS= paste(data$state_code_clean, data$county_code_clean, sep="")
    
    data$location = data$FIPS
    
  }
  
  #Begin set up for pivot longer
  data$"population_2000" = data$POPESTIMATE2000
  data$"population_2001" = data$POPESTIMATE2001 
  data$"population_2002" = data$POPESTIMATE2002 
  data$"population_2003" = data$POPESTIMATE2003
  data$"population_2004" = data$POPESTIMATE2004
  data$"population_2005" = data$POPESTIMATE2005
  data$"population_2006" = data$POPESTIMATE2006
  data$"population_2007" = data$POPESTIMATE2007
  data$"population_2008" = data$POPESTIMATE2008
  data$"population_2009" = data$POPESTIMATE2009
  data$"population_2010" = data$POPESTIMATE2010
  data$"population_2011" = data$POPESTIMATE2011
  data$"population_2012" = data$POPESTIMATE2012
  data$"population_2013" = data$POPESTIMATE2013
  data$"population_2014" = data$POPESTIMATE2014
  data$"population_2015" = data$POPESTIMATE2015
  data$"population_2016" = data$POPESTIMATE2016
  data$"population_2017" = data$POPESTIMATE2017
  data$"population_2018" = data$POPESTIMATE2018
  data$"population_2019" = data$POPESTIMATE2019
  data$"population_2020" = data$POPESTIMATE2020
  data$"population_2021" = data$POPESTIMATE2021
  data$"population_2022" = data$POPESTIMATE2022
  data$"population_2023" = data$POPESTIMATE2023
  
  #data$"deaths_2020" = data$DEATHS2020 #removing deaths for 2020 because it doesn't represent a full year of data
  data$"deaths_2021" = data$DEATHS2021
  data$"deaths_2022" = data$DEATHS2022
  data$"deaths_2023" = data$DEATHS2023
  
  
  ##this will give warning that it doesn't see these vars across all dfs##
  
  data<- data %>%
    select(location,(one_of("population_2000", "population_2001", "population_2002", "population_2003", "population_2004", "population_2005", "population_2006",
                            "population_2007", "population_2008", "population_2009", "population_2010", "population_2011", "population_2012", "population_2013", 
                            "population_2014", "population_2015", "population_2016", "population_2017", "population_2018", "population_2019", "population_2020", 
                            "population_2021", "population_2022", "population_2023","deaths_2021", "deaths_2022", "deaths_2023")))
  
  
  
  data <- data %>%
    pivot_longer(cols=c(one_of("population_2000", "population_2001", "population_2002", "population_2003", "population_2004", "population_2005", "population_2006",
                               "population_2007", "population_2008", "population_2009", "population_2010", "population_2011", "population_2012", "population_2013", 
                               "population_2014", "population_2015", "population_2016", "population_2017", "population_2018", "population_2019", "population_2020", 
                               "population_2021", "population_2022", "population_2023","deaths_2021", "deaths_2022", "deaths_2023")),
                 names_to = c("outcome", "year"),
                 names_sep = "_",
                 values_to = "value")
  
  data$value = as.numeric(gsub(",", '', data$value))
  
  data$year = as.character(data$year) 
  
  data= as.data.frame(data)
  
  list(filename, data) #what to return# 
})


###############################################################################
##Separate into lists for population and deaths now that parent source is different
###############################################################################
data.list.county.pop.00.23 = lapply(data.list.county, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  data= subset(data, data$outcome == "population")
  
  data= as.data.frame(data)
  
  list(filename, data) #what to return# 
})

data.list.county.deaths.21.23 = lapply(data.list.county, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  data= subset(data, data$outcome == "deaths")
  
  data= as.data.frame(data)
  
  list(filename, data) #what to return# 
})

################################################################################
# Adding Death Data for 2010-2019 
#This data used to be with the population estimates but once we updated the data
#For the 2010-2019 intercensal estimates, the data is separate
################################################################################
DATA.DIR.CENSUS.DEATHS="../../data_raw/population/deaths_10_19"

deaths_10_19_files <- Sys.glob(paste0(DATA.DIR.CENSUS.DEATHS, '/*.csv'))

data.list.deaths.10.19 <- lapply(deaths_10_19_files, function(x){
  list(filename=x, data=read.csv(x, header=TRUE))
})

data.list.deaths.10.19.clean = lapply(data.list.deaths.10.19, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$county_code = as.numeric(data$COUNTY)
  data$state_code = as.numeric(data$STATE)
  
  data= subset(data, data$COUNTY != "0")   #Remove county=0 (represents the county population)#
  
  data$state_code_clean= str_pad(data$state_code, width=2, side="left", pad="0")
  data$county_code_clean= str_pad(data$county_code, width=3, side="left", pad="0")
  
  #Combine county and county codes into FIPS- change FIPS to 'location'
  data$FIPS= paste(data$state_code_clean, data$county_code_clean, sep="")
  
  data$location = data$FIPS
  
  #data$"deaths_2010" = data$DEATHS2010 #Deaths in 2010 do not represent a full year of data
  data$"deaths_2011" = data$DEATHS2011
  data$"deaths_2012" = data$DEATHS2012
  data$"deaths_2013" = data$DEATHS2013
  data$"deaths_2014" = data$DEATHS2014
  data$"deaths_2015" = data$DEATHS2015
  data$"deaths_2016" = data$DEATHS2016
  data$"deaths_2017" = data$DEATHS2017
  data$"deaths_2018" = data$DEATHS2018
  data$"deaths_2019" = data$DEATHS2019
  
  data<- data %>%
    select(location,(one_of( "deaths_2011", "deaths_2012", "deaths_2013", "deaths_2014", "deaths_2015", 
                             "deaths_2016","deaths_2017", "deaths_2018", "deaths_2019")))
  
  data <- data %>%
    pivot_longer(cols=c(one_of("deaths_2011", "deaths_2012", "deaths_2013", "deaths_2014", "deaths_2015", 
                               "deaths_2016","deaths_2017", "deaths_2018", "deaths_2019")),
                 names_to = c("outcome", "year"),
                 names_sep = "_",
                 values_to = "value")
  
  data= as.data.frame(data)
  
  list(filename, data) #what to return# 
})

county_deaths_10_19 = lapply(data.list.deaths.10.19.clean, `[[`, 2)

for (data in county_deaths_10_19) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census.deaths',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}



################################################################################
##Update for 8-28-24: Adding US Population (summing all counties)
################################################################################
us.total.pop = lapply(data.list.county.pop.00.23, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  data <- data %>%
    group_by(year)%>%
    mutate(total = sum(value))%>%
    select(-location, -value)%>%
    rename(value = total)%>%
    mutate(location = "US")
  
  data= as.data.frame(data)
  
  data<- data[!duplicated(data), ]
  
  list(filename, data) #what to return# 
})

################################################################################
##Update for 10-7-24: Adding US Deaths Total (summing all counties)
################################################################################
us.total.deaths.21.23 = lapply(data.list.county.deaths.21.23, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  data <- data %>%
    group_by(year)%>%
    mutate(total = sum(value))%>%
    select(-location, -value)%>%
    rename(value = total)%>%
    mutate(location = "US")
  
  data= as.data.frame(data)
  
  data<- data[!duplicated(data), ]
  
  list(filename, data) #what to return# 
})
us.total.deaths.10.19 = lapply(data.list.deaths.10.19.clean, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  data <- data %>%
    group_by(year)%>%
    mutate(total = sum(value))%>%
    select(-location, -value)%>%
    rename(value = total)%>%
    mutate(location = "US")
  
  data= as.data.frame(data)
  
  data<- data[!duplicated(data), ]
  
  list(filename, data) #what to return# 
})

################################################################################
###Put data into Census Manager###
################################################################################  

#County POPULATION Values
county_pop = lapply(data.list.county.pop.00.23, `[[`, 2)

for (data in county_pop) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census.population',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting- 2000-2019 data are intercensal estimates; 2020-2023 data is from the Vintage 2023')
}


#County DEATH Values
county_deaths = lapply(data.list.county.deaths.21.23, `[[`, 2)

for (data in county_deaths) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census.deaths',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}

#US Totals- population
us.total.pop.put = lapply(us.total.pop, `[[`, 2)

for (data in us.total.pop.put) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census.population',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}


#US Totals- deaths
us.total.deaths.put.10.19 = lapply(us.total.deaths.10.19, `[[`, 2)

for (data in us.total.deaths.put.10.19) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census.population',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}

us.total.deaths.put.21.23 = lapply(us.total.deaths.21.23, `[[`, 2)

for (data in us.total.deaths.put.21.23) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census.population',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}

#Save:
save(census.manager, file="Q:/data_managers/data.manager.merge/census.manager_population3.rdata")
