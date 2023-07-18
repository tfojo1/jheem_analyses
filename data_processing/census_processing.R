library(jheem2)
library(tidyverse)

#Create a separate data manager for the census data#

census.manager = create.data.manager('census_manager', description='an additional data manager for census data')

census.manager$register.outcome(
  'population',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Population',
    axis.name = 'Population',
    units = 'population',
    description = "Population Estimate"))

census.manager$register.source('census', full.name = "US Census Bureau", short.name='census')

###Update the ontology once you know what these values should be###
census.manager$register.ontology(
  'census',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('13-24 years', '25-34 years', '35-44 years', '45-54 years','55+ years'),
    race=c('American Indian/Alaska Native', 'Asian', 'Black/African American', 'Hispanic/Latino', 'Multiracial', 'Native Hawaiian/Other Pacific Islander', 'White'),
    sex=c('male','female')
  ))

###Read in Census Files###
DATA.DIR.CENSUS.STATE="../../data_raw/population/state"
DATA.DIR.CENSUS.COUNTY="../../data_raw/population/county"
DATA.DIR.CENSUS.MSA="../../data_raw/population/msa"

census_state_files <- Sys.glob(paste0(DATA.DIR.CENSUS.STATE, '/*.csv'))
census_county_files <- Sys.glob(paste0(DATA.DIR.CENSUS.COUNTY, '/*.csv'))
census_msa_files <- Sys.glob(paste0(DATA.DIR.CENSUS.MSA, '/*.csv'))

#creating a list with sublists of filename, data#
data.list.state.pop <- lapply(census_state_files, function(x){
  list(filename=x, data=read.csv(x, header=TRUE))
})

data.list.county.pop <- lapply(census_county_files, function(x){
  list(filename=x, data=read.csv(x, header=TRUE))
})

data.list.msa.pop <- lapply(census_msa_files, function(x){
  list(filename=x, data=read.csv(x, header=TRUE))
})

########################Create data frames individually to put into manager
#start with county 2020-2022

  #establish locations for all county files#

data.list.county.pop.clean = lapply(data.list.county.pop, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data$state_code = as.numeric(data$STATE)
  data$county_code = as.numeric(data$COUNTY)
  
  
  #Remove county=0 bc this is the state population#
  data= subset(data, data$COUNTY != "0")   #THIS ISNT WORKING 10-19 AND 20-22-FIGURE OTUW HY FILTER ISNT WORKING#
  
  data$state_code_clean= str_pad(data$state_code, width=2, side="left", pad="0")
  data$county_code_clean= str_pad(data$county_code, width=3, side="left", pad="0")
  
  #Combine state and county codes into FIPS- change FIPS to 'location'
  data$FIPS= paste(data$state_code_clean, data$county_code_clean, sep="")
  
   data$location = data$FIPS
   
   data=subset(data, data$location != "09110")
   data=subset(data, data$location != "09120")
   data=subset(data, data$location != "09130")
   data=subset(data, data$location != "09140")
   data=subset(data, data$location != "09150")
   data=subset(data, data$location != "09160")
   data=subset(data, data$location != "09170")
   data=subset(data, data$location != "09180")
   data=subset(data, data$location != "09190")
 #Removing FIPS codes that are causing error in data manager#
  
  list(filename, data) #what to return#
  
})

####is there a better way to do this since the values may change?
county.20.22 = data.list.county.pop.clean[[3]]
county.20.22 = county.20.22[[2]]

county.10.19 = data.list.county.pop.clean[[2]]
county.10.19 = county.10.19[[2]]

county.00.10 = data.list.county.pop.clean[[1]]
county.00.10 = county.00.10[[2]]

#County population 2020 df
county_2020_pop <- county.20.22 %>%
  select(location, ESTIMATESBASE2020) %>%
  mutate(year= "2020") %>%
  mutate(outcome = "population") %>%
  rename(value = "ESTIMATESBASE2020")

#County population 2021 df
county_2021_pop <- county.20.22 %>%
  select(location, POPESTIMATE2021) %>%
  mutate(year= "2021") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2021")

#County population 2022 df
county_2022_pop <- county.20.22 %>%
  select(location, POPESTIMATE2022) %>%
  mutate(year= "2022") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2022")

county_pop_list = list(county_2020_pop, county_2021_pop, county_2022_pop)


##############################################################################
###Put data into census manager###

for (data in county_pop_list) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}
