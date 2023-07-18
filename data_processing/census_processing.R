library(jheem2)
library(tidyverse)

################################################################################
###Create a separate data manager for the census data###
################################################################################

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

################################################################################
                ###Read in Census Files###
################################################################################
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


################################################################################
                ###National Population Estimates###
################################################################################

################################################################################
                ###State Population Estimates 2000-2022###
################################################################################
#Pull 200-2009 state population estimates from county files#
state.county = data.list.county.pop[[1]]
state.county = state.county[[2]]
names(state.abb) <- state.name

state.county.00.09 <- state.county %>%
  filter(COUNTY == "0") %>% #select states only#
  mutate(location = ifelse (STNAME == "District of Columbia", "DC", state.abb[STNAME]))
 

#State population 2000 df
state_2000_pop <- state.county.00.09 %>%
  select(location, POPESTIMATE2000) %>%
  mutate(year= "2000") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2000")

#State population 2001 df
state_2001_pop <- state.county.00.09 %>%
  select(location, POPESTIMATE2001) %>%
  mutate(year= "2001") %>%
  mutate(outcome = "population") %>%

#State population 2002 df
state_2002_pop <- state.county.00.09 %>%
  select(location, POPESTIMATE2002) %>%
  mutate(year= "2002") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2002")

#State population 2003 df
state_2003_pop <- state.county.00.09 %>%
  select(location, POPESTIMATE2003) %>%
  mutate(year= "2003") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2003")

#State population 2004 df
state_2004_pop <- state.county.00.09 %>%
  select(location, POPESTIMATE2004) %>%
  mutate(year= "2004") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2004")

#State population 2005 df
state_2005_pop <- state.county.00.09 %>%
  select(location, POPESTIMATE2005) %>%
  mutate(year= "2005") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2005")

#State population 2006 df
state_2006_pop <- state.county.00.09 %>%
  select(location, POPESTIMATE2006) %>%
  mutate(year= "2006") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2006")

#State population 2007 df
state_2007_pop <- state.county.00.09 %>%
  select(location, POPESTIMATE2007) %>%
  mutate(year= "2007") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2007")

#State population 2008 df
state_2008_pop <- state.county.00.09 %>%
  select(location, POPESTIMATE2008) %>%
  mutate(year= "2008") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2008")

#State population 2009 df
state_2009_pop <- state.county.00.09 %>%
  select(location, POPESTIMATE2009) %>%
  mutate(year= "2009") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2009")
  

#For State populations 2010-2022#
#establish locations for all state files#

data.list.state.pop.clean = lapply(data.list.state.pop, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data= subset(data, data$STATE != "0")   #0 represents various regions#
  data= subset(data, data$NAME != "Puerto Rico") #remove Puerto Rico
  
  names(state.abb) <- state.name 
  data$location = state.abb[data$NAME]
  
  list(filename, data) 
  
})

####QUESTION: THESE LOCATIONS WILL CHANGE###############
state.20.22 = data.list.state.pop.clean[[3]]
state.20.22 = state.20.22[[2]]

state.10.19 = data.list.state.pop.clean[[2]]
state.10.19 = state.10.19[[2]]

#State population 2010 df
state_2010_pop <- state.10.19 %>%
  select(location, POPESTIMATE2010) %>%
  mutate(year= "2010") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2010")

#state population 2011 df
state_2011_pop <- state.10.19 %>%
  select(location, POPESTIMATE2011) %>%
  mutate(year= "2011") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2011")

#state population 2012 df
state_2012_pop <- state.10.19 %>%
  select(location, POPESTIMATE2012) %>%
  mutate(year= "2012") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2012")

#state population 2013 df
state_2013_pop <- state.10.19 %>%
  select(location, POPESTIMATE2013) %>%
  mutate(year= "2013") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2013")

#state population 2014 df
state_2014_pop <- state.10.19 %>%
  select(location, POPESTIMATE2014) %>%
  mutate(year= "2014") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2014")

#state population 2015 df
state_2015_pop <- state.10.19 %>%
  select(location, POPESTIMATE2015) %>%
  mutate(year= "2015") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2015")

#state population 2016 df
state_2016_pop <- state.10.19 %>%
  select(location, POPESTIMATE2016) %>%
  mutate(year= "2016") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2016")

#state population 2017 df
state_2017_pop <- state.10.19 %>%
  select(location, POPESTIMATE2017) %>%
  mutate(year= "2017") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2017")

#state population 2018 df
state_2018_pop <- state.10.19 %>%
  select(location, POPESTIMATE2018) %>%
  mutate(year= "2018") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2018")

#state population 2019 df
state_2019_pop <- state.10.19 %>%
  select(location, POPESTIMATE2019) %>%
  mutate(year= "2019") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2019")

#state population 2020 df
state_2020_pop <- state.20.22 %>%
  select(location, POPESTIMATE2020) %>%
  mutate(year= "2020") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2020")

#state population 2021 df
state_2021_pop <- state.20.22 %>%
  select(location, POPESTIMATE2021) %>%
  mutate(year= "2021") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2021")

#state population 2022 df
state_2022_pop <- state.20.22 %>%
  select(location, POPESTIMATE2022) %>%
  mutate(year= "2022") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2022")

#Create list of state population by year to put into Census Manager
state_pop_list = list(state_2000_pop, state_2001_pop, state_2002_pop, state_2003_pop, state_2004_pop, state_2005_pop, state_2006_pop, state_2007_pop,
                      state_2008_pop, state_2009_pop, state_2010_pop, state_2011_pop, state_2012_pop, state_2013_pop, state_2014_pop,state_2015_pop, 
                      state_2016_pop, state_2017_pop, state_2018_pop, state_2019_pop, state_2020_pop, state_2021_pop, state_2022_pop)

rm(data.list.state.pop, state.county, state.county.00.09, state.10.19, state.20.22, state_2000_pop, state_2001_pop, state_2002_pop, state_2003_pop, 
   state_2004_pop, state_2005_pop, state_2006_pop, state_2007_pop,state_2008_pop, state_2009_pop, state_2010_pop, state_2011_pop, state_2012_pop, 
   state_2013_pop, state_2014_pop,state_2015_pop, state_2016_pop, state_2017_pop, state_2018_pop, state_2019_pop, state_2020_pop, state_2021_pop, state_2022_pop)


################################################################################
                ###county Population Estimates 2000-2022###
################################################################################

#establish locations for all county files#

data.list.county.pop.clean = lapply(data.list.county.pop, function(file){
  
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
   
   data=subset(data, data$location != "09110") #Removing FIPS codes that are causing error in data manager#
   data=subset(data, data$location != "09120")
   data=subset(data, data$location != "09130")
   data=subset(data, data$location != "09140")
   data=subset(data, data$location != "09150")
   data=subset(data, data$location != "09160")
   data=subset(data, data$location != "09170")
   data=subset(data, data$location != "09180")
   data=subset(data, data$location != "09190")
  
  list(filename, data) #what to return#
  
})

####QUESTION: is there a better way to do this since the values may change?
county.20.22 = data.list.county.pop.clean[[3]]
county.20.22 = county.20.22[[2]]

county.10.19 = data.list.county.pop.clean[[2]]
county.10.19 = county.10.19[[2]]

county.00.10 = data.list.county.pop.clean[[1]]
county.00.10 = county.00.10[[2]]

#county population 2000 df
county_2000_pop <- county.00.10 %>%
  select(location, POPESTIMATE2000) %>%
  mutate(year= "2000") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2000")

#county population 2001 df
county_2001_pop <- county.00.10 %>%
  select(location, POPESTIMATE2001) %>%
  mutate(year= "2001") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2001")

#county population 2002 df
county_2002_pop <- county.00.10 %>%
  select(location, POPESTIMATE2002) %>%
  mutate(year= "2002") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2002")

#county population 2003 df
county_2003_pop <- county.00.10 %>%
  select(location, POPESTIMATE2003) %>%
  mutate(year= "2003") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2003")

#county population 2004 df
county_2004_pop <- county.00.10 %>%
  select(location, POPESTIMATE2004) %>%
  mutate(year= "2004") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2004")

#county population 2005 df
county_2005_pop <- county.00.10 %>%
  select(location, POPESTIMATE2005) %>%
  mutate(year= "2005") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2005")

#county population 2006 df
county_2006_pop <- county.00.10 %>%
  select(location, POPESTIMATE2006) %>%
  mutate(year= "2006") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2006")

#county population 2007 df
county_2007_pop <- county.00.10 %>%
  select(location, POPESTIMATE2007) %>%
  mutate(year= "2007") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2007")

#county population 2008 df
county_2008_pop <- county.00.10 %>%
  select(location, POPESTIMATE2008) %>%
  mutate(year= "2008") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2008")

#county population 2009 df
county_2009_pop <- county.00.10 %>%
  select(location, POPESTIMATE2009) %>%
  mutate(year= "2009") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2009")

#county population 2010 df
county_2010_pop <- county.10.19 %>%
  select(location, POPESTIMATE2010) %>%
  mutate(year= "2010") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2010")

#county population 2011 df
county_2011_pop <- county.10.19 %>%
  select(location, POPESTIMATE2011) %>%
  mutate(year= "2011") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2011")

#county population 2012 df
county_2012_pop <- county.10.19 %>%
  select(location, POPESTIMATE2012) %>%
  mutate(year= "2012") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2012")

#county population 2013 df
county_2013_pop <- county.10.19 %>%
  select(location, POPESTIMATE2013) %>%
  mutate(year= "2013") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2013")

#county population 2014 df
county_2014_pop <- county.10.19 %>%
  select(location, POPESTIMATE2014) %>%
  mutate(year= "2014") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2014")

#county population 2015 df
county_2015_pop <- county.10.19 %>%
  select(location, POPESTIMATE2015) %>%
  mutate(year= "2015") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2015")

#county population 2016 df
county_2016_pop <- county.10.19 %>%
  select(location, POPESTIMATE2016) %>%
  mutate(year= "2016") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2016")

#county population 2017 df
county_2017_pop <- county.10.19 %>%
  select(location, POPESTIMATE2017) %>%
  mutate(year= "2017") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2017")

#county population 2018 df
county_2018_pop <- county.10.19 %>%
  select(location, POPESTIMATE2018) %>%
  mutate(year= "2018") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2018")

#county population 2019 df
county_2019_pop <- county.10.19 %>%
  select(location, POPESTIMATE2019) %>%
  mutate(year= "2019") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2019")

#county population 2020 df
county_2020_pop <- county.20.22 %>%
  select(location, POPESTIMATE2020) %>%
  mutate(year= "2020") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2020")

#county population 2021 df
county_2021_pop <- county.20.22 %>%
  select(location, POPESTIMATE2021) %>%
  mutate(year= "2021") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2021")

#county population 2022 df
county_2022_pop <- county.20.22 %>%
  select(location, POPESTIMATE2022) %>%
  mutate(year= "2022") %>%
  mutate(outcome = "population") %>%
  rename(value = "POPESTIMATE2022")

#Create list of county population by year to put into Census Manager
county_pop_list = list(county_2000_pop, county_2001_pop, county_2002_pop, county_2003_pop, county_2004_pop, county_2005_pop, county_2006_pop,
                       county_2007_pop, county_2008_pop, county_2009_pop, county_2010_pop, county_2011_pop, county_2012_pop, county_2013_pop, 
                       county_2014_pop,county_2015_pop, county_2016_pop, county_2017_pop, county_2018_pop, county_2019_pop, county_2020_pop, 
                       county_2021_pop, county_2022_pop)

rm(data.list.county.pop, county.00.10, county.10.19, county.20.22, county_2000_pop, county_2001_pop, county_2002_pop, county_2003_pop, county_2004_pop, county_2005_pop, county_2006_pop,
   county_2007_pop, county_2008_pop, county_2009_pop, county_2010_pop, county_2011_pop, county_2012_pop, county_2013_pop, 
   county_2014_pop,county_2015_pop, county_2016_pop, county_2017_pop, county_2018_pop, county_2019_pop, county_2020_pop, 
   county_2021_pop, county_2022_pop)


################################################################################
                    ###Put data into Census Manager###
################################################################################    

for (data in county_pop_list) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}

for (data in state_pop_list) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}
