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
          ###National Population Estimates 2010-2022###
################################################################################
###Pull National data from State Files; filter by US###

data.list.nat.pop.clean = lapply(data.list.state.pop, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data= subset(data, data$NAME== "United States")   #National values#
  data$location = "US"
  data$outcome= "population"
  
  #Begin set up for pivot longer
  # data$"2000" = data$POPESTIMATE2000
  # data$"2001" = data$POPESTIMATE2001 
  # data$"2002" = data$POPESTIMATE2002 
  # data$"2003" = data$POPESTIMATE2003
  # data$"2004" = data$POPESTIMATE2004
  # data$"2005" = data$POPESTIMATE2005
  # data$"2006" = data$POPESTIMATE2006
  # data$"2007" = data$POPESTIMATE2007
  # data$"2008" = data$POPESTIMATE2008
  # data$"2009" = data$POPESTIMATE2009
  data$"2010" = data$POPESTIMATE2010
  data$"2011" = data$POPESTIMATE2011
  data$"2012" = data$POPESTIMATE2012
  data$"2013" = data$POPESTIMATE2013
  data$"2014" = data$POPESTIMATE2014
  data$"2015" = data$POPESTIMATE2015
  data$"2016" = data$POPESTIMATE2016
  data$"2017" = data$POPESTIMATE2017
  data$"2018" = data$POPESTIMATE2018
  data$"2019" = data$POPESTIMATE2019
  data$"2020" = data$POPESTIMATE2020
  data$"2021" = data$POPESTIMATE2021
  data$"2022" = data$POPESTIMATE2022
  
  
  ##this will give warning that it doesn't see these vars across all dfs##
  data<- data %>%
    select(outcome, location, (one_of("2010","2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019",
                                      "2020", "2021", "2022")))
  
  data <- data %>%
    pivot_longer(cols=c(one_of("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019",
                               "2020", "2021", "2022")),
                 names_to = "year",
                 names_transform = list(year = as.integer),
                 values_to = "value")
  
  data$year = as.character(data$year) 
  
  data= as.data.frame(data)
  
  list(filename, data) #what to return# 
  #this contains all county pop estimates from 2000-2022, ready for census manager#
  
})

################################################################################
                  ###State Population Estimates###
###START WITH 2010-2022, NEED TO REVISIT 2000 LATER###
################################################################################

data.list.state.pop.clean = lapply(data.list.state.pop, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]
  
  data= subset(data, data$STATE != "0")   #0 represents various regions#
  data= subset(data, data$NAME != "Puerto Rico") #remove Puerto Rico
  
  names(state.abb) <- state.name 
  data$location =ifelse(data$NAME == "District of Columbia", "DC", state.abb[data$NAME])
  
  #Begin set up for pivot longer
  # data$"2000" = data$POPESTIMATE2000
  # data$"2001" = data$POPESTIMATE2001 
  # data$"2002" = data$POPESTIMATE2002 
  # data$"2003" = data$POPESTIMATE2003
  # data$"2004" = data$POPESTIMATE2004
  # data$"2005" = data$POPESTIMATE2005
  # data$"2006" = data$POPESTIMATE2006
  # data$"2007" = data$POPESTIMATE2007
  # data$"2008" = data$POPESTIMATE2008
  # data$"2009" = data$POPESTIMATE2009
  data$"2010" = data$POPESTIMATE2010
  data$"2011" = data$POPESTIMATE2011
  data$"2012" = data$POPESTIMATE2012
  data$"2013" = data$POPESTIMATE2013
  data$"2014" = data$POPESTIMATE2014
  data$"2015" = data$POPESTIMATE2015
  data$"2016" = data$POPESTIMATE2016
  data$"2017" = data$POPESTIMATE2017
  data$"2018" = data$POPESTIMATE2018
  data$"2019" = data$POPESTIMATE2019
  data$"2020" = data$POPESTIMATE2020
  data$"2021" = data$POPESTIMATE2021
  data$"2022" = data$POPESTIMATE2022
  
  data$outcome= "population"
  
  ##this will give warning that it doesn't see these vars across all dfs##
  data<- data %>%
    select(outcome, location, (one_of("2010","2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019",
                                      "2020", "2021", "2022")))
  
  data <- data %>%
    pivot_longer(cols=c(one_of("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019",
                               "2020", "2021", "2022")),
                 names_to = "year",
                 names_transform = list(year = as.integer),
                 values_to = "value")
  
  data$year = as.character(data$year)
  
  data= as.data.frame(data)
  
  list(filename, data) #what to return# 
  #this contains all county pop estimates from 2000-2022, ready for census manager#
  
})

################################################################################
          ###county Population Estimates 2000-2022###
################################################################################

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
  data=subset(data, data$location != "09120")  #Update 7/20/23: temporarily removing counties causing location error:
  data=subset(data, data$location != "09130")
  data=subset(data, data$location != "09140")
  data=subset(data, data$location != "09150")
  data=subset(data, data$location != "09160")
  data=subset(data, data$location != "09170")
  data=subset(data, data$location != "09180")
  data=subset(data, data$location != "09190")
  
  data=subset(data, data$location != "02270")
  data=subset(data, data$location != "46113")
  data=subset(data, data$location != "51515")
  
  
  #Begin set up for pivot longer
  data$"2000" = data$POPESTIMATE2000
  data$"2001" = data$POPESTIMATE2001 
  data$"2002" = data$POPESTIMATE2002 
  data$"2003" = data$POPESTIMATE2003
  data$"2004" = data$POPESTIMATE2004
  data$"2005" = data$POPESTIMATE2005
  data$"2006" = data$POPESTIMATE2006
  data$"2007" = data$POPESTIMATE2007
  data$"2008" = data$POPESTIMATE2008
  data$"2009" = data$POPESTIMATE2009
  data$"2010" = data$POPESTIMATE2010
  data$"2011" = data$POPESTIMATE2011
  data$"2012" = data$POPESTIMATE2012
  data$"2013" = data$POPESTIMATE2013
  data$"2014" = data$POPESTIMATE2014
  data$"2015" = data$POPESTIMATE2015
  data$"2016" = data$POPESTIMATE2016
  data$"2017" = data$POPESTIMATE2017
  data$"2018" = data$POPESTIMATE2018
  data$"2019" = data$POPESTIMATE2019
  data$"2020" = data$POPESTIMATE2020
  data$"2021" = data$POPESTIMATE2021
  data$"2022" = data$POPESTIMATE2022
  
  data$outcome= "population"
  
  ##this will give warning that it doesn't see these vars across all dfs##
  data<- data %>%
    select(outcome, location, (one_of("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010",
                                      "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019",
                                      "2020", "2021", "2022")))
  
  
  data <- data %>%
    pivot_longer(cols=c(one_of("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010",
                               "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019",
                               "2020", "2021", "2022")),
                 names_to = "year",
                 names_transform = list(year = as.integer),
                 values_to = "value")
  
  data$year = as.character(data$year) 
  
  data= as.data.frame(data)
  
  list(filename, data) #what to return# 
  #this contains all county pop estimates from 2000-2022, ready for census manager#
  
})

################################################################################
          ###MSA Population Estimates 2010-2022###
################################################################################

data.list.msa.pop.clean = lapply(data.list.msa.pop, function(file){
  
  data=file[["data"]]
  filename = file[["filename"]]

  
  data$location = paste("C", data$CBSA, sep=".")

  
  #Begin set up for pivot longer
  # data$"2000" = data$POPESTIMATE2000
  # data$"2001" = data$POPESTIMATE2001 
  # data$"2002" = data$POPESTIMATE2002 
  # data$"2003" = data$POPESTIMATE2003
  # data$"2004" = data$POPESTIMATE2004
  # data$"2005" = data$POPESTIMATE2005
  # data$"2006" = data$POPESTIMATE2006
  # data$"2007" = data$POPESTIMATE2007
  # data$"2008" = data$POPESTIMATE2008
  # data$"2009" = data$POPESTIMATE2009
  data$"2010" = data$POPESTIMATE2010
  data$"2011" = data$POPESTIMATE2011
  data$"2012" = data$POPESTIMATE2012
  data$"2013" = data$POPESTIMATE2013
  data$"2014" = data$POPESTIMATE2014
  data$"2015" = data$POPESTIMATE2015
  data$"2016" = data$POPESTIMATE2016
  data$"2017" = data$POPESTIMATE2017
  data$"2018" = data$POPESTIMATE2018
  data$"2019" = data$POPESTIMATE2019
  data$"2020" = data$POPESTIMATE2020
  data$"2021" = data$POPESTIMATE2021
  data$"2022" = data$POPESTIMATE2022
  
  data$outcome= "population"
  
  ##this will give warning that it doesn't see these vars across all dfs##
  data<- data %>%
    select(outcome, location, (one_of("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019",
                                      "2020", "2021", "2022")))
  
  
  data <- data %>%
    pivot_longer(cols=c(one_of("2010","2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019",
                               "2020", "2021", "2022")),
                 names_to = "year",
                 names_transform = list(year = as.integer),
                 values_to = "value")
  
  data$year = as.character(data$year)
  data$value = as.numeric(data$value)
  
  data= as.data.frame(data)
  
  list(filename, data) #what to return# 
})

################################################################################
                ###Put data into Census Manager###
################################################################################    

#National Population values
national_pop = lapply(data.list.nat.pop.clean, `[[`, 2)

for (data in national_pop) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}

#State Population Values
state_pop = lapply(data.list.state.pop.clean, `[[`, 2)

for (data in state_pop) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}

#County Population Values
county_pop = lapply(data.list.county.pop.clean, `[[`, 2)

for (data in county_pop) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}

#MSA Population Values
msa_pop = lapply(data.list.msa.pop.clean, `[[`, 2)

for (data in msa_pop) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}

################################################################################
                  ###Save Census Manager###
################################################################################ 

save(census.manager, file="../../cached/census.manager.rdata")



