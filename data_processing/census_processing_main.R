library(jheem2)
library(tidyverse)
library(readr)
library(readxl) 

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

census.manager$register.outcome(
  'births',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Births',
    axis.name = 'Births',
    units = 'births',
    description = "Births"))

census.manager$register.outcome(
  'deaths',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Deaths',
    axis.name = 'Deaths',
    units = 'deaths',
    description = "Deaths"))

census.manager$register.source('census', full.name = "US Census Bureau", short.name='census')

census.manager$register.source('cdc_wonder', full.name = "CDC Wonder", short.name='cdc_wonder')

census.manager$register.ontology(
  'census.cdc.wonder',
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
    race=c('American Indian or Alaska Native', 'Asian or Pacific Islander', 'Black or African American', 'White'),
    ethnicity=c('Hispanic or Latino', 'Not Hispanic or Latino'),
    sex=c('male','female')
  ))

################################################################################
                  ###Read in Census Files###
################################################################################

DATA.DIR.CENSUS.COUNTY="../../data_raw/population/county_00.22"

census_county_files <- Sys.glob(paste0(DATA.DIR.CENSUS.COUNTY, '/*.csv'))

#creating a list with sublists of filename, data#
data.list.county.pop <- lapply(census_county_files, function(x){
  list(filename=x, data=read.csv(x, header=TRUE))
})
################################################################################
            ###Sourcing other files here###
################################################################################ 

#This pulls population and demographics from 1970-1989#
source('data_processing/census_1970_1989.R')

#This pulls single year age groups and demographic data from 1990-2020#
source('data_processing/census_cdc_wonder.R')

################################################################################
          ###COUNTY POPULATION ESTIMATES 2000-2022###
          ##County BIRTH AND DEATH ESTIMATES 2010-2022##
################################################################################

data.list.county = lapply(data.list.county.pop, function(file){
  
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
  
  #Remove popestimate2010 from the 2000-2010 file so the more recent estimate from the
  #2010-2019 file can be used instead
  if(grepl("00.10", filename)) {
    data$POPESTIMATE2010= data$old_2010estimate  #REMOVE CENSUS AND USE POP ESTIMATE#
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
  
  data$"births_2000" = data$BIRTHS2000
  data$"births_2001" = data$BIRTHS2001 
  data$"births_2002" = data$BIRTHS2002 
  data$"births_2003" = data$BIRTHS2003
  data$"births_2004" = data$BIRTHS2004
  data$"births_2005" = data$BIRTHS2005
  data$"births_2006" = data$BIRTHS2006
  data$"births_2007" = data$BIRTHS2007
  data$"births_2008" = data$BIRTHS2008
  data$"births_2009" = data$BIRTHS2009
  data$"births_2010" = data$BIRTHS2010
  data$"births_2010" = data$BIRTHS2010
  data$"births_2011" = data$BIRTHS2011
  data$"births_2012" = data$BIRTHS2012
  data$"births_2013" = data$BIRTHS2013
  data$"births_2014" = data$BIRTHS2014
  data$"births_2015" = data$BIRTHS2015
  data$"births_2016" = data$BIRTHS2016
  data$"births_2017" = data$BIRTHS2017
  data$"births_2018" = data$BIRTHS2018
  data$"births_2019" = data$BIRTHS2019
  data$"births_2020" = data$BIRTHS2020
  data$"births_2021" = data$BIRTHS2021
  data$"births_2022" = data$BIRTHS2022
  
  data$"deaths_2000" = data$DEATHS2000
  data$"deaths_2001" = data$DEATHS2001 
  data$"deaths_2002" = data$DEATHS2002 
  data$"deaths_2003" = data$DEATHS2003
  data$"deaths_2004" = data$DEATHS2004
  data$"deaths_2005" = data$DEATHS2005
  data$"deaths_2006" = data$DEATHS2006
  data$"deaths_2007" = data$DEATHS2007
  data$"deaths_2008" = data$DEATHS2008
  data$"deaths_2009" = data$DEATHS2009
  data$"deaths_2010" = data$DEATHS2010
  data$"deaths_2010" = data$DEATHS2010
  data$"deaths_2011" = data$DEATHS2011
  data$"deaths_2012" = data$DEATHS2012
  data$"deaths_2013" = data$DEATHS2013
  data$"deaths_2014" = data$DEATHS2014
  data$"deaths_2015" = data$DEATHS2015
  data$"deaths_2016" = data$DEATHS2016
  data$"deaths_2017" = data$DEATHS2017
  data$"deaths_2018" = data$DEATHS2018
  data$"deaths_2019" = data$DEATHS2019
  data$"deaths_2020" = data$DEATHS2020
  data$"deaths_2021" = data$DEATHS2021
  data$"deaths_2022" = data$DEATHS2022
  
  
  ##this will give warning that it doesn't see these vars across all dfs##
  
  data<- data %>%
    select(location,(one_of("population_2000", "population_2001", "population_2002", "population_2003", "population_2004", "population_2005", "population_2006",
                            "population_2007", "population_2008", "population_2009", "population_2010", "population_2011", "population_2012", "population_2013", 
                            "population_2014", "population_2015", "population_2016", "population_2017", "population_2018", "population_2019", "population_2020", 
                            "population_2021", "population_2022", "births_2000", "births_2001", "births_2002", "births_2003", "births_2004", "births_2005", "births_2006", 
                            "births_2007", "births_2008", "births_2009", "births_2010", "births_2011", "births_2012", "births_2013", "births_2014", "births_2015", 
                            "births_2016", "births_2017", "births_2018", "births_2019", "births_2020", "births_2021", "births_2022", "deaths_2000", "deaths_2001",
                            "deaths_2002", "deaths_2003", "deaths_2004", "deaths_2005", "deaths_2006", "deaths_2007", "deaths_2008", "deaths_2009", "deaths_2010", 
                            "deaths_2011","deaths_2012", "deaths_2013", "deaths_2014", "deaths_2015", "deaths_2016", "deaths_2017", "deaths_2018", "deaths_2019",
                            "deaths_2020", "deaths_2021", "deaths_2022")))
  
  
  
  data <- data %>%
    pivot_longer(cols=c(one_of("population_2000", "population_2001", "population_2002", "population_2003", "population_2004", "population_2005", "population_2006",
                               "population_2007", "population_2008", "population_2009", "population_2010", "population_2011", "population_2012", "population_2013", 
                               "population_2014", "population_2015", "population_2016", "population_2017", "population_2018", "population_2019", "population_2020", 
                               "population_2021", "population_2022", "births_2000", "births_2001", "births_2002", "births_2003", "births_2004", "births_2005", "births_2006", 
                               "births_2007", "births_2008", "births_2009", "births_2010", "births_2011", "births_2012", "births_2013", "births_2014", "births_2015", 
                               "births_2016", "births_2017", "births_2018", "births_2019", "births_2020", "births_2021", "births_2022", "deaths_2000", "deaths_2001",
                               "deaths_2002", "deaths_2003", "deaths_2004", "deaths_2005", "deaths_2006", "deaths_2007", "deaths_2008", "deaths_2009", "deaths_2010", 
                               "deaths_2011","deaths_2012", "deaths_2013", "deaths_2014", "deaths_2015", "deaths_2016", "deaths_2017", "deaths_2018", "deaths_2019",
                               "deaths_2020", "deaths_2021", "deaths_2022")),
                 names_to = c("outcome", "year"),
                 names_sep = "_",
                 values_to = "value")
  
  data$year = as.character(data$year) 
  
  data= as.data.frame(data)
  
  list(filename, data) #what to return# 
})

################################################################################
                ###Put data into Census Manager###
################################################################################  

#County POPULATION Values
county_pop = lapply(data.list.county, `[[`, 2)

for (data in county_pop) {
  
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

