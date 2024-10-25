library(jheem2)
library(tidyverse)
library(readr)
library(readxl) 
library(haven)
library(locations)

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
    description = "Populaion Estimate"))

census.manager$register.outcome(
  'births',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Births',
    axis.name = 'Births',
    units = 'births',
    description = "Births"))

census.manager$register.outcome(
  'births.denominator',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Births Denominator',
    axis.name = 'Births Denominator',
    units = 'births',
    description = "Births Denominator"))

census.manager$register.outcome(
  'deaths',
  metadata = create.outcome.metadata(  #This represent deaths from the census
    scale = 'non.negative.number',
    display.name = 'Deaths',
    axis.name = 'Deaths',
    units = 'deaths',
    description = "Deaths"))

census.manager$register.outcome(
  'metro.deaths',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Metro Deaths',
    axis.name = 'Metro Deaths',
    units = 'deaths',
    description = "Metro Deaths"))

census.manager$register.outcome(
  'metro.deaths.denominator',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Metro Deaths Denominator',
    axis.name = 'Metro Deaths Denominator',
    units = 'deaths',
    description = "Metro Deaths Denominator"))

#I'm not sure if we need this- this is the denominator value for the national level deaths from CDC Wonder (corresponds to outcoem = deaths, source= cdc wonder)
census.manager$register.outcome(
  'deaths.denominator',
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Deaths Denominator',
    axis.name = 'Deaths Denominator',
    units = 'deaths',
    description = "Deaths Denominator"))

census.manager$register.outcome(
  'female.population', 
  metadata = create.outcome.metadata(
    scale = 'non.negative.number',
    display.name = 'Female Population',
    axis.name = 'Female Population',
    units = 'cases',
    description = "Female Population Age 15-44"))

census.manager$register.outcome(
  'fertility.rate', 
  metadata = create.outcome.metadata(
    scale = 'rate',
    display.name = 'Fertility Rate',
    axis.name = 'Fertility Rate',
    units = '%',
    description = "Fertility Rate"), denominator.outcome = 'female.population') 

#Register "Parent" Sources
census.manager$register.parent.source('census', full.name = 'United States Census Bureau', short.name= "census")
census.manager$register.parent.source('NCHS', full.name = 'National Center for Health Statistics', short.name= "NCHS")
census.manager$register.parent.source('NVSS', full.name = 'National Vital Statistics System', short.name= "NVSS")


#Register Data Sources ('children')
census.manager$register.source('census.population', parent.source= "census", full.name = "Census Population Data", short.name='census.population')
census.manager$register.source('census.deaths', parent.source= "NCHS", full.name = "Census Death Data", short.name='census.deaths')
census.manager$register.source('cdc_wonder', parent.source= "NCHS", full.name = "CDC Wonder", short.name='cdc_wonder')
census.manager$register.source('cdc.wonder.natality', parent.source= "NVSS", full.name = "CDC Wonder Natality Data", short.name='cdc.wonder.natality')


census.manager$register.ontology(
  'census.cdc.wonder.population',
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
 
census.manager$register.ontology(
  'census.cdc.wonder.births.deaths',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c("< 1 year", "1-4 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years",
          "45-49 years", "5-9 years", "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", 
          "85-89 years", "90-94 years", "95-99 years", "100+ years"),
    race=c('American Indian or Alaska Native', 'Asian or Pacific Islander', 'Black or African American', 'White', "More than one race", 'Not Reported', "Unknown or Not Stated", "Not Available"),
    ethnicity=c('Hispanic or Latino', 'Not Hispanic or Latino', 'Unknown or Not Stated', "Not Stated"),
    sex=c('male','female')
  ))

#Creating a metro deaths ontology because metro deaths use year ranges
census.manager$register.ontology(
  'metro.deaths',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c("< 1 year", "1-4 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years",
          "45-49 years", "5-9 years", "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", 
          "85-89 years", "90-94 years", "95-99 years", "100+ years"),
    race=c('American Indian or Alaska Native', 'Asian or Pacific Islander', 'Black or African American', 'White', "More than one race", 'Not Reported', "Unknown or Not Stated", "Not Available"),
    ethnicity=c('Hispanic or Latino', 'Not Hispanic or Latino', 'Unknown or Not Stated', "Not Stated"),
    sex=c('male','female')
  ))

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

census.manager$register.ontology(
  'stratified.census',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('0-4 years', '5-9 years', '10-14 years', '15-19 years', '20-24 years', '25-29 years', '30-34 years', '35-39 years',
          '40-44 years', '45-49 years', '50-54 years', '55-59 years', '60-64 years', '65-69 years', '70-74 years', 
          '75-79 years', '80-84 years', '85+ years'),
    race=c('White', 'Black', 'Asian', 'American Indian and Alaska Native', 'Native Hawaiian and Other Pacific Islander'),
    ethnicity=c('Hispanic', 'Not Hispanic'),
    sex=c('male','female')
  ))

census.manager$register.ontology(
  'cdc.fertility',
  ont = ontology(
    year= NULL,
    location= NULL,
    age=c('15-19 years', '20-24 years', '25-29 years', '30-34 years','35-39 years', '40-44 years'),
    race=c('American Indian or Alaska Native', 'Asian or Pacific Islander', 'Black or African American',   'White'),
    ethnicity = c('Hispanic or Latino', 'Not Hispanic or Latino')
  ))


################################################################################
            ###Sourcing other files here###
################################################################################ 

#This pulls population from 1970-1999#
source('data_processing/census.manager/census_1970_1999.R')

#This pulls single year age groups and demographic data from 2018-2020#
source('data_processing/census.manager/census_cdc_wonder.R')

#This pulls single year age groups and demographic data from 2005-2017#
source('data_processing/census.manager/census_sas_files.R')

#This pulls birth and death data from CDC Wonder#
source('data_processing/census.manager/births_and_deaths.R')

#This pulls the stratified census data by county for 2020-2022
#source('data_processing/census.manager/stratified_census.R')

#Sourcing more recent census population data that is stratified but not
#by single year age just by age group which is why we added it so much later
source('data_processing/census.manager/census.population.10.19.R')

#UPDATE 7-16: Temporarily commenting out 181 to source the newer stratified data
#I'll use this to decide if we want age groups or single year
source('data_processing/census.manager/census.population.20.23.R')

#Fertility rates and female.population
source('data_processing/census.manager/fertility.rate.R')

#National Level 'metro' deaths
source('data_processing/census.manager/mortality.cdc.wonder.R')

#National Population data by age, sex, Age_sex (for SHIELD)
source('data_processing/census.manager/national.population.data.R')

################################################################################
###Read in Census Files###
################################################################################

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
  data$"population_2023" = data$POPESTIMATE2023


  #data$"deaths_2010" = data$DEATHS2010 #removing deaths for 2010 because it doesn't represent a full year of data
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
  #data$"deaths_2020" = data$DEATHS2020 #removing deaths for 2020 because it doesn't represent a full year of data
  data$"deaths_2021" = data$DEATHS2021
  data$"deaths_2022" = data$DEATHS2022
  data$"deaths_2023" = data$DEATHS2023
  

  ##this will give warning that it doesn't see these vars across all dfs##
  
  data<- data %>%
    select(location,(one_of("population_2000", "population_2001", "population_2002", "population_2003", "population_2004", "population_2005", "population_2006",
                            "population_2007", "population_2008", "population_2009", "population_2010", "population_2011", "population_2012", "population_2013", 
                            "population_2014", "population_2015", "population_2016", "population_2017", "population_2018", "population_2019", "population_2020", 
                            "population_2021", "population_2022", "population_2023", "deaths_2011","deaths_2012", "deaths_2013", "deaths_2014", 
                            "deaths_2015", "deaths_2016", "deaths_2017", "deaths_2018", "deaths_2019",
                             "deaths_2021", "deaths_2022", "deaths_2023")))
  
  
  
  data <- data %>%
    pivot_longer(cols=c(one_of("population_2000", "population_2001", "population_2002", "population_2003", "population_2004", "population_2005", "population_2006",
                               "population_2007", "population_2008", "population_2009", "population_2010", "population_2011", "population_2012", "population_2013", 
                               "population_2014", "population_2015", "population_2016", "population_2017", "population_2018", "population_2019", "population_2020", 
                               "population_2021", "population_2022", "population_2023", "deaths_2011","deaths_2012", "deaths_2013", "deaths_2014", 
                               "deaths_2015", "deaths_2016", "deaths_2017", "deaths_2018", "deaths_2019",
                               "deaths_2021", "deaths_2022", "deaths_2023")),
                 names_to = c("outcome", "year"),
                 names_sep = "_",
                 values_to = "value")
  
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

data.list.county.deaths.00.23 = lapply(data.list.county, function(file){
  
  data=file[[2]]
  filename = file[[1]]
  
  data= subset(data, data$outcome == "deaths")
  
  data= as.data.frame(data)
  
  list(filename, data) #what to return# 
})

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
us.total.deaths = lapply(data.list.county.deaths.00.23, function(file){
  
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
    details = 'Census Reporting')
}


#County DEATH Values
county_deaths = lapply(data.list.county.deaths.00.23, `[[`, 2)

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
us.total.deaths.put = lapply(us.total.deaths, `[[`, 2)

for (data in us.total.deaths.put) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census.population',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}


# Aggregating County Population to National -------------------------------
source('data_processing/census.manager/aggregating.national.population.census.R')

################################################################################
                  ###Save Census Manager###
################################################################################ 

save(census.manager, file="../../cached/census.manager.rdata")

#Also save to Q drive
save(census.manager, file="Q:/data_managers/census.manager.rdata")
