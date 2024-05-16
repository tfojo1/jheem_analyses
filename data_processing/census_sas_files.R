#This code is for the single year age data that Todd has for 2005-2017

#library(haven)
################################################################################
              ###Read in SAS Census Files for 2005-2017###
################################################################################

DATA.DIR.CENSUS.SAS="../../data_raw/census_sas"

cdc_sas_files <- list.files(DATA.DIR.CENSUS.SAS, pattern = ".sas7bdat", full.names = "TRUE")

census_sas_data_list <- lapply(cdc_sas_files, function(x) {
  list(filename=x, data=read_sas(x))
})
################################################################################
                            ###Create Mappings###
################################################################################

hisp.sas.mappings = c('1' = 'not hispanic',
                  '2' = 'hispanic')

race.sex.mappings = c('1' = 'white male',
                      '2' = 'white female',
                      '3' = 'black male',
                      '4' = 'black female',
                      '5' = 'american indian or alaska native male',
                      '6' = 'american indian or alaska native female',
                      '7' = 'asian or pacific islander male',
                      '8' = 'asian or pacific islander female') 

################################################################################
                ###Clean SAS Census Files for 2005-2017###
################################################################################

data.list.census.sas.clean = lapply(census_sas_data_list, function(file){

   data=file[["data"]] #apply the function to the data element#
   filename = file[["filename"]] #apply the function to the filename element#
  
   data$ethnicity= hisp.sas.mappings[data$hisp]
   data$race_sex= race.sex.mappings[data$RACESEX]
   str1= "years"
   
   data$state_code= str_pad(data$ST_FIPS, width=2, side="left", pad="0")
   data$county_code= str_pad(data$CO_FIPS, width=3, side="left", pad="0")
   data$location = paste(data$state_code, data$county_code, sep="")
   
   data <- data %>%
   mutate(sex= ifelse(grepl("female", race_sex), "female", "male")) %>%
     mutate(race= ifelse(grepl("white", race_sex), "white",
                         ifelse(grepl("black", race_sex), "black",
                           ifelse(grepl("american indian or alaska native", race_sex), "american indian or alaska native", "asian or pacific islander"))))
   
   if(grepl("2006", filename)) {
     data$AGE = paste(data$AGE, str1, sep= " ")
     data$age = if_else(data$AGE == "0 years", "< 1 year", data$AGE)
     data$age = if_else(data$age == "1 years", "1 year", data$age)
     data$age = if_else(data$age == "85 years", "85+ years", data$age)
     data$population_2005 = data$POP2005
     data$population_2006 = data$POP2006
     data$population_2007 = data$POP2007
     data$population_2008 = data$POP2008
     data$population_2009 = data$POP2009
   }
   
   if(grepl("2010", filename)) {
   data$age = paste(data$age, str1, sep= " ")
   data$age = if_else(data$age == "0 years", "< 1 year", data$age)
   data$age = if_else(data$age == "1 years", "1 year", data$age)
   data$age = if_else(data$age == "85 years", "85+ years", data$age)
   data$population_2010 = data$POP2010_apr
   data$population_2011 = data$POP2011
   data$population_2012 = data$POP2012
   data$population_2013 = data$POP2013
   data$population_2014 = data$POP2014
   data$population_2015 = data$POP2015
   data$population_2016 = data$POP2016
   data$population_2017 = data$POP2017
    }
   data <- data %>%
     select(location, age, race, ethnicity, sex, (one_of("population_2005", "population_2006", "population_2007", "population_2008", "population_2009", "population_2010", "population_2011", 
                                                         "population_2012", "population_2013", "population_2014", "population_2015",
                                                         "population_2016", "population_2017")))
   data <- data %>%
     pivot_longer(cols=c(one_of("population_2005", "population_2006", "population_2007", "population_2008", "population_2009", "population_2010", "population_2011", 
                                "population_2012", "population_2013", "population_2014", "population_2015",
                                "population_2016", "population_2017")),
                  names_to = c("outcome", "year"),
                  names_sep = "_",
                  values_to = "value")
   
 data = as.data.frame(data)
  list(filename, data)

})

################################################################################
                  ###Put into Census Manager###
################################################################################
#This is taking a really long time to run#

census_sas_pops = lapply(data.list.census.sas.clean, `[[`, 2)

for (data in census_sas_pops) {
  
  census.manager$put.long.form(
    data = data,
    ontology.name = 'census',
    source = 'census.population',
    dimension.values = list(),
    url = 'www.census.gov',
    details = 'Census Reporting')
}



# Creating Dummy Counties -------------------------------------------------

#We noticed on 5-15-24 there was an issue with missing counties in dimnames
#This is because there are counties that were historically parts of states
#And then were removed as counties at some point.  We noticed this for FL, MO, and VA.
#we decided to put them here as NA so then we won't run into the dimnames
#related error

# need 9 data frames -------------------------------------------------------------------

  #year_location
  dummy.counties.one<- data.frame(
    outcome = c('population'),
    year = c('2010'),
    location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
    value = as.numeric(NA)
  )
  #year_location_age
  dummy.counties.two <- data.frame(
    outcome = c('population'),
    year = c('2010'),
    location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
    value = as.numeric(NA),
    age = c('< 1 year')
  )
  #year_location_race
  dummy.counties.three <- data.frame(
    outcome = c('population'),
    year = c('2010'),
    location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
    value = as.numeric(NA),
    race = c('white')
  )
  #year_location_ethnicity
  dummy.counties.four  <- data.frame(
    outcome = c('population'),
    year = c('2010'),
    location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
    value = as.numeric(NA),
    ethnicity = c('not hispanic')
  )
  
  #year_location_race_ethnicity
  dummy.counties.five  <- data.frame(
    outcome = c('population'),
    year = c('2010'),
    location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
    value = as.numeric(NA),
    race = c('white'),
    ethnicity = c('not hispanic')
  )
  #year_location_sex
  dummy.counties.six <- data.frame(
    outcome = c('population'),
    year = c('2010'),
    location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
    value = as.numeric(NA),
    sex = c('male')
  )
  #year_location_age_race_ethnicity
  dummy.counties.seven  <- data.frame(
    outcome = c('population'),
    year = c('2010'),
    location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
    value = as.numeric(NA),
    race = c('white'),
    ethnicity = c('not hispanic'),
    age = c('< 1 year')
  )
  #year_location_age_sex
  dummy.counties.eight  <- data.frame(
    outcome = c('population'),
    year = c('2010'),
    location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
    value = as.numeric(NA),
    sex = c('male'),
    age = c('< 1 year')
  )
  #year_location_race_ethnicity_sex
  dummy.counties.nine  <- data.frame(
    outcome = c('population'),
    year = c('2010'),
    location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
    value = as.numeric(NA),
    sex = c('male'),
    race = c('white'),
    ethnicity = c('not hispanic')
  )
  
  #Put into list and put into data manager
  
  dummy.data.all = list (dummy.counties.one, 
                         dummy.counties.two,
                         dummy.counties.three,
                         dummy.counties.four,
                         dummy.counties.five,
                         dummy.counties.six,
                         dummy.counties.seven,
                         dummy.counties.eight,
                         dummy.counties.nine)
  
  for (data in dummy.data.all) {
    
    census.manager$put.long.form(
      data = data,
      ontology.name = 'census',
      source = 'census.population',
      dimension.values = list(),
      url = 'www.census.gov',
      details = 'Census Reporting',
      allow.na.to.overwrite = T )
  }
  