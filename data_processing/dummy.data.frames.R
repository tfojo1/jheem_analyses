
# Creating Data Frames with NAs -------------------------------------------

#Doing two iterations of this bc the values for cdc wonder vs census are different
#There's 2 put statements
#this creates NAs so that historic counties can be included in the surveillance manager



# CDC Wonder --------------------------------------------------------------

year_location
dummy.counties.one<- data.frame(
  outcome = c('population'),
  year = c('2018'),
  location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
  value = as.numeric(NA)
)
#year_location_age
dummy.counties.two <- data.frame(
  outcome = c('population'),
  year = c('2018'),
  location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
  value = as.numeric(NA),
  age = c('< 1 year')
)
#year_location_race
dummy.counties.three <- data.frame(
  outcome = c('population'),
  year = c('2018'),
  location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
  value = as.numeric(NA),
  race = c('American Indian or Alaska Native')
)
#year_location_ethnicity
dummy.counties.four  <- data.frame(
  outcome = c('population'),
  year = c('2018'),
  location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
  value = as.numeric(NA),
  ethnicity = c('Hispanic or Latino')
)

#year_location_race_ethnicity
dummy.counties.five  <- data.frame(
  outcome = c('population'),
  year = c('2018'),
  location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
  value = as.numeric(NA),
  race = c('American Indian or Alaska Native'),
  ethnicity = c('Hispanic or Latino')
)
#year_location_sex
dummy.counties.six <- data.frame(
  outcome = c('population'),
  year = c('2018'),
  location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
  value = as.numeric(NA),
  sex = c('male')
)
#year_location_age_race_ethnicity
dummy.counties.seven  <- data.frame(
  outcome = c('population'),
  year = c('2018'),
  location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
  value = as.numeric(NA),
  race = c('American Indian or Alaska Native'),
  ethnicity = c('Hispanic or Latino'),
  age = c('< 1 year')
)
#year_location_age_sex
dummy.counties.eight  <- data.frame(
  outcome = c('population'),
  year = c('2018'),
  location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
  value = as.numeric(NA),
  sex = c('male'),
  age = c('< 1 year')
)
#year_location_race_ethnicity_sex
dummy.counties.nine  <- data.frame(
  outcome = c('population'),
  year = c('2018'),
  location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
  value = as.numeric(NA),
  sex = c('male'),
  race = c('American Indian or Alaska Native'),
  ethnicity = c('Hispanic or Latino')
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
  ontology.name = 'census.cdc.wonder.population',
  source = 'cdc_wonder',
  dimension.values = list(),
  url ='https://wonder.cdc.gov/',
  details = 'CDC Wonder',
  allow.na.to.overwrite = T )
}


# Census ------------------------------------------------------------------


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