
# Creating Data Frames with NAs -------------------------------------------

#Doing two iterations of this bc the values for cdc wonder vs census are different
#There's 2 put statements
#this creates NAs so that historic counties can be included in the surveillance manager



# CDC Wonder -------------------------------------------------------------- 8-19-24 Removing this because we no longer have cdc wonder in the surveillance manager

# #year_location
# dummy.counties.one<- data.frame(
#   outcome = c('adult.population'),
#   year = c('2018'),
#   location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
#   value = as.numeric(NA)
# )
# #year_location_age
# dummy.counties.two <- data.frame(
#   outcome = c('adult.population'),
#   year = c('2018'),
#   location = c('12025', '51560', '51780', '51123', '29193'), 
#   value = as.numeric(NA),
#   age = c('20 years')
# )
# #year_location_race
# dummy.counties.three <- data.frame(
#   outcome = c('adult.population'),
#   year = c('2018'),
#   location = c('12025', '51560', '51780', '51123', '29193'), 
#   value = as.numeric(NA),
#   race = c('American Indian or Alaska Native')
# )
# #year_location_ethnicity
# dummy.counties.four  <- data.frame(
#   outcome = c('adult.population'),
#   year = c('2018'),
#   location = c('12025', '51560', '51780', '51123', '29193'), 
#   value = as.numeric(NA),
#   ethnicity = c('Hispanic or Latino')
# )
# 
# #year_location_race_ethnicity
# dummy.counties.five  <- data.frame(
#   outcome = c('adult.population'),
#   year = c('2018'),
#   location = c('12025', '51560', '51780', '51123', '29193'), 
#   value = as.numeric(NA),
#   race = c('American Indian or Alaska Native'),
#   ethnicity = c('Hispanic or Latino')
# )
# #year_location_sex
# dummy.counties.six <- data.frame(
#   outcome = c('adult.population'),
#   year = c('2018'),
#   location = c('12025', '51560', '51780', '51123', '29193'), 
#   value = as.numeric(NA),
#   sex = c('male')
# )
# #year_location_age_race_ethnicity
# dummy.counties.seven  <- data.frame(
#   outcome = c('adult.population'),
#   year = c('2018'),
#   location = c('12025', '51560', '51780', '51123', '29193'), 
#   value = as.numeric(NA),
#   race = c('American Indian or Alaska Native'),
#   ethnicity = c('Hispanic or Latino'),
#   age = c('20 years')
# )
# #year_location_age_sex
# dummy.counties.eight  <- data.frame(
#   outcome = c('adult.population'),
#   year = c('2018'),
#   location = c('12025', '51560', '51780', '51123', '29193'), 
#   value = as.numeric(NA),
#   sex = c('male'),
#   age = c('20 years')
# )
# #year_location_race_ethnicity_sex
# dummy.counties.nine  <- data.frame(
#   outcome = c('adult.population'),
#   year = c('2018'),
#   location = c('12025', '51560', '51780', '51123', '29193'), 
#   value = as.numeric(NA),
#   sex = c('male'),
#   race = c('American Indian or Alaska Native'),
#   ethnicity = c('Hispanic or Latino')
# )
# 
# #Put into list and put into data manager
# 
# dummy.data.all = list (dummy.counties.one,
#                        dummy.counties.two,
#                        dummy.counties.three,
#                        dummy.counties.four,
#                        dummy.counties.five,
#                        dummy.counties.six,
#                        dummy.counties.seven,
#                        dummy.counties.eight,
#                        dummy.counties.nine)

# for (data in dummy.data.all) {
# 
# surveillance.manager$put.long.form(
#   data = data,
#   ontology.name = 'census.cdc.wonder.population',
#   source = 'cdc_wonder',
#   dimension.values = list(),
#   url ='https://wonder.cdc.gov/',
#   details = 'CDC Wonder',
#   allow.na.to.overwrite = T )
# }


# Census ------------------------------------------------------------------
# 
# 
#   ##year_location
#   dummy.counties.one<- data.frame(
#     outcome = c('adult.population'),
#     year = c('2010'),
#     location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
#     value = as.numeric(NA)
#   )
#   #year_location_age
#   dummy.counties.two <- data.frame(
#     outcome = c('adult.population'),
#     year = c('2010'),
#     location = c('12025', '51560', '51780', '51123', '29193'), 
#     value = as.numeric(NA),
#     age = c('20 years')
#   )
#   #year_location_race
#   dummy.counties.three <- data.frame(
#     outcome = c('adult.population'),
#     year = c('2010'),
#     location = c('12025', '51560', '51780', '51123', '29193'), 
#     value = as.numeric(NA),
#     race = c('white')
#   )
#   #year_location_ethnicity
#   dummy.counties.four  <- data.frame(
#     outcome = c('adult.population'),
#     year = c('2010'),
#     location = c('12025', '51560', '51780', '51123', '29193'), 
#     value = as.numeric(NA),
#     ethnicity = c('not hispanic')
#   )
# 
#   #year_location_race_ethnicity
#   dummy.counties.five  <- data.frame(
#     outcome = c('adult.population'),
#     year = c('2010'),
#     location = c('12025', '51560', '51780', '51123', '29193'), 
#     value = as.numeric(NA),
#     race = c('white'),
#     ethnicity = c('not hispanic')
#   )
#   #year_location_sex
#   dummy.counties.six <- data.frame(
#     outcome = c('adult.population'),
#     year = c('2010'),
#     location = c('12025', '51560', '51780', '51123', '29193'), 
#     value = as.numeric(NA),
#     sex = c('male')
#   )
#   #year_location_age_race_ethnicity
#   dummy.counties.seven  <- data.frame(
#     outcome = c('adult.population'),
#     year = c('2010'),
#     location = c('12025', '51560', '51780', '51123', '29193'),
#     value = as.numeric(NA),
#     race = c('white'),
#     ethnicity = c('not hispanic'),
#     age = c('20 years')
#   )
#   #year_location_age_sex
#   dummy.counties.eight  <- data.frame(
#     outcome = c('adult.population'),
#     year = c('2010'),
#     location = c('12025', '51560', '51780', '51123', '29193'),
#     value = as.numeric(NA),
#     sex = c('male'),
#     age = c('20 years')
#   )
#   #year_location_race_ethnicity_sex
#   dummy.counties.nine  <- data.frame(
#     outcome = c('adult.population'),
#     year = c('2010'),
#     location = c('12025', '51560', '51780', '51123', '29193'), 
#     value = as.numeric(NA),
#     sex = c('male'),
#     race = c('white'),
#     ethnicity = c('not hispanic')
#   )
# 
#   #Put into list and put into data manager
# 
#   dummy.data.all = list (dummy.counties.one,
#                          dummy.counties.two,
#                          dummy.counties.three,
#                          dummy.counties.four,
#                          dummy.counties.five,
#                          dummy.counties.six,
#                          dummy.counties.seven,
#                          dummy.counties.eight,
#                          dummy.counties.nine)
# 
#   for (data in dummy.data.all) {
# 
#     surveillance.manager$put.long.form(
#       data = data,
#       ontology.name = 'census',
#       source = 'census.population',
#       dimension.values = list(),
#       url = 'www.census.gov',
#       details = 'Census Reporting',
#       allow.na.to.overwrite = T )
#   }

#############################################################################
############################################################################
# COMPLETE REDO -----------------------------------------------------------
#############################################################################
############################################################################


# census.pop ; census (4) -----------------------------------------------------
#This has single year ages

  ##year_location
  dummy.counties.one<- data.frame(
    outcome = c('adult.population'),
    year = c('2020'),
    location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
    value = as.numeric(NA)
  )
  #year_location_age
  dummy.counties.two <- data.frame(
    outcome = c('adult.population'),
    year = c('2020'),
    location = c('12025', '51560', '51780', '51123', '29193'),
    value = as.numeric(NA),
    age = c('20 years')
  )
  
  #year_location_sex
  dummy.counties.three <- data.frame(
    outcome = c('adult.population'),
    year = c('2020'),
    location = c('12025', '51560', '51780', '51123', '29193'),
    value = as.numeric(NA),
    sex = c('male')
  )
  
  #year_location_age_sex
  dummy.counties.four  <- data.frame(
    outcome = c('adult.population'),
    year = c('2020'),
    location = c('12025', '51560', '51780', '51123', '29193'),
    value = as.numeric(NA),
    sex = c('male'),
    age = c('20 years')
  )
  
  
  dummy.data.all = list (dummy.counties.one,
                         dummy.counties.two,
                         dummy.counties.three,
                         dummy.counties.four)
  
  for (data in dummy.data.all) {
    
    surveillance.manager$put.long.form(
      data = data,
      ontology.name = 'census',
      source = 'census.population',
      dimension.values = list(),
      url = 'www.census.gov',
      details = 'Census Reporting',
      allow.na.to.overwrite = T )
  }
  
  

# census.population ; census.grouped.age (9) ----------------------------------
#Uses age groups
  
  ##year_location
  dummy.counties.five<- data.frame(
    outcome = c('adult.population'),
    year = c('2019'),
    location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
    value = as.numeric(NA)
  )
  #year_location_age
  dummy.counties.six <- data.frame(
    outcome = c('adult.population'),
    year = c('2019'),
    location = c('12025', '51560', '51780', '51123', '29193'),
    value = as.numeric(NA),
    age = c('20-24 years')
  )
  
  #year_location_sex
  dummy.counties.seven <- data.frame(
    outcome = c('adult.population'),
    year = c('2019'),
    location = c('12025', '51560', '51780', '51123', '29193'),
    value = as.numeric(NA),
    sex = c('male')
  )
  
  #year_location_age_sex
  dummy.counties.eight  <- data.frame(
    outcome = c('adult.population'),
    year = c('2019'),
    location = c('12025', '51560', '51780', '51123', '29193'),
    value = as.numeric(NA),
    sex = c('male'),
    age = c('20-24 years')
  )
  
  #year_location_race
  dummy.counties.nine <- data.frame(
    outcome = c('adult.population'),
    year = c('2019'),
    location = c('12025', '51560', '51780', '51123', '29193'),
    value = as.numeric(NA),
    race = c('white')
  )
  #year_location_ethnicity
  dummy.counties.ten  <- data.frame(
    outcome = c('adult.population'),
    year = c('2019'),
    location = c('12025', '51560', '51780', '51123', '29193'),
    value = as.numeric(NA),
    ethnicity = c('not hispanic')
  )
  
  #year_location_race_ethnicity
  dummy.counties.eleven  <- data.frame(
    outcome = c('adult.population'),
    year = c('2019'),
    location = c('12025', '51560', '51780', '51123', '29193'),
    value = as.numeric(NA),
    race = c('white'),
    ethnicity = c('not hispanic')
  )
  #year_location_age_race_ethnicity
  dummy.counties.twelve  <- data.frame(
    outcome = c('adult.population'),
    year = c('2019'),
    location = c('12025', '51560', '51780', '51123', '29193'),
    value = as.numeric(NA),
    race = c('white'),
    ethnicity = c('not hispanic'),
    age = c('20-24 years')
  )
  #year_location_race_ethnicity_sex
  dummy.counties.thirteen  <- data.frame(
    outcome = c('adult.population'),
    year = c('2019'),
    location = c('12025', '51560', '51780', '51123', '29193'),
    value = as.numeric(NA),
    sex = c('male'),
    race = c('white'),
    ethnicity = c('not hispanic')
  )
  
  dummy.data.all.two = list (dummy.counties.five,
                         dummy.counties.six,
                         dummy.counties.seven,
                         dummy.counties.eight,
                         dummy.counties.nine,
                         dummy.counties.ten,
                         dummy.counties.eleven,
                         dummy.counties.twelve,
                         dummy.counties.thirteen)
  
  for (data in dummy.data.all.two) {
    
    surveillance.manager$put.long.form(
      data = data,
      ontology.name = 'census.grouped.age',
      source = 'census.population',
      dimension.values = list(),
      url = 'www.census.gov',
      details = 'Census Reporting',
      allow.na.to.overwrite = T )
  }
  
  

# census.aggregated.adult.population ; census (4) -----------------------------
  #Single year age

  ##year_location
  dummy.counties.fourteen<- data.frame(
    outcome = c('adult.population'),
    year = c('2020'),
    location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
    value = as.numeric(NA)
  )
  #year_location_age
  dummy.counties.fiften <- data.frame(
    outcome = c('adult.population'),
    year = c('2020'),
    location = c('12025', '51560', '51780', '51123', '29193'),
    value = as.numeric(NA),
    age = c('20 years')
  )
  
  #year_location_sex
  dummy.counties.sixteen <- data.frame(
    outcome = c('adult.population'),
    year = c('2020'),
    location = c('12025', '51560', '51780', '51123', '29193'),
    value = as.numeric(NA),
    sex = c('male')
  )
  
  #year_location_age_sex
  dummy.counties.seventeen  <- data.frame(
    outcome = c('adult.population'),
    year = c('2020'),
    location = c('12025', '51560', '51780', '51123', '29193'),
    value = as.numeric(NA),
    sex = c('male'),
    age = c('20 years')
  )
  
  
  dummy.data.all.three = list (dummy.counties.fourteen,
                         dummy.counties.fiften,
                         dummy.counties.sixteen,
                         dummy.counties.seventeen)
  
  for (data in dummy.data.all.three) {
    
    surveillance.manager$put.long.form(
      data = data,
      ontology.name = 'census',
      source = 'census.aggregated.adult.population',
      dimension.values = list(),
      url = 'www.census.gov',
      details = 'Census Reporting',
      allow.na.to.overwrite = T )
  }
  

# census aggregated adult population ; census.grouped.age ------------------

  #Uses age groups
  
  ##year_location
  dummy.counties.eighteen<- data.frame(
    outcome = c('adult.population'),
    year = c('2019'),
    location = c('12025', '51560', '51780', '51123', '29193'), #These are the counties that have changed#
    value = as.numeric(NA)
  )
  #year_location_age
  dummy.counties.nineteen <- data.frame(
    outcome = c('adult.population'),
    year = c('2019'),
    location = c('12025', '51560', '51780', '51123', '29193'),
    value = as.numeric(NA),
    age = c('20-24 years')
  )
  
  #year_location_sex
  dummy.counties.twenty <- data.frame(
    outcome = c('adult.population'),
    year = c('2019'),
    location = c('12025', '51560', '51780', '51123', '29193'),
    value = as.numeric(NA),
    sex = c('male')
  )
  
  #year_location_age_sex
  dummy.counties.twentyone  <- data.frame(
    outcome = c('adult.population'),
    year = c('2019'),
    location = c('12025', '51560', '51780', '51123', '29193'),
    value = as.numeric(NA),
    sex = c('male'),
    age = c('20-24 years')
  )
  
  #year_location_race
  dummy.counties.twentytwo <- data.frame(
    outcome = c('adult.population'),
    year = c('2019'),
    location = c('12025', '51560', '51780', '51123', '29193'),
    value = as.numeric(NA),
    race = c('white')
  )
  #year_location_ethnicity
  dummy.counties.twentythree  <- data.frame(
    outcome = c('adult.population'),
    year = c('2019'),
    location = c('12025', '51560', '51780', '51123', '29193'),
    value = as.numeric(NA),
    ethnicity = c('not hispanic')
  )
  
  #year_location_race_ethnicity
  dummy.counties.twentyfour  <- data.frame(
    outcome = c('adult.population'),
    year = c('2019'),
    location = c('12025', '51560', '51780', '51123', '29193'),
    value = as.numeric(NA),
    race = c('white'),
    ethnicity = c('not hispanic')
  )
  #year_location_age_race_ethnicity
  dummy.counties.twentyfive  <- data.frame(
    outcome = c('adult.population'),
    year = c('2019'),
    location = c('12025', '51560', '51780', '51123', '29193'),
    value = as.numeric(NA),
    race = c('white'),
    ethnicity = c('not hispanic'),
    age = c('20-24 years')
  )
  #year_location_race_ethnicity_sex
  dummy.counties.twentysix  <- data.frame(
    outcome = c('adult.population'),
    year = c('2019'),
    location = c('12025', '51560', '51780', '51123', '29193'),
    value = as.numeric(NA),
    sex = c('male'),
    race = c('white'),
    ethnicity = c('not hispanic')
  )
  
  dummy.data.all.four = list (dummy.counties.eighteen,
                             dummy.counties.nineteen,
                             dummy.counties.twenty,
                             dummy.counties.twentyone,
                             dummy.counties.twentytwo,
                             dummy.counties.twentythree,
                             dummy.counties.twentyfour,
                             dummy.counties.twentyfive,
                             dummy.counties.twentysix)
  
  for (data in dummy.data.all.four) {
    
    surveillance.manager$put.long.form(
      data = data,
      ontology.name = 'census.grouped.age',
      source = 'census.aggregated.adult.population',
      dimension.values = list(),
      url = 'www.census.gov',
      details = 'Census Reporting',
      allow.na.to.overwrite = T )
  }
