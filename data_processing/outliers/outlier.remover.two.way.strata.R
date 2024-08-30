#Look into the two way strata- assuming most problematic locations have been
#removed at this point via getting flagged the total or one way 
#But Todd said to check manually and just see if there is any blatant issue
#Look at larger cities

msas = locations::get.all.for.type("CBSA")
states = locations::get.all.for.type("state") 

# How to code strata: stratifications: something like list(character(0), 'sex', c('age', 'sex'), 'race') etc.

#Note: Total Prevalence isn't stratified

# Diagnoses ---------------------------------------------------------------

# Diagnosed Prevalence ----------------------------------------------------

#There are six two way strata (at least for cdc.hiv): 
#age.race
#age.risk
#age.sex

#sex.risk
#sex.race

#.race.risk

#26 outliers
dx.prev.one <- run.outlier.process(outcome= 'diagnosed.prevalence',
                                          stratifications= list(c('age', 'race')),
                                          data.manager= surveillance.manager,
                                          phi = 0.2,
                                          theta = 0.05,
                                          max.year = 2019,
                                          first.choice.year = 2018,
                                          locations= c(states, msas))
#49 outliers
dx.prev.two <- run.outlier.process(outcome= 'diagnosed.prevalence',
                                       stratifications= list(c('age', 'risk')),
                                       data.manager= surveillance.manager,
                                       phi = 0.2,
                                       theta = 0.05,
                                       max.year = 2019,
                                       first.choice.year = 2018,
                                       locations= c(states, msas))

#17 outliers
dx.prev.three <- run.outlier.process(outcome= 'diagnosed.prevalence',
                                   stratifications= list(c('age', 'sex')),
                                   data.manager= surveillance.manager,
                                   phi = 0.2,
                                   theta = 0.05,
                                   max.year = 2019,
                                   first.choice.year = 2018,
                                   locations= c(states, msas))

#No outliers here
dx.prev.four <- run.outlier.process(outcome= 'diagnosed.prevalence',
                                     stratifications= list(c('sex', 'risk')),
                                     data.manager= surveillance.manager,
                                     phi = 0.2,
                                     theta = 0.05,
                                     max.year = 2019,
                                     first.choice.year = 2018,
                                     locations= c(states, msas))
#6 outliers
dx.prev.five <- run.outlier.process(outcome= 'diagnosed.prevalence',
                                     stratifications= list(c('sex', 'race')),
                                     data.manager= surveillance.manager,
                                     phi = 0.2,
                                     theta = 0.05,
                                     max.year = 2019,
                                     first.choice.year = 2018,
                                     locations= c(states, msas))
#32 outliers
dx.prev.six <- run.outlier.process(outcome= 'diagnosed.prevalence',
                                     stratifications= list(c('race', 'risk')),
                                     data.manager= surveillance.manager,
                                     phi = 0.2,
                                     theta = 0.05,
                                     max.year = 2019,
                                     first.choice.year = 2018,
                                     locations= c(states, msas))




###############################################################################
#Use this to check
###############################################################################

underlying.data <- as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location__race__risk)

check <- underlying.data%>%
  filter(location == 'C.17140')%>%
  filter(risk == 'idu')
#Should i just remove things for the bigger outcomes? there are still viable outliers at this level but these numbers are higher


###############################################################################
#EXAMPLES FOR MEETING
###############################################################################

# EXAMPLE ONE -------------------------------------------------------------
 #2012, 2013, DC, 35-44, Multiracial
#How to determine which numbers here are outliers? Shouldn't the earlier numbers be the outliers?

dx.prev.one

example.one.data <- as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location__age__race)
example.one.data <- example.one.data%>%
  filter(location == 'DC')%>%
  filter(race == "Multiracial")%>%
  filter(age == "35-44 years")

