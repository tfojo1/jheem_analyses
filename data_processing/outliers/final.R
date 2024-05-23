
#phi: a percent change from one year to another (default value is 0.15)
#theta: a multiplier that produces a percent change based on how many years apart the two samples are (it has a maximum value) (default value is 0.05)

#True = it is an outlier
# False = the data point is fine as is

# Source outlier finder ---------------------------------------------------

#library(jheem2)
surveillance.manager = load.data.manager(name="surveillance.manager", file="../../cached/surveillance.manager.rdata")
source('data_processing/outlier_finder.R')

options(error=NULL)

# Outcome = diagnoses -----------------------------------------------------
  #TOTAL
diagnoses.adjusted<- run.outlier.process(outcome= 'diagnoses',
                                          stratifications= list(c()), 
                                          data.manager= surveillance.manager,
                                          phi = 0.5,
                                          theta = 0.2,
                                          locations= c(surveillance.manager$get.locations.with.data(outcome="diagnoses")))

diagnoses.adjusted$adjudication <- c('T', 'F', 'F', 'F', 'T', 'F', 'F', 'T')

#Getting Error here- emailed Andrew 5-23-24
run.outlier.process(outcome= 'diagnoses',
                    stratifications= list(c()),
                    data.manager= surveillance.manager,
                    locations= c(surveillance.manager$get.locations.with.data(outcome="diagnoses")),
                    adjudication.data.frame = diagnoses.adjusted)

  #STRATIFIED
#This returns 25
diagnoses.stratified <- run.outlier.process(outcome= 'diagnoses',
                                         stratifications= list('sex', 'age', 'race', 'risk', 
                                                               c('age', 'sex'), c('race', 'sex'), c('race', 'risk'), c('sex', 'risk'), c('age', 'race'), c('age', 'risk'), 
                                                               c('age', 'sex', 'risk'), c('age', 'race', 'risk'), c('age', 'race', 'sex'), c('race', 'sex', 'risk'),
                                                               c('age', 'race', 'sex', 'risk')), 
                                         data.manager= surveillance.manager,
                                         phi = 0.8,
                                         theta = 0.4,
                                         locations= c(surveillance.manager$get.locations.with.data(outcome="diagnoses")))


# Outcome = diagnosed.prevalence ------------------------------------------
  #TOTAL
  #These values of phi/theta seem unrealistic- but still return 71 obs
dx.prev.adjusted<- run.outlier.process(outcome= 'diagnosed.prevalence',
                                         stratifications= list(c()), 
                                         data.manager= surveillance.manager,
                                         phi = 0.8,
                                         theta = 0.7,
                                         locations= c(surveillance.manager$get.locations.with.data(outcome="diagnosed.prevalence")))


# Outcome = hiv.deaths ----------------------------------------------------

hiv.deaths.adjusted<- run.outlier.process(outcome= 'hiv.deaths',
                                         stratifications= list(c()), 
                                         data.manager= surveillance.manager,
                                         phi = 0.3,
                                         theta = 0.1,
                                         locations= c(surveillance.manager$get.locations.with.data(outcome="hiv.deaths")))

hiv.deaths.adjusted$adjudication <- c('F', 'F', 'T', 'F', 'F')

#Getting Error here- emailed Andrew 5-23-24
run.outlier.process(outcome= 'hiv.deaths',
                    stratifications= list(c()),
                    data.manager= surveillance.manager,
                    locations= c(surveillance.manager$get.locations.with.data(outcome="hiv.deaths")),
                    adjudication.data.frame = hiv.deaths.adjusted)

#Ask how to approach strata since sex is really high for hiv.deaths#


# Outcome = PrEP ----------------------------------------------------------

  #Total -> these values for theta/phi are high, total outliers = 25
prep.adjusted<- run.outlier.process(outcome= 'prep',
                                          stratifications= list(c()), 
                                          data.manager= surveillance.manager,
                                          phi = 0.9,
                                          theta = 0.5,
                                          locations= c(surveillance.manager$get.locations.with.data(outcome="prep")))


# Outcome = Suppression -------------------------------------------------------------
  #Total -> zero outliers!
suppression.adjusted <- run.outlier.process(outcome= 'suppression',
                                    stratifications= list(c()), 
                                    data.manager= surveillance.manager,
                                    #phi = 0.15,
                                    #theta = 0.05,
                                    locations= c(surveillance.manager$get.locations.with.data(outcome="suppression")))

  #Stratified -> zero outliers!
suppression.stratified <- run.outlier.process(outcome= 'suppression',
                                              stratifications= list('sex', 'age', 'race', 'risk', 
                                                                c('age', 'sex'), c('race', 'sex'), c('race', 'risk'), c('sex', 'risk'), c('age', 'race'), c('age', 'risk')), 
                                            data.manager= surveillance.manager,
                                            #phi = 0.15,
                                            #theta = 0.05,
                                            locations= c(surveillance.manager$get.locations.with.data(outcome="suppression")))

