
#phi: a percent change from one year to another (default value is 0.15)
#theta: a multiplier that produces a percent change based on how many years apart the two samples are (it has a maximum value) (default value is 0.05)

#True = it is an outlier
# False = the data point is fine as is

#Updated 5-24-24 to have min.year and max.year in the find outlier function- it's less/greater than or equal to

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
                                          phi = 0.32, 
                                          theta = 0.06,
                                          max.year = 2019,
                                          locations= c(surveillance.manager$get.locations.with.data(outcome="diagnoses")))

diagnoses.adjusted$adjudication <- c(T)

#Getting Error here- emailed Andrew 5-23-24
run.outlier.process(outcome= 'diagnoses',
                    stratifications= list(c()),
                    data.manager= surveillance.manager,
                    phi = 0.32, 
                    theta = 0.06,
                    max.year = 2019,
                    locations= c(surveillance.manager$get.locations.with.data(outcome="diagnoses")),
                    adjudication.data.frame = diagnoses.adjusted)

# outcome = total.prevalence ----------------------------------------------
  #Total -> zero outliers!
total.prev.adjusted<- run.outlier.process(outcome= 'total.prevalence',
                                       stratifications= list(c()), 
                                       data.manager= surveillance.manager,
                                       #phi = 0.15,
                                       #theta = 0.05,
                                       #max.year = 2019,
                                       locations= c(surveillance.manager$get.locations.with.data(outcome="total.prevalence")))

# Outcome = diagnosed.prevalence ------------------------------------------
  #TOTAL- TBD on this one, need to review in meeting (below)
dx.prev.adjusted<- run.outlier.process(outcome= 'diagnosed.prevalence',
                                         stratifications= list(c()), 
                                         data.manager= surveillance.manager,
                                         phi = 0.8,
                                         theta = 0.1,
                                         max.year = 2019,
                                         locations= c(surveillance.manager$get.locations.with.data(outcome="diagnosed.prevalence")))

#Check this in meeting- the outlier.finder is identifying location = 09007 with outliers for 2008, 2009.  But i think the outliers should be
#2010, 2011, 2012:
issue = as.data.frame.table(surveillance.manager$data$diagnosed.prevalence$estimate$cdc.hiv$cdc$year__location)
issue <- issue %>%
  filter(location == "09007")


# Outcome = hiv.deaths ----------------------------------------------------
  #Total -> zero outliers!
hiv.deaths.adjusted<- run.outlier.process(outcome= 'hiv.deaths',
                                         stratifications= list(c()), 
                                         data.manager= surveillance.manager,
                                         phi = 0.2,
                                         theta = 0.05,
                                         max.year = 2019,
                                         locations= c(surveillance.manager$get.locations.with.data(outcome="hiv.deaths")))

# Outcome = PrEP ----------------------------------------------------------

  #Total -> TBD, need to discuss in meeting
prep.adjusted<- run.outlier.process(outcome= 'prep',
                                          stratifications= list(c()), 
                                          data.manager= surveillance.manager,
                                          phi = 0.6,
                                          theta = 0.05,
                                          max.year = 2019,
                                          locations= c(surveillance.manager$get.locations.with.data(outcome="prep")))


#Example for meeting: how low should prep numbers start? are those outliers?
prep.adjusted.more <- run.outlier.process(outcome= 'prep',
                                          stratifications= list(c()),
                                          data.manager= surveillance.manager,
                                          phi = 0.5,
                                          theta = 0.05,
                                          max.year = 2019,
                                          locations= c(surveillance.manager$get.locations.with.data(outcome="prep")))
issue = as.data.frame.table(surveillance.manager$data$prep$estimate$aidsvu$aidsvu$year__location)
issue <- issue %>%
  filter(location == "01125")



# Outcome = Suppression -------------------------------------------------------------
  #Total -> zero outliers!
suppression.adjusted <- run.outlier.process(outcome= 'suppression',
                                    stratifications= list(c()), 
                                    data.manager= surveillance.manager,
                                    #phi = 0.15,
                                    #theta = 0.05,
                                    locations= c(surveillance.manager$get.locations.with.data(outcome="suppression")))

# Outcome = awareness -----------------------------------------------------
  #Total -> zero outliers!
awareness.adjusted <- run.outlier.process(outcome= 'awareness',
                                            stratifications= list(c()), 
                                            data.manager= surveillance.manager,
                                            #phi = 0.15,
                                            #theta = 0.05,
                                            locations= c(surveillance.manager$get.locations.with.data(outcome="awareness")))

# Outcome = linkeage_1mo -----------------------------------------------------
  #Total -> zero outliers!
linkeage_1mo.adjusted <- run.outlier.process(outcome= 'linkeage_1mo',
                                          stratifications= list(c()), 
                                          data.manager= surveillance.manager,
                                          #phi = 0.15,
                                          #theta = 0.05,
                                          locations= c(surveillance.manager$get.locations.with.data(outcome="linkeage_1mo")))
# Outcome = linkeage_3mo -----------------------------------------------------
  #Total -> zero outliers!
linkeage_3mo.adjusted <- run.outlier.process(outcome= 'linkeage_3mo',
                                             stratifications= list(c()), 
                                             data.manager= surveillance.manager,
                                             #phi = 0.15,
                                             #theta = 0.05,
                                             locations= c(surveillance.manager$get.locations.with.data(outcome="linkeage_3mo")))
# Outcome = engagement -----------------------------------------------------
  #Total -> zero outliers!
engagement.adjusted <- run.outlier.process(outcome= 'engagement',
                                             stratifications= list(c()), 
                                             data.manager= surveillance.manager,
                                             #phi = 0.15,
                                             #theta = 0.05,
                                             locations= c(surveillance.manager$get.locations.with.data(outcome="engagement")))

# Outcome = cocaine -----------------------------------------------------
  #Ask during meeting- what to do with year ranges?
cocaine.adjusted <- run.outlier.process(outcome= 'cocaine',
                                           stratifications= list(c()), 
                                           data.manager= surveillance.manager,
                                           #phi = 0.15,
                                           #theta = 0.05,
                                           locations= c(surveillance.manager$get.locations.with.data(outcome="cocaine")))

# outcome = syphilis ------------------------------------------------------
  #Total -> zero outliers!
syphilis.adjusted <- run.outlier.process(outcome= 'syphilis',
                                        stratifications= list(c()), 
                                        data.manager= surveillance.manager,
                                        #phi = 0.15,
                                        #theta = 0.05,
                                        locations= c(surveillance.manager$get.locations.with.data(outcome="syphilis")))
# outcome = gonorrhea ------------------------------------------------------
  #Total- TBD, discuss in meeting
gonorrhea.adjusted <- run.outlier.process(outcome= 'gonorrhea',
                                         stratifications= list(c()), 
                                         data.manager= surveillance.manager,
                                         #phi = 0.15,
                                         #theta = 0.05,
                                         max.year = 2019,
                                         locations= c(surveillance.manager$get.locations.with.data(outcome="gonorrhea")))
# outcome = retention ------------------------------------------------------
  #Total -> zero outliers!
retention.adjusted <- run.outlier.process(outcome= 'retention',
                                          stratifications= list(c()), 
                                          data.manager= surveillance.manager,
                                          #phi = 0.15,
                                          #theta = 0.05,
                                          #max.year = 2019,
                                          locations= c(surveillance.manager$get.locations.with.data(outcome="retention")))
# outcome = hiv.tests ------------------------------------------------------
  #Total- going to discuss in meeting
hiv.tests.adjusted <- run.outlier.process(outcome= 'hiv.tests',
                                          stratifications= list(c()), 
                                          data.manager= surveillance.manager,
                                          #phi = 0.15,
                                          #theta = 0.05,
                                          max.year = 2019,
                                          locations= c(surveillance.manager$get.locations.with.data(outcome="hiv.tests")))
# outcome = hiv.test.positivity ------------------------------------------------------
  #Total -> zero outliers
cdc.hiv.test.positivity.adjusted <- run.outlier.process(outcome= 'cdc.hiv.test.positivity',
                                          stratifications= list(c()), 
                                          data.manager= surveillance.manager,
                                          #phi = 0.15,
                                          #theta = 0.05,
                                          #max.year = 2019,
                                          locations= c(surveillance.manager$get.locations.with.data(outcome="cdc.hiv.test.positivity")))


# outcome = adult.immigration ---------------------------------------------------
  #Total- this doesn't work bc this outcome is in year ranges
adult.immigration.adjusted <- run.outlier.process(outcome= 'adult.immigration',
                                                        stratifications= list(c()), 
                                                        data.manager= surveillance.manager,
                                                        #phi = 0.15,
                                                        #theta = 0.05,
                                                        #max.year = 2019,
                                                        locations= c(surveillance.manager$get.locations.with.data(outcome="adult.immigration")))

# outcome = aids.diagnoses ---------------------------------------------------
#Total- this doesn't work bc this outcome is in year ranges
aids.diagnoses.adjusted <- run.outlier.process(outcome= 'aids.diagnoses',
                                                  stratifications= list(c()), 
                                                  data.manager= surveillance.manager,
                                                  #phi = 0.15,
                                                  #theta = 0.05,
                                                  max.year = 1997,
                                                  locations= c(surveillance.manager$get.locations.with.data(outcome="aids.diagnoses")))



