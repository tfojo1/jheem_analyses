
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

# Outcome = retention.of.engaged -----------------------------------------------------
#Total -> zero outliers!
retention.of.engaged.adjusted <- run.outlier.process(outcome= 'retention.of.engaged',
                                           stratifications= list(c()), 
                                           data.manager= surveillance.manager,
                                           #phi = 0.15,
                                           #theta = 0.05,
                                           locations= c(surveillance.manager$get.locations.with.data(outcome="retention.of.engaged")))

# outcome = ps.syphilis ------------------------------------------------------
  #Total

ps.syphilis.adjusted <- run.outlier.process(outcome= 'ps.syphilis',
                                        stratifications= list(c()), 
                                        data.manager= surveillance.manager,
                                        phi = 0.5,
                                        theta = 0.05,
                                        max.year = 2019,
                                        locations= c(surveillance.manager$get.locations.with.data(outcome="ps.syphilis")))

ps.syphilis.adjusted$adjudication <- c(T)

run.outlier.process(outcome= 'ps.syphilis',
                    stratifications= list(c()),
                    data.manager= surveillance.manager,
                    phi = 0.5, 
                    theta = 0.05,
                    max.year = 2019,
                    locations= c(surveillance.manager$get.locations.with.data(outcome="ps.syphilis")),
                    adjudication.data.frame = ps.syphilis.adjusted)

# outcome = early.syphilis ------------------------------------------------------
#Total

early.syphilis.adjusted <- run.outlier.process(outcome= 'early.syphilis',
                                         stratifications= list(c()), 
                                         data.manager= surveillance.manager,
                                         phi = 0.8,
                                         theta = 0.05,
                                         max.year = 2019,
                                         locations= c(surveillance.manager$get.locations.with.data(outcome="early.syphilis")))
# outcome = early.syphilis ------------------------------------------------------
#Total

congenital.syphilis.adjusted <- run.outlier.process(outcome= 'congenital.syphilis',
                                               stratifications= list(c()), 
                                               data.manager= surveillance.manager,
                                               phi = 0.8,
                                               theta = 0.05,
                                               max.year = 2019,
                                               locations= c(surveillance.manager$get.locations.with.data(outcome="congenital.syphilis")))
# outcome = gonorrhea ------------------------------------------------------
  #Total- TBD, discuss in meeting

#why are all of these 2018?
gonorrhea.adjusted <- run.outlier.process(outcome= 'gonorrhea',
                                         stratifications= list(c()), 
                                         data.manager= surveillance.manager,
                                         phi = 0.7,
                                         theta = 0.05,
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

# outcome = hiv.test.positivity ------------------------------------------------------
  #Total -> zero outliers
cdc.hiv.test.positivity.adjusted <- run.outlier.process(outcome= 'cdc.hiv.test.positivity',
                                          stratifications= list(c()), 
                                          data.manager= surveillance.manager,
                                          #phi = 0.15,
                                          #theta = 0.05,
                                          #max.year = 2019,
                                          locations= c(surveillance.manager$get.locations.with.data(outcome="cdc.hiv.test.positivity")))


# outcome = aids.diagnoses ---------------------------------------------------
#Total- 
  #Each source here needs a different reference year (cdc.aids data is from 1981-2001; cdc.surveillance.reports are from 1993-2007)
  #  *The case definition changed in 1993*

#cdc.surveillance.reports
aids.diagnoses.source.two<- run.outlier.process(outcome= 'aids.diagnoses',
                                                stratifications= list(c()), 
                                                data.manager= surveillance.manager,
                                                phi = 0.8, 
                                                theta = 0.2,
                                                max.year = 2000, 
                                                locations= c(surveillance.manager$get.locations.with.data(outcome="aids.diagnoses")))
aids.diagnoses.source.two <- aids.diagnoses.source.two%>%
  filter(source == "cdc.surveillance.reports")%>%
  filter(year != "1993")#Is it ok to remove 1993?

aids.diagnoses.source.two$adjudication <- c(T, T, T, T, T, T, T, F, F, F, T, T, T, T, T, T, T)

run.outlier.process(outcome= 'aids.diagnoses',
                    stratifications= list(c()),
                    data.manager= surveillance.manager,
                    phi = 0.8, 
                    theta = 0.2,
                    max.year = 2000,
                    locations= c(surveillance.manager$get.locations.with.data(outcome="aids.diagnoses")),
                    adjudication.data.frame = aids.diagnoses.source.two)




#cdc.aids
aids.diagnoses.source.one <- run.outlier.process(outcome= 'aids.diagnoses',
                                                  stratifications= list(c()), 
                                                  data.manager= surveillance.manager,
                                                  phi = 0.6,
                                                  theta = 0.05,
                                                  max.year = 1996,
                                                  locations= c(surveillance.manager$get.locations.with.data(outcome="aids.diagnoses")))
aids.diagnoses.source.one <- aids.diagnoses.source.one%>%
  filter(source == "cdc.aids")%>%
  filter(year != "1993") #Is it ok to remove 1993?


aids.diagnoses.source.one$adjudication <- c(T, T, T, T, T, T, T, F, F, F, T, T, T, T, T, T, T)

run.outlier.process(outcome= 'aids.diagnoses',
                    stratifications= list(c()),
                    data.manager= surveillance.manager,
                    phi = 0.6, 
                    theta = 0.05,
                    max.year = 1996,
                    locations= c(surveillance.manager$get.locations.with.data(outcome="aids.diagnoses")),
                    adjudication.data.frame = aids.diagnoses.source.one)



# #Examine aids outliers
# issue = as.data.frame.table(surveillance.manager$data$aids.diagnoses$estimate$cdc.surveillance.reports$cdc.msa.reports$year__location)
# issue <- issue %>%
#   filter(location == "C.45060")