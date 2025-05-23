
#phi: a percent change from one year to another (default value is 0.15)
#theta: a multiplier that produces a percent change based on how many years apart the two samples are (it has a maximum value) (default value is 0.05)

#True = it is an outlier
# False = the data point is fine as is

#Updated 5-24-24 to have min.year and max.year in the find outlier function- it's less/greater than or equal to

# Source outlier finder ---------------------------------------------------

#library(jheem2)
#surveillance.manager = load.data.manager(name="surveillance.manager", file="../../cached/surveillance.manager.rdata")
source('data_processing/outliers/outlier_finder.R')

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


# Update for 12-10-24 -----------------------------------------------------
#Remove diagnosed.prevalence data for aggregated.county
#There are dips in 2010-2011 data for some MSAs
#The rule to remove these points is: If there is a 2% of greater decrease in any of the yers 2009-2013, then remove 2010, 2011, 2012 AND 2013
#Decided to do this prior to removing other outliers for this outcome

x = surveillance.manager$data$diagnosed.prevalence$estimate$cdc.aggregated.county$cdc$year__location
delta = (x[as.character(2010:2013), ] - x[as.character(2009:2012), ])/(x[as.character(2009:2012), ]) #identify % change
delta< (-0.02) #want to know which is true
to.remove.mask = apply(delta < (-0.02), MARGIN = 'location', FUN= any, na.rm=T) #if any of those years has a less than 2% decrease from the previous year, for each col is any of these values true.  #could replace margin = 2 with margin = 'location'
to.remove.codes = names(to.remove.mask)[to.remove.mask] #gives you names - for these you remove all the 4 years

year <- rep(c("2010", "2011", "2012", "2013"), times=52)
location <- rep(c("C.12060", "C.12580", "C.14460", "C.16740", "C.16980", "C.19820", "C.32820", "C.35380", "C.35620", "C.37980", "C.41180", "C.41740", "C.42660"), each=4)
dx.prev.agg.county.remove <- data.frame(year, location)
dx.prev.agg.county.remove$source = "cdc.aggregated.county"
dx.prev.agg.county.remove$ontology = "cdc"
dx.prev.agg.county.remove$adjudication = 'TRUE' #Create df here and then combine with other outliers below


# Outcome = diagnosed.prevalence, source = msa.surveillance.reports ------------------------------------------
  #TOTAL- want smaller degree of variability here bc prevalence should be more consistent
dx.prev.adjusted.one<- run.outlier.process(outcome= 'diagnosed.prevalence',
                                         stratifications= list(c()), 
                                         data.manager= surveillance.manager,
                                         phi = 0.2,
                                         theta = 0.05,
                                         max.year = 2019,
                                         first.choice.year = 2018,
                                         locations= c(surveillance.manager$get.locations.with.data(outcome="diagnosed.prevalence")))%>%
  filter(source == "cdc.surveillance.reports")

#Need to manually remove 2018 for C.3340 and C.47900 because their 2018 value is incorrect
dx.prev.adjusted.one$adjudication <- c(T, T, T, T, T, F, F, T, T, F, T, T, T, T, F)
dx.prev.adjusted.one <- dx.prev.adjusted.one %>%
  add_row(year = "2018", location = 'C.33340', source ='cdc.surveillance.reports', ontology = 'cdc.msa.reports', adjudication = TRUE)%>%
  add_row(year = "2016", location = 'C.47900', source ='cdc.surveillance.reports', ontology = 'cdc.msa.reports', adjudication = TRUE)%>%
  add_row(year = "2015", location = 'C.47900', source ='cdc.surveillance.reports', ontology = 'cdc.msa.reports', adjudication = TRUE)%>%
  add_row(year = "2014", location = 'C.47900', source ='cdc.surveillance.reports', ontology = 'cdc.msa.reports', adjudication = TRUE)%>%
  #Manually Removing Diagnosed Prevalence data for Sacramento 2015 or before for source = aggregated county
  add_row(year = "2015", location = 'C.40900', source ='cdc.surveillance.reports', ontology = 'cdc.msa.reports', adjudication = TRUE)%>%
  add_row(year = "2014", location = 'C.40900', source ='cdc.surveillance.reports', ontology = 'cdc.msa.reports', adjudication = TRUE)

run.outlier.process(outcome= 'diagnosed.prevalence',
                    stratifications= list(c()),
                    data.manager= surveillance.manager,
                    phi = 0.2,
                    theta = 0.05,
                    max.year = 2019,
                    first.choice.year = 2018,
                    locations= c(surveillance.manager$get.locations.with.data(outcome="diagnosed.prevalence")),
                    adjudication.data.frame = dx.prev.adjusted.one)



# Outcome = diagnosed.prevalence, source = cdc.hiv ------------------------
#Because there is a lot here, we decided just to straighten them out for MSAs (and states)

states = locations::get.all.for.type("state") #Returns no outliers for these locations and this source
msas = locations::get.all.for.type("CBSA")
dx.prev.adjusted.two <- run.outlier.process(outcome= 'diagnosed.prevalence',
                                           stratifications= list(c()), 
                                           data.manager= surveillance.manager,
                                           phi = 0.2,
                                           theta = 0.05,
                                           max.year = 2019,
                                           first.choice.year = 2018,
                                           locations= c(states, msas))%>%
                              filter(source == "cdc.hiv")

dx.prev.adjusted.two <- dx.prev.adjusted.two %>%
  add_row(year = "2019", location = 'C.26420', source ='cdc.hiv', ontology = 'cdc', adjudication = TRUE) #Decided to manually remove this point on 12-11-24
 
run.outlier.process(outcome= 'diagnosed.prevalence',
                    stratifications= list(c()),
                    data.manager= surveillance.manager,
                    phi = 0.2,
                    theta = 0.05,
                    max.year = 2019,
                    first.choice.year = 2018,
                    locations= c(states, msas),
                    adjudication.data.frame = dx.prev.adjusted.two)

# Outcome = diagnosed.prevalence, source = cdc.aggregated.county ------------------------

dx.prev.adjusted.three <- run.outlier.process(outcome= 'diagnosed.prevalence',
                                              stratifications= list(c()), 
                                              data.manager= surveillance.manager,
                                              phi = 0.2,
                                              theta = 0.05,
                                              max.year = 2019,
                                              first.choice.year = 2018,
                                              locations= c(states, msas))%>%
  filter(source == "cdc.aggregated.county")

dx.prev.adjusted.three$adjudication <- c(T)

#Manually Removing Diagnosed Prevlaence data for Sacramento 2015 or before for source = aggregated county
dx.prev.adjusted.three <- dx.prev.adjusted.three %>%
add_row(year = "2015", location = 'C.40900', source ='cdc.aggregated.county', ontology = 'cdc', adjudication = TRUE)%>%
add_row(year = "2014", location = 'C.40900', source ='cdc.aggregated.county', ontology = 'cdc', adjudication = TRUE)%>%
add_row(year = "2013", location = 'C.40900', source ='cdc.aggregated.county', ontology = 'cdc', adjudication = TRUE)%>%
add_row(year = "2012", location = 'C.40900', source ='cdc.aggregated.county', ontology = 'cdc', adjudication = TRUE)%>%
add_row(year = "2011", location = 'C.40900', source ='cdc.aggregated.county', ontology = 'cdc', adjudication = TRUE)%>%
add_row(year = "2010", location = 'C.40900', source ='cdc.aggregated.county', ontology = 'cdc', adjudication = TRUE)%>%
add_row(year = "2009", location = 'C.40900', source ='cdc.aggregated.county', ontology = 'cdc', adjudication = TRUE)%>%
add_row(year = "2008", location = 'C.40900', source ='cdc.aggregated.county', ontology = 'cdc', adjudication = TRUE)


#Combine with the other identified diagnosed.prevalence outliers from above: 
all.dx.prev.agg.county = rbind(dx.prev.adjusted.three, dx.prev.agg.county.remove)

run.outlier.process(outcome= 'diagnosed.prevalence',
                    stratifications= list(c()),
                    data.manager= surveillance.manager,
                    phi = 0.2,
                    theta = 0.05,
                    max.year = 2019,
                    first.choice.year = 2018,
                    locations= c(states, msas),
                    adjudication.data.frame = all.dx.prev.agg.county) #This dataframe includes identified outliers + those we decided to remove



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
  #Total-no outliers.
ps.syphilis.adjusted <- run.outlier.process(outcome= 'ps.syphilis',
                                        stratifications= list(c()), 
                                        data.manager= surveillance.manager,
                                        phi = 0.8,
                                        theta = 0.05,
                                        max.year = 2019,
                                        locations= c(surveillance.manager$get.locations.with.data(outcome="ps.syphilis")))


# outcome = early.syphilis ------------------------------------------------------
#Total

early.syphilis.adjusted <- run.outlier.process(outcome= 'early.syphilis',
                                         stratifications= list(c()), 
                                         data.manager= surveillance.manager,
                                         phi = 0.8,
                                         theta = 0.05,
                                         max.year = 2019,
                                         first.choice.year =  2018,
                                         locations= c(surveillance.manager$get.locations.with.data(outcome="early.syphilis")))

early.syphilis.adjusted$adjudication <- c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE,
TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)

run.outlier.process(outcome= 'early.syphilis',
                    stratifications= list(c()),
                    data.manager= surveillance.manager,
                    phi = 0.8,
                    theta = 0.05,
                    max.year = 2019,
                    first.choice.year =  2018,
                    locations= c(surveillance.manager$get.locations.with.data(outcome="early.syphilis")),
                    adjudication.data.frame = early.syphilis.adjusted)
# outcome = congenital.syphilis - zero outliers ------------------------------------------------------


congenital.syphilis.adjusted <- run.outlier.process(outcome= 'congenital.syphilis',
                                               stratifications= list(c()), 
                                               data.manager= surveillance.manager,
                                               phi = 0.8,
                                               theta = 0.05,
                                               max.year = 2019,
                                               locations= c(surveillance.manager$get.locations.with.data(outcome="congenital.syphilis")))
# outcome = gonorrhea ------------------------------------------------------

gonorrhea.adjusted <- run.outlier.process(outcome= 'gonorrhea',
                                          stratifications= list(c()),
                                          data.manager= surveillance.manager,
                                          phi = 3.0,
                                          theta = 0.5,
                                          phi.and.theta.on.additive.scale = F,
                                          minimum.flagged.change = 49,
                                          locations= c(surveillance.manager$get.locations.with.data(outcome="gonorrhea")))

gonorrhea.adjusted$adjudication <- c(T)

run.outlier.process(outcome= 'gonorrhea',
                    stratifications= list(c()),
                    data.manager= surveillance.manager,
                    phi = 3.0,
                    theta = 0.5,
                    phi.and.theta.on.additive.scale = F,
                    minimum.flagged.change = 49,
                    locations= c(surveillance.manager$get.locations.with.data(outcome="gonorrhea")),
                    adjudication.data.frame = gonorrhea.adjusted)

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


# outcome = aids.diagnoses part 1---------------------------------------------------
#Total- 
  #Each source here needs a different reference year (cdc.aids data is from 1981-2001; cdc.surveillance.reports are from 1993-2007)

#cdc.surveillance.reports (updated 6-10 after data change)
aids.diagnoses.source.two<- run.outlier.process(outcome= 'aids.diagnoses',
                                                stratifications= list(c()), 
                                                data.manager= surveillance.manager,
                                                phi = 0.9, 
                                                theta = 0.3,
                                                locations= c(surveillance.manager$get.locations.with.data(outcome="aids.diagnoses")))%>%
                          filter(source == "cdc.surveillance.reports")


aids.diagnoses.source.two$adjudication <- c(F, F, T, T, F, T, F, T, T, T, T, T, T, T, T, T, T, T, T, T, T, F, T, T, T, T)

run.outlier.process(outcome= 'aids.diagnoses',
                    stratifications= list(c()),
                    data.manager= surveillance.manager,
                    phi = 0.9,
                    theta = 0.3,
                    locations= c(surveillance.manager$get.locations.with.data(outcome="aids.diagnoses")),
                    adjudication.data.frame = aids.diagnoses.source.two)



# outcome = aids.diagnoses part 2 -----------------------------------------

#cdc.aids (updated 6-10 after data change)
aids.diagnoses.source.one <- run.outlier.process(outcome= 'aids.diagnoses',
                                                  stratifications= list(c()), 
                                                  data.manager= surveillance.manager,
                                                  phi = 0.9,
                                                  theta = 0.5,
                                                  locations= c(surveillance.manager$get.locations.with.data(outcome="aids.diagnoses")))%>%
                              filter(source == "cdc.aids")

aids.diagnoses.source.one$adjudication <- c(F, F, F, F, F, F, T)

run.outlier.process(outcome= 'aids.diagnoses',
                    stratifications= list(c()),
                    data.manager= surveillance.manager,
                    phi = 0.9,
                    theta = 0.5,
                    locations= c(surveillance.manager$get.locations.with.data(outcome="aids.diagnoses")),
                    adjudication.data.frame = aids.diagnoses.source.one)

# Outcome = aids.diagnosed.prevalence -------------------------------------
aids.dx.prev.adjusted <- run.outlier.process(outcome= 'aids.diagnosed.prevalence',
                                         stratifications= list(c()), 
                                         data.manager= surveillance.manager,
                                         phi = 0.4, 
                                         theta = 0.05,
                                         first.choice.year =  2000,
                                         locations= c(surveillance.manager$get.locations.with.data(outcome="aids.diagnosed.prevalence")))

aids.dx.prev.adjusted$adjudication <- c(T)

run.outlier.process(outcome= 'aids.diagnosed.prevalence',
                    stratifications= list(c()),
                    data.manager= surveillance.manager,
                    phi = 0.4, 
                    theta = 0.05,
                    first.choice.year =  2000,
                    locations= c(surveillance.manager$get.locations.with.data(outcome="aids.diagnosed.prevalence")),
                    adjudication.data.frame = aids.dx.prev.adjusted)




