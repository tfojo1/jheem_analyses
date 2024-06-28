#####
# CHECKING ----------------------------------------------------------------
issue <- diagnoses.one.way %>%
  filter(location == "IN")

underlying.data = as.data.frame.table(surveillance.manager$data$diagnoses$estimate$cdc.hiv$cdc$year__location__sex)

underlying.data <- underlying.data %>%
  filter(location == "IN")

###########



# Diagnoses- one way strata -----------------------------------------------

diagnoses.stratified <- run.outlier.process(outcome= 'diagnoses',
                                          stratifications= list('sex', 'race', 'age', 'risk'), 
                                         #stratifications= list('sex'),
                                         data.manager= surveillance.manager,
                                         phi = 0.32, 
                                         theta = 0.06,
                                         max.year = 2019,
                                         locations= c(surveillance.manager$get.locations.with.data(outcome="diagnoses")))

# diagnosed prevalence- one way strata --------------------------------------------

states = locations::get.all.for.type("state") #Returns no outliers for these locations and this source
msas = locations::get.all.for.type("CBSA")

dx.prev.stratified <- run.outlier.process(outcome= 'diagnosed.prevalence',
                                          stratifications= list('sex', 'race', 'age', 'risk'),
                                            data.manager= surveillance.manager,
                                            phi = 0.2,
                                            theta = 0.05,
                                            max.year = 2019,
                                            first.choice.year = 2018,
                                            locations= c(states, msas))

# hiv.deaths - one way strata ----------------------------------------------------

hiv.deaths.stratified<- run.outlier.process(outcome= 'hiv.deaths',
                                          stratifications= list('sex', 'race', 'age', 'risk'),
                                          data.manager= surveillance.manager,
                                          phi = 0.2,
                                          theta = 0.05,
                                          max.year = 2019,
                                          locations= c(surveillance.manager$get.locations.with.data(outcome="hiv.deaths")))

# suppression - one way strata ----------------------------------------------------

suppression.stratified <- run.outlier.process(outcome= 'suppression',
                                            stratifications= list('sex', 'race', 'age', 'risk'), 
                                            data.manager= surveillance.manager,
                                            #phi = 0.15,
                                            #theta = 0.05,
                                            locations= c(surveillance.manager$get.locations.with.data(outcome="suppression")))


# linkeage_1mo stratified -------------------------------------------------

linkeage_1mo.stratified <- run.outlier.process(outcome= 'linkeage_1mo',
                                             stratifications= list('sex', 'race', 'age', 'risk'), 
                                             data.manager= surveillance.manager,
                                             #phi = 0.15,
                                             #theta = 0.05,
                                             locations= c(surveillance.manager$get.locations.with.data(outcome="linkeage_1mo")))

# linkeage_3mo stratified -------------------------------------------------

linkeage_3mo.stratified <- run.outlier.process(outcome= 'linkeage_3mo',
                                             stratifications= list('sex', 'race', 'age', 'risk'), 
                                             data.manager= surveillance.manager,
                                             #phi = 0.15,
                                             #theta = 0.05,
                                             locations= c(surveillance.manager$get.locations.with.data(outcome="linkeage_3mo")))


# retention of engaged stratified -----------------------------------------

retention.of.engaged.stratified <- run.outlier.process(outcome= 'retention.of.engaged',
                                                     stratifications= list('sex', 'race', 'age', 'risk'), 
                                                     data.manager= surveillance.manager,
                                                     #phi = 0.15,
                                                     #theta = 0.05,
                                                     locations= c(surveillance.manager$get.locations.with.data(outcome="retention.of.engaged")))

# engaged straitifed ------------------------------------------------------
engagement.stratified <- run.outlier.process(outcome= 'engagement',
                                             stratifications= list('sex', 'race', 'age', 'risk'), 
                                           data.manager= surveillance.manager,
                                           #phi = 0.15,
                                           #theta = 0.05,
                                           locations= c(surveillance.manager$get.locations.with.data(outcome="engagement")))


# ps.syphilis stratified --------------------------------------------------

ps.syphilis.stratified <- run.outlier.process(outcome= 'ps.syphilis',
                                            stratifications= list('sex', 'race', 'age', 'risk'), 
                                            data.manager= surveillance.manager,
                                            phi = 0.5,
                                            theta = 0.05,
                                            max.year = 2019,
                                            locations= c(surveillance.manager$get.locations.with.data(outcome="ps.syphilis")))


# early syphilis stratified -----------------------------------------------

early.syphilis.stratified <- run.outlier.process(outcome= 'early.syphilis',
                                                 stratifications= list('sex', 'race', 'age', 'risk'), 
                                               data.manager= surveillance.manager,
                                               phi = 0.8,
                                               theta = 0.05,
                                               max.year = 2019,
                                               first.choice.year =  2018,
                                               locations= c(surveillance.manager$get.locations.with.data(outcome="early.syphilis")))


# gonorrhea - stratified --------------------------------------------------

gonorrhea.stratified <- run.outlier.process(outcome= 'gonorrhea',
                                          stratifications= list('sex', 'race', 'age', 'risk'), 
                                          data.manager= surveillance.manager,
                                          phi = 0.6,
                                          theta = 0.05,
                                          max.year = 2019,
                                          locations= c(surveillance.manager$get.locations.with.data(outcome="gonorrhea")))

# retention - stratified ---------------------------------------------------------------
retention.stratified <- run.outlier.process(outcome= 'retention',
                                          stratifications= list('sex', 'race', 'age', 'risk'), 
                                          data.manager= surveillance.manager,
                                          #phi = 0.15,
                                          #theta = 0.05,
                                          #max.year = 2019,
                                          locations= c(surveillance.manager$get.locations.with.data(outcome="retention")))


# aids.diagnoses stratified -----------------------------------------------
#This outcome hs two sources but only one is stratified and the other is total
aids.diagnoses.stratified<- run.outlier.process(outcome= 'aids.diagnoses',
                                                 stratifications= list('sex', 'race', 'age', 'risk'), 
                                                 data.manager= surveillance.manager,
                                                 phi = 0.9,
                                                 theta = 0.5,
                                                 locations= c(surveillance.manager$get.locations.with.data(outcome="aids.diagnoses")))%>%
                                                filter(source == "cdc.aids")


# aids.diagnoses.deceased.by.2001 stratified ------------------------------
aids.diagnoses.deceased.by.2001.stratified <- run.outlier.process(outcome= 'aids.diagnoses.deceased.by.2001',
                                            stratifications= list('sex', 'race', 'age', 'risk'), 
                                            data.manager= surveillance.manager,
                                            #phi = 0.15,
                                            #theta = 0.05,
                                            #max.year = 2019,
                                            locations= c(surveillance.manager$get.locations.with.data(outcome="aids.diagnoses.deceased.by.2001")))

# proportion.msm stratified -----------------------------------------------
proportion.msm.stratified <- run.outlier.process(outcome= 'proportion.msm',
                                                                  stratifications= list('sex', 'race', 'age', 'risk'), 
                                                                  data.manager= surveillance.manager,
                                                                  #phi = 0.15,
                                                                  #theta = 0.05,
                                                                  #max.year = 2019,
                                                                  locations= c(surveillance.manager$get.locations.with.data(outcome="proportion.msm")))

# proportion.tested stratified --------------------------------------------

proportion.tested.stratified <- run.outlier.process(outcome= 'proportion.msm',
                                                 stratifications= list('sex', 'race', 'age', 'risk'), 
                                                 data.manager= surveillance.manager,
                                                 #phi = 0.15,
                                                 #theta = 0.05,
                                                 #max.year = 2019,
                                                 locations= c(surveillance.manager$get.locations.with.data(outcome="proportion.tested")))
