
# BRFSS & SHIELD ----
#This maps SHILED to BRFSS data (because the BRFSS data only has male and MSM as risk+sex)
register.ontology.mapping('shiled.to.brfss.sex.risk',
                          from.dimensions = c('sex'),
                          to.dimensions = c('sex', 'risk'),
                          mappings = rbind(c('msm', 'male', 'msm'),
                                           c('heterosexual_male', 'male', 'not_msm'),
                                           c('female', 'female', 'not_msm'))
)


#Maps BRFSS racial groups (this is where proportion tested data is from) to SHIELD racial groups
register.ontology.mapping('brfss.to.shield.race',
                          from.dimension = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('american indian/alaska native', 'other'),
                                           c('asian', 'other'),
                                           c('black', 'black'),
                                           c('hispanic', 'hispanic'),
                                           c('native hawaiian/other pacific islander', 'other'),
                                           c('other race', 'other'),
                                           c('white', 'other')))

# CDC & SHIELD ----
#Maps racial categories from CDC data to the races we use in SHILED
register.ontology.mapping('cdc.to.shiled.race1',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('american indian/alaska native', 'other'),
                                           c('asian', 'other'),
                                           c('black/african american', 'black'),
                                           c('hispanic/latino', 'hispanic'),
                                           c('native hawaiian/other pacific islander', 'other'),
                                           c('white', 'other'))
)
#Maps racial categories from CDC MSA Surveillance Reports to races we use for SHILED
register.ontology.mapping('cdc.to.shield.race2',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('american indian/alaska native', 'other'),
                                           c('asian', 'other'),
                                           c('black/african american', 'black'),
                                           c('hispanic/latino', 'hispanic'),
                                           c('white', 'other')))
# CENSUS & SHIELD ----

# converting sex groups in SHIELD to sexes reported in CENSUS
register.ontology.mapping('shield.to.census.sex',
                          from.dimensions=c('sex'),
                          to.dimensions = c('sex'),
                          mappings = rbind(c('female', 'female'),
                                           c('msm','male'),
                                           c('heterosexual_male','male')))

#Maps racial grouped used in the Census Immigration data to the racial groups used in SHIELD
register.ontology.mapping('census.to.SHIELD.race.ethnicity1',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('hispanic or latino', 'hispanic'),
                                           c('black', 'black'),
                                           c('other', 'other'))) # needed?

#Maps Census racial groups (specifically race+ethnicity) to what we use in SHIELD 
register.ontology.mapping('census.to.shield.race.ethnicity2',
                          from.dimensions = c('race', 'ethnicity'),
                          to.dimensions = 'race',
                          mappings = rbind(c('white', 'hispanic', 'hispanic'),
                                           c('white', 'not hispanic', 'other'),
                                           c('black', 'hispanic', 'hispanic'),
                                           c('black', 'not hispanic', 'black'),
                                           c('american indian or alaska native', 'hispanic', 'hispanic'),
                                           c('american indian or alaska native', 'not hispanic', 'other'),
                                           c('asian or pacific islander', 'hispanic', 'hispanic'),
                                           c('asian or pacific islander', 'not hispanic', 'other')))

#Maps Census racial groups  (ethnicity only) to what we use in SHIELD 
register.ontology.mapping('census.to.shield.ethnicity',
                          from.dimensions = c('race'),
                          to.dimensions = 'race',
                          mappings = rbind(c('hispanic', 'hispanic'),
                                           c('not hispanic', 'other')))

 
# WONDER to CDC/CENSUS ----
#I'm keeping a few for SHIELD because we are using CDC Wonder for fertility rates
register.ontology.mapping('wonder.to.census.race.1',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('american indian or alaska native', 'american indian or alaska native'),
                                           c('asian or pacific islander', 'asian or pacific islander'),
                                           c('black or african american', 'black'),
                                           c('white', 'white')))

register.ontology.mapping('wonder.to.census.ethnicity.1',
                          from.dimensions = 'ethnicity',
                          to.dimensions = 'ethnicity',
                          mappings = rbind(c('hispanic or latino', 'hispanic'),
                                           c('not hispanic or latino', 'not hispanic')))

# WONDER & SHIELD ----
#Mapping CDC Wonder racial groups to SHIELD (for SHIELD this is for fertility)
register.ontology.mapping('wonder.to.shield.race',
                          from.dimensions = c('race'),
                          to.dimensions = 'race',
                          mappings = rbind(c('american indian or alaska native', 'other'),
                                           c('asian or pacific islander', 'other'),
                                           c('black or african american', 'black'),
                                           c('white', 'other')))

register.ontology.mapping('wonder.to.shield.race.ethnicity',
                          from.dimensions = c('race', 'ethnicity'),
                          to.dimensions = 'race',
                          mappings = rbind(c('american indian or alaska native', 'hispanic or latino', 'hispanic'),
                                           c('american indian or alaska native', 'not hispanic or latino', 'other'),
                                           c('asian or pacific islander', 'hispanic or latino', 'hispanic'),
                                           c('asian or pacific islander', 'not hispanic or latino', 'other'),
                                           c('black or african american', 'hispanic or latino', 'hispanic'),
                                           c('black or african american', 'not hispanic or latino', 'black'),
                                           c('white','hispanic or latino','hispanic'),
                                           c('white','not hispanic or latino','other')))

# EMORY & SHIELD ----
#Maps Emory's sex groups to CDC (Emory has an estimate of MSM and so the Emory data is only relevant to men)
register.ontology.mapping('emory.to.cdc.sex.temporary',
                          from.dimensions = 'sex',
                          to.dimensions = 'sex',
                          mappings = rbind(c('male', 'male'),
                                           c(NA, 'female')))


# CENSUS & BRFSS ----
#This maps data from Census to BRFSS- this is also to help the pull function make sense of the data
register.ontology.mapping('census.to.brfss.race.ethnicity',
                          from.dimensions = c('race', 'ethnicity'),
                          to.dimensions = 'race',
                          mappings = rbind(c('american indian or alaska native', 'hispanic', 'hispanic'),
                                           c('american indian or alaska native', 'not hispanic', 'american indian/alaska native'),
                                           c('asian or pacific islander', 'hispanic', 'hispanic'),
                                           c('asian or pacific islander', 'not hispanic', 'asian/pacific islander'),
                                           c('black', 'hispanic', 'hispanic'),
                                           c('black', 'not hispanic', 'black'),
                                           c('white', 'hispanic', 'hispanic'),
                                           c('white', 'not hispanic', 'white'),
                                           c(NA, NA, 'other')))

register.ontology.mapping('brfss.to.census.race1',
                          from.dimension = 'race',
                          to.dimension = 'race',
                          mappings = rbind(
                            c('american indian/alaska native', 'american indian and alaska native'),
                            c('asian', 'asian'),
                            c('black', 'black'),
                            c('hispanic', 'hispanic'),
                            c('native hawaiian/other pacific islander', 'native hawaiian and other pacific islander'),
                            c('other race', NA),
                            c('white', 'white')
                          ))

#This maps data from BRFSS to Census population data
#We have both BRFSS data and Census population in SHIELD and this helps the pull function understand how to conceptualize those data sources together
register.ontology.mapping('brfss.to.census.race2',
                          from.dimensions = c('race', 'ethnicity'),
                          to.dimensions = 'race',
                          mappings = rbind(c('american indian or alaska native', 'hispanic or latino', 'hispanic'),
                                           c('american indian or alaska native', 'not hispanic or latino', 'american indian/alaska native'),
                                           c('asian or pacific islander', 'hispanic or latino', 'hispanic'),
                                           c('asian or pacific islander', 'not hispanic or latino', 'asian/pacific islander'),
                                           c('black or african american', 'hispanic or latino', 'hispanic'),
                                           c('black or african american', 'not hispanic or latino', 'black'),
                                           c('white','hispanic or latino','hispanic'),
                                           c('white','not hispanic or latino','white'),
                                           c(NA, NA, 'other')))

#This maps data from BRFSS to census population data
register.ontology.mapping('brfss.to.census.race3',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('american indian/alaska native', 'american indian/alaska native'),
                                           c('asian', 'asian/pacific islander'),
                                           c('black', 'black'),
                                           c('hispanic', 'hispanic'),
                                           c('native hawaiian/other pacific islander', 'asian/pacific islander'),
                                           c('other race', 'other'),
                                           c('white', 'white')))
# NHANES & SHIELD ----
#Maps NHANES racial groups to those used in SHIELD
register.ontology.mapping('nhanes.to.shield.race',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('black, non hispanic', 'black'),
                                           c('hispanic', 'hispanic'),
                                           c('other', 'other'),
                                           c('white, non hispanic', 'other')))
# CENSUS TO CENSUS ----
#Maps the racial groups from stratified Census data to the other racial groups in the Census data
register.ontology.mapping('stratified.census.to.census.race.ethnicity',
                          from.dimensions = c('race', 'ethnicity'),
                          to.dimensions =  c('race', 'ethnicity'),
                          mappings = rbind(c('american indian and alaska native', 'hispanic', 'american indian or alaska native', 'hispanic'),
                                           c('american indian and alaska native', 'not hispanic', 'american indian or alaska native', 'not hispanic'),
                                           c('asian', 'hispanic', 'asian or pacific islander', 'hispanic'),
                                           c('asian', 'not hispanic', 'asian or pacific islander', 'not hispanic'),
                                           c('black', 'hispanic', 'black', 'hispanic'),
                                           c('black', 'not hispanic', 'black', 'not hispanic'),
                                           c('native hawaiian and other pacific islander', 'hispanic', 'asian or pacific islander', 'hispanic'),
                                           c('native hawaiian and other pacific islander', 'not hispanic', 'asian or pacific islander', 'not hispanic'),
                                           c('white', 'hispanic', 'white', 'hispanic'),
                                           c('white', 'not hispanic', 'white', 'not hispanic')))
