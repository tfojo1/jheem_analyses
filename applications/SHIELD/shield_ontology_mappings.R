
# BRFSS & SHIELD ----
#This maps SHILED to BRFSS data (because the BRFSS data only has male and MSM as risk+sex)
register.ontology.mapping('shiled.to.brfss.sex.risk',
                          from.dimensions = c('sex'),
                          to.dimensions = c('sex', 'risk'),
                          mappings = rbind(c('msm', 'male', 'msm'),
                                           c('heterosexual_male', 'male', 'not_msm'),
                                           c('female', 'female', 'not_msm'))
)
#This maps data from BRFSS to Census population data
#We have both BRFSS data and Census population in SHIELD and this helps the pull function understand how to conceptualize those data sources together
register.ontology.mapping('brfss.prop.tested.to.shield',
                          from.dimensions = c('race', 'ethnicity'),
                          to.dimensions = 'race',
                          mappings = rbind(c('American Indian or Alaska Native', 'Hispanic or Latino', 'Hispanic'),
                                           c('American Indian or Alaska Native', 'Not Hispanic or Latino', 'American Indian/Alaska Native'),
                                           c('Asian or Pacific Islander', 'Hispanic or Latino', 'Hispanic'),
                                           c('Asian or Pacific Islander', 'Not Hispanic or Latino', 'Asian/Pacific Islander'),
                                           c('Black or African American', 'Hispanic or Latino', 'Hispanic'),
                                           c('Black or African American', 'Not Hispanic or Latino', 'Black'),
                                           c('More than one race', 'Hispanic or Latino', 'Hispanic'),
                                           c('More than one race', 'Not Hispanic or Latino', 'Multiracial'),
                                           c('White','Hispanic or Latino','Hispanic'),
                                           c('White','Not Hispanic or Latino','White'),
                                           c(NA, NA, 'other')))

#This maps data from BRFSS to census population data
register.ontology.mapping('brfss.prop.tested.to.shield.2',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('American Indian/Alaska Native', 'American Indian/Alaska Native'),
                                           c('Asian', 'Asian/Pacific Islander'),
                                           c('Black', 'Black'),
                                           c('Hispanic', 'Hispanic'),
                                           c('Multiracial', 'Multiracial'),
                                           c('Native Hawaiian/Other Pacific Islander', 'Asian/Pacific Islander'),
                                           c('Other race', 'other'),
                                           c('White', 'White')))

#Maps BRFSS racial groups (this is where proportion tested data is from) to SHIELD racial groups
register.ontology.mapping('brfss.prop.tested.to.shield.race',
                          from.dimension = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('American Indian/Alaska Native', 'other'),
                                           c('Asian', 'other'),
                                           c('Black', 'black'),
                                           c('Hispanic', 'hispanic'),
                                           c('Multiracial', 'other'),
                                           c('Native Hawaiian/Other Pacific Islander', 'other'),
                                           c('Other race', 'other'),
                                           c('White', 'other')))

# CDC & SHIELD ----
#Maps racial categories from CDC data to the races we use in SHILED
register.ontology.mapping('cdc.to.shiled.race',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('American Indian/Alaska Native', 'other'),
                                           c('Asian', 'other'),
                                           c('Black/African American', 'black'),
                                           c('Hispanic/Latino', 'hispanic'),
                                           c('Multiracial', 'other'),
                                           c('Native Hawaiian/Other Pacific Islander', 'other'),
                                           c('White', 'other'))
)
#Maps racial categories from CDC MSA Surveillance Reports to races we use for SHILED
register.ontology.mapping('cdc.msa.reports.to.shield.race',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('American Indian/Alaska Native', 'other'),
                                           c('Asian', 'other'),
                                           c('Black/African American', 'black'),
                                           c('Hispanic/Latino', 'hispanic'),
                                           c('White', 'other')))
# CENSUS & SHIELD ----

#Maps racial grouped used in the Census Immigration data to the racial groups used in SHIELD
register.ontology.mapping('census.immigration.to.SHIELD.race',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('Hispanic or Latino', 'hispanic'),
                                           c('White, Non-Hispanic', 'white'),
                                           c('Black', 'black'),
                                           c('Other', 'other'))) # needed?
# converting sex groups in SHIELD to sexes reported in CENSUS
register.ontology.mapping('shield.to.census.sex',
                          from.dimensions=c('sex'),
                          to.dimensions = c('sex'),
                          mappings = rbind(c('female', 'female'),
                                           c('msm','male'),
                                           c('heterosexual_male','male')))


#Maps Census racial groups (specifically race+ethnicity) to what we use in SHIELD 
register.ontology.mapping('census.to.shield.race',
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
register.ontology.mapping('census.to.shield.ethnicity.only',
                          from.dimensions = c('race'),
                          to.dimensions = 'race',
                          mappings = rbind(c('hispanic', 'hispanic'),
                                           c('not hispanic', 'other')))

# WONDER to CDC/CENSUS ----
#I'm keeping a few for SHIELD because we are using CDC Wonder for fertility rates
register.ontology.mapping('wonder.to.census.race.2',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('American Indian or Alaska Native', 'american indian or alaska native'),
                                           c('Asian or Pacific Islander', 'asian or pacific islander'),
                                           c('Black or African American', 'black'),
                                           c('White', 'white'),
                                           c('More than one race', NA)))
register.ontology.mapping('wonder.to.census.ethnicity.1',
                          from.dimensions = 'ethnicity',
                          to.dimensions = 'ethnicity',
                          mappings = rbind(c('Hispanic or Latino', 'hispanic'),
                                           c('Not Hispanic or Latino', 'not hispanic'),
                                           c('Unknown or Not Stated', NA),
                                           c('Not Stated', NA)))
register.ontology.mapping('wonder.to.census.ethnicity.2',
                          from.dimensions = 'ethnicity',
                          to.dimensions = 'ethnicity',
                          mappings = rbind(c('Hispanic or Latino', 'hispanic'),
                                           c('Not Hispanic or Latino', 'not hispanic')))
register.ontology.mapping('wonder.to.census.race.3',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('American Indian or Alaska Native', 'american indian or alaska native'),
                                           c('Asian or Pacific Islander', 'asian or pacific islander'),
                                           c('Black or African American', 'black'),
                                           c('White', 'white'),
                                           c('More than one race', NA),
                                           c('Not Reported', NA),
                                           c('Unknown or Not Stated', NA),
                                           c('Not Available', NA)))


# WONDER & SHIELD ----
#Mapping CDC Wonder racial groups to SHIELD (for SHIELD this is for fertility)
register.ontology.mapping('wonder.to.SHIELD.race.2',
                          from.dimensions = c('race'),
                          to.dimensions = 'race',
                          mappings = rbind(c('American Indian or Alaska Native', 'other'),
                                           c('Asian or Pacific Islander', 'other'),
                                           c('Black or African American', 'black'),
                                           c('White', 'other'),
                                           c('More than one race', 'other')))

register.ontology.mapping('wonder.to.SHIELD.race.ethnicity',
                          from.dimensions = c('race', 'ethnicity'),
                          to.dimensions = 'race',
                          mappings = rbind(c('American Indian or Alaska Native', 'Hispanic or Latino', 'hispanic'),
                                           c('American Indian or Alaska Native', 'Not Hispanic or Latino', 'other'),
                                           c('Asian or Pacific Islander', 'Hispanic or Latino', 'hispanic'),
                                           c('Asian or Pacific Islander', 'Not Hispanic or Latino', 'other'),
                                           c('Black or African American', 'Hispanic or Latino', 'hispanic'),
                                           c('Black or African American', 'Not Hispanic or Latino', 'black'),
                                           c('More than one race', 'Hispanic or Latino', 'hispanic'),
                                           c('More than one race', 'Not Hispanic or Latino', 'other'),
                                           c('White','Hispanic or Latino','hispanic'),
                                           c('White','Not Hispanic or Latino','other')))

# EMORY & SHIELD ----
#Maps Emory's sex groups to CDC (Emory has an estimate of MSM and so the Emory data is only relevant to men)
register.ontology.mapping('emory.sex.to.cdc.sex.temporary',
                          from.dimensions = 'sex',
                          to.dimensions = 'sex',
                          mappings = rbind(c('male', 'male'),
                                           c(NA, 'female')))


# CENSUS & BRFSS ----
#This maps data from Census to BRFSS- this is also to help the pull function make sense of the data
register.ontology.mapping('census.to.brfss.race.ethnicity',
                          from.dimensions = c('race', 'ethnicity'),
                          to.dimensions = 'race',
                          mappings = rbind(c('american indian or alaska native', 'hispanic', 'Hispanic'),
                                           c('american indian or alaska native', 'not hispanic', 'American Indian/Alaska Native'),
                                           c('asian or pacific islander', 'hispanic', 'Hispanic'),
                                           c('asian or pacific islander', 'not hispanic', 'Asian/Pacific Islander'),
                                           c('black', 'hispanic', 'Hispanic'),
                                           c('black', 'not hispanic', 'Black'),
                                           c('white', 'hispanic', 'Hispanic'),
                                           c('white', 'not hispanic', 'White'),
                                           c(NA, NA, 'Multiracial'),
                                           c(NA, NA, 'other')))

# NHANES & SHIELD ----
#Maps NHANES racial groups to those used in SHIELD
register.ontology.mapping('nhanes.to.SHIELD.race',
                          from.dimensions = 'race',
                          to.dimensions = 'race',
                          mappings = rbind(c('black, non hispanic', 'black'),
                                           c('hispanic', 'hispanic'),
                                           c('other', 'other'),
                                           c('white, non hispanic', 'other')))
# CENSUS TO CENSUS ----
#Maps the racial groups from stratified Census data to the other racial groups in the Census data
register.ontology.mapping('stratified.census.to.census.race',
                          from.dimensions = c('race', 'ethnicity'),
                          to.dimensions =  c('race', 'ethnicity'),
                          mappings = rbind(c('American Indian and Alaska Native', 'Hispanic', 'american indian or alaska native', 'hispanic'),
                                           c('American Indian and Alaska Native', 'Not Hispanic', 'american indian or alaska native', 'not hispanic'),
                                           c('Asian', 'Hispanic', 'asian or pacific islander', 'hispanic'),
                                           c('Asian', 'Not Hispanic', 'asian or pacific islander', 'not hispanic'),
                                           c('Black', 'Hispanic', 'black', 'hispanic'),
                                           c('Black', 'Not Hispanic', 'black', 'not hispanic'),
                                           c('Native Hawaiian and Other Pacific Islander', 'Hispanic', 'asian or pacific islander', 'hispanic'),
                                           c('Native Hawaiian and Other Pacific Islander', 'Not Hispanic', 'asian or pacific islander', 'not hispanic'),
                                           c('White', 'Hispanic', 'white', 'hispanic'),
                                           c('White', 'Not Hispanic', 'white', 'not hispanic')))
