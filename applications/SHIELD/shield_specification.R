#CASHED FOLDER:
# https://livejohnshopkins-my.sharepoint.com/personal/tfojo1_jh_edu/_layouts/15/onedrive.aspx?e=5%3A940bf48ba6e0498495fea5596e3dc8e7&sharingv2=true&fromShare=true&at=9&CID=425e54af%2De78b%2D4d53%2D8df4%2D6abb10af6339&id=%2Fpersonal%2Ftfojo1%5Fjh%5Fedu%2FDocuments%2FJHEEM2&FolderCTID=0x012000E74D427C3A55BC45A1C18C850CDA2DB4&view=0
# Excel Sheet:
# https://livejohnshopkins-my.sharepoint.com/:x:/g/personal/zdansky1_jh_edu/EVrQ-OpGqlVIpBi_KE0P6v4B2rTpIvYcyUtLz9e1NH_oig?e=kd8bjH&wdLOR=c06087FCD-0041-804E-BB9F-F582185054BC
# Website
# https://jheem.shinyapps.io/EndingHIV/

cat('*** Running Shiled_specification.R ***\n')

# Working directory is set to the main JHEEM_Analysis folder:
source('applications/SHIELD/shield_source_code.R')


# Caching required objects: 
# HIV testing priors from BRFSS
# source("applications/SHIELD/inputs/input_cache_hiv_testing_prior_brfss.R")
# PRENATAL care coverage estiamtes from Wonder 
# source("applications/SHIELD/inputs/input_cache_prenatal_prior_from_wonder.R")

shield.solver= create.solver.metadata(rtol = 0.001, atol=0.03) #rtol,atol
##--------------------------------------------------------------------------------------------------------------#
#*** INITIAL SET-UP *** ----
SHIELD.SPECIFICATION = create.jheem.specification(version = 'shield',
                                                  default.solver.metadata = shield.solver,
                                                  iteration=1,
                                                  description = 'The initial SHIELD version, set up to model national epidemic',
                                                  start.year = DEFAULT.START.YEAR,
                                                  age.endpoints=c(0,15,20,25,30,35,40,45,50,55,65,Inf), #11 agegroups, similar to atlas [0-15][16-20];
                                                  compartments.for.infected.only = list(
                                                      continuum = c('undiagnosed', 'diagnosed.untreated'),
                                                      stage = c('primary','secondary', 'early.latent','late.latent','tertiary','cns')                                                 
                                                  ),
                                                  
                                                  compartments.for.uninfected.only = list(
                                                      profile=c('susceptible','diagnosed.treated')),
                                                  
                                                  compartments.for.infected.and.uninfected = list(
                                                      location = 'location',
                                                      age = 'all.ages',
                                                      race=c('black','hispanic','other'),
                                                      sex= c('heterosexual_male', 'msm', 'female')
                                                  ),
                                                  compartment.value.aliases = list(
                                                      #try using aliases so that if we change the specification up here, the rest of the code doesnt break
                                                      # helps to define specifications for groups of compartments later on
                                                      ps.stages=c('primary','secondary'),
                                                      early.stages=c('primary','secondary','early.latent'),
                                                      late.stages=c('late.latent','tertiary','cns'),
                                                      location=function(location){location}
                                                  )                                                  
                                                  
)


#*** INITIAL POPULATION *** ----
# Specify the initial compartment sizes  
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'n.initial.population',
                        value = 0)
# Read data on proportion of males that are MSM in year 2010
register.model.element(SHIELD.SPECIFICATION,
                       name = 'prp.msm.of.male',
                       scale = 'proportion',
                       get.functional.form.function = get.proportion.msm.of.male.by.race.functional.form,
                       functional.form.from.time=2010,
                       functional.form.to.time=2010)
# step2: uses an element with functional form to get required values
register.model.element(SHIELD.SPECIFICATION,
                       name = 'n.initial.female.population',
                       get.value.function = get.n.initial.female.population,
                       scale = 'non.negative.number')
register.model.element(SHIELD.SPECIFICATION,
                       name = 'n.initial.male.population',
                       get.value.function = get.n.initial.male.population,
                       scale = 'non.negative.number')
# step3: assigns values to specific population subgroups where the values are read from an upcoming element below
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'n.initial.population',
                               applies.to = list(sex='female'),
                               value = 'n.initial.female.population')
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'n.initial.population',
                               applies.to = list(sex='msm'),
                               value = expression(n.initial.male.population * prp.msm.of.male))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'n.initial.population',
                               applies.to = list(sex='heterosexual_male'),
                               value = expression(n.initial.male.population * (1-prp.msm.of.male)))
#-----------------------------#
##---- Infected ---- 
#-----------------------------#
# Because the true size of infected compartments are unknown, we approximate them based on number of new diagnosis (and will add multipliers to tune the true compartment sizes in the model). 
# First, we need to estimate the POPULATION PROPORTION (RATE) of diagnosis in year 1970 as = "n diag/population size" 
# Since diagnoses data is unavailable in 1970, we need to use another (later year) to approximate 1970
# We will use the first year that data is reported in each city 
# Population Proportion (Rate)= #diagnosesin stage X / population size 

# We assume all infected cases are undiagnosed/untreated

#proportion of Population Proportion diagnosed with syphilis (denom: total population) in 1970 
register.model.element(SHIELD.SPECIFICATION,
                       name = 'popProp.ps.diag.1970',
                       scale = 'non.negative.number',
                       value = SHIELD_BASE_PARAMETER_VALUES['popProp.ps.diag.1970'])
register.model.element(SHIELD.SPECIFICATION,
                       name = 'popProp.el.diag.1970',
                       scale = 'non.negative.number',
                       value = SHIELD_BASE_PARAMETER_VALUES['popProp.el.diag.1970'])
register.model.element(SHIELD.SPECIFICATION,
                       name = 'popProp.lu.diag.1970',
                       scale = 'non.negative.number',
                       value = SHIELD_BASE_PARAMETER_VALUES['popProp.lu.diag.1970'])

# Calibration multipliers
# Finally, we add 6 parameters to tune infected population size in calibration (multipliers for (ps, el, lu) for (msm vs het))
register.model.element(SHIELD.SPECIFICATION,
                       name = 'ps.diagnoses.msm.multiplier.1970', scale = 'non.negative.number',
                       value = 1)
register.model.element(SHIELD.SPECIFICATION,
                       name = 'ps.diagnoses.heterosexual.multiplier.1970', scale = 'non.negative.number',
                       value = 1)

register.model.element(SHIELD.SPECIFICATION,
                       name = 'el.diagnoses.msm.multiplier.1970', scale = 'non.negative.number',
                       value = 1)
register.model.element(SHIELD.SPECIFICATION,
                       name = 'el.diagnoses.heterosexual.multiplier.1970', scale = 'non.negative.number',
                       value = 1)
 
register.model.element(SHIELD.SPECIFICATION,
                       name = 'lu.diagnoses.msm.multiplier.1970', scale = 'non.negative.number',
                       value = 1)
register.model.element(SHIELD.SPECIFICATION,
                       name = 'lu.diagnoses.heterosexual.multiplier.1970', scale = 'non.negative.number',
                       value = 1)

# proportion of PS diagnoses that are in the primary stage in 1970
register.model.element(SHIELD.SPECIFICATION,
                       name = 'prp.ps.in.primary.stage.1970', scale = 'non.negative.number',
                       value = SHIELD_BASE_PARAMETER_VALUES['prp.ps.in.primary.stage.1970'])

### **** Populating the infected stages *****

# Estimating size of infected population
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'prop.initial.population.infected',
                        value = 0)
# Primary 
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name='prop.initial.population.infected',
                               applies.to=list(continuum='undiagnosed', stage='primary', sex='msm' ),
                               value=expression(popProp.ps.diag.1970 * ps.diagnoses.msm.multiplier.1970 * prp.ps.in.primary.stage.1970))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name='prop.initial.population.infected',
                               applies.to=list(continuum='undiagnosed', stage='primary', sex='heterosexual_male' ),
                               value=expression(popProp.ps.diag.1970 * ps.diagnoses.heterosexual.multiplier.1970* prp.ps.in.primary.stage.1970))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name='prop.initial.population.infected',
                               applies.to=list(continuum='undiagnosed', stage='primary', sex='female' ),
                               value=expression(popProp.ps.diag.1970 * ps.diagnoses.heterosexual.multiplier.1970 * prp.ps.in.primary.stage.1970))
# Secondary 
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name='prop.initial.population.infected',
                               applies.to=list(continuum='undiagnosed', stage='secondary', sex='msm' ),
                               value=expression(popProp.ps.diag.1970 * ps.diagnoses.msm.multiplier.1970 * (1-prp.ps.in.primary.stage.1970)))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name='prop.initial.population.infected',
                               applies.to=list(continuum='undiagnosed', stage='secondary', sex='heterosexual_male' ),
                               value=expression(popProp.ps.diag.1970 * ps.diagnoses.heterosexual.multiplier.1970* (1-prp.ps.in.primary.stage.1970)))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name='prop.initial.population.infected',
                               applies.to=list(continuum='undiagnosed', stage='secondary', sex='female' ),
                               value=expression(popProp.ps.diag.1970 * ps.diagnoses.heterosexual.multiplier.1970 * (1-prp.ps.in.primary.stage.1970)))
# Early latent
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name='prop.initial.population.infected',
                               applies.to=list(continuum='undiagnosed', stage='early.latent', sex='msm'),
                               value=expression(popProp.el.diag.1970 * el.diagnoses.msm.multiplier.1970))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name='prop.initial.population.infected',
                               applies.to=list(continuum='undiagnosed', stage='early.latent', sex='heterosexual_male'),
                               value=expression(popProp.el.diag.1970 * el.diagnoses.heterosexual.multiplier.1970 ))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name='prop.initial.population.infected',
                               applies.to=list(continuum='undiagnosed', stage='early.latent', sex='female'),
                               value=expression(popProp.el.diag.1970 * el.diagnoses.heterosexual.multiplier.1970 ))
# Late Latent / Unknown
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name='prop.initial.population.infected',
                               applies.to=list(continuum='undiagnosed', stage='late.latent', sex='msm' ),
                               value=expression(popProp.lu.diag.1970 * lu.diagnoses.msm.multiplier.1970 ))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name='prop.initial.population.infected',
                               applies.to=list(continuum='undiagnosed', stage='late.latent', sex='heterosexual_male' ),
                               value=expression(popProp.lu.diag.1970 * lu.diagnoses.heterosexual.multiplier.1970 ))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name='prop.initial.population.infected',
                               applies.to=list(continuum='undiagnosed', stage='late.latent', sex='female' ),
                               value=expression(popProp.lu.diag.1970 * lu.diagnoses.heterosexual.multiplier.1970))

### **** Register the infected population *****
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'n.initial.population.infected',
                        value = expression(n.initial.population *prop.initial.population.infected))

register.initial.population(SHIELD.SPECIFICATION,
                            group = 'infected',
                            value = 'n.initial.population.infected')

#-----------------------------#
## Uninfected ---- 
#-----------------------------#
# we need to calculate the size of infected population without the infected dimensions (stage, continuum)
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'n.initial.population.infected.all.stages',
                        value = 0)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name='n.initial.population.infected.all.stages',
                               applies.to=list(sex='msm'),
                               value=expression(
                                   (popProp.ps.diag.1970 * ps.diagnoses.msm.multiplier.1970 +
                                        popProp.el.diag.1970 * el.diagnoses.msm.multiplier.1970 +
                                        popProp.lu.diag.1970 * lu.diagnoses.msm.multiplier.1970) * n.initial.population))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name='n.initial.population.infected.all.stages',
                               applies.to=list(sex=c('female','heterosexual_male')),
                               value=expression(
                                   (popProp.ps.diag.1970 * ps.diagnoses.heterosexual.multiplier.1970 +
                                        popProp.el.diag.1970 * el.diagnoses.heterosexual.multiplier.1970 +
                                        popProp.lu.diag.1970 * lu.diagnoses.heterosexual.multiplier.1970) * n.initial.population))

## **** Register Uninfected Population ****
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'n.initial.population.uninfected',
                        value = 0)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'n.initial.population.uninfected',
                               value = expression(n.initial.population - n.initial.population.infected.all.stages),
                               applies.to = list(profile='susceptible')
)
register.initial.population(SHIELD.SPECIFICATION,
                            group = 'uninfected',
                            value = 'n.initial.population.uninfected')



##---- Fix Strata Sizes----
#Set Whether to Fix Strata Sizes During a Time Period # a simplifying assumption to avoid modeling population demographic dynamics before year X
# 2007 earliest year for complete census data. we fix strata to 2010
register.fixed.model.strata(SHIELD.SPECIFICATION,
                            applies.after.time = -Inf,
                            applies.before.time = DEFAULT.FIX.STRATA.YEAR,
                            fix.strata = T,
                            dimensions.to.fix = c('location','age','race','sex')
)

#*** BIRTHS *** ----
# we model births based on women's fertility rates
register.model.element(SHIELD.SPECIFICATION,
                       name = 'fertility.rate',
                       get.functional.form.function = get.fertility.rate.functional.form,
                       functional.form.from.time = DEFAULT.FERTILITY.START.YEARS,   #2005: the projections remain fix at this year's value for years before. 
                       scale = 'rate')

##---- Birth Proportions ----
# to determine where newborns should go
# currently set as a fix ratio
register.model.element(SHIELD.SPECIFICATION,
                       name = 'prp.births.male',
                       scale = 'proportion',
                       value = SHIELD_BASE_PARAMETER_VALUES['ratio.birth.male.to.female'] / (1+SHIELD_BASE_PARAMETER_VALUES['ratio.birth.male.to.female']))
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'birth.proportions',
                        value =0)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               applies.to = list(sex.to='msm'),
                               name = 'birth.proportions',
                               value = expression( prp.births.male*prp.msm.of.male ))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               applies.to = list(sex.to='heterosexual_male'),
                               name = 'birth.proportions',
                               value = expression( prp.births.male*(1-prp.msm.of.male)  ))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               applies.to = list(sex.to='female'),
                               name = 'birth.proportions',
                               value = expression( 1-prp.births.male ))
##---- Natality ----
register.natality(specification = SHIELD.SPECIFICATION,
                  parent.groups = c('infected', 'uninfected'),
                  applies.to = list(sex='female',age=FERTILE.AGES), #only women 15-44 give birth
                  child.groups = 'uninfected',
                  fertility.rate.value = 'fertility.rate',
                  birth.proportions.value = 'birth.proportions', # when they are born, where do they go? (e.g., what age, what race, what sex will they have)
                  parent.child.concordant.dimensions = c('race'),# the race of the cild will be the same
                  all.births.into.compartments = list(profile='susceptible',age= 1),
                  tag = 'births')

#*** MORTALITY *** ----
##---- General Mortality ----
register.model.element(SHIELD.SPECIFICATION,
                       name = 'rate.general.mortality',
                       get.functional.form.function = get.general.mortality.rates.functional.form,
                       scale = 'rate')
register.mortality(SHIELD.SPECIFICATION,
                   tag = 'general.mortality',
                   groups = c('infected','uninfected'),
                   mortality.rate.value = 'rate.general.mortality')
# ##---- Syphilis Mortality ----
# register.model.element.values(SHIELD.SPECIFICATION,
#                               'rate.syphilis.mortality.primary'=SHIELD_BASE_PARAMETER_VALUES['rate.syphilis.mortality.primary'],
#                               'rate.syphilis.mortality.secondary'=SHIELD_BASE_PARAMETER_VALUES['rate.syphilis.mortality.secondary'],
#                               'rate.syphilis.mortality.early.latent'=SHIELD_BASE_PARAMETER_VALUES['rate.syphilis.mortality.early.latent'],
#                               'rate.syphilis.mortality.late.latent'=SHIELD_BASE_PARAMETER_VALUES['rate.syphilis.mortality.late.latent'],
#                               'rate.syphilis.mortality.tertiary'=SHIELD_BASE_PARAMETER_VALUES['rate.syphilis.mortality.tertiary'],
#                               'rate.syphilis.mortality.cns'=SHIELD_BASE_PARAMETER_VALUES['rate.syphilis.mortality.cns'],
#                               scale = 'rate')
# register.model.quantity(SHIELD.SPECIFICATION,
#                         name = 'rate.syphilis.mortality',
#                         scale = 'rate',
#                         value = 0)
# register.model.quantity.subset(SHIELD.SPECIFICATION,
#                                applies.to = list(stage='primary'),  
#                                name = 'rate.syphilis.mortality',
#                                value = 'rate.syphilis.mortality.primary')
# register.model.quantity.subset(SHIELD.SPECIFICATION,
#                                applies.to = list(stage='secondary'),
#                                name = 'rate.syphilis.mortality',
#                                value = 'rate.syphilis.mortality.secondary')
# register.model.quantity.subset(SHIELD.SPECIFICATION,
#                                applies.to = list(stage='early.latent'),
#                                name = 'rate.syphilis.mortality',
#                                value = 'rate.syphilis.mortality.early.latent')
# register.model.quantity.subset(SHIELD.SPECIFICATION,
#                                applies.to = list(stage='late.latent'),
#                                name = 'rate.syphilis.mortality',
#                                value = 'rate.syphilis.mortality.late.latent')
# register.model.quantity.subset(SHIELD.SPECIFICATION,
#                                applies.to = list(stage='tertiary'),
#                                name = 'rate.syphilis.mortality',
#                                value = 'rate.syphilis.mortality.tertiary')
# register.model.quantity.subset(SHIELD.SPECIFICATION,
#                                applies.to = list(stage='cns'),
#                                name = 'rate.syphilis.mortality',
#                                value = 'rate.syphilis.mortality.cns')
# # register mortality:
# register.mortality(SHIELD.SPECIFICATION,
#                    tag = 'syphilis.mortality',
#                    groups = c('infected'), 
#                    mortality.rate.value = 'rate.syphilis.mortality')



#*** AGING *** ----
register.model.element(SHIELD.SPECIFICATION,
                       name = 'rate.general.aging',
                       scale = 'rate',
                       get.functional.form.function = get.empiric.aging.rates,
                       functional.form.from.time = DEFAULT.AGING.START.YEAR)  #2005: the projections remain fix at this year's value for years before. 

register.aging(SHIELD.SPECIFICATION,
               groups = c('uninfected','infected'),
               aging.rate.value = 'rate.general.aging')

#*** MIGRATION *** ----
# immigration/emigration doesn't depend on disease state. It is used to fit the population size but doesn't change the prevalence of the disease
# The MSA priors comes from census reports - This data more represent movements between MSA and not the immigration from other countries
# total immigration/population: prior is some fraction of this distributed equally by age and race
# the mechanism to model migrations in JHEEM is births- we need to decide which compartments newborn go to-null.proportion is used to describe the proportion of births that are concordant with the parent
##---- Immigration ----
register.model.element(SHIELD.SPECIFICATION,
                       name = 'rate.immigration',
                       get.functional.form.function = get.immigration.rates.functional.form,
                       functional.form.from.time = DEFAULT.MIGRATION.START.YEAR,  #2005: the projections remain fix at this year's value for years before. 
                       scale = 'rate')
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'null.proportions', 
                        value = 1)
register.natality(specification = SHIELD.SPECIFICATION,
                  parent.groups = 'uninfected',
                  child.groups = 'uninfected',
                  fertility.rate.value = 'rate.immigration',
                  birth.proportions.value = 'null.proportions', # because we're actually fixing all the strata below 
                  parent.child.concordant.dimensions = c('age','race','sex','profile'), 
                  all.births.into.compartments = list(), 
                  tag = "immigration")
# We use the natality mechanism to model immigration, but shouldnt count them in births
register.natality(specification = SHIELD.SPECIFICATION,
                  parent.groups = 'infected',
                  child.groups = 'infected',
                  fertility.rate.value = 'rate.immigration',
                  birth.proportions.value = 'null.proportions', # because we're actually fixing all the strata below 
                  parent.child.concordant.dimensions = c('age','race','sex','continuum','stage'),
                  all.births.into.compartments = list(),
                  tag = "immigration")

##---- Emigration ----
register.model.element(SHIELD.SPECIFICATION,
                       name = 'rate.emigration',
                       get.functional.form.function = get.emigration.rates.functional.form,
                       functional.form.from.time = DEFAULT.MIGRATION.START.YEAR, #2005: the projections remain fix at this year's value for years before. 
                       scale = 'rate')
# We use the mortality mechanism to model emigrations, but shouldnt count them in deaths
register.mortality(SHIELD.SPECIFICATION,
                   mortality.rate.value = "rate.emigration",
                   groups = c("uninfected","infected"), 
                   tag = "emigration")

#*** TRANSMISSION *** ----
# Transmission has 4 elements: 1.susceptibility, 2.transmissibility, 3.contact, 4.new infection proportion (where does new infection go to? e.g., infected PrEP, infected not on PrEP)
# e.g., model a flat transmission rate that applies to all groups
#each parameter requires a prior distribution
# 1- those that will be calibrated (calibrating dist)
# 2- those that wont be calibrated: e.g., jitters in the future (sampling dist)
register.transmission(SHIELD.SPECIFICATION,
                      contact.value = 'sexual.contact',
                      susceptibility.value = 'sexual.susceptibility',
                      transmissibility.value = 'sexual.transmissibility',
                      new.infection.proportions.value = 'new.infection.proportions',
                      tag = 'sexual.transmission',
                      new.infections.applies.to = list(continuum='undiagnosed',stage='primary'))
##---- Sexual Contact ----
register.model.element(SHIELD.SPECIFICATION,
                       name='global.transmission.rate',
                       scale = 'rate',
                       value = 1 ) #tuned in calib_parameters

# rate of contact between infected and uninfected
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'sexual.contact',
                        value = expression(global.transmission.rate *
                                               rate.sexual.transmission *
                                               sexual.contact.matrix))

register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'sexual.contact.matrix',
                        value = expression(sexual.contact.by.age*
                                               sexual.contact.by.sex*
                                               sexual.contact.by.race ))

##---- Sexual Contact: Transmission Rates ----
# probability of transmission through sexual act, it depends on the recipient, person getting infected
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'rate.sexual.transmission',
                        value = 0)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'rate.sexual.transmission',
                               applies.to = list(sex.from=c('heterosexual_male','msm'),
                                                 sex.to=c('heterosexual_male','msm')),
                               value = 'transmission.rate.msm') #we can add msm.peak.multiplier later if needed
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'rate.sexual.transmission',
                               applies.to = list(sex.from=c('heterosexual_male','msm'),
                                                 sex.to=c('female')),
                               value = 'transmission.rate.heterosexual')
register.model.quantity.subset(SHIELD.SPECIFICATION, #right now it's assuming that female to male is the same as male to female
                               name = 'rate.sexual.transmission',
                               applies.to = list(sex.from=c('female'),
                                                 sex.to=c('heterosexual_male','msm')),
                               value = 'transmission.rate.heterosexual')
#spline models can project negative values - we should truncate to 0
# use the log scale: exponentiate the values # log scale for the knots,
# we define this as a spline with 2 knots, but we have no rpior about their values. they will change in calibration
# Future Expnasions: msm.trate.by.race #add more alphas #if we assume the multiplier by black vs other remain the sma over time we only ned 2
register.model.element(SHIELD.SPECIFICATION,
                       name = 'transmission.rate.msm',
                       scale='rate',
                       functional.form.from.time = 1970, 
                       functional.form = create.natural.spline.functional.form(
                           knot.times =c("1970"=1970, "1990"=1990, "1995"=1995, "2000"=2000, "2010"=2010,"2020"=2020),
                           knot.values =list("1970"=0, "1990"=0, "1995"=0, "2000"=0, "2010"=0,"2020"=0),
                           knots.are.on.transformed.scale = T, #knots on the log scale (value is exp(0))
                           knot.link = 'log',
                           link='identity', #linear projections between the knots     
                           #
                           after.time = 2030, #values between 2020-2030 are scaled down to change up to 50% of modeled channge between 2010-2020
                           after.modifier = 0.5,
                           after.modifier.increasing.change.link = 'identity',
                           after.modifier.decreasing.change.link = 'log',
                           modifiers.apply.to.change = T,
                           min=0 #even after using log for knots, value can be negative so we need to truncate
                       )
) 
#if we wanted to use a natural spline without log transformation:
# knot.values = list(time0=1,time1=1,time2=1) , knots.are.on.transformed.scale = F, #on the identity scale
#use a log(y) transformation, so that all returned values are positive
# this will also help with data that is skewed to right (long right tail)

register.model.element(SHIELD.SPECIFICATION,
                       name = 'transmission.rate.heterosexual',
                       functional.form.from.time = 1970, 
                       scale='rate',
                       functional.form = create.natural.spline.functional.form(knot.times =c("1970"=1970,"1990"=1990, "1995"=1995, "2000"=2000, "2010"=2010,"2020"=2020),
                                                                               knot.values =list("1970"=0, "1990"=0, "1995"=0, "2000"=0, "2010"=0,"2020"=0),
                                                                               knots.are.on.transformed.scale = T, #knots on the log scale (value is exp(0))
                                                                               min=0,
                                                                               knot.link = 'log',
                                                                               link='identity',
                                                                               after.time = 2030, #values between 2020-2030 are scaled down to change up to 50% of modeled change between 2010-2020
                                                                               after.modifier = 0.5,
                                                                               after.modifier.increasing.change.link = 'identity',
                                                                               after.modifier.decreasing.change.link = 'log',
                                                                               modifiers.apply.to.change = T)
)

##---- Sexual Contact: By AGE ----
# builds an empty sexual contact matrix and applies it to each group based on who they have sexual contact with
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'sexual.contact.by.age',
                        value = 0)
# sexual contact by age for females who have sex with male partners
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.contact.by.age',
                               applies.to = list(sex.to='female',
                                                 sex.from=c('heterosexual_male','msm')),
                               value = get.female.sexual.age.contact.proportions)

# sexual contact by age for males who have sex with male partners
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.contact.by.age',
                               applies.to = list(sex.to=c('heterosexual_male','msm'),
                                                 sex.from=c('heterosexual_male','msm')),
                               value = get.msm.sexual.age.contact.proportions)
#
# x=get.msm.sexual.age.contact.proportions(specification.metadata = get.specification.metadata('shield',location = 'C.12580'),
#                                        single.year.msm.age.counts = get.msm.single.year.age.counts(location = 'C.12580',specification.metadata = get.specification.metadata('shield',location = 'C.12580'),population.years = 2007),
#                                        single.year.age.sexual.availability = get.sexual.availability(),age.mixing.sd.mult = 1)
# sexual contact by age for males who have sex with female partners
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.contact.by.age',
                               applies.to = list(sex.to=c('heterosexual_male','msm'),
                                                 sex.from='female'),
                               value = get.heterosexual_male.sexual.age.contact.proportions) # this is using a function to calculate the value of a quantity (subset) at run-time

# from males...to females...
# from (HIV+) to susceptible
# age.from, sex.from, age.to, sex.to
#
# sex.from female. age 19 HIV+
# sex.to hetrosexual male age.to 19
# for each person at risk of HIV infecyon, what proportion of contact sar form diff groups
#> females, msms, het males
#> for female: get.heterosexual_male.sexual.age.contact.proportions
#> msm, males: get.msm.sexual.age.contact.proportions
#what's the number of new trans in each startutm: number of new transmision to each statum * size
# the proportion of partners in this stratum * prev of HIV in this stratum
#all of the components going into a a function for a quantity have to be quantities
# we need this as an input to the get.....sexual.age.contact.proportions functions above
register.model.element(SHIELD.SPECIFICATION,
                       name = 'age.mixing.sd.mult',
                       value = 1,
                       scale='ratio')

# pulls single year age counts for females based on population proportion
register.model.element(SHIELD.SPECIFICATION,
                       name = 'single.year.female.age.counts',
                       get.value.function = get.female.single.year.age.counts,
                       dimension.values = list(age=CENSUS.AGES),
                       resolve.dimension.values.against.model = F,
                       scale='non.negative.number')
register.model.element(SHIELD.SPECIFICATION,
                       name = 'single.year.msm.age.counts',
                       get.value.function = get.msm.single.year.age.counts,
                       dimension.values = list(age=CENSUS.AGES),
                       resolve.dimension.values.against.model = F,
                       scale='non.negative.number')
register.model.element(SHIELD.SPECIFICATION,
                       name = 'single.year.heterosexual_male.age.counts',
                       get.value.function = get.heterosexual_male.single.year.age.counts,
                       dimension.values = list(age=CENSUS.AGES),
                       resolve.dimension.values.against.model = F,
                       scale='non.negative.number')

# pulls propotion of population of people in each age bucket that are sexually available
register.model.element(SHIELD.SPECIFICATION,
                       name = 'single.year.age.sexual.availability',
                       value = get.sexual.availability(),
                       dimension.values = list(age=CENSUS.AGES),
                       resolve.dimension.values.against.model = F,
                       scale='non.negative.number')

##---- Sexual Contact: By SEX ----
# Set up elements
register.model.element(SHIELD.SPECIFICATION,
                       name = 'oe.female.pairings.with.msm',
                       value = SHIELD_BASE_PARAMETER_VALUES['oe.female.pairings.with.msm'],
                       scale = 'ratio')

register.model.element(SHIELD.SPECIFICATION,
                       name = 'fraction.heterosexual_male.pairings.with.male',
                       value = SHIELD_BASE_PARAMETER_VALUES['fraction.heterosexual_male.pairings.with.male'],
                       scale = 'ratio')

register.model.element(SHIELD.SPECIFICATION,
                       name = 'fraction.msm.pairings.with.female',
                       value = mean(PAIRING.INPUT.MANAGER$msm.sex.with.female.estimates),
                       scale = 'ratio')
###
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'fraction.male.male.that.are.with.msm',
                        value = expression(prp.msm.of.male /
                                               (prp.msm.of.male +
                                                    (1-prp.msm.of.male) * fraction.heterosexual_male.pairings.with.male))
)
###
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'sexual.contact.by.sex',
                        value = 0)
# To female from het
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.contact.by.sex',
                               applies.to=list(sex.from='heterosexual_male',
                                               sex.to='female'),
                               value = expression((1-prp.msm.of.male)/
                                                      (1-prp.msm.of.male +
                                                           prp.msm.of.male * oe.female.pairings.with.msm))
)
# To female from msm
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.contact.by.sex',
                               applies.to=list(sex.from='msm',
                                               sex.to='female'),
                               value = expression(prp.msm.of.male * oe.female.pairings.with.msm/
                                                      (1-prp.msm.of.male +
                                                           prp.msm.of.male * oe.female.pairings.with.msm))
)
# To MSM from female, msm, het male
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.contact.by.sex',
                               applies.to=list(sex.from='female',
                                               sex.to='msm'),
                               value = 'fraction.msm.pairings.with.female')
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.contact.by.sex',
                               applies.to=list(sex.from='msm',
                                               sex.to='msm'),
                               value = expression((1-fraction.msm.pairings.with.female) *
                                                      fraction.male.male.that.are.with.msm))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.contact.by.sex',
                               applies.to=list(sex.from='heterosexual_male',
                                               sex.to='msm'),
                               value = expression((1-fraction.msm.pairings.with.female) *
                                                      (1-fraction.male.male.that.are.with.msm))
)
# To heterosexual male from female and msm and other het male
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.contact.by.sex',
                               applies.to=list(sex.from='female',
                                               sex.to='heterosexual_male'),
                               value = expression((1-fraction.heterosexual_male.pairings.with.male))
)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.contact.by.sex',
                               applies.to=list(sex.from='msm',
                                               sex.to='heterosexual_male'),
                               value = expression(fraction.heterosexual_male.pairings.with.male *
                                                      fraction.male.male.that.are.with.msm)
)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.contact.by.sex',
                               applies.to=list(sex.from='heterosexual_male',
                                               sex.to='heterosexual_male'),
                               value = expression(fraction.heterosexual_male.pairings.with.male *
                                                      (1-fraction.male.male.that.are.with.msm))
)

# Susceptibility of uninfected persons
##---- Sexual Contact: By RACE ----
# oes for racial mixing with the same sex are estiamted from 4 studies. we assume equally likely mixing with other groups
baseline.sexual.oes = array(c(SHIELD_BASE_PARAMETER_VALUES['oe.sexual.byrace.bb'],1,1,
                              1,SHIELD_BASE_PARAMETER_VALUES['oe.sexual.byrace.hh'],1,
                              1,1,SHIELD_BASE_PARAMETER_VALUES['oe.sexual.byrace.oo']),
                            dim = c(race.to=3, race.from=3),
                            dimnames = list(race.to=c('black','hispanic','other'),
                                            race.from=c('black','hispanic','other')))

register.model.element(
    SHIELD.SPECIFICATION,
    name = 'base.race.sexual.oes',
    scale = 'ratio',
    get.functional.form.function = get.geographically.aggregated.race.oes,
    within.county.race.oes = baseline.sexual.oes,
    dimension.values = list(
        race.to   = c('black','hispanic','other'),
        race.from = c('black','hispanic','other')
    )
)

register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'sexual.contact.by.race',
                        value = sexual.oes.to.contact.proportions)


register.model.element(SHIELD.SPECIFICATION,
                       name = 'black.black.sexual.multi',
                       scale = 'ratio',
                       value = 1
)

register.model.element(SHIELD.SPECIFICATION,
                       name = 'hispanic.hispanic.sexual.multi',
                       scale = 'ratio',
                       value = 1)

register.model.element(SHIELD.SPECIFICATION,
                       name = 'other.other.sexual.multi',
                       scale = 'ratio',
                       value = 1)

register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'race.sexual.oes',
                        value = "base.race.sexual.oes")

register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'race.sexual.oes',
                               value = 'black.black.sexual.multi',
                               applies.to = list(race.to = "black", race.from = "black"),
                               apply.function = "multiply")

register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'race.sexual.oes',
                               value = 'hispanic.hispanic.sexual.multi',
                               applies.to = list(race.to = "hispanic", race.from = "hispanic"),
                               apply.function = "multiply")

register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'race.sexual.oes',
                               value = 'other.other.sexual.multi',
                               applies.to = list(race.to = "other", race.from = "other"),
                               apply.function = "multiply")

# The race sub-values
register.model.element(SHIELD.SPECIFICATION,
                       name='race.population.counts',
                       scale = 'non.negative.number',
                       get.value.function = get.race.population.counts)



##---- DOXY PEP ----
# relative risk: default=1 no protection
# Susceptibility of uninfected persons

register.model.element(SHIELD.SPECIFICATION,
                       name = 'doxy.rr',
                       scale = 'ratio',
                       value = 1
)

register.model.element(SHIELD.SPECIFICATION,
                       name = 'doxy.coverage',
                       scale = 'ratio',
                       value = 1
)

# register.model.element(
#     SHIELD.SPECIFICATION,
#     name = 'doxy.coverage',
#     scale = 'proportion',
#     functional.form = create.logistic.linear.functional.form(
#                                                     intercept = 0, # fixed logit-scale intercept
#                                                     slope = 0,
#                                                     anchor.year = 2021, # logistic curve “starts” here
#                                                     #max = ... ,
#                                                     parameters.are.on.logit.scale = TRUE
#     ),
#     functional.form.from.time = 2021,  
#     functional.form.to.time   = 2025
# )

register.model.quantity(SHIELD.SPECIFICATION,
                               name = 'sexual.susceptibility',
                               value = 1
)


## MSM: apply doxy coverage + RR
## this is the ONLY place doxy.coverage & doxy.rr are used
register.model.quantity.subset(SHIELD.SPECIFICATION,
                        name = 'sexual.susceptibility',
                        applies.to = list(sex = 'msm'),
                        value = expression((1 - doxy.coverage) + doxy.coverage * doxy.rr)
)


## Infectiousness ----
# Secondary stage has max infection, the primary and EL infectiousness is set as a ratio relative to secondary
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'sexual.transmissibility',
                        value = 0)
register.model.element(SHIELD.SPECIFICATION,
                       name = 'secondary.transmissibility',
                       scale = 'non.negative.number',
                       value = SHIELD_BASE_PARAMETER_VALUES['secondary.transmissibility'])
register.model.element(SHIELD.SPECIFICATION,
                       name = 'primary.rel.secondary.transmissibility',
                       scale = 'ratio',
                       value = SHIELD_BASE_PARAMETER_VALUES['primary.rel.secondary.transmissibility'])
register.model.element(SHIELD.SPECIFICATION,
                       name = 'el.rel.secondary.transmissibility',
                       scale = 'ratio',
                       value = SHIELD_BASE_PARAMETER_VALUES['el.rel.secondary.transmissibility'])
##apply:
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.transmissibility',
                               applies.to=list(stage='secondary'),
                               value = 'secondary.transmissibility')
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.transmissibility',
                               applies.to=list(stage='primary'),
                               value = expression(primary.rel.secondary.transmissibility*secondary.transmissibility))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.transmissibility',
                               applies.to=list(stage='early.latent'),
                               value = expression(el.rel.secondary.transmissibility * secondary.transmissibility))

## where do new infections go to? ----
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'new.infection.proportions',
                        value = 1)


##--------------------------------------------------------------------------------------------------------------#


#*** SYPHILIS NATURAL HISTORY *** ----
##---- Stage Transitions ----
###----- Primary > Secondary > EL-----
register.model.element(SHIELD.SPECIFICATION,
                       name = 'duration.primary',
                       scale = 'time',
                       value = SHIELD_BASE_PARAMETER_VALUES['duration.primary'])
register.model.element(SHIELD.SPECIFICATION,
                       name = 'duration.secondary',
                       scale = 'time',
                       value = SHIELD_BASE_PARAMETER_VALUES['duration.secondary'])
register.model.element(SHIELD.SPECIFICATION,
                       name = 'duration.early.latent',
                       scale = 'time',
                       value = SHIELD_BASE_PARAMETER_VALUES['duration.early.latent'])

# progression to next stage:
register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = 'primary',
                    to.compartments = 'secondary',
                    value =expression(1/duration.primary) , 
                    tag = 'progression.primary.secondary')
register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = 'secondary',
                    to.compartments = 'early.latent',
                    value = expression(1/duration.secondary),
                    tag = 'progression.secondary.el')

###---- Relapse ---- (Early Latent to Secondary):
register.model.element(SHIELD.SPECIFICATION,
                       name = 'prop.early.latent.to.secondary',
                       scale = 'proportion',
                       value = SHIELD_BASE_PARAMETER_VALUES['prop.early.latent.to.secondary']
)


register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = 'early.latent',
                    to.compartments = 'secondary',
                    value = expression(prop.early.latent.to.secondary * (1/duration.early.latent)),
                    tag = 'progression.el.secondary'
)

###---- EL to LL Progression -----
register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = 'early.latent',
                    to.compartments = 'late.latent',
                    value = expression((1-prop.early.latent.to.secondary) * (1/duration.early.latent)),#those who dont relapse
                    tag = 'progression.el.ll'
)

###---- LL to Tertiary (by sex) -----
register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = 'late.latent',
                    to.compartments = 'tertiary',
                    value = 'rate.late.latent.to.tertiary',
                    tag = 'progression.ll.tertiary'
)
register.model.element(SHIELD.SPECIFICATION,
                       name = 'rate.late.latent.to.tertiary.male',
                       scale = 'rate',
                       value = SHIELD_BASE_PARAMETER_VALUES['rate.late.latent.to.tertiary.male'])
register.model.element(SHIELD.SPECIFICATION,
                       name = 'rate.late.latent.to.tertiary.female',
                       scale = 'rate',
                       value = SHIELD_BASE_PARAMETER_VALUES['rate.late.latent.to.tertiary.female'])
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'rate.late.latent.to.tertiary',
                        value = 'rate.late.latent.to.tertiary.male')  
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'rate.late.latent.to.tertiary',
                               applies.to = list(sex='female'),
                               value = 'rate.late.latent.to.tertiary.female')
###---- EL to CNS (by sex) -----
register.model.element(SHIELD.SPECIFICATION,
                       name = 'rate.early.stage.to.cns',
                       scale = 'rate',
                       value = SHIELD_BASE_PARAMETER_VALUES['rate.early.stage.to.cns']) #* can add multiplier to sample 
register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = c('primary'),
                    to.compartments = 'cns',
                    value = 'rate.early.stage.to.cns',
                    tag = 'progression.primary.cns')
register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = c('secondary'),
                    to.compartments = 'cns',
                    value = 'rate.early.stage.to.cns',
                    tag = 'progression.secondary.cns')
register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = c('early.latent'),
                    to.compartments = 'cns',
                    value = 'rate.early.stage.to.cns',
                    tag = 'progression.el.cns')
###---- LL to CNS (by sex) -----
register.model.element(SHIELD.SPECIFICATION,
                       name = 'rate.late.latent.to.cns.male',
                       scale = 'rate',
                       value = SHIELD_BASE_PARAMETER_VALUES['rate.late.latent.to.cns.male']) 
register.model.element(SHIELD.SPECIFICATION,
                       name = 'rate.late.latent.to.cns.female',
                       scale = 'rate',
                       value = SHIELD_BASE_PARAMETER_VALUES['rate.late.latent.to.cns.female']) 
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'rate.late.latent.to.cns',
                        value = 'rate.late.latent.to.cns.male')  
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'rate.late.latent.to.cns',
                               applies.to = list(sex='female'),
                               value = 'rate.late.latent.to.cns.female')
register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = 'late.latent',
                    to.compartments = 'cns',
                    value = 'rate.late.latent.to.cns',
                    tag = 'progression.ll.cns')
#*** CONTINUUM TRANSISION *** ----
##---- 1-SYMPTHOMATIC TESTING ----
# Register "Effective" symptomatic proportions for primary and secondary stages
# We assume 100% care seeking and testing rate for "symptomatic" cases in Primary&Secondary Stage (effective proportion) 
# Those who don’t notice the symptoms or don’t seek care based on symptoms are treated as asymptomatic.
# We assume 100% care/seeking and testing rate for symptomatic individuals
base.prp.symptomatic.primary = array(
    c(SHIELD_BASE_PARAMETER_VALUES['prp.symptomatic.primary.heterosexual_male.est'],
      SHIELD_BASE_PARAMETER_VALUES['prp.symptomatic.primary.female.est'],
      SHIELD_BASE_PARAMETER_VALUES['prp.symptomatic.primary.msm.est']
    ),dim = c(sex = 3),dimnames = list(sex = c("heterosexual_male","female","msm")))

register.model.element(SHIELD.SPECIFICATION,
                       name = "prp.symptomatic.primary",
                       scale = 'proportion',
                       functional.form.from.time = 1970, 
                       functional.form = create.natural.spline.functional.form(
                           knot.times = c("1970"=1970,  "1990" = 1990, "1995" = 1995,"2000"=2000, "2010"=2010, "2020" = 2020),
                           knot.values = list("1970" = base.prp.symptomatic.primary,
                                              "1990" = base.prp.symptomatic.primary,
                                              "1995" = base.prp.symptomatic.primary,
                                              "2000" =  base.prp.symptomatic.primary,
                                              "2010" =  base.prp.symptomatic.primary,
                                              "2020" =  base.prp.symptomatic.primary),
                           #
                           knots.are.on.transformed.scale = F,
                           knot.link = "logit", #I think that the knots multipliers are in the logit scale, right? 
                           link="logit",
                           #
                           min=0, #do we need these for the proportion? 
                           max=1
                           #
                           
                       ))
base.prp.symptomatic.secondary = array(
    c(SHIELD_BASE_PARAMETER_VALUES['prp.symptomatic.secondary.heterosexual_male.est'],
      SHIELD_BASE_PARAMETER_VALUES['prp.symptomatic.secondary.female.est'],
      SHIELD_BASE_PARAMETER_VALUES['prp.symptomatic.secondary.msm.est']
    ),dim = c(sex = 3),dimnames = list(sex = c("heterosexual_male","female","msm")))

register.model.element(SHIELD.SPECIFICATION,
                       name = "prp.symptomatic.secondary",
                       scale = 'proportion',
                       functional.form.from.time = 1970, 
                       functional.form = create.natural.spline.functional.form(
                           knot.times = c("1970"=1970, "1990" = 1990, "1995" = 1995,"2000"=2000, "2010"=2010, "2020" = 2020),
                           knot.values = list("1970" = base.prp.symptomatic.secondary,
                                              "1990" = base.prp.symptomatic.secondary,
                                              "1995" = base.prp.symptomatic.secondary,
                                              "2000" =  base.prp.symptomatic.secondary,
                                              "2010" =  base.prp.symptomatic.secondary,
                                              "2020" =  base.prp.symptomatic.secondary),
                           knots.are.on.transformed.scale = F,
                           knot.link = "logit", #I think that the knots multipliers are in the logit scale, right? 
                           link="logit",
                           #
                           min=0, #do we need these for the proportion? 
                           max=1
                       ))

# For now, we assume 100% test.sensitivity as well. 
# proportion of symp.testing that are successfully diagnosed: 
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'prp.diagnosed.sym.testing.primary',
                        scale = 'proportion',
                        value =0)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'prp.diagnosed.sym.testing.primary',
                               applies.to = list(stage='primary'),
                               value = 'prp.symptomatic.primary') # can also add: *sensitivity of test in primary * care seeking behavior
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'prp.diagnosed.sym.testing.secondary',
                        scale = 'proportion',
                        value =0)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'prp.diagnosed.sym.testing.secondary',
                               applies.to = list(stage='secondary'),
                               value = 'prp.symptomatic.secondary') # can also add: *sensitivity of test in primary * care seeking behavior

#  Testing is modeled "during the stage", i.e. at the same time as the stage is active. 
#  r= -log(1-p)/t
# we only calculate the rate here, and model the transitions below 
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'rate.testing.symptomatic',
                        scale = 'rate',
                        value = 0)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'rate.testing.symptomatic',
                               applies.to = list(stage='primary'),
                               value = expression(-log(1-prp.diagnosed.sym.testing.primary)/duration.primary))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'rate.testing.symptomatic',
                               applies.to = list(stage='secondary'),
                               value = expression(-log(1-prp.diagnosed.sym.testing.secondary)/duration.secondary))
register.model.element(SHIELD.SPECIFICATION,
                       name = 'duration.tertiary',
                       scale = 'non.negative.number',
                       value =SHIELD_BASE_PARAMETER_VALUES['duration.tertiary'] )
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'rate.testing.symptomatic',
                               applies.to = list(stage=c('tertiary')),
                               value =expression(1/duration.tertiary))
register.model.element(SHIELD.SPECIFICATION,
                       name = 'duration.cns',
                       scale = 'rate',
                       value =SHIELD_BASE_PARAMETER_VALUES['duration.cns'] )
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'rate.testing.symptomatic',
                               applies.to = list(stage=c('cns')),
                               value =expression(1/duration.cns))


##---- 2-SCREENING FOR ALL ----
# We estimate this from HIV Testing rates in JHEEM : background testing rate has a functional form and parameters are tuned 
# we will calibrate this to testing data, and assume that STI screening rate is a function of HIV rate
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'rate.testing.hiv.without.covid',
                        scale = 'rate',
                        value=0)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'rate.testing.hiv.without.covid',
                               applies.to = list(age=c("15-19 years" ,"20-24 years" ,"25-29 years" ,"30-34 years", "35-39 years", "40-44 years", "45-49 years" ,"50-54 years", "55-64 years" ,"65+ years"  )),
                               value='rate.testing.hiv.without.covid.over.14')
register.model.element(SHIELD.SPECIFICATION,
                       name = 'rate.testing.hiv.without.covid.over.14',
                       scale = 'rate',
                       get.functional.form.function = get.hiv.testing.functional.form,
                       functional.form.scale = 'proportion', #the functional form takes a proportion and produces a rate (from BRFSS: have u had a hiv test in the last year?)
                       functional.form.from.time = DEFAULT.HIV.TESTING.START.YEAR) #2010: the projections remain fix at this year's value for years before. 
register.model.quantity(SHIELD.SPECIFICATION, 
                        name = 'rate.testing.hiv',
                        scale = 'rate',
                        value = 'rate.testing.hiv.without.covid')

# defining a STI multiplier over time:
# function to map the ratio of STI tests relative to hiv.tests in the US (for STI screening)
register.model.element(SHIELD.SPECIFICATION,
                       name = 'multiplier.syphilis.screening.to.hiv.tests',
                       scale = 'ratio',
                       functional.form.from.time = 1970,  #the projections remain fix at this year's value for years before.
                       functional.form = create.natural.spline.functional.form(
                           knot.times = c("1970"=1970, "1990"=1990,"1995"=1995, "2000"=2000, "2010"=2010,"2020"=2020),
                           knot.values=list("1970"=1, "1990"=1,"1995"=1, "2000"=1, "2010"=1,"2020"=1),  
                           knots.are.on.transformed.scale = F,
                           knot.link = "log",
                           link = "identity", #linear projections between the knots (avoid exponential growth)
                           #
                           knot.min = 0, #knot values can not fall below 0
                           min=0, #projected spline values can not fall below 0
                           after.time = 2030, #values between 2020-2030 are scaled down to change up to 50% of modeled change between 2010-2020
                           after.modifier = 0.5,
                           after.modifier.increasing.change.link = 'identity',
                           after.modifier.decreasing.change.link = 'log',
                           modifiers.apply.to.change = T
                       ) 
) 
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'rate.sti.screening',
                        scale = 'rate',
                        value = expression(rate.testing.hiv * 
                                               multiplier.syphilis.screening.to.hiv.tests *
                                               sti.screening.by.stage))

# additional STI multiplier by stage 
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'sti.screening.by.stage',
                        scale = 'ratio',
                        value = 1)
register.model.element(SHIELD.SPECIFICATION,
                       name = 'sti.screening.multiplier.ps',
                       scale = 'ratio',
                       value = SHIELD_BASE_PARAMETER_VALUES['sti.screening.multiplier.ps'])
register.model.element(SHIELD.SPECIFICATION,
                       name = 'sti.screening.multiplier.el',
                       scale = 'ratio',
                       value = SHIELD_BASE_PARAMETER_VALUES['sti.screening.multiplier.el'])
register.model.element(SHIELD.SPECIFICATION,
                       name = 'sti.screening.multiplier.ll',
                       scale = 'ratio',
                       value = SHIELD_BASE_PARAMETER_VALUES['sti.screening.multiplier.ll'])
register.model.element(SHIELD.SPECIFICATION,
                       name = 'sti.screening.multiplier.tertiary',
                       scale = 'ratio',
                       value = SHIELD_BASE_PARAMETER_VALUES['sti.screening.multiplier.tertiary'])
register.model.element(SHIELD.SPECIFICATION,
                       name = 'sti.screening.multiplier.cns',
                       scale = 'ratio',
                       value = SHIELD_BASE_PARAMETER_VALUES['sti.screening.multiplier.cns'])


register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sti.screening.by.stage',
                               applies.to = list(stage = c("primary", "secondary")),
                               value = 'sti.screening.multiplier.ps')
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sti.screening.by.stage',
                               applies.to = list(stage = c("early.latent")),
                               value = 'sti.screening.multiplier.el')
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sti.screening.by.stage',
                               applies.to = list(stage = c("late.latent")),
                               value = 'sti.screening.multiplier.ll')
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sti.screening.by.stage',
                               applies.to = list(stage = c("tertiary")),
                               value = 'sti.screening.multiplier.tertiary')
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sti.screening.by.stage',
                               applies.to = list(stage = c("cns")),
                               value = 'sti.screening.multiplier.cns')


##---- 3-PRENATAL SCREENING FOR PREGNANT WOMEN ----
# prop of pregnant women receiving 'successful' prenatal screening 
#'@PK:TBD: How to model treatment failures that still result in congenital syphilis? 
register.model.element(SHIELD.SPECIFICATION,
                       name = 'b.model.prenatal.care',
                       scale = 'non.negative.number',
                       value = SHIELD_BASE_PARAMETER_VALUES['b.model.prenatal.care']) #Boolean switch to turn on/off prenatal care

register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'prp.received.prenatal.care',
                        scale = 'proportion',
                        value = expression(b.model.prenatal.care*(prp.prenatal.care.first.trimester+prp.prenatal.care.second.trimester+prp.prenatal.care.third.trimester)))
register.model.element(SHIELD.SPECIFICATION,
                       name = 'prp.births.multi.born',
                       scale = 'proportion',
                       value = SHIELD_BASE_PARAMETER_VALUES['prp.births.multi.born'])
# This rate is not applied to fertile women yet. we do that below when we are using it: 
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'rate.prenatal.care',
                        scale = 'rate',
                        value = expression( fertility.rate/(1+prp.births.multi.born) * prp.received.prenatal.care)) # we factor multi-births into this, added zero 

##---- 4- CONTACT TRACING ----
# number of contacts traced among those diagnosed with early syphilis 
# how many diagnosis are generated via contact tracing of a new diagnosis.... variable by age, race... 
# rate of diagnosis through CT= rate of diagnosis at baseline * average number of new diagnosis per index case

# average number of new cases diagnosed treated per index case. + # diagnosed but not treated
# number of people empirically treated (who have syphilis) + number diagnosed/treated 
# number of diagnosis by stage #now  we need to split this by age race sex

# contact tracing cascade: 
register.model.element(SHIELD.SPECIFICATION,
                       name = 'prop.index.cases.reached.for.contact.tracing',
                       scale = 'proportion',
                       value = SHIELD_BASE_PARAMETER_VALUES['prop.index.cases.reached.for.contact.tracing'])
register.model.element(SHIELD.SPECIFICATION,
                       name = 'contacts.diagnosed.treated.per.index.case',
                       scale = 'ratio',
                       value = SHIELD_BASE_PARAMETER_VALUES['contacts.diagnosed.treated.per.index.case'])
register.model.element(SHIELD.SPECIFICATION,
                       name = 'contacts.empirically.treated.infected.per.index.case',
                       scale = 'ratio',
                       value = SHIELD_BASE_PARAMETER_VALUES['contacts.empirically.treated.infected.per.index.case'])

# stage of infection among newly diagnosed contacts: 
register.model.element.values(SHIELD.SPECIFICATION,
                              'prp.infected.contacts.in.primary' = SHIELD_BASE_PARAMETER_VALUES['prp.infected.contacts.in.primary'],
                              'prp.infected.contacts.in.secondary' = SHIELD_BASE_PARAMETER_VALUES['prp.infected.contacts.in.secondary'],
                              'prp.infected.contacts.in.early.latent' = SHIELD_BASE_PARAMETER_VALUES['prp.infected.contacts.in.early.latent'], 
                              scale = 'proportion')
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'prp.infected.contacts.in.late.latent',
                        scale = 'proportion',
                        value = expression(1-prp.infected.contacts.in.primary-prp.infected.contacts.in.secondary-prp.infected.contacts.in.early.latent))

register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'prp.infected.contacts.by.stage',
                        value = 0)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name ='prp.infected.contacts.by.stage',
                               applies.to = list(stage='primary'),
                               value = 'prp.infected.contacts.in.primary')
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name ='prp.infected.contacts.by.stage',
                               applies.to = list(stage='secondary'),
                               value = 'prp.infected.contacts.in.secondary')
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name ='prp.infected.contacts.by.stage',
                               applies.to = list(stage='early.latent'),
                               value = 'prp.infected.contacts.in.early.latent')
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name ='prp.infected.contacts.by.stage',
                               applies.to = list(stage='late.latent'),
                               value = 'prp.infected.contacts.in.late.latent')

# Calculating index cases diagnosis rates that can trigger contact tracing for those indexes:  
# a function of symp.testing, sti.screening and prenatal.screening.
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'index.case.diagnosis.rate',
                        scale='rate',
                        dimensions = c('location', 'age','race','sex','stage'),
                        value = 0)
register.model.quantity.subset(SHIELD.SPECIFICATION, 
                               name = 'index.case.diagnosis.rate',
                               applies.to = list(sex=c('msm','heterosexual_male'),age='all.ages', stage=c('primary','secondary','early.latent')),
                               value = expression(rate.testing.symptomatic + rate.sti.screening ))
register.model.quantity.subset(SHIELD.SPECIFICATION, 
                               name = 'index.case.diagnosis.rate',
                               applies.to = list(sex=c('female'),age=NON.FERTILE.AGES, stage=c('primary','secondary','early.latent')),
                               value = expression(rate.testing.symptomatic + rate.sti.screening ))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'index.case.diagnosis.rate',
                               applies.to = list(sex='female',age=FERTILE.AGES,stage=c('primary','secondary','early.latent')),
                               value =expression(rate.testing.symptomatic + rate.sti.screening + rate.prenatal.care))

# Mapping contact tracing rates from index cases to their contacts:
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'rate.of.contacts.per.case',
                        value = get.rate.of.contacts.per.case)
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'rate.infected.contacts.diagnosed.treated',
                        scale='rate',
                        value = expression(rate.of.contacts.per.case * 
                                               prop.index.cases.reached.for.contact.tracing* 
                                               contacts.diagnosed.treated.per.index.case * 
                                               prp.infected.contacts.by.stage))
register.model.quantity(SHIELD.SPECIFICATION,  
                        name = 'rate.infected.contacts.empirically.treated',
                        scale='rate',
                        value = expression(rate.of.contacts.per.case * 
                                               prop.index.cases.reached.for.contact.tracing*
                                               contacts.empirically.treated.infected.per.index.case *
                                               prp.infected.contacts.by.stage))

#*** TREATMENT *** --#----
# a proportion will receive immediate treatment, another group will be delayed
# we need to separate this for each testing because the treatment is different


# ##---- Time-varying treatment multiplier (applies to all trt routes) ----
# 
# register.model.element(
#     SHIELD.SPECIFICATION,
#     name  = 'treatment.multiplier',
#     scale = 'ratio',
#     functional.form.from.time = 1970,
#     functional.form = create.natural.spline.functional.form(
#         knot.times  = c("1970"=1970, "1990"=1990, "1995"=1995, "2000"=2000, "2010"=2010, "2020"=2020, "2030"=2030),
#         knot.values = list("1970"=0,   "1990"=0,  "1995"=0,   "2000"=0,   "2010"=0,   "2020"=0,   "2030"=0),
#         knots.are.on.transformed.scale = TRUE, 
#         knot.link = 'log',
#         link = 'identity',
#         min = 0    # keep multiplier non-negative after transform
#     )
# )

##---- Immediate Treatments ----
register.model.element(SHIELD.SPECIFICATION,
                       name = 'prp.treated.immediately.following.screening',
                       scale = 'proportion',
                       value = SHIELD_BASE_PARAMETER_VALUES['prp.treated.immediately.following.screening'])

register.model.element(SHIELD.SPECIFICATION,
                       name = 'prp.treated.immediately.following.testing.symptomatic',
                       scale = 'proportion',
                       value = SHIELD_BASE_PARAMETER_VALUES['prp.treated.immediately.following.testing.symptomatic']) 

# All immediate treatments except prenatal: #1-testing, 2-sti.screeing, 3-contact tracing
# & then adding all those receiving prenatal screening
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'rate.diagnosis.immediate.treatment',
                        value = expression(rate.testing.symptomatic * prp.treated.immediately.following.testing.symptomatic + 
                                               rate.sti.screening * prp.treated.immediately.following.screening + 
                                               rate.infected.contacts.diagnosed.treated 
                        ))


# register.model.quantity(SHIELD.SPECIFICATION,
#                         name  = 'rate.diagnosis.immediate.treatment.scaled',
#                         scale = 'rate',
#                         value = expression(rate.diagnosis.immediate.treatment * treatment.multiplier)
# )

register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'rate.diagnosis.immediate.treatment',
                               apply.function = 'add',
                               applies.to = list(sex='female',age=FERTILE.AGES),
                               value= expression(rate.prenatal.care )
)
##---- Remission ---- [infected to uninfected]
# this has 2 components: 
## 1) those diagnosed and immediately treated (including contacts who were diagnosed/treated) 
## 2) those contacts who were empirically treated (but not diagnosed)

## 1) "Diagnosis & Immediate Treatment"
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'remission.prp',
                        value = 1)
register.remission(SHIELD.SPECIFICATION,
                   applies.to = list(continuum = 'undiagnosed'),
                   all.remissions.into.compartments = list(profile = 'diagnosed.treated'),
                   remission.rate.value = 'rate.diagnosis.immediate.treatment',
                   remission.proportions.value = 'remission.prp',
                   tag = 'remission.treated.immediately')

## 2) "No Diagnosis & Emperical Treatment"

# register.model.quantity(SHIELD.SPECIFICATION,
#                         name  = 'rate.infected.contacts.empirically.treated.scaled',
#                         scale = 'rate',
#                         value = expression(rate.infected.contacts.empirically.treated * treatment.multiplier)
# )

register.remission(SHIELD.SPECIFICATION,
                   applies.to = list(continuum = 'undiagnosed'),
                   all.remissions.into.compartments = list(profile = 'diagnosed.treated'),
                   remission.rate.value = 'rate.infected.contacts.empirically.treated',
                   remission.proportions.value = 'remission.prp',
                   tag = 'remission.treated.emperically.post.ct')




##---- Delayed Treatments ----
# only for sym.testing and sti.screening:
# assuming that all prenatal care results in immediate treatment and those delayed treatment failing to avert congenital syphilis are accounted in the outcome
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'rate.diagnosis.delayed.treatment',
                        value = expression(rate.testing.symptomatic * (1-prp.treated.immediately.following.testing.symptomatic)+
                                               rate.sti.screening * (1- prp.treated.immediately.following.screening )
                        ))


register.transition(SHIELD.SPECIFICATION,
                    dimension = 'continuum',
                    groups = 'infected',
                    from.compartments =  'undiagnosed',
                    to.compartments = 'diagnosed.untreated',
                    value = 'rate.diagnosis.delayed.treatment',
                    tag='progression.undiagnosed.to.diagnosedUntreated')

# how long will it take for someone to get treated after a positive diagnosis? 
register.model.element(SHIELD.SPECIFICATION,
                       name = 'rate.treatment.after.delay',
                       scale = 'rate',
                       value = SHIELD_BASE_PARAMETER_VALUES['rate.treatment.after.delay']) 

# register.model.quantity(
#     SHIELD.SPECIFICATION,
#     name  = 'rate.treatment.after.delay.scaled',
#     scale = 'rate',
#     value = expression(rate.treatment.after.delay * treatment.multiplier)
# )

register.remission(SHIELD.SPECIFICATION,
                   applies.to = list(continuum = 'diagnosed.untreated'),
                   all.remissions.into.compartments = list(profile = 'diagnosed.treated'),
                   remission.rate.value = 'rate.treatment.after.delay',
                   remission.proportions.value='remission.prp',
                   tag = 'remission.treated.after.delay' )


#*** CONGENITAL SYPHILIS *** ----
##---- Vertical transmission By Stage ----
register.model.element(SHIELD.SPECIFICATION,
                       name='prob.vertical.transmission.mothers.early.syphilis',
                       scale='proportion',
                       value=SHIELD_BASE_PARAMETER_VALUES['prob.vertical.transmission.mothers.early.syphilis']) 
register.model.element(SHIELD.SPECIFICATION,
                       name='prob.vertical.transmission.mothers.late.syphilis',
                       scale='proportion',
                       value=SHIELD_BASE_PARAMETER_VALUES['prob.vertical.transmission.mothers.late.syphilis'])  
register.model.quantity(SHIELD.SPECIFICATION,
                        name='prob.vertical.transmission.by.stage',
                        scale='proportion',
                        value='prob.vertical.transmission.mothers.late.syphilis')
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name='prob.vertical.transmission.by.stage',
                               applies.to = list(stage=c('primary','secondary','early.latent')),
                               value='prob.vertical.transmission.mothers.early.syphilis')

##---- Prenatal Care Coverage ----
register.model.element(SHIELD.SPECIFICATION,
                       name='prp.prenatal.care.first.trimester',
                       scale='proportion', 
                       get.functional.form.function = get.prp.prenatal.care.functional.form.first.trimester,
                       functional.form.from.time = DEFAULT.PRENATAL.CARE.START.YEAR) #1980: the projections remain fix at this year's value for years before. 
#
register.model.element(SHIELD.SPECIFICATION,
                       name='prp.prenatal.care.second.trimester.of.those.not.screened.first',
                       scale='proportion',
                       get.functional.form.function = get.prp.prenatal.care.functional.form.second.trimester.of.those.not.screened.first,
                       functional.form.from.time = DEFAULT.PRENATAL.CARE.START.YEAR) #1980: the projections remain fix at this year's value for years before. 
#
register.model.element(SHIELD.SPECIFICATION,
                       name='prp.prenatal.care.third.trimester.of.those.not.screened.first.second',
                       scale='proportion',
                       get.functional.form.function = get.prp.prenatal.care.functional.form.third.trimester.of.those.not.screened.first.second,
                       functional.form.from.time = DEFAULT.PRENATAL.CARE.START.YEAR) #1980: the projections remain fix at this year's value for years before. 
#
register.model.quantity(SHIELD.SPECIFICATION,
                        name='prp.prenatal.care.second.trimester',
                        scale='proportion',
                        value=expression((1-prp.prenatal.care.first.trimester) * prp.prenatal.care.second.trimester.of.those.not.screened.first))
#
register.model.quantity(SHIELD.SPECIFICATION,
                        name='prp.prenatal.care.third.trimester',
                        scale='proportion',
                        value=expression((1-prp.prenatal.care.first.trimester -prp.prenatal.care.second.trimester) * prp.prenatal.care.third.trimester.of.those.not.screened.first.second))
#
register.model.quantity(SHIELD.SPECIFICATION,
                        name='prp.no.prenatal.care',
                        scale='proportion',
                        value=expression(1- prp.prenatal.care.first.trimester -prp.prenatal.care.second.trimester -prp.prenatal.care.third.trimester))
##---- Risk Ratios for Congenital Syphilis Based on Prenatal Care ----
register.model.element(SHIELD.SPECIFICATION,
                       name='rr.congenital.syphilis.no.prenatal.care', 
                       scale='ratio',
                       value=SHIELD_BASE_PARAMETER_VALUES['rr.congenital.syphilis.no.prenatal.care'])
register.model.element(SHIELD.SPECIFICATION,
                       name='rr.congenital.syphilis.prenatal.care.first.trimester', 
                       scale='ratio',
                       value=SHIELD_BASE_PARAMETER_VALUES['rr.congenital.syphilis.prenatal.care.first.trimester'])
register.model.element(SHIELD.SPECIFICATION,
                       name='rr.congenital.syphilis.prenatal.care.second.trimester',
                       scale='ratio',
                       value=SHIELD_BASE_PARAMETER_VALUES['rr.congenital.syphilis.prenatal.care.second.trimester'])
register.model.element(SHIELD.SPECIFICATION,
                       name='rr.congenital.syphilis.prenatal.care.third.trimester',
                       scale='ratio',
                       value=SHIELD_BASE_PARAMETER_VALUES['rr.congenital.syphilis.prenatal.care.third.trimester'])
# assuming rr.congenital.syphilis.no.prenatal.care =1


##-----------------#######----------------#######-----------------##----
#*** OUTPUTS *** --#----
##--------------------------------------------------------------------------------------------------------------#
# !!!for dynamic transitions that change over time (e.g., testing), the anchor points are coded at the begginign of the year 
# (e.g., if transmission changes from 2000 to 2020, these dates represent jan first of those years)

##---- Population ----
track.point.outcome(SHIELD.SPECIFICATION,
                    name='point.population',
                    outcome.metadata = NULL, #we are not saving it
                    scale='non.negative.number',
                    save=F,
                    value=expression(infected+uninfected),
                    keep.dimensions = c('location','age','race','sex') #collapse on stage and continuum for infected and on profile as well
)
track.integrated.outcome(SHIELD.SPECIFICATION,
                         name='population',
                         outcome.metadata = create.outcome.metadata(display.name = 'Population',
                                                                    description = 'Population size',
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Persons',
                                                                    units = 'persons',
                                                                    singular.unit = 'person'), #will read the scale from metadata
                         value.to.integrate = 'point.population',
                         corresponding.data.outcome = 'population' ,
                         keep.dimensions = c('location','age','race','sex')
)


##---- Fertility Rate ----
track.cumulative.outcome(SHIELD.SPECIFICATION,
                         name='fertility.rate',
                         value=expression(births.from/population),
                         denominator.outcome = 'population',
                         keep.dimensions =  c('location','age','race','sex'),
                         subset.dimension.values = list(sex='female'),
                         corresponding.data.outcome = 'fertility.rate',
                         outcome.metadata = create.outcome.metadata(display.name = 'Fetility Rate',
                                                                    description = 'Fetility Rate',
                                                                    scale = 'rate',
                                                                    axis.name = 'Rate',
                                                                    units = 'rate',
                                                                    singular.unit = 'person')
)


##---- Births ----
track.dynamic.outcome(SHIELD.SPECIFICATION, #expensive: may be able to remove and approximate 
                      name='births.from',
                      outcome.metadata = create.outcome.metadata(display.name = 'Births',
                                                                 description = 'Births',
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Persons',
                                                                 units = 'persons',
                                                                 singular.unit = 'person'),
                      scale='non.negative.number',
                      dynamic.quantity.name = 'births.from', #model has an internal definition for births  #births from is conditional on parent's characteristics
                      corresponding.data.outcome = 'births',
                      keep.dimensions = c('location','age','race','sex'), #collapse on stage and continuum for infected and on profile as well
                      subset.dimension.values = list(sex='female'), #this seems redundant but it filters all the male rows with 0 values
                      exclude.tags = 'immigration'
)

##---- Deaths ----
track.dynamic.outcome(SHIELD.SPECIFICATION, #expensive
                      name='deaths',
                      outcome.metadata = create.outcome.metadata(display.name = 'Total Deaths',
                                                                 description = 'Total Deaths',
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Deaths',
                                                                 units = 'deaths',
                                                                 singular.unit = 'deaths'),
                      scale='non.negative.number',
                      dynamic.quantity.name = 'mortality', #internal JHEEM construct for deaths
                      corresponding.data.outcome = 'deaths',
                      groups = NULL, 
                      keep.dimensions = c('location'), #'age','race','sex' <saving on time>
                      exclude.tags = 'emigration',
)

##---- Migration ----
# We can track stratified values directly, but to save the speed, we estiamte total rate first, and 
# approximate age,sex,race stratifications by multiplying migration * population
# track.dynamic.outcome(SHIELD.SPECIFICATION,
#                       name = 'immigration.total',
#                       outcome.metadata = create.outcome.metadata(display.name = 'Immigration',
#                                                                  description = "Number of People Immigrating into the Population in the Past Year",
#                                                                  scale = 'non.negative.number',
#                                                                  axis.name = 'Number Immigrating',
#                                                                  units = 'individuals'),
#                       dynamic.quantity.name = 'births',
#                       corresponding.data.outcome = 'immigration', 
#                       include.tags = "immigration",
#                       keep.dimensions = c('location')
# )  
# # total immigration rate
# track.cumulative.outcome(SHIELD.SPECIFICATION,
#                          name = 'immigration.rate',
#                          value = expression(emigration.total/population),
#                          outcome.metadata = create.outcome.metadata(display.name = 'Immigration',
#                                                                     description = "Number of People Immigrating into the Population in the Past Year",
#                                                                     scale = 'rate',
#                                                                     axis.name = 'Rate',
#                                                                     units = 'rate'),
#                          denominator.outcome = 'population',
#                          corresponding.data.outcome = 'immigration' ,
#                          keep.dimensions = c('location')
# )
# # stratified immigration values: 
# track.cumulative.outcome(SHIELD.SPECIFICATION,
#                          name = 'immigration',
#                          value = expression(immigration.rate * population),
#                          outcome.metadata = create.outcome.metadata(display.name = 'Immigration',
#                                                                     description = "Number of People Immigrating into the Population in the Past Year",
#                                                                     scale = 'non.negative.number',
#                                                                     axis.name = 'Number Emigrating',
#                                                                     units = 'individuals'),
#                          scale='non.negative.number',
#                          corresponding.data.outcome = 'immigration' ,
#                          keep.dimensions = c('location','age','race','sex')
# )
# total emigration value by location:
# track.dynamic.outcome(SHIELD.SPECIFICATION,
#                       name = 'emigration.total',
#                       outcome.metadata = create.outcome.metadata(display.name = 'Emigration',
#                                                                  description = "Number of People Emigrating from the Population in the Past Year",
#                                                                  scale = 'non.negative.number',
#                                                                  axis.name = 'Number Emigrating',
#                                                                  units = 'individuals'),
#                       dynamic.quantity.name = 'mortality',
#                       corresponding.data.outcome = 'emigration', 
#                       include.tags = "emigration",
#                       keep.dimensions = c('location'))
# total emigration rate
# track.cumulative.outcome(SHIELD.SPECIFICATION,
#                          name = 'emigration.rate',
#                          value = expression(emigration.total/population),
#                          outcome.metadata = create.outcome.metadata(display.name = 'Emigration',
#                                                                     description = "Rate People Emigrating from the Population in the Past Year",
#                                                                     scale = 'rate',
#                                                                     axis.name = 'Rate',
#                                                                     units = 'rate'),
#                          denominator.outcome = 'population',
#                          corresponding.data.outcome = 'emigration' ,
#                          keep.dimensions = c('location')
# )
# # statified emigration values: 
# track.cumulative.outcome(SHIELD.SPECIFICATION,
#                          name = 'emigration',
#                          value = expression(emigration.rate * population),
#                          outcome.metadata = create.outcome.metadata(display.name = 'Emigration',
#                                                                     description = "Number of People Emigrating from the Population in the Past Year",
#                                                                     scale = 'non.negative.number',
#                                                                     axis.name = 'Number Emigrating',
#                                                                     units = 'individuals'),
#                          scale='non.negative.number',
#                          corresponding.data.outcome = 'emigration' ,
#                          keep.dimensions = c('location','age','race','sex')
# )
track.dynamic.outcome(SHIELD.SPECIFICATION,
                      name = 'immigration',
                      outcome.metadata = create.outcome.metadata(display.name = 'Immigration',
                                                                 description = "Number of People Immigrating into the Population in the Past Year",
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Number Immigrating',
                                                                 units = 'individuals'),
                      dynamic.quantity.name = 'births',
                      corresponding.data.outcome = 'immigration',
                      include.tags = "immigration",
                      keep.dimensions = c('location','age','race','sex')
)
track.dynamic.outcome(SHIELD.SPECIFICATION,
                      name = 'emigration',
                      outcome.metadata = create.outcome.metadata(display.name = 'Emigration',
                                                                 description = "Number of People Emigrating from the Population in the Past Year",
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Number Emigrating',
                                                                 units = 'individuals'),
                      dynamic.quantity.name = 'mortality',
                      corresponding.data.outcome = 'emigration', 
                      include.tags = "emigration",
                      keep.dimensions = c('location','age','race','sex')
)
##---- Prevalence ----
track.integrated.outcome(SHIELD.SPECIFICATION,
                         name='prevalence',
                         outcome.metadata = create.outcome.metadata(display.name = 'Infected Population',
                                                                    description = 'Infected Population size',
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Persons',
                                                                    units = 'persons',
                                                                    singular.unit = 'person'), #will read the scale from metadata
                         value.to.integrate = 'infected',
                         # corresponding.data.outcome = 'population' ,
                         keep.dimensions = c('location','age','race','sex','stage','continuum')
)
##---- STI screening -----
track.cumulative.proportion.from.rate(SHIELD.SPECIFICATION,
                                      name = 'sti.testing',
                                      outcome.metadata = create.outcome.metadata(display.name = 'Proportion with STI screening in the Past Year',
                                                                                 description = "The Proportion of General Population who Received STI screening in the Past Year",
                                                                                 scale = 'proportion',
                                                                                 axis.name = 'Proportion Tested',
                                                                                 units = '%'),
                                      rate.value = 'rate.sti.screening',
                                      denominator.outcome =  'prevalence',
                                      keep.dimensions = c('location','age','race','sex','stage'),
                                      # force.dim.names.to.keep.dimensions = T, #forces to keep the dimensions that we specify
                                      corresponding.data.outcome = 'proportion.tested.for.hiv'
)

##---- HIV Testing -----
track.cumulative.proportion.from.rate(SHIELD.SPECIFICATION,
                                      name = 'hiv.testing',
                                      outcome.metadata = create.outcome.metadata(display.name = 'Proportion with HIV Test in the Past Year',
                                                                                 description = "The Proportion of General Population who Received an HIV Test in the Past Year",
                                                                                 scale = 'proportion',
                                                                                 axis.name = 'Proportion Tested',
                                                                                 units = '%'),
                                      rate.value = 'rate.testing.hiv.over.18',
                                      denominator.outcome =  'population.over.18',
                                      keep.dimensions = c('location','age','race','sex'), 
                                      force.dim.names.to.keep.dimensions = T, #forces to keep the dimensions that we specify  
                                      corresponding.data.outcome = 'proportion.tested.for.hiv', 
                                      subset.dimension.values = list(age=c('15-19 years','20-24 years','25-29 years', '30-34 years','35-39 years','40-44 years', '45-49 years','50-54 years','55-64 years','65+ years')), #we can drop the first agegroup because BRFSS data starts from 18-24
                                      rename.dimension.values = list(age=c('15-19 years'='18-19 years')) #the code is smart to recognize that this agegroup falls within 18-24
)





### RATE.VALUE:
# SHILED agegroups are: ('0-14', '15-19','20-24','25-34','35-44','45-54','55-64','65+')
# BRFSS data includes '18-24','25-29','30-34',....
# we need to calculate the number of tests done among 18-19 year olds in SHIELD and calibrate it to '18-19' in BRFSS
# Here is the Math: 
#RATE(18,19)= nTESTS(18,19)/POP(18,19) : rate of tests done among 18-19 year olds is calculated by dividing the number of tests done among that population
#nTESTS(15-19)= RATE(15-19)* POP(15-19) : number of tests done among 15-19 year olds is calculated by multiplying the rate of tests done among that population
#nTESTS(18-19/15-19)  = nTESTS(15-19) * fraction.hiv.tests.18.19.among.15.19 / POP(18,19)
#                     = RATE(15-19)* POP(15-19) * fraction.hiv.tests.18.19.among.15.19 / POP(18,19)
#                     = fraction.hiv.tests.18.19.among.15.19 * RATE(15-19) * 1/fraction.18.19.among.15.19 
# so we need to compute: fraction.18.19.among.15.19: which is the fraction of 15-19 year olds that are 18-19 years old
# we can estimate fraction.hiv.tests.18.19.among.15.19 from CDC data


# what fraction of 15-19 year olds are 18-19 years?
register.model.element(SHIELD.SPECIFICATION,
                       'fraction.population.18.19.among.15.19',  
                       scale = 'proportion',
                       get.value.function = get.fraction.over.age,
                       age = 18,
                       denom.age.bracket.index=2 # sets the denominator to age.index=2 [15-19]
)

#what fraction of CDC supported HIV tests among 15-19 years oldes were done by those age 18-19?
register.model.element(SHIELD.SPECIFICATION,
                       'fraction.hiv.tests.18.19.among.15.19',
                       scale = 'proportion',
                       value = SHIELD_BASE_PARAMETER_VALUES['fraction.hiv.tests.18.19.among.15.19'], 
)  

# Build the quantity that we want to integrate in outputs:
register.model.quantity(SHIELD.SPECIFICATION,
                        'rate.testing.hiv.over.18', 
                        value = 'rate.testing.hiv' )

register.model.quantity.subset(SHIELD.SPECIFICATION,
                               'rate.testing.hiv.over.18',
                               applies.to = list('age'='15-19 years'),
                               value = expression(rate.testing.hiv * fraction.hiv.tests.18.19.among.15.19 / fraction.population.18.19.among.15.19))

### DENOMINATOR.OUTCOME: 
# we need to track the population over age of 18

register.model.element(SHIELD.SPECIFICATION,
                       'fraction.population.over.18',  
                       scale = 'proportion',
                       get.value.function = get.fraction.over.age,
                       age = 18,
                       denom.age.bracket.index=""
) #since we didnt supply a denominator.age.index, it will loop over all ages and returns a vector of fractions


track.cumulative.outcome(SHIELD.SPECIFICATION,
                         name = 'population.over.18',
                         value = expression(population * fraction.population.over.18),
                         scale = 'non.negative.number',
                         keep.dimensions = c('location','age','race','sex'),
                         save = F,
                         subset.dimension.values = list(age=c('15-19 years','20-24 years','25-29 years', '30-34 years','35-39 years','40-44 years', '45-49 years','50-54 years','55-64 years','65+ years')), #we can drop the first agegroup because BRFSS data starts from 18-24
                         rename.dimension.values = list(age=c('15-19 years'='18-19 years')), #the code is smart to recognize that this agegroup falls within 18-24
                         outcome.metadata = NULL)


track.point.outcome(
    SHIELD.SPECIFICATION,
    name             = 'adult.point.population',
    outcome.metadata = NULL,            # internal helper; not saved
    scale            = 'non.negative.number',
    save             = F,           # helper only
    value            = expression( (infected + uninfected) * fraction.population.over.18 ),
    keep.dimensions  = c('location','age','race','sex')
)

track.integrated.outcome(
    SHIELD.SPECIFICATION,
    name  = 'adult.population',
    outcome.metadata  = create.outcome.metadata(
        display.name  = 'Adult Population',
        description   = 'Number of individuals aged 18+',
        scale         = 'non.negative.number',
        axis.name     = 'Population',
        units         = 'people',
        singular.unit = 'person'
    ),
    value.to.integrate = 'adult.point.population',
    corresponding.data.outcome = 'adult.population',
    keep.dimensions = c('location','age','race','sex'),
    subset.dimension.values = list(
        age = c('15-19 years','20-24 years','25-29 years','30-34 years',
                '35-39 years','40-44 years','45-49 years','50-54 years',
                '55-64 years','65+ years')
    ),
    rename.dimension.values= list(
        age = c('15-19 years'='18-19 years')
    ),
    save = T
)







##---- Syphilis Incidence ---- 
# (new infections + reinfections)
track.dynamic.outcome(SHIELD.SPECIFICATION,
                      name = 'incidence',
                      outcome.metadata = create.outcome.metadata(display.name = 'Incidence',
                                                                 description = 'Number of Individuals Infected with Syphilis in the Past Year',
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Cases',
                                                                 units = 'cases',
                                                                 singular.unit = 'case'),
                      dynamic.quantity.name = 'incidence.from', # use of '.from' helps us track where individuals are coming from (differentiate new vs re-infections)
                      keep.dimensions = c('location','age','race','sex','profile')
)
##---- Syphilis Diagnosis ----
register.model.element(SHIELD.SPECIFICATION,
                       name = 'fraction.el.misclassified.ll',
                       scale = 'proportion',
                       value = SHIELD_BASE_PARAMETER_VALUES['fraction.el.misclassified.ll'],
)  
register.model.element(SHIELD.SPECIFICATION,
                       name = 'fraction.ll.misclassified.el',
                       scale = 'proportion',
                       value = SHIELD_BASE_PARAMETER_VALUES['fraction.ll.misclassified.el'],
)  
### Diagnosis & Treatment ----
#  diagnosis and treatment (both immediate and delayed)
track.dynamic.outcome(SHIELD.SPECIFICATION,
                      name = 'diagnosis.immediate.treatment',
                      outcome.metadata = create.outcome.metadata(display.name = 'Number of Individuals Diagnosed and Immediately Treated in the Past Year',
                                                                 description = 'Number of Individuals Diagnosed and Immediately Treated in the Past Year',
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Cases',
                                                                 units = 'cases',
                                                                 singular.unit = 'case'),
                      scale='non.negative.number',
                      dynamic.quantity.name = 'remission.from', 
                      corresponding.data.outcome = 'total.syphilis.diagnoses'  , #<just for comparison>
                      exclude.tags = c('remission.treated.emperically.post.ct','remission.treated.after.delay'), #excluding those emperically treated post contact tracing and delayed treatment 
                      keep.dimensions = c('location','age','race','sex','stage') #have to keep these dimensions because they're used for ps and other stages below
)
track.transition(SHIELD.SPECIFICATION,
                 name = 'diagnosis.delayed.treatment',
                 outcome.metadata = create.outcome.metadata(display.name = 'Number of Individuals Diagnosed but Remained Untreated in the Past Year',
                                                            description = 'Number of Individuals Diagnosed but Remained Untreated in the Past Year',
                                                            scale = 'non.negative.number',
                                                            axis.name = 'Cases',
                                                            units = 'cases',
                                                            singular.unit = 'case'),
                 dimension = 'continuum',
                 from.compartments = 'undiagnosed',
                 to.compartments = 'diagnosed.untreated',
                 corresponding.data.outcome = 'total.syphilis.diagnoses'  , #<just for comparison>
                 keep.dimensions = c('location','age','race','sex','stage')
)

### TOTAL Diagnoses ----
track.cumulative.outcome(SHIELD.SPECIFICATION,
                         name = 'diagnosis.total', 
                         value = expression(diagnosis.immediate.treatment+diagnosis.delayed.treatment),
                         outcome.metadata = create.outcome.metadata(display.name = 'Total Diagnosis', # 'Number of Individuals with a Diagnosis of Non-Congenital Syphilis in the Past Year',
                                                                    description = 'Number of Individuals with a Diagnosis of Non-Congenital Syphilis in the Past Year',
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Cases',
                                                                    units = 'cases',
                                                                    singular.unit = 'case'),
                         scale='non.negative.number',
                         corresponding.data.outcome = 'total.syphilis.diagnoses',  
                         keep.dimensions = c('location','age','race','sex','stage') 
)
### PS Diagnosis ----
track.cumulative.outcome(SHIELD.SPECIFICATION,
                         name = 'diagnosis.ps',
                         value = expression(diagnosis.total),
                         subset.dimension.values = list(stage='ps.stages'),  
                         outcome.metadata = create.outcome.metadata(display.name = 'PS Diagnosis', #'1 Number of Individuals with a Diagnosis of Primary and Secondary Syphilis in the Past Year',
                                                                    description = 'Number of Individuals with a Diagnosis of Primary and Secondary Syphilis in the Past Year',
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Cases',
                                                                    units = 'cases',
                                                                    singular.unit = 'case'),
                         scale='non.negative.number',
                         corresponding.data.outcome = 'ps.syphilis.diagnoses' ,
                         keep.dimensions = c('location','age','race','sex')
)
###  Early Latent Syphilis: True Estimate ----
track.cumulative.outcome(SHIELD.SPECIFICATION,
                         name = 'diagnosis.el.true',
                         value = expression(diagnosis.total),
                         subset.dimension.values = list(stage='early.latent'),  
                         outcome.metadata = create.outcome.metadata(display.name = 'EL (true) Diagnosis', #Number of Individuals with a Diagnosis of Early Latent Syphilis in the Past Year',
                                                                    description = 'Number of Individuals with a Diagnosis of Early Latent Syphilis in the Past Year',
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Cases',
                                                                    units = 'cases',
                                                                    singular.unit = 'case'),
                         scale='non.negative.number',
                         corresponding.data.outcome = 'early.syphilis.diagnoses',#<just for comparison>
                         keep.dimensions = c('location','age','race','sex') 
)
### Late Latent Syphilis: True Estimate  ----
track.cumulative.outcome(SHIELD.SPECIFICATION,
                         name = 'diagnosis.ll.true', 
                         value = expression(diagnosis.total),
                         subset.dimension.values = list(stage='late.latent'),
                         outcome.metadata = create.outcome.metadata(display.name = 'LL (true) Diagnosis', #1Number of Individuals with a Diagnosis of Late Latent Syphilis in the Past Year',
                                                                    description = 'Number of Individuals with a Diagnosis of Late Latent Syphilis in the Past Year',
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Cases',
                                                                    units = 'cases',
                                                                    singular.unit = 'case'),
                         scale='non.negative.number',
                         corresponding.data.outcome = 'unknown.duration.or.late.syphilis.diagnoses',  #<just for comparison>
                         keep.dimensions = c('location','age','race','sex') 
)
###  Early Latent Syphilis: Misclassified Estimate reported <used in calibration> ----
track.cumulative.outcome(SHIELD.SPECIFICATION,
                         name = 'diagnosis.el.misclassified',
                         value = expression(diagnosis.el.true *(1-fraction.el.misclassified.ll) + 
                                                diagnosis.ll.true * fraction.ll.misclassified.el),
                         outcome.metadata = create.outcome.metadata(display.name = 'EL (misclass) Diagnosis',#'Number of Individuals with a Diagnosis of Early Latent Syphilis (including misclassification) in the Past Year',
                                                                    description = 'Number of Individuals with a Diagnosis of Early Latent Syphilis (including misclassification) in the Past Year',
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Cases',
                                                                    units = 'cases',
                                                                    singular.unit = 'case'),
                         force.dim.names.to.keep.dimensions = T,
                         scale='non.negative.number',
                         corresponding.data.outcome = 'early.syphilis.diagnoses',
                         keep.dimensions = c('location','age','race','sex') 
)
###  Late Latent Syphilis: Misclassified Estimate in the model ----
track.cumulative.outcome(SHIELD.SPECIFICATION,
                         name = 'diagnosis.ll.misclassified',
                         value = expression( diagnosis.ll.true *(1- fraction.ll.misclassified.el) + 
                                                 diagnosis.el.true * fraction.el.misclassified.ll),
                         outcome.metadata = create.outcome.metadata(display.name = 'LL (misclass) Diagnosis',#'Number of Individuals with a Diagnosis of Late Latent Syphilis (including misclassification) in the Past Year',
                                                                    description = 'Number of Individuals with a Diagnosis of Late Latent Syphilis (including misclassification) in the Past Year',
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Cases',
                                                                    units = 'cases',
                                                                    singular.unit = 'case'),
                         scale='non.negative.number',
                         corresponding.data.outcome = 'unknown.duration.or.late.syphilis.diagnoses',#<just for comparison>
                         keep.dimensions = c('location','age','race','sex') 
)
###  Tertiary Diagnosis  ----
# (all cases are symptomatic: no misclassification)
track.cumulative.outcome(SHIELD.SPECIFICATION,
                         name = 'diagnosis.tertiary',
                         value = expression(diagnosis.total),
                         subset.dimension.values = list(stage=c('tertiary')),
                         outcome.metadata = create.outcome.metadata(display.name = 'Number of Individuals with a Diagnosis of Tertiary Syphilis in the Past Year',
                                                                    description = 'Number of Individuals with a Diagnosis of Tertiary Syphilis in the Past Year',
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Cases',
                                                                    units = 'cases',
                                                                    singular.unit = 'case'),
                         scale='non.negative.number',
                         # corresponding.data.outcome = 'unknown.duration.or.late.syphilis.diagnoses', #<just for comparison>
                         keep.dimensions = c('location','age','race','sex') 
) 

###  CNS diagnosis  ----
#(all cases are symptomatic: no misclassification)
track.cumulative.outcome(SHIELD.SPECIFICATION,
                         name = 'diagnosis.cns',
                         value = expression(diagnosis.total),
                         subset.dimension.values = list(stage=c('cns')),
                         outcome.metadata = create.outcome.metadata(display.name = 'Number of Individuals with a Diagnosis of CNS Syphilis in the Past Year',
                                                                    description = 'Number of Individuals with a Diagnosis of CNS Syphilis in the Past Year',
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Cases',
                                                                    units = 'cases',
                                                                    singular.unit = 'case'),
                         scale='non.negative.number',
                         # corresponding.data.outcome = 'unknown.duration.or.late.syphilis.diagnoses', #<just for comparison>
                         keep.dimensions = c('location','age','race','sex') 
) 
###  Late Syphilis True ----
# (including LL, Tertirary and CNS): True Estimate
track.cumulative.outcome(SHIELD.SPECIFICATION,
                         name = 'diagnosis.late.true', 
                         value = expression(diagnosis.total),
                         subset.dimension.values = list(stage='late.stages'),
                         outcome.metadata = create.outcome.metadata(display.name = 'Late (true) Diagnosis',# 'Number of Individuals with a Diagnosis of Late Stage (Late Latent, Tertiary or CNS) Syphilis in the Past Year',
                                                                    description = 'Number of Individuals with a Diagnosis of Late Stage (Late Latent, Tertiary or CNS) Syphilis in the Past Year',
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Cases',
                                                                    units = 'cases',
                                                                    singular.unit = 'case'),
                         scale='non.negative.number',
                         corresponding.data.outcome = 'unknown.duration.or.late.syphilis.diagnoses',#<just for comparison>
                         keep.dimensions = c('location','age','race','sex')
)

###  Late Syphilis Misclassified <used in calibration> ----
# (including LL, Tertirary and CNS): Misclassified Estimate reported 
track.cumulative.outcome(SHIELD.SPECIFICATION,
                         name = 'diagnosis.late.misclassified',
                         value = expression(diagnosis.ll.misclassified +diagnosis.tertiary+diagnosis.cns),
                         outcome.metadata = create.outcome.metadata(display.name = 'Late (misclass) Diagnosis',# 'Number of Individuals with a Diagnosis of Late (Late Latent, Tertiary or CNS) Syphilis (including Misclassification) in the Past Year',
                                                                    description = 'Number of Individuals with a Diagnosis of Late (Late Latent, Tertiary or CNS) Syphilis (including Misclassification) in the Past Year',
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Cases',
                                                                    units = 'cases',
                                                                    singular.unit = 'case'),
                         scale='non.negative.number',
                         corresponding.data.outcome = 'unknown.duration.or.late.syphilis.diagnoses',
                         keep.dimensions = c('location','age','race','sex') 
)

### Congenital Diagnoses -----
track.dynamic.outcome(SHIELD.SPECIFICATION,
                      name='diagnosis.congenital',
                      groups = 'infected',
                      subset.dimension.values = list(sex='female',age=FERTILE.AGES),
                      outcome.metadata = create.outcome.metadata(display.name = 'Number of Congenital Syphilis Diagnosis in the Past Year',
                                                                 description = 'Number of Congenital Syphilis Diagnosis in the Past Year',
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Persons',
                                                                 units = 'persons',
                                                                 singular.unit = 'person'),
                      scale='non.negative.number',
                      multiply.by = expression(prob.vertical.transmission.by.stage * (
                          #prp of infected pregnant women whp pass on congenital syphilis to newborn
                          prp.prenatal.care.first.trimester * rr.congenital.syphilis.prenatal.care.first.trimester +
                              prp.prenatal.care.second.trimester * rr.congenital.syphilis.prenatal.care.second.trimester  +
                              prp.prenatal.care.third.trimester * rr.congenital.syphilis.prenatal.care.third.trimester +
                              prp.no.prenatal.care * rr.congenital.syphilis.no.prenatal.care    
                      ) ), 
                      dynamic.quantity.name = 'births.from',  #model has an internal definition for births  #births from is conditional on parent's characteristics
                      corresponding.data.outcome = 'congenital.syphilis.diagnoses' ,
                      keep.dimensions = c('location') #collapse on stage and continuum for infected and on profile as well
)

track.cumulative.outcome(SHIELD.SPECIFICATION,
                         name = 'proportion.births.congenital',
                         value = 'diagnosis.congenital',
                         value.is.numerator = T,
                         denominator.outcome = 'births.from',
                         outcome.metadata = create.outcome.metadata(display.name = 'Proportion of Congenital Syphilis Diagnosis in the Past Year',
                                                                    description = 'Proportion of Congenital Syphilis Diagnosis in the Past Year',
                                                                    scale = 'proportion',
                                                                    axis.name = 'proportion',
                                                                    units = 'percent',
                                                                    singular.unit = 'percent'),
                         corresponding.data.outcome = "proportion.of.congenital.syphilis.births",
                         keep.dimensions = c('location') #collapse on stage and continuum for infected and on profile as well
)

##---- Prenatal.care.coverage.by trimester ---- 
track.integrated.outcome(SHIELD.SPECIFICATION,
                         name = 'prp.prenatal.care.first.trimester',
                         value.to.integrate = "prp.prenatal.care.first.trimester", #the number of births are also time varying but here we are approximating
                         denominator.outcome = "births.from", #or we can define pregnancies= births /births
                         subset.dimension.values = list(sex='female',age=FERTILE.AGES),
                         outcome.metadata = create.outcome.metadata(display.name = 'Proportion of Births Starting Prenatal Care in the First Trimester',
                                                                    description = 'Proportion of Births Starting Prenatal Care in the First Trimester',
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion',
                                                                    units = 'proportion',
                                                                    singular.unit = 'proportion'),
                         keep.dimensions = c('location','age','race'),
                         corresponding.data.outcome ="prenatal.care.initiation.first.trimester"
)
track.integrated.outcome(SHIELD.SPECIFICATION,
                         name = 'prp.prenatal.care.second.trimester',
                         value.to.integrate = "prp.prenatal.care.second.trimester",
                         denominator.outcome = "births.from",  
                         subset.dimension.values = list(sex='female',age=FERTILE.AGES),
                         outcome.metadata = create.outcome.metadata(display.name = 'Proportion of Births Starting Prenatal Care in the Second Trimester',
                                                                    description = 'Proportion of Births Starting Prenatal Care in the Second Trimester',
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion',
                                                                    units = 'proportion',
                                                                    singular.unit = 'proportion'),
                         keep.dimensions = c('location','age','race'),
                         corresponding.data.outcome ="prenatal.care.initiation.second.trimester"
)                  
track.integrated.outcome(SHIELD.SPECIFICATION,
                         name = 'prp.prenatal.care.third.trimester',
                         value.to.integrate = "prp.prenatal.care.third.trimester",
                         denominator.outcome = "births.from",  
                         subset.dimension.values = list(sex='female',age=FERTILE.AGES),
                         outcome.metadata = create.outcome.metadata(display.name = 'Proportion of Births Starting Prenatal Care in the Third Trimester',
                                                                    description = 'Proportion of Births Starting Prenatal Care in the Third Trimester',
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion',
                                                                    units = 'proportion',
                                                                    singular.unit = 'proportion'),
                         keep.dimensions = c('location','age','race'),
                         corresponding.data.outcome ="prenatal.care.initiation.third.trimester")  
track.integrated.outcome(SHIELD.SPECIFICATION,
                         name = 'prp.no.prenatal.care',
                         value.to.integrate = "prp.no.prenatal.care",
                         denominator.outcome = "births.from",  
                         subset.dimension.values = list(sex='female',age=FERTILE.AGES),
                         outcome.metadata = create.outcome.metadata(display.name = 'Proportion of Births with No Prenatal Care',
                                                                    description = 'Proportion of Births with No Prenatal Care',
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion',
                                                                    units = 'proportion',
                                                                    singular.unit = 'proportion'),
                         keep.dimensions = c('location','age','race'),
                         corresponding.data.outcome ="no.prenatal.care"
)  
##---- Syphilis Treatment Initiations ----
## Immediate, Delayed and emperical treatment
track.dynamic.outcome(SHIELD.SPECIFICATION,
                      name = 'treatment',
                      outcome.metadata = create.outcome.metadata(display.name = 'Treatments', #
                                                                 description = 'Number of Individuals Receiving Treatment in the Past Year',
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Cases',
                                                                 units = 'cases',
                                                                 singular.unit = 'case'),
                      scale='non.negative.number',
                      dynamic.quantity.name = 'remission.from', #where they come from
                      keep.dimensions = c('location','age','race','sex','stage')
)

track.dynamic.outcome(SHIELD.SPECIFICATION,
                      name = 'emperical.treatment',
                      outcome.metadata = create.outcome.metadata(display.name = 'Emperical Trt',# 'Number of Individuals Emperically Treated in the Past Year',
                                                                 description = 'Number of Individuals Emperically Treated in the Past Year',
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Cases',
                                                                 units = 'cases',
                                                                 singular.unit = 'case'),
                      scale='non.negative.number',
                      dynamic.quantity.name = 'remission.from', 
                      # corresponding.data.outcome = 'total.syphilis.diagnoses'  , #<just for comparison>
                      exclude.tags = c('remission.treated.immediately','remission.treated.after.delay'), #excluding those with immediate and delayed treatment post-diagnosis
                      keep.dimensions = c('location','age','race','sex','stage') #have to keep these dimensions because they're used for ps and other stages below
)


##** REGISTER THE SPECIFICATION ----
register.model.specification(SHIELD.SPECIFICATION)
#

register.calibrated.parameters.for.version('shield',
                                           distribution = SHIELD.FULL.PARAMETERS.PRIOR,
                                           apply.function = SHIELD.APPLY.PARAMETERS.FN,
                                           sampling.blocks = SHIELD.FULL.PARAMETERS.SAMPLING.BLOCKS,
                                           calibrate.to.year = 2030,
                                           join.with.previous.version = F)
print('Calibration parameters registered for DEMOGRAPHIC model')


print('SHIELD specification sourced successfully!')
cat('*** Shield_specification.R completed! ***\n')


#next steps ----
# miscarriages due to untreated syphilis
# deaths among infants born with congenital syphilis?
# syphilis deaths (tertiary)
# Emigration from US total 

#'@Todd: we have modeled prenatal care coverage for congenital syphilis diagnosis, but we havent counted those new diagnosis and treatments that result for mothers 
#'@Todd: how can we adjust the population size before 2010?
#'@Todd: removing excess compartments/outcomes to speed up
#'
#'
#'@PK: check parameterization: prop immediate diagnosis
#'@PK: check parameterization: prop symptomatic women 