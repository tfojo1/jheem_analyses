cat('*** Running Shiled_specification.R ***\n')
DEFAULT.START.YEAR=1960 # simulation start year
DEFAULT.FIX.STRATA.YEAR=2010 # full population breakdown is available post-2010, and birth data is available post 2007. 
DEFAULT.AGING.YEAR=2007 # differential aging starts in 2007
DEFAULT.MIGRATION.YEAR=2007 # migration

#CASHED FOLDER:
# https://livejohnshopkins-my.sharepoint.com/personal/tfojo1_jh_edu/_layouts/15/onedrive.aspx?e=5%3A940bf48ba6e0498495fea5596e3dc8e7&sharingv2=true&fromShare=true&at=9&CID=425e54af%2De78b%2D4d53%2D8df4%2D6abb10af6339&id=%2Fpersonal%2Ftfojo1%5Fjh%5Fedu%2FDocuments%2FJHEEM2&FolderCTID=0x012000E74D427C3A55BC45A1C18C850CDA2DB4&view=0
# Excel Sheet:
# https://livejohnshopkins-my.sharepoint.com/:x:/g/personal/zdansky1_jh_edu/EVrQ-OpGqlVIpBi_KE0P6v4B2rTpIvYcyUtLz9e1NH_oig?e=kd8bjH&wdLOR=c06087FCD-0041-804E-BB9F-F582185054BC
# https://jheem.shinyapps.io/EndingHIV/

# > specification.metadata=get.specification.metadata('shield','C.12580')
# > specification.metadata=get.specification.metadata('shield','US')



# NEXT STEPS:
# Develop a functional form for fertility 
# likelihoods 

# Working directory is set to the main JHEEM_Analysis folder:
# JHEEM.DIR='~/OneDrive - Johns Hopkins/JHEEM/Simulation/code/jheem_analyses/'
# SHIELD.DIR='~/OneDrive - Johns Hopkins/JHEEM/Simulation/code/jheem_analyses/applications/SHIELD/'
# setwd(JHEEM.DIR)
# setwd('../../')
source('applications/SHIELD/shield_source_code.R')
##--------------------------------------------------------------------------------------------------------------#
#** INITIAL SET-UP --#----
SHIELD.SPECIFICATION = create.jheem.specification(version = 'shield',
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
                                                    location = 'US',
                                                    age = 'all.ages',
                                                    race=c('black','hispanic','other'),
                                                    sex= c('heterosexual.male', 'msm', 'female')
                                                  ),
                                                  compartment.value.aliases = list(
                                                    #try using aliases so that if we change the specification up here, the rest of the code doesnt break
                                                    # helps to define specifications for groups of compartments later on
                                                    ps.stages=c('primary','secondary'),
                                                    late.stages=c('late.latent','tertiary','cns')
                                                  )
)


#** INITIAL POPULATION --#----
# Specify the initial compartment sizes  
# step1: defines a blank quantity
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'n.initial.population',
                        value = 0)
#data is available from the year 2010
register.model.element(SHIELD.SPECIFICATION,
                       name = 'proportion.msm.of.male',
                       scale = 'proportion',
                       get.functional.form.function = get.proportion.msm.of.male.by.race.functional.form,
                       functional.form.from.time=2010,
                       functional.form.to.time=2010)

# step2: assigns values to specific population subgroups where the values are read from an upcoming element below
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'n.initial.population',
                               applies.to = list(sex='female'),
                               value = 'n.initial.female.population')
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'n.initial.population',
                               applies.to = list(sex='msm'),
                               value = expression(n.initial.male.population * proportion.msm.of.male))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'n.initial.population',
                               applies.to = list(sex='heterosexual.male'),
                               value = expression(n.initial.male.population * (1-proportion.msm.of.male)))

# step3: uses an element with functional form to get required values
register.model.element(SHIELD.SPECIFICATION,
                       name = 'n.initial.female.population',
                       get.value.function = get.n.initial.female.population,
                       scale = 'non.negative.number')
register.model.element(SHIELD.SPECIFICATION,
                       name = 'n.initial.male.population',
                       get.value.function = get.n.initial.male.population,
                       scale = 'non.negative.number')

##---- Infected ----
register.model.element(SHIELD.SPECIFICATION,
                       name = 'proportion.initial.population.infected.syphilis',
                       scale = 'non.negative.number',
                       value = SHIELD_BASE_PARAMETER_VALUES['proportion.initial.population.infected.syphilis'] ) #@PK: switch to proportion of pop infected 
register.model.element.values(SHIELD.SPECIFICATION,
                              'proportion.initial.population.infected.syphilis.primary'=SHIELD_BASE_PARAMETER_VALUES['proportion.initial.population.infected.syphilis.primary'],
                              'proportion.initial.population.infected.syphilis.secondary'=SHIELD_BASE_PARAMETER_VALUES['proportion.initial.population.infected.syphilis.secondary'],
                              'proportion.initial.population.infected.syphilis.early.latent'=SHIELD_BASE_PARAMETER_VALUES['proportion.initial.population.infected.syphilis.early.latent'],
                              'proportion.initial.population.infected.syphilis.late.latent'=SHIELD_BASE_PARAMETER_VALUES['proportion.initial.population.infected.syphilis.late.latent'],
                              'proportion.initial.population.infected.syphilis.tertiary'=SHIELD_BASE_PARAMETER_VALUES['proportion.initial.population.infected.syphilis.tertiary'],
                              scale = 'non.negative.number')

# Assuming they are all undiagnosed
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'n.initial.population.infected.all.stages',
                        value =  expression(n.initial.population * proportion.initial.population.infected.syphilis)) #age, race, sex

register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'n.initial.population.infected',
                        value = 0)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'n.initial.population.infected',
                               applies.to = list(continuum='undiagnosed', stage='primary'),
                               value = expression(n.initial.population.infected.all.stages*proportion.initial.population.infected.syphilis.primary ))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'n.initial.population.infected',
                               applies.to = list(continuum='undiagnosed', stage='secondary'),
                               value = expression(n.initial.population.infected.all.stages*proportion.initial.population.infected.syphilis.secondary ))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'n.initial.population.infected',
                               applies.to = list(continuum='undiagnosed', stage='early.latent'),
                               value = expression(n.initial.population.infected.all.stages*proportion.initial.population.infected.syphilis.early.latent ))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'n.initial.population.infected',
                               applies.to = list(continuum='undiagnosed', stage='late.latent'),
                               value = expression(n.initial.population.infected.all.stages*proportion.initial.population.infected.syphilis.late.latent ))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'n.initial.population.infected',
                               applies.to = list(continuum='undiagnosed', stage='tertiary'),
                               value = expression(n.initial.population.infected.all.stages*proportion.initial.population.infected.syphilis.tertiary ))
#register:
register.initial.population(SHIELD.SPECIFICATION,
                            group = 'infected',
                            value = 'n.initial.population.infected')
##---- Uninfected ----
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'n.initial.population.uninfected',
                        value = 0)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'n.initial.population.uninfected',
                               value = expression(n.initial.population - n.initial.population.infected.all.stages),
                               applies.to = list(profile='susceptible'))
#register:
register.initial.population(SHIELD.SPECIFICATION,
                            group = 'uninfected',
                            value = 'n.initial.population.uninfected')

##---- Fix Strata Sizes----
#'@title Set Whether to Fix Strata Sizes During a Time Period # a simplifying assumption to avoid modeling population demographic dynamics before year X
# 2007 earliest year for complete census data. we fix strata to 2010
register.fixed.model.strata(SHIELD.SPECIFICATION,
                            applies.after.time = -Inf,
                            applies.before.time = DEFAULT.FIX.STRATA.YEAR,
                            fix.strata = T,
                            dimensions.to.fix = c('location','age','race','sex')
)

#** BIRTHS --# ----
# we model births based on women's fertility rates
register.model.element(SHIELD.SPECIFICATION,
                       name = 'fertility.rate',
                       get.functional.form.function = get.fertility.rate.functional.form,
                       functional.form.from.time = DEFAULT.POPULATION.YEARS, #only projects values from 2010 forward
                       scale = 'rate')

##---- Birth Proportions ----
# to determine where newborns should go
# currently set as a fix ratio
register.model.element(SHIELD.SPECIFICATION,
                       name = 'proportion.births.male',
                       scale = 'proportion',
                       value = SHIELD_BASE_PARAMETER_VALUES['ratio.birth.male.to.female'] / (1+SHIELD_BASE_PARAMETER_VALUES['ratio.birth.male.to.female']))
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'birth.proportions',
                        value =0)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               applies.to = list(sex.to='msm'),
                               name = 'birth.proportions',
                               value = expression( proportion.births.male*proportion.msm.of.male ))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               applies.to = list(sex.to='heterosexual.male'),
                               name = 'birth.proportions',
                               value = expression( proportion.births.male*(1-proportion.msm.of.male)  ))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               applies.to = list(sex.to='female'),
                               name = 'birth.proportions',
                               value = expression( 1-proportion.births.male ))
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

#** MORTALITY --# ----
##---- General Mortality ----
register.model.element(SHIELD.SPECIFICATION,
                       name = 'rate.general.mortality',
                       get.functional.form.function = get.general.mortality.rates.functional.form,
                       scale = 'rate')
register.mortality(SHIELD.SPECIFICATION,
                   tag = 'general.mortality',
                   groups = c('infected','uninfected'),
                   mortality.rate.value = 'rate.general.mortality')
#'##---- TBD: Syphilis Mortality ----
register.model.element.values(SHIELD.SPECIFICATION,
                       'rate.syphilis.mortality.primary'=SHIELD_BASE_PARAMETER_VALUES['rate.syphilis.mortality.primary'],
                       'rate.syphilis.mortality.secondary'=SHIELD_BASE_PARAMETER_VALUES['rate.syphilis.mortality.secondary'],
                       'rate.syphilis.mortality.early.latent'=SHIELD_BASE_PARAMETER_VALUES['rate.syphilis.mortality.early.latent'],
                       'rate.syphilis.mortality.late.latent'=SHIELD_BASE_PARAMETER_VALUES['rate.syphilis.mortality.late.latent'],
                       'rate.syphilis.mortality.tertiary'=SHIELD_BASE_PARAMETER_VALUES['rate.syphilis.mortality.tertiary'],
                       'rate.syphilis.mortality.cns'=SHIELD_BASE_PARAMETER_VALUES['rate.syphilis.mortality.cns'],
                       'rate.syphilis.mortality.congenital'=SHIELD_BASE_PARAMETER_VALUES['rate.syphilis.mortality.congenital'],
                       scale = 'rate')
#@PK: add a quantity, subset to stages, then register one mortality
register.mortality(SHIELD.SPECIFICATION,
                   tag = 'syphilis.mortality.primary',
                   groups = c('infected'), applies.to = list(stage='primary'),
                   mortality.rate.value = 'rate.syphilis.mortality.primary')
register.mortality(SHIELD.SPECIFICATION,
                   tag = 'syphilis.mortality.secondary',
                   groups = c('infected'), applies.to = list(stage='secondary'),
                   mortality.rate.value = 'rate.syphilis.mortality.secondary')
register.mortality(SHIELD.SPECIFICATION,
                   tag = 'syphilis.mortality.early.latent',
                   groups = c('infected'), applies.to = list(stage='early.latent'),
                   mortality.rate.value = 'rate.syphilis.mortality.early.latent')
register.mortality(SHIELD.SPECIFICATION,
                   tag = 'syphilis.mortality.late.latent',
                   groups = c('infected'), applies.to = list(stage='late.latent'),
                   mortality.rate.value = 'rate.syphilis.mortality.late.latent')
register.mortality(SHIELD.SPECIFICATION,
                   tag = 'syphilis.mortality.cns',
                   groups = c('infected'), applies.to = list(stage='cns'),
                   mortality.rate.value = 'rate.syphilis.mortality.cns')
register.mortality(SHIELD.SPECIFICATION,
                   tag = 'syphilis.mortality.tertiary',
                   groups = c('infected'), applies.to = list(stage='tertiary'),
                   mortality.rate.value = 'rate.syphilis.mortality.tertiary')

#'@PK: do we have data on miscarriages due to untreated syphilis?  *****
#whats the likelihood of having a miscarriage with untreated syphilis?  *****


## -- AGING --## ----
register.model.element(SHIELD.SPECIFICATION,
                       name = 'rate.general.aging',
                       scale = 'rate',
                       get.functional.form.function = get.empiric.aging.rates,
                       functional.form.from.time = DEFAULT.AGING.YEAR)

register.aging(SHIELD.SPECIFICATION,
               groups = c('uninfected','infected'),
               aging.rate.value = 'rate.general.aging')

#** TBD: MIGRATION --# ----
# immigration/emigration doesn't depend on disease state. It is used to fit the population size but doesn't change the prevalence of the disease
# The MSA priors comes from census reports - This data more represent movements between MSA and not the immigration from other countries
# total immigration/population: prior is some fraction of this distributed equally by age and race
# the machanism to model migrations in JHEEM is births- we need to decide which compartments newborn go to-null.proportion is used to describe the proportion of births that are concordant with the parent
##---- Immigration ----
register.model.element(SHIELD.SPECIFICATION,
                       name = 'rate.immigration',
                       get.functional.form.function = get.immigration.rates.functional.form, #'@Todd
                       functional.form.from.time = DEFAULT.MIGRATION.YEAR,
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

# We use the natality mechanism to model immigrations, but shouldnt count them in births
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
                       functional.form.from.time = DEFAULT.MIGRATION.YEAR,
                       scale = 'rate')

# We use the mortalit mechanism to model emigrations, but shouldnt count them in deaths
register.mortality(SHIELD.SPECIFICATION,
                   mortality.rate.value = "rate.immigration",
                   groups = c("uninfected","infected"), 
                   tag = "emigration")


#** TRANSMISSION --# ----
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
# all.new.infections.into.compartments #@Todd what is this option ????

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
                                             sexual.contact.by.age*
                                             sexual.contact.by.sex*
                                             sexual.contact.by.race ))
##---- Sexual Contact: Transmission Rates ----
# probability of transmission through sexual act, it depends on the recipient, person getting infected
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'rate.sexual.transmission',
                        value = 0)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'rate.sexual.transmission',
                               applies.to = list(sex.from=c('heterosexual.male','msm'),
                                                 sex.to=c('heterosexual.male','msm')),
                               value = 'transmission.rate.msm') #we can add msm.peak.multiplier later if needed
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'rate.sexual.transmission',
                               applies.to = list(sex.from=c('heterosexual.male','msm'),
                                                 sex.to=c('female')),
                               value = 'transmission.rate.heterosexual')
register.model.quantity.subset(SHIELD.SPECIFICATION, #right now it's assuming that female to male is the same as male to female
                               name = 'rate.sexual.transmission',
                               applies.to = list(sex.from=c('female'),
                                                 sex.to=c('heterosexual.male','msm')),
                               value = 'transmission.rate.heterosexual')
#spline models can project negative values - we should truncate to 0
# use the log scale: exponentiate the values # log scale for the knots,
# we define this as a spline with 2 knots, but we have no rpior about their values. they will change in calibration
# Future Expnasions: msm.trate.by.race #add more alphas #if we assume the multiplier by black vs other remain the sma over time we only ned 2

#'@PK: to review
register.model.element(SHIELD.SPECIFICATION,
                       name = 'transmission.rate.msm',
                       functional.form = create.natural.spline.functional.form(knot.times = c(time0=1990, time1=2000, time2=2010,time3=2020),
                                                                               #if we wanted to use a natural spline without log transformation:
                                                                               # knot.values = list(time0=1,time1=1,time2=1) , knots.are.on.transformed.scale = F, #on the identity scale
                                                                               #use a log(y) transformation, so that all returned values are positive
                                                                               # this will also help with data that is skewed to right (long right tail)
                                                                               knot.values = list(time0=0,time1=0,time2=0,time3=0) ,
                                                                               knots.are.on.transformed.scale = T, #knots on the log scale (value is exp(0))
                                                                               #
                                                                               min=0, #even after using log for knots, value can be negative so we need to truncate
                                                                               knot.link = 'log',link='identity'), #knots on the log-scale and values on the identity scale
                       functional.form.from.time = 1980, #the projections remain fix at this year's value for years before.  
                       scale='rate') #spline with 2010/2020

register.model.element(SHIELD.SPECIFICATION,
                       name = 'transmission.rate.heterosexual',
                       functional.form = create.linear.spline.functional.form(knot.times = c(time0=1990, time1=2000, time2=2010,time3=2020),
                                                                              knot.values = list(time0=0,time1=0,time2=0,time3=0) ,
                                                                              knots.are.on.transformed.scale = T, #knots on the log scale (value is exp(0))
                                                                              min=0,
                                                                              knot.link = 'log',link='identity') ,
                       functional.form.from.time = 1980, #0 or -Inf #@TODD
                       scale='rate')

##---- Sexual Contact: By AGE ----
# builds an empty sexual contact matrix and applies it to each group based on who they have sexual contact with
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'sexual.contact.by.age',
                        value = 0)
# sexual contact by age for females who have sex with male partners
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.contact.by.age',
                               applies.to = list(sex.to='female',
                                                 sex.from=c('heterosexual.male','msm')),
                               value = get.female.sexual.age.contact.proportions)

# sexual contact by age for males who have sex with male partners
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.contact.by.age',
                               applies.to = list(sex.to=c('heterosexual.male','msm'),
                                                 sex.from=c('heterosexual.male','msm')),
                               value = get.msm.sexual.age.contact.proportions)
#
# x=get.msm.sexual.age.contact.proportions(specification.metadata = get.specification.metadata('shield',location = 'C.12580'),
#                                        single.year.msm.age.counts = get.msm.single.year.age.counts(location = 'C.12580',specification.metadata = get.specification.metadata('shield',location = 'C.12580'),population.years = 2007),
#                                        single.year.age.sexual.availability = get.sexual.availability(),age.mixing.sd.mult = 1)
# sexual contact by age for males who have sex with female partners
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.contact.by.age',
                               applies.to = list(sex.to=c('heterosexual.male','msm'),
                                                 sex.from='female'),
                               value = get.heterosexual.male.sexual.age.contact.proportions)

# from males...to females...
# from (HIV+) to susceptible
# age.from, sex.from, age.to, sex.to
#
# sex.from female. age 19 HIV+
# sex.to hetrosexual male age.to 19
# for each person at risk of HIV infecyon, what proportion of contact sar form diff groups
#> females, msms, het males
#> for female: get.heterosexual.male.sexual.age.contact.proportions
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
                       name = 'single.year.heterosexual.male.age.counts',
                       get.value.function = get.heterosexual.male.single.year.age.counts,
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
                       name = 'fraction.heterosexual.male.pairings.with.male',
                       value = SHIELD_BASE_PARAMETER_VALUES['fraction.heterosexual.male.pairings.with.male'],
                       scale = 'ratio')

register.model.element(SHIELD.SPECIFICATION,
                       name = 'fraction.msm.pairings.with.female',
                       value = mean(PAIRING.INPUT.MANAGER$msm.sex.with.female.estimates),
                       scale = 'ratio')
###
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'fraction.male.male.that.are.with.msm',
                        value = expression(proportion.msm.of.male /
                                             (proportion.msm.of.male +
                                                (1-proportion.msm.of.male) * fraction.heterosexual.male.pairings.with.male))
)
###
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'sexual.contact.by.sex',
                        value = 0)
# To female from het
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.contact.by.sex',
                               applies.to=list(sex.from='heterosexual.male',
                                               sex.to='female'),
                               value = expression((1-proportion.msm.of.male)/
                                                    (1-proportion.msm.of.male +
                                                       proportion.msm.of.male * oe.female.pairings.with.msm))
)
# To female from msm
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.contact.by.sex',
                               applies.to=list(sex.from='msm',
                                               sex.to='female'),
                               value = expression(proportion.msm.of.male * oe.female.pairings.with.msm/
                                                    (1-proportion.msm.of.male +
                                                       proportion.msm.of.male * oe.female.pairings.with.msm))
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
                               applies.to=list(sex.from='heterosexual.male',
                                               sex.to='msm'),
                               value = expression((1-fraction.msm.pairings.with.female) *
                                                    (1-fraction.male.male.that.are.with.msm))
)
# To heterosexual male from female and msm and other het male
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.contact.by.sex',
                               applies.to=list(sex.from='female',
                                               sex.to='heterosexual.male'),
                               value = expression((1-fraction.heterosexual.male.pairings.with.male))
)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.contact.by.sex',
                               applies.to=list(sex.from='msm',
                                               sex.to='heterosexual.male'),
                               value = expression(fraction.heterosexual.male.pairings.with.male *
                                                    fraction.male.male.that.are.with.msm)
)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.contact.by.sex',
                               applies.to=list(sex.from='heterosexual.male',
                                               sex.to='heterosexual.male'),
                               value = expression(fraction.heterosexual.male.pairings.with.male *
                                                    (1-fraction.male.male.that.are.with.msm))
)



# Susceptibility of uninfected persons
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'sexual.susceptibility',
                        value = 1)

# Infectiousness of infected persons
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'sexual.transmissibility',
                        value = 0)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.transmissibility',
                               applies.to=list(stage='primary'),
                               value = 'primary.rel.secondary.transmissibility')# >> register as an element> so we can vary it if we want to

register.model.element(SHIELD.SPECIFICATION,
                       name = 'primary.rel.secondary.transmissibility',
                       scale = 'ratio',
                       value = SHIELD_BASE_PARAMETER_VALUES['primary.rel.secondary.transmissibility'])

register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.transmissibility',
                               applies.to=list(stage='secondary'),
                               value = 1)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.transmissibility',
                               applies.to=list(stage='early.latent'),
                               value = .25)

# where do nw infections go to?
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'new.infection.proportions',
                        value = 1)


##--------------------------------------------------------------------------------------------------------------#

##---- Sexual Contact: By RACE ----
# oes for racial mixing with the same sex are estiamted from 4 studies. we assume equally likely mixing with other groups
baseline.sexual.oes = array(c(SHIELD_BASE_PARAMETER_VALUES['oe.sexual.byrace.bb'],1,1,
                              1,SHIELD_BASE_PARAMETER_VALUES['oe.sexual.byrace.hh'],1,
                              1,1,SHIELD_BASE_PARAMETER_VALUES['oe.sexual.byrace.oo']),
                            dim = c(race.to=3, race.from=3),
                            dimnames = list(race.to=c('black','hispanic','other'),
                                            race.from=c('black','hispanic','other')))

register.model.element(SHIELD.SPECIFICATION,
                       name = 'race.sexual.oes',
                       scale = 'ratio',
                       get.functional.form.function = get.geographically.aggregated.race.oes,
                       within.county.race.oes = baseline.sexual.oes)

register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'sexual.contact.by.race',
                        value = sexual.oes.to.contact.proportions)

# The race sub-values
register.model.element(SHIELD.SPECIFICATION,
                       name='race.population.counts',
                       scale = 'non.negative.number',
                       get.value.function = get.race.population.counts)




#** NATURAL HISTORY --# ----
##---- Stage Transitions ----
### Primary > Secondary > EL
register.model.element(SHIELD.SPECIFICATION,
                       name = 'duration.primary',
                       scale = 'time',
                       value = SHIELD_BASE_PARAMETER_VALUES['duration.primary'])
register.model.element(SHIELD.SPECIFICATION,
                       name = 'duration.secondary',
                       scale = 'time',
                       value = SHIELD_BASE_PARAMETER_VALUES['duration.secondary'])
register.model.element(SHIELD.SPECIFICATION,
                       name = 'duration.el',
                       scale = 'time',
                       value = SHIELD_BASE_PARAMETER_VALUES['duration.el'])
register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    applies.to = list('continuum'='undiagnosed'),
                    from.compartments = 'primary',
                    to.compartments = 'secondary',
                    value = expression(1/duration.primary * (1-proportion.diagnosed.primary)) ,
                    tag = 'primary to secondary undiagnosed')
register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    applies.to = list('continuum'='undiagnosed'),
                    groups = 'infected',
                    from.compartments = 'secondary',
                    to.compartments = 'early.latent',
                    value = expression(1/duration.secondary * (1-proportion.diagnosed.secondary)),
                    tag = 'secondary to EL undiagnosed')

register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    applies.to = list('continuum'='diagnosed.untreated'),
                    from.compartments = 'primary',
                    to.compartments = 'secondary',
                    value = 48 , #'@PK: to ensure that they can't remain in this stage 
                    tag = 'primary to secondary diagnosed')

register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    applies.to = list('continuum'='diagnosed.untreated'),
                    groups = 'infected',
                    from.compartments = 'secondary',
                    to.compartments = 'early.latent',
                    value = 48,#'@PK: to ensure that they can't remain in this stage 
                    tag = 'secondary to EL diagnosed')
### Relapse:
register.model.element(SHIELD.SPECIFICATION,
                       name = 'prop.el.to.secondary',
                       scale = 'proportion',
                       value = SHIELD_BASE_PARAMETER_VALUES['prop.el.to.secondary'])
register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = 'early.latent',
                    to.compartments = 'secondary',
                    value = expression(prop.el.to.secondary * (1/duration.el)))
### EL to LL:
register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = 'early.latent',
                    to.compartments = 'late.latent',
                    value = expression((1-prop.el.to.secondary) * (1/duration.el)))
## LL to tertiary (by sex)
register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = 'late.latent',
                    to.compartments = 'tertiary',
                    value = 'rate.ll.to.tertiary')
register.model.element(SHIELD.SPECIFICATION,
                       name = 'rate.ll.to.tertiary.male',
                       scale = 'rate',
                       value = SHIELD_BASE_PARAMETER_VALUES['rate.ll.to.tertiary.male'])
register.model.element(SHIELD.SPECIFICATION,
                       name = 'rate.ll.to.tertiary.female',
                       scale = 'rate',
                       value = SHIELD_BASE_PARAMETER_VALUES['rate.ll.to.tertiary.female'])
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'rate.ll.to.tertiary',
                        value = 'rate.ll.to.tertiary.male') #*some multiplier
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'rate.ll.to.tertiary',
                               applies.to = list(sex='female'),
                               value = 'rate.ll.to.tertiary.female')
## states to CNS (by sex)
register.model.element(SHIELD.SPECIFICATION,
                       name = 'rate.non.ll.to.cns',
                       scale = 'rate',
                       value = SHIELD_BASE_PARAMETER_VALUES['rate.non.ll.to.cns']) #* can add multiplier to sample 
register.transition(SHIELD.SPECIFICATION, #@Todd: we will need to merge these 3 into one
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = c('primary'),
                    to.compartments = 'cns',
                    value = 'rate.non.ll.to.cns')
register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = c('secondary'),
                    to.compartments = 'cns',
                    value = 'rate.non.ll.to.cns')
register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = c('early.latent'),
                    to.compartments = 'cns',
                    value = 'rate.non.ll.to.cns')
#
register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = 'late.latent',
                    to.compartments = 'cns',
                    value = 'rate.ll.to.cns')
register.model.element(SHIELD.SPECIFICATION,
                       name = 'rate.ll.to.cns.male',
                       scale = 'rate',
                       value = SHIELD_BASE_PARAMETER_VALUES['rate.ll.to.cns.male']) 
register.model.element(SHIELD.SPECIFICATION,
                       name = 'rate.ll.to.cns.female',
                       scale = 'rate',
                       value = SHIELD_BASE_PARAMETER_VALUES['rate.ll.to.cns.female']) 
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'rate.ll.to.cns',
                        value = 'rate.ll.to.cns.male')  
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'rate.ll.to.cns',
                               applies.to = list(sex='female'),
                               value = 'rate.ll.to.cns.female')

#** CONTINUUM TRANSISION --# ----
##---- 1-SYMPTHOMATIC TESTING ----
# proportions that are asymptomatic during primary and secondary stages:
register.model.element.values(SHIELD.SPECIFICATION,
                              'prp.asymptomatic.primary.msm'=SHIELD_BASE_PARAMETER_VALUES['prp.asymptomatic.primary.msm'] ,
                              'prp.asymptomatic.primary.heterosexual.male'=SHIELD_BASE_PARAMETER_VALUES['prp.asymptomatic.primary.heterosexual.male'] ,
                              'prp.asymptomatic.primary.female'=SHIELD_BASE_PARAMETER_VALUES['prp.asymptomatic.primary.female'] ,
                              'prp.asymptomatic.secondary.msm'=SHIELD_BASE_PARAMETER_VALUES['prp.asymptomatic.secondary.msm'] ,
                              'prp.asymptomatic.secondary.heterosexual.male'=SHIELD_BASE_PARAMETER_VALUES['prp.asymptomatic.secondary.heterosexual.male'] ,
                              'prp.asymptomatic.secondary.female'=SHIELD_BASE_PARAMETER_VALUES['prp.asymptomatic.secondary.female'] ,
                              scale = 'rate')

##primary #@PK: to remove this
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'prp.asymptomatic',
                        value = 0) 
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'prp.asymptomatic',
                               applies.to = list(stage='primary',sex='msm'),
                               value = 'prp.asymptomatic.primary.msm')  
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'prp.asymptomatic',
                               applies.to = list(stage='primary',sex='heterosexual.male'),
                               value = 'prp.asymptomatic.primary.heterosexual.male')      
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'prp.asymptomatic',
                               applies.to = list(stage='primary',sex='female'),
                               value = 'prp.asymptomatic.primary.female')      
##secondary
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'prp.asymptomatic',
                               applies.to = list(stage='secondary',sex='msm'),
                               value = 'prp.asymptomatic.secondary.msm')  
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'prp.asymptomatic',
                               applies.to = list(stage='secondary',sex='heterosexual.male'),
                               value = 'prp.asymptomatic.secondary.heterosexual.male')      
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'prp.asymptomatic',
                               applies.to = list(stage='secondary',sex='female'),
                               value = 'prp.asymptomatic.secondary.female')  

# Symptomatic testing rates:
# baseline testing rate * prp symptomatic
# (1-proportion.diagnosed.primary)
register.model.quantity(SHIELD.SPECIFICATION,
                       name = 'rate.testing.symptomatic',
                       scale = 'rate',
                       value = 0)

register.model.quantity.subset(SHIELD.SPECIFICATION,
                        name = 'rate.testing.symptomatic',
                        applies.to = list(stage='primary'),
                        value = expression(1/duration.primary * proportion.diagnosed.primary))

register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'rate.testing.symptomatic',
                               applies.to = list(stage='secondary'),
                               value = expression(1/duration.secondary * proportion.diagnosed.secondary))

register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'proportion.diagnosed.primary',
                        scale = 'proportion',
                        value =0)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                        name = 'proportion.diagnosed.primary',
                        applies.to = list(stage='primary'),
                        value = expression(1-prp.asymptomatic)) #*sensitivity of test in primary * care seeking behavior

register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'proportion.diagnosed.secondary',
                        scale = 'proportion',
                        value =0)

register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'proportion.diagnosed.secondary',
                               applies.to = list(stage='secondary'),
                               value = expression(1-prp.asymptomatic)) #*sensitivity of test in primary * care seeking behavior

register.model.element(SHIELD.SPECIFICATION,
                        name = 'rate.testing.tertiary',
                        scale = 'rate',
                        value =12) #average of 1month

register.model.element(SHIELD.SPECIFICATION,
                       name = 'rate.testing.cns',
                       scale = 'rate',
                       value =12) #average of 1month

register.model.quantity.subset(SHIELD.SPECIFICATION,
                        name = 'rate.testing.symptomatic',
                        applies.to = list(stage=c('tertiary')),
                        value ='rate.testing.tertiary' )

register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'rate.testing.symptomatic',
                               applies.to = list(stage=c('cns')),
                               value ='rate.testing.cns' )

##---- 2-SCREENING FOR ALL ----

#TBD- we want to estimate this from HIV Testing rates in JHEEM 
# background testing rate has a functional form and parameters are tuned 
# wei will calibrate this to testing data, and assume that screening rate is a multiple of that 

register.model.element(SHIELD.SPECIFICATION,
                       name = 'rate.testing.hiv.without.covid',
                       scale = 'rate',
                       get.functional.form.function = get.hiv.testing.functional.form,
                       functional.form.scale = 'proportion', #the funcational form takes a proportion and produces a rate (from BRFSS: have u had a hiv test in the last year)
                       functional.form.from.time = 2010)
register.model.quantity(SHIELD.SPECIFICATION, #later on we will add covid to it
                        name = 'rate.testing.hiv',
                        scale = 'rate',
                        value = 'rate.testing.hiv.without.covid')
# defining a STI multiplier
register.model.element(SHIELD.SPECIFICATION,
                       name = 'multiplier.syphilis.screening.to.hiv.tests',
                       scale = 'ratio',
                       get.functional.form.function = get.syphilis.to.hiv.testing.functional.form,
                       functional.form.from.time = 1980)

register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'rate.screening',
                        scale = 'rate',
                        value = expression(rate.testing.hiv * multiplier.syphilis.screening.to.hiv.tests))

##---- 3-PRENATAL SCREENING FOR PREGNANT WOMEN----
#TBD: prop of pregnant women receiving 'successful' prenatal screening 
# How to model treatment failures that still result in congenital syphilis? 
register.model.element(SHIELD.SPECIFICATION,
                       name = 'proportion.adequate.prenatal.screening.base',
                       scale = 'rate',
                       value = .86) #this will be a functional form of age/race/location 

register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'rate.prenatal.screening',
                        scale = 'rate',
                        value = expression( fertility.rate/(1.03) * proportion.adequate.prenatal.screening.base)) #is fertility.rate 0 for other ages?
# we need to factor multi-births into this (assuming 3% for now)
##---- Treatment ----
# a proportion will receive immediate treatment, another group will be delayed
# we need to sepearete this for each testing because the tratment is different 
register.model.element(SHIELD.SPECIFICATION,
                       name = 'proportion.treated.immediately.following.screening',
                       scale = 'proportion',
                       value = 0.9) # TBD 
register.model.element(SHIELD.SPECIFICATION,
                       name = 'proportion.treated.immediately.following.testing.symptomatic',
                       scale = 'proportion',
                       value = 0.9) # TBD 
register.model.element(SHIELD.SPECIFICATION,
                       name = 'proportion.treated.immediately.following.prenatal.screening',
                       scale = 'proportion',
                       value = 0.9) # TBD
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'rate.treated.immediately',
                        value = expression(rate.screening * proportion.treated.immediately.following.screening +
                                             rate.testing.symptomatic * proportion.treated.immediately.following.testing.symptomatic))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'rate.treated.immediately',
                               apply.function = 'add',
                               applies.to = list(sex='female',age=FERTILE.AGES),
                               value= expression(rate.prenatal.screening * proportion.treated.immediately.following.prenatal.screening))
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'rate.treatment.delayed',
                        value = expression(rate.screening * (1- proportion.treated.immediately.following.screening )+
                                             rate.testing.symptomatic * (1-proportion.treated.immediately.following.testing.symptomatic)))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'rate.treatment.delayed',
                               apply.function = 'add',
                               applies.to = list(sex='female',age=FERTILE.AGES),
                               value= expression(rate.prenatal.screening * (1-proportion.treated.immediately.following.prenatal.screening)))

###---- Remission:
register.remission(SHIELD.SPECIFICATION,
                   applies.to = list(continuum = 'undiagnosed'),
                   all.remissions.into.compartments = list(profile = 'diagnosed.treated'),
                   remission.rate.value = 'rate.treated.immediately',
                   remission.proportions.value = 'remission.prp',
                   tag = 'treated.immediately'
)
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'remission.prp',
                        value = 1)

###---- Delayed treatment:
#@'Todd: we want these prople to move from primary.undiagnosed to secondary.diagnosed.untreated 
#but transitions dont move in multiple dimensions 
register.transition(SHIELD.SPECIFICATION,
                    dimension = 'continuum',
                    groups = 'infected',
                    from.compartments =  'undiagnosed',
                    to.compartments = 'diagnosed.untreated',
                    value = 'rate.treatment.delayed')

register.model.element(SHIELD.SPECIFICATION,
                       name = 'rate.treated.with.delay',
                       scale = 'rate',
                       value = 1) # TBD: how long will it take for someone to get treated after a positive diagnosis? #'@PK

register.remission(SHIELD.SPECIFICATION,
                   applies.to = list(continuum = 'diagnosed.untreated'),
                   all.remissions.into.compartments = list(profile = 'diagnosed.treated'),
                   remission.rate.value = 'rate.treated.with.delay',
                   remission.proportions.value='remission.prp',
                   tag = 'treated.with.delay' )


#** TBD/CONTACT TRACING --# ----
#'@Todd
#'@PK: we should check the data on this first
#*
#*
#** OUTPUTS --#----
##--------------------------------------------------------------------------------------------------------------#
# !!!for dynamic transitions that change over time (e.g., testing), the anchor points are coded at the begginign of the year 
# (e.g., if transmission changes from 2000 to 2020, these dates represent jan 1st of those years)

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
                         keep.dimensions = c('location','age','race','sex'),
                         
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
track.dynamic.outcome(SHIELD.SPECIFICATION,
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
                      save=T,
                      exclude.tags = 'immigration'
)

##---- Deaths ----
#'@Todd: do we need to record syphilis deaths?
track.dynamic.outcome(SHIELD.SPECIFICATION,
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
                      save=T,
                      keep.dimensions = c('location','age','race','sex'),
                      exclude.tags = 'emigration',
)

##---- Migration ----
track.dynamic.outcome(SHIELD.SPECIFICATION,
                      name = 'immigration',
                      outcome.metadata = create.outcome.metadata(display.name = 'Immigration',
                                                                 description = "Number of People Immigrating into the Population in the Past Year",
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Number Immigrating',
                                                                 units = 'individuals'),
                      dynamic.quantity.name = 'births',
                      corresponding.data.outcome = 'adult.immigration', #'@Zoe: need to make sure we have this for the national model
                      include.tags = "immigration",
                      keep.dimensions = c('location','age','race','sex'))

track.dynamic.outcome(SHIELD.SPECIFICATION,
                      name = 'emigration',
                      outcome.metadata = create.outcome.metadata(display.name = 'Emigration',
                                                                 description = "Number of People Emigrating from the Population in the Past Year",
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Number Emigrating',
                                                                 units = 'individuals'),
                      dynamic.quantity.name = 'mortality',
                      corresponding.data.outcome = 'adult.emigration', #'@Zoe: need to make sure we have this for the national model
                      include.tags = "emigration",
                      keep.dimensions = c('location','age','race','sex'))

##---- HIV Testing -----
## ---- HIV testing:
# track.cumulative.proportion.from.rate(SHIELD.SPECIFICATION,
#                                       name = 'hiv.testing',
#                                       outcome.metadata = create.outcome.metadata(display.name = 'Proportion Tested',
#                                                                                  description = "The Proportion of General Population who Received an HIV Test in the Past Year",
#                                                                                  scale = 'proportion',
#                                                                                  axis.name = 'Proportion Tested',
#                                                                                  units = '%'),
#                                       rate.value = 'rate.testing.hiv.over.18',  
#                                       denominator.outcome =  'population.over.18',
#                                       keep.dimensions = c('location','age','race','sex'),
#                                       corresponding.data.outcome = 'proportion.tested', #'@Zoe: can you please rename this to proportion.hiv.tested?
#                                       subset.dimension.values = list(age=c('15-19 years','20-24 years','25-29 years', '30-34 years','35-39 years','40-44 years', '45-49 years','50-64 years','55-64 years','65+ years')), #we can drop the first agegroup because BRFSS data starts from 18-24
#                                       rename.dimension.values = list(age=c('15-19 years'='18-19 years')), #the code is smart to recognize that this agegroup falls within 18-24
#                                       save = T) #'@Todd: ERROR
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
                       ) #comes from CDC #@PK: to be checked 

# Build the quantity that we want to integrate in outputs:
register.model.quantity(SHIELD.SPECIFICATION,
                        'rate.testing.hiv.over.18', 
                        value = 'rate.testing.hiv' )

register.model.quantity.subset(SHIELD.SPECIFICATION,
                        'rate.testing.hiv.over.18',
                        applies.to = list('age'='15-19'),
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
                         subset.dimension.values = list(age=c('15-19 years','20-24 years','25-29 years', '30-34 years','35-39 years','40-44 years', '45-49 years','50-64 years','55-64 years','65+ years')), #we can drop the first agegroup because BRFSS data starts from 18-24
                         rename.dimension.values = list(age=c('15-19 years'='18-19 years')), #the code is smart to recognize that this agegroup falls within 18-24
                         outcome.metadata = NULL)


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
track.dynamic.outcome(SHIELD.SPECIFICATION,
                      name = 'diagnosis.total',
                      outcome.metadata = create.outcome.metadata(display.name = 'Number of Individuals with a Diagnosis of Non-Congenital Syphilis in the Past Year',
                                                                 description = 'Number of Individuals with a Diagnosis of Non-Congenital Syphilis in the Past Year',
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Cases',
                                                                 units = 'cases',
                                                                 singular.unit = 'case'),
                      scale='non.negative.number',
                      dynamic.quantity.name = 'remission.from', 
                      # corresponding.data.outcome = 'total.syphilis' #'@Zoe: should we add historical values
                      keep.dimensions = c('location','age','race','sex','stage')
)

track.cumulative.outcome(SHIELD.SPECIFICATION,
                         name = 'diagnosis.primary.secondary',
                         value = expression(diagnosis.total),
                         subset.dimension.values = list(stage='ps.stages'), # ps.stages=c('primary','secondary'),
                         outcome.metadata = create.outcome.metadata(display.name = 'Number of Individuals with a Diagnosis of Primary and Secondary Syphilis in the Past Year',
                                                                    description = 'Number of Individuals with a Diagnosis of Primary and Secondary Syphilis in the Past Year',
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Cases',
                                                                    units = 'cases',
                                                                    singular.unit = 'case'),
                         scale='non.negative.number',
                         corresponding.data.outcome = 'ps.syphilis' ,
                         keep.dimensions = c('location','age','race','sex')
)

track.cumulative.outcome(SHIELD.SPECIFICATION,
                         name = 'diagnosis.early.latent',
                         value = expression(diagnosis.total),
                         subset.dimension.values = list(stage='early.latent'),  
                         outcome.metadata = create.outcome.metadata(display.name = 'Number of Individuals with a Diagnosis of Early Latent Syphilis in the Past Year',
                                                                    description = 'Number of Individuals with a Diagnosis of Early Latent Syphilis in the Past Year',
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Cases',
                                                                    units = 'cases',
                                                                    singular.unit = 'case'),
                         scale='non.negative.number',
                         corresponding.data.outcome = 'early.syphilis',
                         keep.dimensions = c('location','age','race','sex')
)

track.cumulative.outcome(SHIELD.SPECIFICATION,
                         name = 'diagnosis.late.latent.unknown',
                         value = expression(diagnosis.total),
                         subset.dimension.values = list(stage='late.stages'), # late.stages=c('late.latent','tertiary','cns')
                         outcome.metadata = create.outcome.metadata(display.name = 'Number of Individuals with a Diagnosis of Late Latent Or Unknown Syphilis in the Past Year',
                                                                    description = 'Number of Individuals with a Diagnosis of Late Latent Or Unknown Syphilis in the Past Year',
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Cases',
                                                                    units = 'cases',
                                                                    singular.unit = 'case'),
                         scale='non.negative.number',
                         corresponding.data.outcome = 'unknown.duration.or.late.syphilis', 
                         keep.dimensions = c('location','age','race','sex')
)

### CNS
track.dynamic.outcome(SHIELD.SPECIFICATION,
                      name = 'diagnosis.cns',
                      subset.dimension.values = list(stage='cns'),
                      outcome.metadata = create.outcome.metadata(display.name = 'Number of Individuals with a Diagnosis of CNS Syphilis in the Past Year',
                                                                 description = 'Number of Individuals with a Diagnosis of CNS Syphilis in the Past Year',
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Cases',
                                                                 units = 'cases',
                                                                 singular.unit = 'case'),
                      scale='non.negative.number',
                      dynamic.quantity.name = 'remission.from', #where they come from
                      # corresponding.data.outcome = 'cns.syphilis', #'Zoe: can we add this for historical years
                      keep.dimensions = c('location','age','race','sex','stage')
)
### congenital diagnosis
track.dynamic.outcome(SHIELD.SPECIFICATION,
                      name='diagnosis.congenital',
                      groups = 'infected',
                      outcome.metadata = create.outcome.metadata(display.name = 'Number of Congenital Syphilis Diagnosis in the Past Year',
                                                                 description = 'Number of Congenital Syphilis Diagnosis in the Past Year',
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Persons',
                                                                 units = 'persons',
                                                                 singular.unit = 'person'),
                      scale='non.negative.number',
                      multiply.by = expression(proportion.adequate.prenatal.screening.base * (1-efficacy.of.prenatal.screening) * proportion.treated.immediately.following.prenatal.screening +
                                                 (1-proportion.adequate.prenatal.screening.base* proportion.treated.immediately.following.prenatal.screening )  ), #prp of preg that pass on congenital syphilis
                      dynamic.quantity.name = 'births.from',  #model has an internal definition for births  #births from is conditional on parent's characteristics
                      corresponding.data.outcome = 'congenital.syphilis',
                      save=T,
                      keep.dimensions = c('location') #collapse on stage and continuum for infected and on profile as well
)



register.model.element(SHIELD.SPECIFICATION,
                       name='efficacy.of.prenatal.screening',
                       scale='proportion',
                       value=.982)

# HIV test # TBD 
# track.cumulative.proportion.from.rate(SHIELD.SPECIFICATION,
#                                       name = 'testing',
#                                       outcome.metadata = create.outcome.metadata(display.name = 'Proportion Tested',
#                                                                                  description = 'The Proportion of General Population who Received an HIV Test in the Past Year',
#                                                                                  scale = 'proportion',
#                                                                                  axis.name = 'Proportion Tested',
#                                                                                  units = '%'),
#                                       rate.value = 'general.population.testing.over.18',
#                                       denominator.outcome = 'cumulative.uninfected.over.18',
#                                       keep.dimensions = c('location','age','race','sex','risk'),
#                                       corresponding.data.outcome = 'proportion.tested',
#                                       rename.dimension.values = list(age=c('13-24 years'='18-24 years')),
#                                       save = T)
##---- Syphilis Treatment Initiations ----
## Immediate and Delayed
track.dynamic.outcome(SHIELD.SPECIFICATION,
                      name = 'trt.initiation',
                      outcome.metadata = create.outcome.metadata(display.name = 'Treatment Initiation',
                                                                 description = 'Number of Individuals Starting Treatment in the Past Year',
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Cases',
                                                                 units = 'cases',
                                                                 singular.unit = 'case'),
                      dynamic.quantity.name = 'remission.from', #where they come from
                      keep.dimensions = c('location','age','race','sex','stage')
)



##** REGISTER THE SPECIFICATION ----
register.model.specification(SHIELD.SPECIFICATION)
#

register.calibrated.parameters.for.version('shield',
                                           distribution = SHIELD.FULL.PARAMETERS.PRIOR,
                                           apply.function = SHIELD.APPLY.PARAMETERS.FN,
                                           sampling.blocks = SHIELD.FULL.PARAMETERS.SAMPLING.BLOCKS,
                                           calibrate.to.year = 2025,
                                           join.with.previous.version = F)
print('Calibration parameters registered for DEMOGRAPHIC model')


print('SHIELD specification sourced successfully!')
cat('*** Shield_specification.R completed! ***\n')


#next steps: 
# list of needed parameters
# adding all outcomes
# paramater mappings
# Adding likelihoods 
#