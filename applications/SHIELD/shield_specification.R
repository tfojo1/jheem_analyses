cat("*** Running Shiled_specification.R ***\n")
DEFAULT.FIX.STRATA.YEAR=2010 # full population breakdown is available post-2010, and birth data is available post 2007. 

#CASHED FOLDER:
# https://livejohnshopkins-my.sharepoint.com/personal/tfojo1_jh_edu/_layouts/15/onedrive.aspx?e=5%3A940bf48ba6e0498495fea5596e3dc8e7&sharingv2=true&fromShare=true&at=9&CID=425e54af%2De78b%2D4d53%2D8df4%2D6abb10af6339&id=%2Fpersonal%2Ftfojo1%5Fjh%5Fedu%2FDocuments%2FJHEEM2&FolderCTID=0x012000E74D427C3A55BC45A1C18C850CDA2DB4&view=0
# Excel Sheet:
# https://livejohnshopkins-my.sharepoint.com/:x:/g/personal/zdansky1_jh_edu/EVrQ-OpGqlVIpBi_KE0P6v4B2rTpIvYcyUtLz9e1NH_oig?e=kd8bjH&wdLOR=c06087FCD-0041-804E-BB9F-F582185054BC
# https://jheem.shinyapps.io/EndingHIV/

# > specification.metadata=get.specification.metadata("shield","C.12580")
# > specification.metadata=get.specification.metadata("shield","US")



# NEXT STEPS:
# Develop a functional form for fertility 
# likelihoods 

# Working directory is set to the main JHEEM_Analysis folder:
# JHEEM.DIR="~/OneDrive - Johns Hopkins/JHEEM/Simulation/code/jheem_analyses/"
# SHIELD.DIR="~/OneDrive - Johns Hopkins/JHEEM/Simulation/code/jheem_analyses/applications/SHIELD/"
# setwd(JHEEM.DIR)
# setwd('../../')
source('applications/SHIELD/shield_source_code.R')
##--------------------------------------------------------------------------------------------------------------#
#-- INITIAL SET-UP --#----
SHIELD.SPECIFICATION = create.jheem.specification(version = 'shield',
                                                  iteration=1,
                                                  description = "The initial SHIELD version, set up to model national epidemic",
                                                  start.year = 1940,
                                                  age.endpoints=c(0,15,20,25,30,35,40,45,50,55,65,Inf), #11 agegroups, similar to atlas [0-15][16-20];
                                                  compartments.for.infected.only = list(
                                                    continuum = c('undiagnosed', 'diagnosed.untreated'),
                                                    stage = c('primary','secondary', 'el','ll','tertiary','cns')                                                  ),
                                                  
                                                  compartments.for.uninfected.only = list(
                                                    profile=c('susceptible','diagnosed.treated')),
                                                  
                                                  compartments.for.infected.and.uninfected = list(
                                                    location = "US",
                                                    age = 'all.ages',
                                                    race=c('black','hispanic','other'),
                                                    sex= c('heterosexual_male', 'msm', 'female')
                                                  ),
                                                  compartment.value.aliases = list(
                                                    #try using aliases so that if we change the specification up here, the rest of the code doesnt break
                                                    # helps to define specifications for groups of compartments later on
                                                    diagnosed.treated.states=c('diagnosed.treated'),
                                                    ps.stages=c('primary','secondary'),
                                                    early.stages=c('primary','secondary','el'),
                                                    late.stages=c('ll','tertiary','cns')
                                                  )
)


##---- Fix Strata Sizes----
#'@title Set Whether to Fix Strata Sizes During a Time Period # a simplifying assumption to avoid modeling population demographic dynamics before year X
# 2007 earliest year for complete census data
register.fixed.model.strata(SHIELD.SPECIFICATION,
                            applies.after.time = -Inf,
                            applies.before.time = DEFAULT.FIX.STRATA.YEAR,
                            fix.strata = T,
                            dimensions.to.fix = c('location','age','race','sex')
)
#-- INITIAL POPULATION --#----
# Specify the initial compartment sizes  
##---- Base Population ----
# step1: defines a blank quantity
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'base.initial.population',
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
                               name = 'base.initial.population',
                               applies.to = list(sex='female'),
                               value = 'base.initial.female.population')
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'base.initial.population',
                               applies.to = list(sex='msm'),
                               value = expression(base.initial.male.population * proportion.msm.of.male))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'base.initial.population',
                               applies.to = list(sex='heterosexual_male'),
                               value = expression(base.initial.male.population * (1-proportion.msm.of.male)))
# step3: uses an element with functional form to get required values
register.model.element(SHIELD.SPECIFICATION,
                       name = 'base.initial.female.population',
                       get.value.function = get.base.initial.female.population,
                       scale = 'non.negative.number')
register.model.element(SHIELD.SPECIFICATION,
                       name = 'base.initial.male.population',
                       get.value.function = get.base.initial.male.population,
                       scale = 'non.negative.number')

##---- Infected ----
# dummy values: 0.5% are infected, and they are 50% in PS and 50% in Ter stage and all undiagnosed
# register.model.quantity(SHIELD.SPECIFICATION,
#                         name = 'initial.population.infected',
#                         value = 0 )
# register.model.quantity.subset(SHIELD.SPECIFICATION,
#                                name = 'initial.population.infected',
#                                value = expression(base.initial.population* 0.005 * .5),
#                                applies.to = list(continuum='undiagnosed', stage='ps'))
# register.model.quantity.subset(SHIELD.SPECIFICATION,
#                                name = 'initial.population.infected',
#                                value = expression(base.initial.population* 0.005 *.5),
#                                applies.to = list(continuum='undiagnosed', stage='ter'))
# register.initial.population(SHIELD.SPECIFICATION,
#                             group = 'infected',
#                             value = 'initial.population.infected')
##---- Uninfected ----
register.initial.population(SHIELD.SPECIFICATION,
                            group = 'uninfected',
                            value = 'initial.population.uninfected')
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'initial.population.uninfected',
                        value = 1)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'initial.population.uninfected',
                               value = expression(base.initial.population * 1),
                               applies.to = list(profile='susceptible'))

#-- NATALITY --# ----
# we model births based on women's fertility rates
register.natality(specification = SHIELD.SPECIFICATION,
                  parent.groups = c('infected', 'uninfected'),
                  applies.to = list(sex='female',age=FERTILE.AGES), #only women 15-44 give birth
                  child.groups = 'uninfected',
                  fertility.rate.value = 'fertility.rate',
                  birth.proportions.value = 'birth.proportions', # when they are born, where do they go? (e.g., what age, what race, what sex will they have)
                  parent.child.concordant.dimensions = c('race'),# the race of the cild will be the same
                  all.births.into.compartments = list(profile='susceptible',age= 1),
                  tag = 'births')
##---- Fertility ----
register.model.element(SHIELD.SPECIFICATION,
                       name = 'fertility.rate',
                       get.functional.form.function = get.fertility.rates.functional.form,
                       functional.form.from.time = DEFAULT.POPULATION.YEARS, #only projects values from 2010 forward
                       scale = 'rate')

##---- Birth Proportions ----
# to determine where newborns should go
# currently set as a fix ratio
register.model.element(SHIELD.SPECIFICATION,
                       name = 'fraction.male.births',
                       scale = 'proportion',
                       value = SHIELD_BASE_PARAMETER_VALUES['male.to.female.birth.ratio'] / (1+SHIELD_BASE_PARAMETER_VALUES['male.to.female.birth.ratio']))
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'birth.proportions',
                        value =0)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               applies.to = list(sex.to='msm'),
                               name = 'birth.proportions',
                               value = expression( fraction.male.births*proportion.msm.of.male ))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               applies.to = list(sex.to='heterosexual_male'),
                               name = 'birth.proportions',
                               value = expression( fraction.male.births*(1-proportion.msm.of.male)  ))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               applies.to = list(sex.to='female'),
                               name = 'birth.proportions',
                               value = expression( 1-fraction.male.births ))

#-- MORTALITY --# ----
##---- General Mortality ----
register.mortality(SHIELD.SPECIFICATION,
                   tag = 'general.mortality',
                   groups = c('infected','uninfected'),
                   mortality.rate.value = 'general.mortality.rate')
register.model.element(SHIELD.SPECIFICATION,
                       name = 'general.mortality.rate',
                       get.functional.form.function = get.location.mortality.rates.functional.form,
                       scale = 'rate')

## -- AGING --## ----
register.model.element(SHIELD.SPECIFICATION,
                       name = 'general.aging',
                       scale = 'rate',
                       get.functional.form.function = get.empiric.aging.rates,
                       functional.form.from.time = 2007)

register.aging(SHIELD.SPECIFICATION,
               groups = c('uninfected','infected'),
               aging.rate.value = 'general.aging')

#-- TBD/ MIGRATION --# ----
# immigration/emigration doesnt depent on disease state- it's used to fit the population size but doesnt change the prevalence of the disease
# the prior comes from census reports (prp getting in and leaving MSAs) - these didnt represent immigrations from other countries but more movements between MSAs
# oneway stratification only for one timepoint  (2011-2015) (2016-2020) breakdown by age, by race, by sex only for 2020
# 2010-2020

# total immigration/population: prior is some fraction of this distributed equally by age and race

# when we add immigration, we need to make sure that we dont count immigrants in Births and emmigrants as Deaths!!


#-- TRANSMISSION --# ----
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
                       name="global.trate",
                       scale = "rate",
                       value = 1 )

# rate of contact between infected and uninfected
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'sexual.contact',
                        value = expression(global.trate *
                                             sexual.transmission.rates *
                                             sexual.contact.by.age*
                                             sexual.contact.by.sex*
                                             sexual.contact.by.race ))
##---- Sexual Contact: Transmission Rates ----
# probability of transmission through sexual act, it depends on the recipient, person getting infected
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'sexual.transmission.rates',
                        value = 0)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.transmission.rates',
                               applies.to = list(sex.from=c('heterosexual_male','msm'),
                                                 sex.to=c('heterosexual_male','msm')),
                               value = 'msm.trates') #we can add msm.peak.multiplier later if needed
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.transmission.rates',
                               applies.to = list(sex.from=c('heterosexual_male','msm'),
                                                 sex.to=c('female')),
                               value = 'heterosexual.trates')
register.model.quantity.subset(SHIELD.SPECIFICATION, #right now it's assuming that female to male is the same as male to female
                               name = 'sexual.transmission.rates',
                               applies.to = list(sex.from=c('female'),
                                                 sex.to=c('heterosexual_male','msm')),
                               value = 'heterosexual.trates')
#spline models can project negative values - we should truncate to 0
# use the log scale: exponentiate the values # log scale for the knots,
# we define this as a spline with 2 knots, but we have no rpior about their values. they will change in calibration
# Future Expnasions: msm.trate.by.race #add more alphas #if we assume the multiplier by black vs other remain the sma over time we only ned 2
register.model.element(SHIELD.SPECIFICATION,
                       name = 'msm.trates',
                       functional.form = create.natural.spline.functional.form(knot.times = c(time0=2000, time1=2010, time2=2020),
                                                                               #if we wanted to use a natural spline without log transformation:
                                                                               # knot.values = list(time0=1,time1=1,time2=1) , knots.are.on.transformed.scale = F, #on the identity scale
                                                                               #use a log(y) transformation, so that all returned values are positive
                                                                               # this will also help with data that is skewed to right (long right tail)
                                                                               knot.values = list(time0=0,time1=0,time2=0) ,
                                                                               knots.are.on.transformed.scale = T, #knots on the log scale (value is exp(0))
                                                                               #
                                                                               min=0, #even after using log for knots, value can be negative so we need to truncate
                                                                               knot.link = 'log',link='identity'), #knots on the log-scale and values on the identity scale
                       functional.form.from.time = 1960, #0 or -Inf #@TODD: what is this?
                       scale='rate') #spline with 2010/2020

register.model.element(SHIELD.SPECIFICATION,
                       name = 'heterosexual.trates',
                       functional.form = create.linear.spline.functional.form(knot.times = c(time0=2000, time1=2010, time2=2020),
                                                                              knot.values = list(time0=0,time1=0,time2=0) ,
                                                                              knots.are.on.transformed.scale = T, #knots on the log scale (value is exp(0))
                                                                              min=0,
                                                                              knot.link = 'log',link='identity') ,
                       functional.form.from.time = 1960, #0 or -Inf #@TODD
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
                                                 sex.from=c('heterosexual_male','msm')),
                               value = get.female.sexual.age.contact.proportions)

# sexual contact by age for males who have sex with male partners
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.contact.by.age',
                               applies.to = list(sex.to=c('heterosexual_male','msm'),
                                                 sex.from=c('heterosexual_male','msm')),
                               value = get.msm.sexual.age.contact.proportions)
#
# x=get.msm.sexual.age.contact.proportions(specification.metadata = get.specification.metadata("shield",location = "C.12580"),
#                                        single.year.msm.age.counts = get.msm.single.year.age.counts(location = "C.12580",specification.metadata = get.specification.metadata("shield",location = "C.12580"),population.years = 2007),
#                                        single.year.age.sexual.availability = get.sexual.availability(),age.mixing.sd.mult = 1)
# sexual contact by age for males who have sex with female partners
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.contact.by.age',
                               applies.to = list(sex.to=c('heterosexual_male','msm'),
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
                               applies.to=list(sex.from='heterosexual_male',
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
                               value = expression((1-fraction.heterosexual.male.pairings.with.male))
)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.contact.by.sex',
                               applies.to=list(sex.from='msm',
                                               sex.to='heterosexual_male'),
                               value = expression(fraction.heterosexual.male.pairings.with.male *
                                                    fraction.male.male.that.are.with.msm)
)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.contact.by.sex',
                               applies.to=list(sex.from='heterosexual_male',
                                               sex.to='heterosexual_male'),
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
                               applies.to=list(stage="primary"),
                               value = 'primary.rel.secondary.transmissibility')# >> register as an element> so we can vary it if we want to

register.model.element(SHIELD.SPECIFICATION,
                       name = 'primary.rel.secondary.transmissibility',
                       scale = 'ratio',
                       value = SHIELD_BASE_PARAMETER_VALUES['primary.rel.secondary.transmissibility'])

register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.transmissibility',
                               applies.to=list(stage="secondary"),
                               value = 1)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.transmissibility',
                               applies.to=list(stage="el"),
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
                       'race.population.counts',
                       scale = 'non.negative.number',
                       get.value.function = get.race.population.counts)




#-- NATURAL HISTORY --# ----
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
                    from.compartments = 'primary',
                    to.compartments = 'secondary',
                    value = expression(1/duration.primary))
register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = 'secondary',
                    to.compartments = 'el',
                    value = expression(1/duration.secondary))
### Relapse:
register.model.element(SHIELD.SPECIFICATION,
                       name = 'prop.el.to.secondary',
                       scale = 'proportion',
                       value = SHIELD_BASE_PARAMETER_VALUES['prop.el.to.secondary'])
register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = 'el',
                    to.compartments = 'secondary',
                    value = expression(prop.el.to.secondary * (1/duration.el)))
### EL to LL:
register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = 'el',
                    to.compartments = 'll',
                    value = expression((1-prop.el.to.secondary) * (1/duration.el)))
## LL to tertiary (by sex)
register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = 'll',
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
                    from.compartments = c('el'),
                    to.compartments = 'cns',
                    value = 'rate.non.ll.to.cns')
#
register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = 'll',
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

#-- CONTINUUM TRANSISION --# ----
##---- 1-SYMPTHOMATIC TESTING ----
# proportions that are asymptomatic during primary and secondary stages:
register.model.element(SHIELD.SPECIFICATION,
                       name = 'prp.asymptomatic.primary.msm',
                       scale = 'rate',
                       value = SHIELD_BASE_PARAMETER_VALUES['prp.asymptomatic.primary.msm'])
register.model.element(SHIELD.SPECIFICATION,
                       name = 'prp.asymptomatic.primary.heterosexual_male',
                       scale = 'rate',
                       value = SHIELD_BASE_PARAMETER_VALUES['prp.asymptomatic.primary.heterosexual_male'])
register.model.element(SHIELD.SPECIFICATION,
                       name = 'prp.asymptomatic.primary.female',
                       scale = 'rate',
                       value = SHIELD_BASE_PARAMETER_VALUES['prp.asymptomatic.primary.female'])
register.model.element(SHIELD.SPECIFICATION,
                       name = 'prp.asymptomatic.secondary.msm',
                       scale = 'rate',
                       value = SHIELD_BASE_PARAMETER_VALUES['prp.asymptomatic.secondary.msm'])
register.model.element(SHIELD.SPECIFICATION,
                       name = 'prp.asymptomatic.secondary.heterosexual_male',
                       scale = 'rate',
                       value = SHIELD_BASE_PARAMETER_VALUES['prp.asymptomatic.secondary.heterosexual_male'])
register.model.element(SHIELD.SPECIFICATION,
                       name = 'prp.asymptomatic.secondary.female',
                       scale = 'rate',
                       value = SHIELD_BASE_PARAMETER_VALUES['prp.asymptomatic.secondary.female'])
#
##primary
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'prp.asymptomatic',
                        value = 0) 
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'prp.asymptomatic',
                               applies.to = list(stage='primary',sex='msm'),
                               value = 'prp.asymptomatic.primary.msm')  
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'prp.asymptomatic',
                               applies.to = list(stage='primary',sex='heterosexual_male'),
                               value = 'prp.asymptomatic.primary.heterosexual_male')      
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
                               applies.to = list(stage='secondary',sex='heterosexual_male'),
                               value = 'prp.asymptomatic.secondary.heterosexual_male')      
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'prp.asymptomatic',
                               applies.to = list(stage='secondary',sex='female'),
                               value = 'prp.asymptomatic.secondary.female')  
#latent (1 for all)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'prp.asymptomatic',
                               applies.to = list(stage=c('el','ll')),
                               value = SHIELD_BASE_PARAMETER_VALUES['prp.asymptomatic.latent']) 
#tertiary and CNS (0 for all)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'prp.asymptomatic',
                               applies.to = list(stage=c('tertiary','cns')),
                               value = SHIELD_BASE_PARAMETER_VALUES['prp.asymptomatic.tertiary.cns']) 
# Symptomatic testing rates:
# baseline testing rate * prp symptomatic
register.model.element(SHIELD.SPECIFICATION,
                       name = 'rate.testing.symptomatic.base',
                       scale = 'rate',
                       value = SHIELD_BASE_PARAMETER_VALUES['rate.testing.symptomatic.base'])
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'rate.testing.symptomatic',
                        scale = 'rate',
                        value = expression(rate.testing.symptomatic.base * (1-prp.asymptomatic)))
##---- 2-SCREENING FOR ALL ----
register.model.element(SHIELD.SPECIFICATION,
                       name = 'rate.screening',
                       scale = 'rate',
                       value = 1)
#TBD- we want to estimate this from HIV Testing rates in JHEEM 

##---- 3-PRENATAL SCREENING FOR PREGNANT WOMEN----
#TBD: prop of pregnant women receiving 'successful' prenatal screening 
# How to model treatment failures that still result in congenital syphilis? 
register.model.element(SHIELD.SPECIFICATION,
                       name = 'proportion.prenatal.screening.base',
                       scale = 'rate',
                       value = 1)
register.model.quantity(SHIELD.SPECIFICATION,
                       name = 'rate.prenatal.screening',
                       scale = 'rate',
                       value = 0)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                        name = 'rate.prenatal.screening',
                        applies.to = list(group='infected',sex='female',age=FERTILE.AGES), #only women 15-44 give birth
                        value = expression( fertility.rate * proportion.prenatal.screening.base)) #is fertility.rate 0 for other ages?
                        

##---- Modeling the tests/diagnosis -----
#  the model knows that testing applies to undiagnosed group from where they end up in the following steps, so we dont have to apply to that dimension (continuum = 'undiagnosed')
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'rate.testing.total',
                        scale='rate',
                        value= 0) 
#adding screening+symptomatic testing
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'rate.testing.total',
                               applies.to = list( group='infected'),
                               expression(rate.screening +rate.testing.symptomatic) )
#adding prenatal testing for eligible women
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'rate.testing.total',
                               applies.to = list(group='infected',sex='female',age=FERTILE.AGES),
                               value = 'rate.prenatal.screening',
                               apply.function = 'add')
 
##---- Treatment ----
# a proportion will receive immediate treatment, another group will be delayed
register.model.element(SHIELD.SPECIFICATION,
                       name = 'proportion.treated.immediately',
                       scale = 'proportion',
                       value = 0) 
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'rate.treated.immediately',
                        value = expression(rate.testing.total * proportion.treated.immediately))
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'rate.treatment.delayed',
                        value = expression(rate.testing.total * (1-proportion.treated.immediately)))

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
register.transition(SHIELD.SPECIFICATION,
                    dimension = 'continuum',
                    groups = 'infected',
                    from.compartments =  'undiagnosed',
                    to.compartments = 'diagnosed.untreated',
                    value = 'rate.treatment.delayed')

register.model.element(SHIELD.SPECIFICATION,
                       name = 'rate.treated.with.delay',
                       scale = 'rate',
                       value = 1) ## ??????

register.remission(SHIELD.SPECIFICATION,
                   applies.to = list(continuum = 'diagnosed.untreated'),
                   all.remissions.into.compartments = list(profile = 'diagnosed.treated'),
                   remission.rate.value = 'rate.treated.with.delay',
                   remission.proportions.value='remission.prp',
                   tag = 'treated.with.delay' )


#-- OUTPUTS --#----
##--------------------------------------------------------------------------------------------------------------#
# !!!for dynamic transitions that change over time (e.g., testing), the anchor points are coded at the begginign of the year 
# (e.g., if transmission changes from 2000 to 2020, these dates represent jan 1st of those years)

## Population ----
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
                                                                    description = "Population size",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Persons',
                                                                    units = 'persons',
                                                                    singular.unit = 'person'), #will read the scale from metadata
                         value.to.integrate = 'point.population',
                         corresponding.data.outcome = 'population' ,
                         keep.dimensions = c('location','age','race','sex'),
                         
)

## Fertility Rate ----
track.cumulative.outcome(SHIELD.SPECIFICATION,
                         name='fertility.rate',
                         value=expression(births.from/population),
                         denominator.outcome = 'population',
                         keep.dimensions =  c('location','age','race','sex'),
                         subset.dimension.values = list(sex='female'),
                         corresponding.data.outcome = "fertility.rate",
                         outcome.metadata = create.outcome.metadata(display.name = 'Fetility Rate',
                                                                    description = "Fetility Rate",
                                                                    scale = 'rate',
                                                                    axis.name = 'Rate',
                                                                    units = 'rate',
                                                                    singular.unit = 'person')
)


## Births ----
track.dynamic.outcome(SHIELD.SPECIFICATION,
                      name='births.from',
                      outcome.metadata = create.outcome.metadata(display.name = 'Births',
                                                                 description = "Births",
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Persons',
                                                                 units = 'persons',
                                                                 singular.unit = 'person'),
                      scale='non.negative.number',
                      save=T,
                      corresponding.data.outcome = "births",
                      dynamic.quantity.name = 'births.from', #model has an internal definition for births  #births from is conditional on parent's characteristics
                      keep.dimensions = c('location','age','race','sex'), #collapse on stage and continuum for infected and on profile as well
                      subset.dimension.values = list(sex='female') #this seems redundant but it filters all the male rows with 0 values.
)

## Deaths ----
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
                      # exclude.tags = "emigration",
                      save=T,
                      keep.dimensions = c('location','age','race','sex')
)

## Incidence ---- #@TODD: incidence has starting and ending compartment. why is it a dynamic outcome not a transition? 
# (new infections + reinfections)
track.dynamic.outcome(SHIELD.SPECIFICATION,
                      name = 'incidence',
                      outcome.metadata = create.outcome.metadata(display.name = 'Incidence',
                                                                 description = "Number of Individuals Infected with Syphilis in the Past Year",
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Cases',
                                                                 units = 'cases',
                                                                 singular.unit = 'case'),
                      dynamic.quantity.name = 'incidence.from', # use of ".from" helps us track where individuals are coming from (differentiate new vs re-infections)
                      keep.dimensions = c('location','age','race','sex','profile')
)

## Treatment Initiations ----
# # : Immediate and Delayed
# track.dynamic.outcome(SHIELD.SPECIFICATION,
#                       name = 'trt.initiation',
#                       outcome.metadata = create.outcome.metadata(display.name = 'Treatment Initiation',
#                                                                  description = "Number of Individuals Starting Treatment in the Past Year",
#                                                                  scale = 'non.negative.number',
#                                                                  axis.name = 'Cases',
#                                                                  units = 'cases',
#                                                                  singular.unit = 'case'),
#                       dynamic.quantity.name = "remission.from", #where they come from
#                       keep.dimensions = c('location','age','race','sex','stage')
# )

## Diagnosis ----
## All Diagnosis (at all stages ): has 2 components
### 1st component:new diagnosis that dont receive treatment immediately (transition)
# track.transition(SHIELD.SPECIFICATION,
#                  name = 'diag.untreated',
#                  #display name on the graph
#                  outcome.metadata = create.outcome.metadata(display.name = 'Diagnoses: Delayed Treatment',
#                                                             description = "Number of Individuals with a New Diagnosis of Syphilis that dont Start Treatment Immediately in the Past Year",
#                                                             scale = 'non.negative.number',
#                                                             axis.name = 'Cases',
#                                                             units = 'cases',
#                                                             singular.unit = 'case'),
#                  dimension = 'continuum',#transition along the continuum
#                  from.compartments = 'undiagnosed',
#                  to.compartments = 'diagnosed.untreated',
#                  keep.dimensions = c('location','age','race','sex','stage')
# )
# #2 components: new diagnosis that receive treatment immediately (remission)
# track.dynamic.outcome(SHIELD.SPECIFICATION,
#                       name = 'diag.treated',
#                       outcome.metadata = create.outcome.metadata(display.name = 'Diagnoses: Immediate Treatment',
#                                                                  description = "Number of Individuals with a New Diagnosis of Syphilis that Start Treatment Immediately in the Past Year",
#                                                                  scale = 'non.negative.number',
#                                                                  axis.name = 'Cases',
#                                                                  units = 'cases',
#                                                                  singular.unit = 'case'),
#                       dynamic.quantity.name = "remission.from", #where they come from
#                       subset.dimension.values = list(continuum='undiagnosed'), #only counting those who receive immediata trt
#                       keep.dimensions = c('location','age','race','sex','stage')
# )
# 
# #Total diagnosis
# #We usually try to outline the outcomes here with the final outcomes we need for calibrations: so we break them by stage
# ## primarySecondary
# track.cumulative.outcome(SHIELD.SPECIFICATION,
#                          name = "diag.ps",
#                          value = expression(diag.untreated + diag.treated),
#                          subset.dimension.values = list(stage='ps.stages'),
#                          outcome.metadata = create.outcome.metadata(display.name = 'Diagnoses PS',
#                                                                     description = "Number of Individuals with a Diagnosis of Primary-Secondary Syphilis in the Past Year",
#                                                                     scale = 'non.negative.number',
#                                                                     axis.name = 'Cases',
#                                                                     units = 'cases',
#                                                                     singular.unit = 'case'),
#                          keep.dimensions = c('location','age','race','sex'),
#                          corresponding.data.outcome = 'ps.syphilis' #corresponding to the name in data manager
# )
# track.cumulative.outcome(SHIELD.SPECIFICATION,
#                          name = "diag.el",
#                          value = expression(diag.untreated + diag.treated),
#                          subset.dimension.values = list(stage='el.stages'),
#                          outcome.metadata = create.outcome.metadata(display.name = 'Diagnoses EL',
#                                                                     description = "Number of Individuals with a Diagnosis of Early Latent Syphilis in the Past Year",
#                                                                     scale = 'non.negative.number',
#                                                                     axis.name = 'Cases',
#                                                                     units = 'cases',
#                                                                     singular.unit = 'case'),
#                          keep.dimensions = c('location','age','race','sex'),
#                          corresponding.data.outcome = 'el.syphilis' #corresponding to the name in data manager
# )
# track.cumulative.outcome(SHIELD.SPECIFICATION,
#                          name = "diag.late",
#                          value = expression(diag.untreated + diag.treated),
#                          subset.dimension.values = list(stage='late.stages'),
#                          outcome.metadata = create.outcome.metadata(display.name = 'Diagnoses Late',
#                                                                     description = "Number of Individuals with a Diagnosis of Late Latent & Teritiary Syphilis in the Past Year",
#                                                                     scale = 'non.negative.number',
#                                                                     axis.name = 'Cases',
#                                                                     units = 'cases',
#                                                                     singular.unit = 'case'),
#                          keep.dimensions = c('location','age','race','sex'),
#                          corresponding.data.outcome = 'unknown.duration.or.late.syphilis' #corresponding to the name in data manager
# )

##-- REGISTER THE SPECIFICATION ----
register.model.specification(SHIELD.SPECIFICATION)
#

register.calibrated.parameters.for.version('shield',
                                           distribution = SHIELD.FULL.PARAMETERS.PRIOR,
                                           apply.function = SHIELD.APPLY.PARAMETERS.FN,
                                           sampling.blocks = SHIELD.FULL.PARAMETERS.SAMPLING.BLOCKS,
                                           calibrate.to.year = 2025,
                                           join.with.previous.version = F)
print("Calibration parameters registered for DEMOGRAPHIC model")


print("SHIELD specification sourced successfully!")
cat("*** Shield_specification.R completed! ***\n")

