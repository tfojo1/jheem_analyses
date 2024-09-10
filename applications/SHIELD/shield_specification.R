# installed.packages("jheem") @Todd: is this ready?
# NEXT STEPS:
### contact matrix from JHEEM
### Testing : symptomatic vs screening ....
### transitions between continuum stages
### birth , death, migration

# fix the initial population (we have census for 2007, what are the good values to see historical sysphilis rates )

# TBD:
# think about the statistics that we want to record: we will record their average between Jan 1 and Dec 31
# proportion of people aware of diagnosis ?
# proportion loving with diag and not track.integrated.outcome(prevalece of syphilis)?

#@Todd: how to remove all the warnings
# > specification.metadata=get.specification.metadata("shield","C.12580")
# > specification.metadata=get.specification.metadata("shield","US")

# Source libraries ----
# Working directory is set to the main JHEEM_Analysis folder:
setwd("~/OneDrive - Johns Hopkins/JHEEM/Simulation/code/jheem_analyses/")
# setwd('../../')
#
source('../jheem_analyses/source_code.R') # a file that contains all the necessary functions for the JHEEM
source('../jheem_analyses/applications/SHIELD/shield_parameters.R') # a file that contains the SHIELD specification
source('../jheem_analyses/applications/SHIELD/shield_ontology_mappings.R') # a file that contains the SHIELD specification
source('../jheem_analyses/applications/SHIELD/R/shield_specification_helpers.R') # a file that contains the SHIELD specification

# source('../../source_code.R') # a file that contains all the necessary functions for the JHEEM
# source('../shield_parameters.R') # a file that contains the SHIELD specification
# source('../shield_ontology_mappings.R') # a file that contains the SHIELD specification
# source('shield_specification_helpers.R') # a file that contains the SHIELD specification

##--------------------------------------------------------------------------------------------------------------#
# INITIAL SET-UP ----
SHIELD.SPECIFICATION = create.jheem.specification(version = 'shield',
                                                  iteration=1,
                                                  description = "The initial SHIELD version, set up to model national epidemic",
                                                  start.year = 1940,
                                                  age.endpoints=c(0,15,20,25,30,35,40,45,50,55,65,Inf), #11 agegroups, similar to atlas [0-15][16-20];
                                                  compartments.for.infected.only = list(
                                                    continuum = c('undiagnosed', 'diagnosed.untreated'),
                                                    stage = c('ps', 'el','ll','ter')
                                                  ),
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
                                                    ps.stages=c('ps'),
                                                    el.stages=c('el'),
                                                    late.stages=c('ll','ter')
                                                  )
)


# FIX STRATA SIZES ----

#'@title Set Whether to Fix Strata Sizes During a Time Period # a simplifying assumption to avoid modeling population demographic dynamics before year X
# 2007 earliest year for complete census data
register.fixed.model.strata(SHIELD.SPECIFICATION,
                            applies.after.time = -Inf,
                            applies.before.time = 2007,
                            fix.strata = T,
                            dimensions.to.fix = c('location','age','race','sex')
)



# INITIAL POPULATION ----
# Specify the initial compartment sizes in year 1940
## BASE POPULATION ----
# step1: defines a blank quantity
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'base.initial.population',
                        value = 0)
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
register.model.element(SHIELD.SPECIFICATION, #Todd: to be reviewed!
                       name = 'proportion.msm.of.male',
                       scale = 'proportion',
                       get.functional.form.function = get.proportion.msm.of.male.by.race.functional.form,
                       functional.form.from.time=2010,
                       functional.form.to.time=2010)
## INFECTED ----
# dummy values: 0.5% are infected, and they are 50% in PS and 50% in Ter stage and all undiagnosed
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'initial.population.infected',
                        value = 0 )
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'initial.population.infected',
                               value = expression(base.initial.population* 0.005 * .5),
                               applies.to = list(continuum='undiagnosed', stage='ps'))
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'initial.population.infected',
                               value = expression(base.initial.population* 0.005 *.5),
                               applies.to = list(continuum='undiagnosed', stage='ter'))
register.initial.population(SHIELD.SPECIFICATION,
                            group = 'infected',
                            value = 'initial.population.infected')
## UNINFECTED ----
register.initial.population(SHIELD.SPECIFICATION,
                             group = 'uninfected',
                            value = 'initial.population.uninfected')
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'initial.population.uninfected',
                        value = 0)
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'initial.population.uninfected',
                               value = expression(base.initial.population * .995),
                               applies.to = list(profile='susceptible'))


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
                      new.infections.applies.to = list(continuum='undiagnosed',stage='ps'))
# all.new.infections.into.compartments #@Todd what is this option ????

## Sexual Contact ----
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
## Sexual Contact: Transmission Rates ----
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
                                                                               min=0, #truncate to 0 #Todd: why do we need this?
                                                                               knot.link = 'log',link='identity'), #knots on the log-scale and values on the identity scale
                       functional.form.from.time = 1960, #0 or -Inf #@TODD: what is this?
                       scale='rate') #spline with 2010/2020

register.model.element(SHIELD.SPECIFICATION,
                       name = 'heterosexual.trates',
                       functional.form = create.linear.spline.functional.form(knot.times = c(time0=2000, time1=2010, time2=2020),
                                                                              knot.values = list(time0=0,time1=0,time2=0) ,
                                                                              knots.are.on.transformed.scale = T, #knots on the log scale (value is exp(0))
                                                                              min=0, #truncate to 0 #Todd: why do we need this?
                                                                              knot.link = 'log',link='identity') ,
                       functional.form.from.time = 1960, #0 or -Inf #@TODD
                       scale='rate')

## Sexual Contact: By AGE ----
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

# sexual contact by age for males who have sex with female partners
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'sexual.contact.by.age',
                               applies.to = list(sex.to=c('heterosexual_male','msm'),
                                                 sex.from='female'),
                               value = get.heterosexual.male.sexual.age.contact.proportions)

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

## Sexual Contact: By SEX ----
# Set up elements
register.model.element(SHIELD.SPECIFICATION,
                       name = 'oe.female.pairings.with.msm',
                       value = SHIELD_BASE_PARAMETER_VALUES['oe.female.pairings.with.msm'], #Todd: to review
                       scale = 'ratio')

register.model.element(SHIELD.SPECIFICATION,
                       name = 'fraction.heterosexual.male.pairings.with.male',
                       value = SHIELD_BASE_PARAMETER_VALUES['fraction.heterosexual.male.pairings.with.male'], #Todd: to review,
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
                        value = 1)

# where do nw infections go to?
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'new.infection.proportions',
                        value = 1)


##--------------------------------------------------------------------------------------------------------------#

## Sexual Contact: By RACE ----
#Todd: what are these? where is this coming from? how can I move this to base parameter ? what sthe reference?
baseline.sexual.oes = array(c(3.76,1,1,1,2.19,1,1,1,1.55),
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



# MORTALITY  ----
##--------------------------------------------------------------------------------------------------------------#
# different sources of mortality (general, syphilis related, etc.)

register.mortality(SHIELD.SPECIFICATION,
                   tag = 'general.mortality',
                   groups = c('infected','uninfected'),
                   mortality.rate.value = 'general.mortality.rate')

register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'general.mortality.rate',
                        value = 0.01)


##--------------------------------------------------------------------------------------------------------------#
# NATALITY   ----
##--------------------------------------------------------------------------------------------------------------#
# Fertility and birth rate
register.natality(specification = SHIELD.SPECIFICATION,
                  parent.groups = c('infected', 'uninfected'),
                  applies.to = list(sex='female'), #only women give birth
                  child.groups = 'uninfected',
                  fertility.rate.value = 'general.fertility.rate',
                  birth.proportions.value = 'general.birth.proportions', # when they are born, where do they go? (e.g., what age, what race, what sex will they have)
                  parent.child.concordant.dimensions = c('race'),# the race of the cild will be the same
                  all.births.into.compartments = list(profile='susceptible',age= 1),
                  tag = 'births')

# e.g., assuming 50% of births are female and 10% of male are msm
# required elements to decide where new births should go
register.model.element(SHIELD.SPECIFICATION,
                       name = 'proportion.births.female',
                       scale = 'proportion',
                       value = 0.5)

register.model.element(SHIELD.SPECIFICATION,
                       name = 'proportion.male.msm',
                       scale = 'proportion',
                       value = 0.1)

register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'general.birth.proportions',
                        value = 'proportion.births.female')# this will apply to all unless we define the subsets below:

register.model.quantity.subset(SHIELD.SPECIFICATION,
                               applies.to = list(sex.to='heterosexual_male'), # there is a sex.from option as well that is used to differntiate proportions based on who the parent is
                               name = 'general.birth.proportions',
                               value = expression( (1- proportion.births.female)*(1-proportion.male.msm) ))

register.model.quantity.subset(SHIELD.SPECIFICATION,
                               applies.to = list(sex.to='msm'),
                               name = 'general.birth.proportions',
                               value = expression( (1- proportion.births.female)*(proportion.male.msm) ))

register.model.element(SHIELD.SPECIFICATION,
                       name = 'general.fertility.rate',
                       scale = 'rate',
                       value = 0.024 #this will come from census data by race for women in each county
)


##--------------------------------------------------------------------------------------------------------------#
# CONTINUUM TRANSISION ----
##--------------------------------------------------------------------------------------------------------------#
# e.g., fix screening rate of 10% for all infected groups, assuming 90% get treated immediately and 10% remain untreated
#assuming those who are diag.untrt will seek trt after a year
register.model.element(SHIELD.SPECIFICATION,
                       name = 'screening.rate',
                       scale = 'rate',
                       value = 0.1)
register.model.element(SHIELD.SPECIFICATION,
                       name = 'proportion.immediately.treated',
                       scale = 'proportion',
                       value = 0.9)
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'screening.immediate.treatment.rate',
                        value = expression(screening.rate * proportion.immediately.treated))
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'screening.delayed.treatment.rate',
                        value = expression(screening.rate * (1-proportion.immediately.treated)))

#screening followed by immediate trt
register.remission(SHIELD.SPECIFICATION,
                   applies.to = list(continuum = 'undiagnosed'),
                   all.remissions.into.compartments = list(profile = 'diagnosed.treated'),
                   remission.rate.value = 'screening.immediate.treatment.rate',
                   remission.proportions.value = 'remission.prp',
                   tag = 'screening.immediate.trt'
)
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'remission.prp',
                        value = 1)

#screening followed by delayed trt
register.transition(SHIELD.SPECIFICATION,
                    dimension = 'continuum',
                    groups = 'infected',
                    from.compartments =  'undiagnosed',
                    to.compartments = 'diagnosed.untreated',
                    value = 'screening.delayed.treatment.rate')

# delayed treatment option
register.model.element(SHIELD.SPECIFICATION,
                       name = 'delayed.treatment.rate',
                       scale = 'rate',
                       value = 1)

register.remission(SHIELD.SPECIFICATION,
                   applies.to = list(continuum = 'diagnosed.untreated'),
                   all.remissions.into.compartments = list(profile = 'diagnosed.treated'),
                   remission.rate.value = 'delayed.treatment.rate',
                   remission.proportions.value='remission.prp',
                   tag = 'delayed.trt' )

##--------------------------------------------------------------------------------------------------------------#
## Stage Transitions ----
##--------------------------------------------------------------------------------------------------------------#
# e.g., assuming a fix duration for each state: ps= 3months, earlyLatent=9months, LateLatent=10years, Teritiary=infinit
register.model.element(SHIELD.SPECIFICATION,
                       name = 'duration.ps',
                       scale = 'time',
                       value = 0.25) #3/12

register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = 'ps',
                    to.compartments = 'el',
                    value = expression(1/duration.ps))
##
register.model.element(SHIELD.SPECIFICATION,
                       name = 'duration.el',
                       scale = 'time',
                       value = 0.75) #9/12

register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = 'el',
                    to.compartments = 'll',
                    value = expression(1/duration.el))
##
register.model.element(SHIELD.SPECIFICATION,
                       name = 'duration.ll',
                       scale = 'time',
                       value = 10)

register.transition(SHIELD.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = 'll',
                    to.compartments = 'ter',
                    value = expression(1/duration.ll))

##--------------------------------------------------------------------------------------------------------------#
# OUTPUTS ----
##--------------------------------------------------------------------------------------------------------------#
# The model reports 2 categories of outcomes:
# 1-cumulative outcomes, reported between jan1 to dec31)
# 2-point estimates of compartment sizes reported in Jan 1st

# The model projects the size of infected.uninfected population by default in Jan 1st of each year

# For calibration purposes, we fit these estiamtes agains CDC, but it's unclear if CDC data represents Jan 1st or another time during the year (e.g., HIV prevalence, prop suppressed)
# Because of this, we generally calculate the average value between Jan 1st and Dec 31st of each year and report that as the annual output

# !!!for dynamic transitions that change over time (e.g., testing), the anchor points are coded at the begginign of the year (e.g., if transmission changes from 2000 to 2020, these dates represent jan 1st of those years)

#Dynamic outputs: (in addition to compartment size)
# track.dynamic.outcome: a more general definition, calculated at each step of solver (mortality birth etc)
# track.transition: people move from one compartment to another #tracking transitions along one dimension (e.g., continuum)


## All Diagnosis (at all stages ): has 2 components
### 1st component:new diagnosis that dont receive treatment immediately (transition)
track.transition(SHIELD.SPECIFICATION,
                 name = 'diag.untreated',
                 #display name on the graph
                 outcome.metadata = create.outcome.metadata(display.name = 'Diagnoses: Delayed Treatment',
                                                            description = "Number of Individuals with a New Diagnosis of Syphilis that dont Start Treatment Immediately in the Past Year",
                                                            scale = 'non.negative.number',
                                                            axis.name = 'Cases',
                                                            units = 'cases',
                                                            singular.unit = 'case'),
                 dimension = 'continuum',#transition along the continuum
                 from.compartments = 'undiagnosed',
                 to.compartments = 'diagnosed.untreated',
                 keep.dimensions = c('location','age','race','sex','stage')
)
#2 components: new diagnosis that receive treatment immediately (remission)
track.dynamic.outcome(SHIELD.SPECIFICATION,
                      name = 'diag.treated',
                      outcome.metadata = create.outcome.metadata(display.name = 'Diagnoses: Immediate Treatment',
                                                                 description = "Number of Individuals with a New Diagnosis of Syphilis that Start Treatment Immediately in the Past Year",
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Cases',
                                                                 units = 'cases',
                                                                 singular.unit = 'case'),
                      dynamic.quantity.name = "remission.from", #where they come from
                      subset.dimension.values = list(continuum='undiagnosed'), #only counting those who receive immediata trt
                      keep.dimensions = c('location','age','race','sex','stage')
)

#Total diagnosis
#We usually try to outline the outcomes here with the final outcomes we need for calibrations: so we break them by stage
## primarySecondary
track.cumulative.outcome(SHIELD.SPECIFICATION,
                         name = "diag.ps",
                         value = expression(diag.untreated + diag.treated),
                         subset.dimension.values = list(stage='ps.stages'),
                         outcome.metadata = create.outcome.metadata(display.name = 'Diagnoses PS',
                                                                    description = "Number of Individuals with a Diagnosis of Primary-Secondary Syphilis in the Past Year",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Cases',
                                                                    units = 'cases',
                                                                    singular.unit = 'case'),
                         keep.dimensions = c('location','age','race','sex'),
                         corresponding.data.outcome = 'ps.syphilis' #corresponding to the name in data manager
)
track.cumulative.outcome(SHIELD.SPECIFICATION,
                         name = "diag.el",
                         value = expression(diag.untreated + diag.treated),
                         subset.dimension.values = list(stage='el.stages'),
                         outcome.metadata = create.outcome.metadata(display.name = 'Diagnoses EL',
                                                                    description = "Number of Individuals with a Diagnosis of Early Latent Syphilis in the Past Year",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Cases',
                                                                    units = 'cases',
                                                                    singular.unit = 'case'),
                         keep.dimensions = c('location','age','race','sex'),
                         corresponding.data.outcome = 'el.syphilis' #corresponding to the name in data manager
)
track.cumulative.outcome(SHIELD.SPECIFICATION,
                         name = "diag.late",
                         value = expression(diag.untreated + diag.treated),
                         subset.dimension.values = list(stage='late.stages'),
                         outcome.metadata = create.outcome.metadata(display.name = 'Diagnoses Late',
                                                                    description = "Number of Individuals with a Diagnosis of Late Latent & Teritiary Syphilis in the Past Year",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Cases',
                                                                    units = 'cases',
                                                                    singular.unit = 'case'),
                         keep.dimensions = c('location','age','race','sex'),
                         corresponding.data.outcome = 'unknown.duration.or.late.syphilis' #corresponding to the name in data manager
)
##########################
# Incidence (new infections + reinfections)
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
##########################
# New Treatment Initiations: Immediate and Delayed
track.dynamic.outcome(SHIELD.SPECIFICATION,
                      name = 'trt.initiation',
                      outcome.metadata = create.outcome.metadata(display.name = 'Treatment Initiation',
                                                                 description = "Number of Individuals Starting Treatment in the Past Year",
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Cases',
                                                                 units = 'cases',
                                                                 singular.unit = 'case'),
                      dynamic.quantity.name = "remission.from", #where they come from
                      keep.dimensions = c('location','age','race','sex','stage')
)



##--------------------------------------------------------------------------------------------------------------#
##--------------------------------------------------------------------------------------------------------------#
##-- REGISTER THE SPECIFICATION ----
##--------------------------------------------------------------------------------------------------------------#
##--------------------------------------------------------------------------------------------------------------#


register.model.specification(SHIELD.SPECIFICATION)

#@Melissa:
#set this up for shield, and place the global transmission rate to play around
# source('../jheem_analyses/applications/EHE/ehe_parameters_helpers.R')
# source('../jheem_analyses/applications/EHE/ehe_parameters.R')
# source('../jheem_analyses/applications/EHE/ehe_parameter_mapping.R')
#
register.calibrated.parameters.for.version('shield',
                                           distribution = SHIELD.PARAMETERS.PRIOR,
                                           apply.function = SHIELD.APPLY.PARAMETERS.FN,
                                           sampling.blocks = SHIELD.PARAMETER.SAMPLING.BLOCKS,
                                           calibrate.to.year = 2025,
                                           join.with.previous.version = F)


print("SHIELD specification sourced successfully!")


## Notes: ----


# interventions will be defined by parameters too (outside of specification)
# Element: a scalar value or a functional form
## Scalar: constant value or fix over over time
## Functional form: either vary by strata or changes over time (an equation that will be evaluated to create value of that parameter )
# QUANTITIES can be equal to another quantity or an element, or an expression (accepts elements) or a function of other quantities
# a quantity has to have a scale if we want to intervene in them


# We generally use additional parameters to model deviations from our prior knowledge and calibrate them to data
# Parameters affect elements, quantities are calculated from elements

