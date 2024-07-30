#Quantity vs element?

#'@title Register a Model Element
# name The name of the model element. Cannot overlap with the names of model quantities
# scale: Can be 'rate', 'ratio', 'proportion', 'time', 'number', 'non.negative.number'
# value: A numeric value that serves as the value for this element if it is not otherwise specified. If NULL, then a value must be specified either by a get.value.function (a function which returns a value), or by a functional.form



# Source supporting files
# source('../jheem_analyses/source_code.R') #@Todd: relevant?


##--------------------##
##--------------------##
##-- INITIAL SET-UP --##
##--------------------##
##--------------------##   
#'@title Create a Model Specification for Running the JHEEM
# should we inherit any existing model? 
# should we start in 1970 or earlier?

SHIELD.SPECIFICATION = create.jheem.specification(version = 'shield', #A single character value denoting the version
                                                  #sub-versions=1, #A character vector indicating the names of 'sub-versions' of the model. A sub-version has the same structure but records a different set of outcomes
                                                  iteration = 1,#A single character or numeric value denoting the iteration of the model specification for this version
                                               description = "The initial SHIELD version, set up to model national epidemic", #A short text description of this version
                                               
                                               #parent.version = ??? The character version of the specification from which this new specification should inherit. Must have been registered
                                               
                                               start.year = 1970,#The numeric year at which simulations should start. If NULL inherits the parent specification's start year #
                                               
                                               age.endpoints=c(13,25,35,45,55,Inf),
                                               
                                               compartments.for.infected.only = list( #The names of the lists represent dimensions, and the values the compartments for each dimension.
                                                 continuum = c('undiagnosed', 'diagnosed'),
                                                 stage = c('primarySecondary', 'earlyLatent','lateLatent','tertiary')
                                               ),
                                               compartments.for.uninfected.only = list(),
                                               compartments.for.infected.and.uninfected = list(
                                                 location = 'location',#??? How to create a national model?
                                                 age = 'all.ages',
                                                 race=c('black','hispanic','other'),
                                                 sex= c('heterosexual_male', 'msm', 'heterosexual_male')
                                               ),
                                               
                                               compartment.value.aliases = list( #???  named list representing substitutions to be made into compartment names (both in compartments.for.infected.only, compartments.for.uninfected.only, compartments.for.infected.and.uninfected and in subsequent references in registering model quantities)
                                                 location = function(location){location}, #???
                                                 
                                                 diagnosed.states='diagnosed',
                                                 undiagnosed.states=c('undiagnosed'),
                                                 
                                                 primarySecondary.stages = 'primarySecondary',
                                                 earlyLatent.stages = 'earlyLatent',
                                                 lateLatent.stages = 'lateLatent',
                                                 tertiary.stages = 'tertiary',
                                               )
)

##----------------------##
##-- Fix Strata Sizes --##
##----------------------##
#'@title Set Whether to Fix Strata Sizes During a Time Period
# Set Whether to Fix Strata Sizes During a Time Period
# Todd: what's this? 
register.fixed.model.strata(SHIELD.SPECIFICATION, #The jheem.specification object
                            # applies.after.time = 1970,
                            applies.before.time = 2007, #single numeric values giving the time frame over whether this setting applies
                            fix.strata = T,
                            dimensions.to.fix = c('location','age','race','sex') #A character vector denoting which dimensions should be fixed. These should be dimensions common to both infected and uninfected groups. These are only used if fix.strata==TRUE
                            )




##------------------------##
##-- Initial Population --##
##------------------------##
#'@title Set The Initial Model Population
#'
#'@inheritParams register.model.quantity
#'@param specification The jheem.specification object
#'@param group Which group ("infected" or "uninfected") this initial population is for


###### Infected
register.initial.population(SHIELD.SPECIFICATION,
                            group = 'infected',
                            value = 'initial.population.infected' #this quantity is defined below 
                            )
#starts the initial.population.infected at 0? 
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'initial.population.infected',
                        value = 0)
#what is this? 
register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'initial.population.infected',
                               applies.to = list(continuum='undiagnosed',
                                                 stage = 1),
                               value = expression(0*base.initial.population + seed.population)) #the 0* gives us the right dimensions


###### Uninfected
register.initial.population(SHIELD.SPECIFICATION,
                            group = 'uninfected',
                            value = 'initial.population.uninfected')

# define the quantity initial.population.uninfected
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'initial.population.uninfected',
                        value = expression(base.initial.population - seed.population)) #what is an expression?

#define the quantity base.initial.population.sans.risk
register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'base.initial.population.sans.risk', #sans??? 
                        value = 0)

register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'base.initial.population.sans.risk',
                               applies.to = list(sex='female'),
                               value = 'base.initial.female.population')

register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'base.initial.population.sans.risk',
                               applies.to = list(sex='msm'),
                               value = expression(base.initial.male.population * proportion.msm.of.male))

register.model.quantity.subset(SHIELD.SPECIFICATION,
                               name = 'base.initial.population.sans.risk',
                               applies.to = list(sex='heterosexual_male'),
                               value = expression(base.initial.male.population * (1-proportion.msm.of.male)))

register.model.quantity(SHIELD.SPECIFICATION,
                        name = 'base.initial.population',
                        value = 0)

register.model.element(SHIELD.SPECIFICATION,
                       name = 'base.initial.female.population',
                       get.value.function = get.base.initial.female.population,
                       scale = 'non.negative.number')

register.model.element(SHIELD.SPECIFICATION,
                       name = 'base.initial.male.population',
                       get.value.function = get.base.initial.male.population,
                       scale = 'non.negative.number')

register.model.element(SHIELD.SPECIFICATION,
                       name = 'seed.population',
                       get.value.function = get.seed.population,
                       scale = 'non.negative.number')



#-------------#
#-- Testing --#
#-------------#
TESTING.FIRST.YEAR.FRACTION.OF.RAMP = 0.5^(1993-1982)
register.model.element(EHE.SPECIFICATION,
                       name = 'general.population.testing.without.covid',
                       scale = 'rate',
                       
                       get.functional.form.function = get.testing.functional.form,
                       functional.form.scale = 'proportion',
                       functional.form.from.time = 2010,
                       
                       ramp.scale = 'proportion',
                       ramp.times = c(1981,1982,1993),
                       ramp.values = c(0,0.5*TESTING.FIRST.YEAR.FRACTION.OF.RAMP,0.5),
                       ramp.interpolate.links = c('identity','log','identity'))

register.model.quantity(EHE.SPECIFICATION,
                        name = 'general.population.testing',
                        scale = 'rate',
                        value = expression(
                          general.population.testing.without.covid *
                            (1-(1-max.covid.effect.testing.reduction) * covid.on *
                               (1-testing.mobility.correlation+(testing.mobility.correlation*covid.mobility.change))))
)

register.model.quantity(EHE.SPECIFICATION,
                        name = 'undiagnosed.testing.increase',
                        value = expression(
                          undiagnosed.testing.increase.without.covid *
                            (1-(1-max.covid.effect.undiagnosed.testing.rr.increase) * covid.on *
                               (1-undiagnosed.testing.rr.mobility.correlation+
                                  (undiagnosed.testing.rr.mobility.correlation*covid.mobility.change)))), 
                        scale = 'non.negative.number')

register.model.element(EHE.SPECIFICATION,
                       name = 'undiagnosed.testing.increase.without.covid',
                       get.functional.form.function = get.undiagnosed.testing.increase.functional.form, 
                       scale = 'non.negative.number')

register.model.quantity(EHE.SPECIFICATION,
                        name = 'testing.of.undiagnosed',
                        value = expression(general.population.testing*(1+undiagnosed.testing.increase)))

register.model.element(EHE.SPECIFICATION,
                       name = 'max.covid.effect.testing.reduction',
                       scale = 'ratio',
                       get.functional.form.function = get.covid.max.testing.effect)

register.model.element(EHE.SPECIFICATION,
                       name = 'max.covid.effect.undiagnosed.testing.rr.increase',
                       scale = 'ratio',
                       get.functional.form.function = get.undiagnosed.testing.covid.rr.functional.form) 

# get.covid.max.testing.effect is a relative risk (e.g., 0.8 if testing is 80% of what it would be without covid)
# testing.no.covid is time-varying, max.covid.effect.testing.reduction is (1- relative risk) (static)
# if testing mobility correlation is 0, always the max reduction; if testing mobility correlation is 1, moves the same as mobility
# if covid.on = 0 , go back to testing.no.covid 



##--------------------------------##
##--------------------------------##
##--        TRANSITIONS         --##
##--------------------------------##
##--------------------------------##

##---------------------------##
##-- Continuum Transitions --##
##---------------------------##
register.transition(EHE.SPECIFICATION,
                    dimension = 'continuum',
                    from.compartments = 'undiagnosed',
                    to.compartments = 'diagnosed',
                    groups = 'infected',
                    value = 'testing.of.undiagnosed')


##-----------------------##
##-- Stage Transitions --##
##-----------------------##

register.transition(EHE.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = 'primarySecondary',
                    to.compartments = 'earlyLaten',
                    value = expression(1/acute.hiv.duration))

register.model.element(EHE.SPECIFICATION,
                       name = 'acute.hiv.duration',
                       scale = 'time',
                       value = EHE_BASE_PARAMETER_VALUES['acute.infection.duration'])

##----------------------------##
##----------------------------##
##--        NATALITY        --##
##----------------------------##
##----------------------------##

register.natality(specification = EHE.SPECIFICATION,
                  parent.groups = 'uninfected',
                  child.groups = 'uninfected',
                  fertility.rate.value = 'fertility',
                  birth.proportions.value = 'birth.proportions',
                  parent.child.concordant.dimensions = 'race',
                  all.births.into.compartments = list(age=1, risk=1))

register.natality(specification = EHE.SPECIFICATION,
                  parent.groups = 'infected',
                  child.groups = 'uninfected',
                  fertility.rate.value = 'fertility',
                  birth.proportions.value = 'birth.proportions',
                  parent.child.concordant.dimensions = 'race',
                  all.births.into.compartments = list(age=1, risk=1))


##---------------##
##-- Fertility --##                              
##---------------##

register.model.element(EHE.SPECIFICATION,
                       name = 'fertility',
                       get.functional.form.function = get.location.birth.rates.functional.form,
                       functional.form.from.time = 2007,
                       scale = 'rate')


##-----------------------##
##-- Birth Proportions --##
##-----------------------##

register.model.element(EHE.SPECIFICATION,
                       name = 'fraction.male.births',
                       scale = 'proportion',
                       value = 1.05 / (1+1.05)) #1.05 is male-to-female birth ratio

register.model.element(EHE.SPECIFICATION,
                       name = 'proportion.msm.of.male',
                       scale = 'proportion',
                       get.functional.form.function = get.proportion.msm.of.male.by.race.functional.form,
                       functional.form.from.time=2010,
                       functional.form.to.time=2010) 

register.model.quantity(EHE.SPECIFICATION,
                        name = 'birth.proportions',
                        value = 0)

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'birth.proportions',
                               applies.to = list(sex.to='msm'),
                               value = expression(fraction.male.births * proportion.msm.of.male))

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'birth.proportions',
                               applies.to = list(sex.to='heterosexual_male'),
                               value = expression(fraction.male.births * (1-proportion.msm.of.male)) )

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'birth.proportions',
                               applies.to = list(sex.to='female'),
                               value = expression(1-fraction.male.births) )



##--------------------------------##
##--------------------------------##
##--        MORTALITY           --##
##--------------------------------##
##--------------------------------##

##-------------------##
##-- HIV Mortality --##
##-------------------##

register.mortality(EHE.SPECIFICATION, 
                   tag = 'infection.specific.mortality',
                   groups = 'infected',
                   mortality.rate.value = expression((1-suppression) * unsuppressed.hiv.mortality.rate))

register.model.element(EHE.SPECIFICATION,
                       name = 'unsuppressed.hiv.mortality.rate',
                       scale = 'rate',
                       functional.form = create.logistic.spline.functional.form(knot.values = list(rate0=1,
                                                                                                   rate2=1), #placeholder
                                                                                knot.times = c(rate0=2000,
                                                                                               rate2=2010),
                                                                                knot.link = 'identity',
                                                                                fraction.of.asymptote.after.end=0.025,
                                                                                fraction.of.asymptote.before.start=0.025,
                                                                                fraction.of.asymptote.for.change.dir=0.02,
                                                                                overwrite.knot.values.with.alphas = T),
                       functional.form.from.time = 2000,
                       functional.form.to.time = 2010,
                       ramp.times = c(pre.peak=1970, peak.start=1980, peak.end=1996),
                       ramp.values = c(pre.peak=1, peak.start=1, peak.end=1), #placeholder
                       ramp.value.application = 'absolute')




##-----------------------##
##-- General Mortality --##
##-----------------------##

register.mortality(EHE.SPECIFICATION,
                   tag = 'non.idu.general.mortality',
                   groups = c('uninfected','infected'),
                   mortality.rate.value = 'non.idu.general.mortality')

register.mortality(EHE.SPECIFICATION,
                   tag = 'idu.mortality',
                   groups = c('uninfected','infected'),
                   mortality.rate.value = 'idu.mortality.rate',
                   applies.to = list(risk='active.idu.states'))

register.model.element(EHE.SPECIFICATION,
                       name = 'non.idu.general.mortality',
                       get.functional.form.function = get.location.mortality.rates.functional.form,
                       scale = 'rate')

register.model.element(EHE.SPECIFICATION,
                       name = 'idu.mortality.rate',
                       value = EHE_BASE_PARAMETER_VALUES['idu.mortality'],
                       scale = 'rate')

##--------------------##
##--------------------##
##--        AGING   --##
##--------------------##
##--------------------##

##--------------------##
##-- Uninfected Aging --##
##--------------------##

#register.transition(EHE.SPECIFICATION,
#                    dimension = 'age',
#                    from.compartments = 1,
#                    to.compartments = 2,
#                    value=1/4,
#                    groups='all')

register.model.element(EHE.SPECIFICATION,
                       name = 'uninfected.aging',
                       scale = 'rate',
                       get.functional.form.function = get.empiric.aging.rates,
                       functional.form.from.time = 2007)

register.aging(EHE.SPECIFICATION,
               groups = 'uninfected',
               aging.rate.value = 'uninfected.aging')

##--------------------##
##-- Infected Aging --##
##--------------------##

register.aging(EHE.SPECIFICATION,
               groups = 'infected',
               aging.rate.value = 'hiv.positive.aging.rates')

register.model.element(EHE.SPECIFICATION,
                       name = 'hiv.positive.aging.rates',
                       get.functional.form.function = get.empiric.hiv.aging.rates,
                       functional.form.from.time = 1980,
                       scale='rate')


##----------------------------##
##----------------------------##
##--        MIGRATION       --##
##----------------------------##
##----------------------------##

##-----------------##
##-- Immigration --##
##-----------------##

register.model.element(EHE.SPECIFICATION,
                       name = 'immigration',
                       get.functional.form.function = get.immigration.rates.functional.form,
                       functional.form.from.time = 2007,
                       scale = 'rate')

register.model.quantity(EHE.SPECIFICATION,
                        name = 'null.proportions',
                        value = 1)

register.natality(specification = EHE.SPECIFICATION,
                  parent.groups = 'uninfected',
                  child.groups = 'uninfected',
                  fertility.rate.value = 'immigration',
                  birth.proportions.value = 'null.proportions', # because we're actually fixing all the strata below 
                  parent.child.concordant.dimensions = c('age','race','sex','risk'),
                  all.births.into.compartments = list(),
                  tag = "immigration")

register.natality(specification = EHE.SPECIFICATION,
                  parent.groups = 'infected',
                  child.groups = 'infected',
                  fertility.rate.value = 'immigration',
                  birth.proportions.value = 'null.proportions', # because we're actually fixing all the strata below 
                  parent.child.concordant.dimensions = c('age','race','sex','risk','continuum','stage'),
                  all.births.into.compartments = list(),
                  tag = "immigration")

##----------------##
##-- Emigration --##
##----------------##

register.model.element(EHE.SPECIFICATION,
                       name = 'emigration',
                       get.functional.form.function = get.emigration.rates.functional.form,
                       functional.form.from.time = 2007,
                       scale = 'rate')

register.mortality(EHE.SPECIFICATION,
                   mortality.rate.value = "emigration",
                   groups = c("uninfected","infected"), 
                   tag = "emigration")



##--------------------------------##
##--------------------------------##
##--        TRANSMISSION        --##
##--------------------------------##
##--------------------------------##

register.transmission(EHE.SPECIFICATION,
                      contact.value = 'sexual.contact',
                      susceptibility.value = 'sexual.susceptibility',
                      transmissibility.value = 'sexual.transmissibility',
                      new.infection.proportions.value = 'new.infection.proportions',
                      tag = 'sexual.transmission',
                      all.new.infections.into.compartments = list(stage=1),
                      new.infections.applies.to = list(continuum='undiagnosed.states'))

register.transmission(EHE.SPECIFICATION,
                      contact.value = 'idu.contact',
                      susceptibility.value = 'idu.susceptibility',
                      transmissibility.value = 'idu.transmissibility',
                      new.infection.proportions.value = 'new.infection.proportions',
                      tag = 'idu.transmission',
                      all.new.infections.into.compartments = list(stage=1),
                      from.applies.to = list(risk='active.idu.states'),
                      to.applies.to = list(risk='active.idu.states'),
                      new.infections.applies.to = list(continuum='undiagnosed.states'))


##--------------------##
##-- Susceptibility --##
##--------------------##

register.model.quantity(EHE.SPECIFICATION,
                        name = 'proportion.of.sexual.transmissions.in.prep.eligible',
                        scale = 'proportion',
                        value = expression(1/(1+exp(-( # expit because this will be a logit normal distribution
                          # mean + (z score*standard dev)
                          logit.mean.proportion.prep.eligible + 
                            prep.fraction.sexual.transmission.avoidable.z*logit.sd.proportion.prep.eligible)  
                        )))) 

register.model.element(EHE.SPECIFICATION,
                       name = 'logit.mean.proportion.prep.eligible',
                       value = get.fraction.sexual.transmission.avoidable.logit.parameter(get.mean = T),
                       scale='number')

register.model.element(EHE.SPECIFICATION,
                       name = 'logit.sd.proportion.prep.eligible',
                       value = get.fraction.sexual.transmission.avoidable.logit.parameter(get.mean = F),
                       scale='non.negative.number') 

register.model.element(EHE.SPECIFICATION,
                       name = 'prep.fraction.sexual.transmission.avoidable.z',
                       value = 0,
                       scale='number') 

register.model.quantity(EHE.SPECIFICATION,
                        name = 'sexual.susceptibility',
                        value = expression( sexual.susceptibility.without.covid * sexual.susceptibility.covid.multiplier)
)

register.model.quantity(EHE.SPECIFICATION,
                        name = 'sexual.susceptibility.covid.multiplier',
                        value = expression((1-(1-max.covid.effect.sexual.transmission.reduction) * covid.on *
                                              (1-sexual.transmission.mobility.correlation+
                                                 (sexual.transmission.mobility.correlation*covid.mobility.change))))
)

register.model.element(EHE.SPECIFICATION,
                       name = 'max.covid.effect.sexual.transmission.reduction',
                       scale = 'ratio',
                       get.functional.form.function = get.covid.max.sexual.transmission.effect)


register.model.quantity(EHE.SPECIFICATION,
                        name = 'sexual.susceptibility.without.covid',
                        value = expression( base.sexual.susceptibility *
                                              (proportion.of.sexual.transmissions.in.prep.eligible*
                                                 (all.prep.risk + 1-all.prep.coverage) + 
                                                 (1-proportion.of.sexual.transmissions.in.prep.eligible)))
)

register.model.quantity(EHE.SPECIFICATION,
                        name = 'idu.susceptibility',
                        value = expression( idu.susceptibility.without.covid * idu.susceptibility.covid.multiplier)
)

register.model.quantity(EHE.SPECIFICATION,
                        name = 'idu.susceptibility.covid.multiplier',
                        value = expression((1-(1-max.covid.effect.idu.transmission.reduction) * covid.on *
                                              (1-idu.transmission.mobility.correlation+
                                                 (idu.transmission.mobility.correlation*covid.mobility.change))))
)

register.model.element(EHE.SPECIFICATION,
                       name = 'max.covid.effect.idu.transmission.reduction',
                       scale = 'ratio',
                       value=1)

register.model.quantity(EHE.SPECIFICATION,
                        name='idu.susceptibility.without.covid',
                        value = expression( base.idu.susceptibility *
                                              (all.prep.risk + 1-all.prep.coverage) *
                                              (1 + needle.exchange*(needle.exchange.rr-1)) )
)


register.model.element(EHE.SPECIFICATION,
                       name = 'base.sexual.susceptibility',
                       scale = 'rate',
                       functional.form = create.static.functional.form(value=1, link='log'))

register.model.element(EHE.SPECIFICATION,
                       name = 'base.idu.susceptibility',
                       scale = 'rate',
                       functional.form = create.static.functional.form(value=1, link='log'))


##-------------------------------##
##-- New Infection Proportions --##
##-------------------------------##

register.model.quantity(EHE.SPECIFICATION,
                        name = 'new.infection.proportions',
                        value = 0)


register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'new.infection.proportions',
                               value = expression( (1-all.prep.coverage) / (all.prep.risk + 1-all.prep.coverage)),
                               applies.to = list(continuum='undiagnosed')
)

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'new.infection.proportions',
                               value = expression(all.prep.risk / (all.prep.risk + 1-all.prep.coverage)),
                               applies.to = list(continuum='undiagnosed_from_prep')
)


##----------------------##
##-- Transmissibility --##
##----------------------##


#-- Acute transmissibility --#
register.model.quantity(EHE.SPECIFICATION,
                        name = 'acute.vs.chronic.transmissibility',
                        value = 1)

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'acute.vs.chronic.transmissibility',
                               value = 'acute.transmissibility.rr',
                               applies.to = list(stage='acute'))

register.model.element(EHE.SPECIFICATION,
                       name = 'acute.transmissibility.rr',
                       value = EHE_BASE_PARAMETER_VALUES['acute.transmissibility.rr'],
                       scale = 'ratio')

#-- IDU Transmissibility --#
register.model.quantity(EHE.SPECIFICATION,
                        name = 'idu.transmissibility',
                        value = expression((1-suppression) * diagnosed.vs.undiagnosed.idu.transmissibility * acute.vs.chronic.transmissibility))

register.model.quantity(EHE.SPECIFICATION,
                        name = 'diagnosed.vs.undiagnosed.idu.transmissibility',
                        value = 1)

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'diagnosed.vs.undiagnosed.idu.transmissibility',
                               value = 'diagnosed.needle.sharing.rr',
                               applies.to = list(continuum='diagnosed.states'))

register.model.element(EHE.SPECIFICATION,
                       name = 'diagnosed.needle.sharing.rr',
                       scale = 'ratio',
                       value = EHE_BASE_PARAMETER_VALUES['diagnosed.needle.sharing.rr'])


#-- Sexual Transmissibility --#

register.model.quantity(EHE.SPECIFICATION,
                        name = 'sexual.transmissibility',
                        value = expression((1-suppression) * diagnosed.vs.undiagnosed.sexual.transmissibility * acute.vs.chronic.transmissibility))

register.model.quantity(EHE.SPECIFICATION,
                        name = 'diagnosed.vs.undiagnosed.sexual.transmissibility',
                        value = 1)

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'diagnosed.vs.undiagnosed.sexual.transmissibility',
                               value = 'diagnosed.sexual.transmission.rr',
                               applies.to = list(continuum='diagnosed.states'))

register.model.element(EHE.SPECIFICATION,
                       name = 'diagnosed.sexual.transmission.rr',
                       scale = 'ratio',
                       value = as.numeric(EHE_BASE_PARAMETER_VALUES['diagnosed.msm.condomless.rr']))
#register.model.element(EHE.SPECIFICATION,
#                                                name = 'diagnosed.sexual.transmission.rr',
#                                                scale = 'ratio',
#                                                model = create.static.model(value = array(c(heterosexual_male = as.numeric(EHE_BASE_PARAMETER_VALUES['diagnosed.het.male.condomless.rr']),
#                                                                                            msm = as.numeric(EHE_BASE_PARAMETER_VALUES['diagnosed.msm.condomless.rr']),
#                                                                                            female = as.numeric(EHE_BASE_PARAMETER_VALUES['diagnosed.female.condomless.rr'])),
#                                                                                          dim=c(sex=3), dimnames=list(sex=c('heterosexual_male','msm','female')) ),
#                                                                            scale = 'log'))


##----------------------------------------##
##-- Sexual Transmission Contact Arrays --##
##----------------------------------------##

register.model.quantity(EHE.SPECIFICATION,
                        name = 'sexual.contact',
                        value = expression(global.trate *
                                             sexual.transmission.rates * 
                                             sexual.contact.by.age *
                                             sexual.contact.by.race *
                                             sexual.contact.by.sex *
                                             sexual.contact.by.risk))


register.model.element(EHE.SPECIFICATION,
                       name = 'global.trate',
                       value = 1,
                       scale = 'rate')

#---------------------------#
#-- Sexual Contact by Sex --#
#---------------------------#

# Set up elements
register.model.element(EHE.SPECIFICATION,
                       name = 'oe.female.pairings.with.msm',
                       value = 0.0895, #Pathela 2006
                       scale = 'ratio')

register.model.element(EHE.SPECIFICATION,
                       name = 'fraction.heterosexual.male.pairings.with.male',
                       value = 0.004, 
                       scale = 'ratio')

register.model.element(EHE.SPECIFICATION,
                       name = 'fraction.msm.pairings.with.female',
                       value = mean(PAIRING.INPUT.MANAGER$msm.sex.with.female.estimates), 
                       scale = 'ratio')

register.model.quantity(EHE.SPECIFICATION,
                        name = 'fraction.male.male.that.are.with.msm',
                        value = expression(proportion.msm.of.male / 
                                             (proportion.msm.of.male + 
                                                (1-proportion.msm.of.male) * fraction.heterosexual.male.pairings.with.male))
)

register.model.quantity(EHE.SPECIFICATION,
                        name = 'sexual.contact.by.sex',
                        value = 0)

# To female
register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.sex',
                               applies.to=list(sex.from='heterosexual_male',
                                               sex.to='female'),
                               value = expression((1-proportion.msm.of.male)/
                                                    (1-proportion.msm.of.male +
                                                       proportion.msm.of.male * oe.female.pairings.with.msm))
)

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.sex',
                               applies.to=list(sex.from='msm',
                                               sex.to='female'),
                               value = expression(proportion.msm.of.male * oe.female.pairings.with.msm/
                                                    (1-proportion.msm.of.male +
                                                       proportion.msm.of.male * oe.female.pairings.with.msm))
)

# To MSM
register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.sex',
                               applies.to=list(sex.from='female',
                                               sex.to='msm'),
                               value = 'fraction.msm.pairings.with.female')

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.sex',
                               applies.to=list(sex.from='msm',
                                               sex.to='msm'),
                               value = expression((1-fraction.msm.pairings.with.female) *
                                                    fraction.male.male.that.are.with.msm)
)

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.sex',
                               applies.to=list(sex.from='heterosexual_male',
                                               sex.to='msm'),
                               value = expression((1-fraction.msm.pairings.with.female) *
                                                    (1-fraction.male.male.that.are.with.msm))
)

# To heterosexual male
register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.sex',
                               applies.to=list(sex.from='female',
                                               sex.to='heterosexual_male'),
                               value = expression((1-fraction.heterosexual.male.pairings.with.male))
)

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.sex',
                               applies.to=list(sex.from='msm',
                                               sex.to='heterosexual_male'),
                               value = expression(fraction.heterosexual.male.pairings.with.male *
                                                    fraction.male.male.that.are.with.msm)
)

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.sex',
                               applies.to=list(sex.from='heterosexual_male',
                                               sex.to='heterosexual_male'),
                               value = expression(fraction.heterosexual.male.pairings.with.male *
                                                    (1-fraction.male.male.that.are.with.msm))
)


#---------------------------#
#-- Sexual Contact by Age --#
#---------------------------#

register.model.quantity(EHE.SPECIFICATION,
                        name = 'sexual.contact.by.age',
                        value = 0)


register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.age',
                               applies.to = list(sex.to='female',
                                                 sex.from=c('heterosexual_male','msm')),
                               value = get.female.sexual.age.contact.proportions)

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.age',
                               applies.to = list(sex.to=c('heterosexual_male','msm'),
                                                 sex.from=c('heterosexual_male','msm')),
                               value = get.msm.sexual.age.contact.proportions)


register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.age',
                               applies.to = list(sex.to=c('heterosexual_male','msm'),
                                                 sex.from='female'),
                               value = get.heterosexual.male.sexual.age.contact.proportions)



register.model.element(EHE.SPECIFICATION,
                       name = 'age.mixing.sd.mult',
                       value = 1,
                       scale='ratio')

register.model.element(EHE.SPECIFICATION,
                       name = 'single.year.age.sexual.availability',
                       value = get.sexual.availability(),
                       dimension.values = list(age=CENSUS.AGES),
                       resolve.dimension.values.against.model = F,
                       scale='non.negative.number')

register.model.element(EHE.SPECIFICATION,
                       name = 'single.year.female.age.counts',
                       get.value.function = get.female.single.year.age.counts,
                       dimension.values = list(age=CENSUS.AGES),
                       resolve.dimension.values.against.model = F,
                       scale='non.negative.number')

register.model.element(EHE.SPECIFICATION,
                       name = 'single.year.msm.age.counts',
                       get.value.function = get.msm.single.year.age.counts,
                       dimension.values = list(age=CENSUS.AGES),
                       resolve.dimension.values.against.model = F,
                       scale='non.negative.number')

register.model.element(EHE.SPECIFICATION,
                       name = 'single.year.heterosexual.male.age.counts',
                       get.value.function = get.heterosexual.male.single.year.age.counts,
                       dimension.values = list(age=CENSUS.AGES),
                       resolve.dimension.values.against.model = F,
                       scale='non.negative.number')

#----------------------------#
#-- Sexual Contact by Race --#
#----------------------------#

baseline.sexual.oes = array(c(3.76,1,1,1,2.19,1,1,1,1.55),
                            dim = c(race.to=3, race.from=3),
                            dimnames = list(race.to=c('black','hispanic','other'),
                                            race.from=c('black','hispanic','other')))

register.model.element(EHE.SPECIFICATION,
                       name = 'race.sexual.oes',
                       scale = 'ratio',
                       get.functional.form.function = get.geographically.aggregated.race.oes,
                       within.county.race.oes = baseline.sexual.oes)

register.model.quantity(EHE.SPECIFICATION,
                        name = 'sexual.contact.by.race',
                        value = sexual.oes.to.contact.proportions)

# The race sub-values

register.model.element(EHE.SPECIFICATION,
                       'race.population.counts',
                       scale = 'non.negative.number',
                       get.value.function = get.race.population.counts)

#----------------------------#
#-- Sexual Contact by Risk --#
#----------------------------#

register.model.quantity(EHE.SPECIFICATION,
                        name = 'sexual.contact.by.risk',
                        value = 0)

register.model.quantity(EHE.SPECIFICATION,
                        name = 'active.idu.counts',
                        value = expression(base.initial.population.sans.risk *
                                             prevalence.active.idu))
register.model.quantity(EHE.SPECIFICATION,
                        name = 'prior.idu.counts',
                        value = expression(base.initial.population.sans.risk *
                                             prevalence.prior.idu))
register.model.quantity(EHE.SPECIFICATION,
                        name = 'ever.idu.counts',
                        value = expression(base.initial.population.sans.risk *
                                             prevalence.ever.idu))
register.model.quantity(EHE.SPECIFICATION,
                        name = 'never.idu.counts',
                        value = expression(base.initial.population.sans.risk *
                                             prevalence.never.idu))



register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.risk',
                               applies.to = list(risk.from='never_IDU', risk.to='never_IDU'),
                               value = expression(never.idu.counts * never.idu.sexual.oe /
                                                    (never.idu.counts * never.idu.sexual.oe +
                                                       ever.idu.counts * oe.never.idu.pairings.with.idu))
)
register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.risk',
                               applies.to = list(risk.from='active_IDU', risk.to='never_IDU'),
                               value = expression(active.idu.counts * oe.never.idu.pairings.with.idu /
                                                    (never.idu.counts * never.idu.sexual.oe +
                                                       ever.idu.counts * oe.never.idu.pairings.with.idu))
)
register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.risk',
                               applies.to = list(risk.from='IDU_in_remission', risk.to='never_IDU'),
                               value = expression(prior.idu.counts * oe.never.idu.pairings.with.idu /
                                                    (never.idu.counts * never.idu.sexual.oe +
                                                       ever.idu.counts * oe.never.idu.pairings.with.idu))
)

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.risk',
                               applies.to = list(risk.from='never_IDU', risk.to='active_IDU'),
                               value = expression(never.idu.counts /
                                                    (never.idu.counts +
                                                       ever.idu.counts * idu.sexual.oe))
)
register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.risk',
                               applies.to = list(risk.from='active_IDU', risk.to='active_IDU'),
                               value = expression(active.idu.counts * idu.sexual.oe /
                                                    (never.idu.counts +
                                                       ever.idu.counts * idu.sexual.oe))
)
register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.risk',
                               applies.to = list(risk.from='IDU_in_remission', risk.to='active_IDU'),
                               value = expression(prior.idu.counts * idu.sexual.oe /
                                                    (never.idu.counts +
                                                       ever.idu.counts * idu.sexual.oe))
)

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.risk',
                               applies.to = list(risk.from='never_IDU', risk.to='IDU_in_remission'),
                               value = expression(never.idu.counts /
                                                    (never.idu.counts +
                                                       ever.idu.counts * idu.sexual.oe))
)
register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.risk',
                               applies.to = list(risk.from='active_IDU', risk.to='IDU_in_remission'),
                               value = expression(active.idu.counts * idu.sexual.oe /
                                                    (never.idu.counts +
                                                       ever.idu.counts * idu.sexual.oe))
)
register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.risk',
                               applies.to = list(risk.from='IDU_in_remission', risk.to='IDU_in_remission'),
                               value = expression(prior.idu.counts * idu.sexual.oe /
                                                    (never.idu.counts +
                                                       ever.idu.counts * idu.sexual.oe))
)

register.model.element.values(EHE.SPECIFICATION,
                              scale = 'ratio',
                              never.idu.sexual.oe = 1,
                              idu.sexual.oe = 1,
                              oe.never.idu.pairings.with.idu = EHE_BASE_PARAMETER_VALUES['oe.never.idu.pairings.with.idu'])

#-------------------------------#
#-- Sexual Transmission Rates --#
#-------------------------------#

register.model.quantity(EHE.SPECIFICATION,
                        name = 'sexual.transmission.rates',
                        value = 0,
                        scale = 'rate')

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.transmission.rates',
                               applies.to = list(sex.from=c('heterosexual_male','msm'),
                                                 sex.to=c('heterosexual_male','msm')),
                               value = 'msm.trates')

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.transmission.rates',
                               applies.to = list(sex.from=c('heterosexual_male','msm'),
                                                 sex.to=c('female')),
                               value = 'heterosexual.trates')

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.transmission.rates',
                               applies.to = list(sex.from=c('female'),
                                                 sex.to=c('heterosexual_male','msm')),
                               value = expression(heterosexual.trates * male.vs.female.heterosexual.rr))

# The flattened rates lets us track this as an outcome
register.model.quantity(EHE.SPECIFICATION,
                        name = 'flattened.sexual.transmission.rates',
                        value = 0,
                        scale = 'rate')

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'flattened.sexual.transmission.rates',
                               applies.to = list(sex.to='msm'),
                               value = expression(global.trate*msm.trates))

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'flattened.sexual.transmission.rates',
                               applies.to = list(sex.to='female'),
                               value = expression(global.trate * heterosexual.trates))

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'flattened.sexual.transmission.rates',
                               applies.to = list(sex.to='heterosexual_male'),
                               value = expression(global.trate * heterosexual.trates * male.vs.female.heterosexual.rr))

register.model.element(EHE.SPECIFICATION,
                       name = 'male.vs.female.heterosexual.rr',
                       value = 3.75/4.75 * 87.4/92,
                       # 1) ratio of female.to.male vs male.to.female - from Maunank's paper
                       # 2) ratio of condomless vaginal sex (male vs female)
                       scale = 'ratio')

register.model.element(EHE.SPECIFICATION,
                       name = 'msm.trates',
                       scale = 'rate',
                       functional.form = create.natural.spline.functional.form(knot.times=TRATE.KNOT.TIMES,
                                                                               knot.values = list(rate0=1,
                                                                                                  rate1=1,
                                                                                                  rate2=1),
                                                                               #link = 'log',
                                                                               link = 'identity',
                                                                               min = 0,
                                                                               knot.link = 'log',
                                                                               after.time = TRATE.AFTER.TIME,
                                                                               after.modifier = 1,
                                                                               modifiers.apply.to.change = T,
                                                                               overwrite.modifiers.with.alphas = T
                       ),
                       functional.form.from.time = 2000,
                       ramp.times = c(1960, 1970, 1980, 1990),
                       ramp.values = c(pre.peak=1, peak.start=1, peak.end=1, t0.start=1))


register.model.element(EHE.SPECIFICATION,
                       name = 'heterosexual.trates',
                       scale = 'rate',
                       functional.form = create.natural.spline.functional.form(knot.times=TRATE.KNOT.TIMES,
                                                                               knot.values = list(rate0=1,
                                                                                                  rate1=1,
                                                                                                  rate2=1),
                                                                               #link = 'log',
                                                                               link = 'identity',
                                                                               min = 0,
                                                                               knot.link = 'log',
                                                                               after.time = TRATE.AFTER.TIME,
                                                                               after.modifier = 1,
                                                                               modifiers.apply.to.change = T,
                                                                               #   after.modifier.application = 'additive.on.link.scale',
                                                                               overwrite.modifiers.with.alphas = T
                       ),
                       functional.form.from.time = 2000,
                       ramp.times = c(1970, 1980, 1990),
                       ramp.values = c(pre.peak=1, peak.start=1, peak.end=1))


##--------------------------##
##--------------------------##
##-- SET TRACKED OUTCOMES --##
##--------------------------##
##--------------------------##

track.transition(EHE.SPECIFICATION,
                 name = 'new',
                 outcome.metadata = create.outcome.metadata(display.name = 'New Diagnoses',
                                                            description = "Number of Individuals with a New Diagnosis of HIV in the Past Year",
                                                            scale = 'non.negative.number',
                                                            axis.name = 'Cases',
                                                            units = 'cases',
                                                            singular.unit = 'case'),
                 dimension = 'continuum',
                 from.compartments = 'undiagnosed.states',
                 to.compartments = 'first.diagnosed.states',
                 keep.dimensions = c('location','age','race','sex','risk'),
                 corresponding.data.outcome = 'diagnoses')

track.dynamic.outcome(EHE.SPECIFICATION,
                      name = 'incidence',
                      outcome.metadata = create.outcome.metadata(display.name = 'Incidence',
                                                                 description = "Number of Incident HIV Infections in the Past Year",
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Infections',
                                                                 units = 'infections',
                                                                 singular.unit = 'infection'),
                      dynamic.quantity.name = 'incidence',
                      keep.dimensions = c('location','age','race','sex','risk'))

track.point.outcome(EHE.SPECIFICATION,
                    'point.population',
                    outcome.metadata = NULL,
                    scale = 'non.negative.number',
                    value = expression(uninfected+infected),
                    keep.dimensions = c('location','age','race','sex','risk'),
                    save = F)

track.point.outcome(EHE.SPECIFICATION,
                    'general.population.testing',
                    outcome.metadata = create.outcome.metadata(display.name = 'HIV Testing Rate',
                                                               description = "The average number of HIV tests per year among the general population",
                                                               scale = 'rate',
                                                               axis.name = 'Tests per Year',
                                                               units = 'tests/yr',
                                                               singular.unit = 'test/yr'),
                    value = 'general.population.testing',
                    value.is.numerator = F,
                    denominator.outcome = 'point.population',
                    keep.dimensions = c('location','age','race','sex','risk'))

track.integrated.outcome(EHE.SPECIFICATION,
                         name = 'cumulative.uninfected',
                         outcome.metadata = NULL,
                         value.to.integrate = 'uninfected',
                         keep.dimensions = c('location','age','race','sex','risk'),
                         scale = 'non.negative.number',
                         save = F)

track.integrated.outcome(EHE.SPECIFICATION,
                         name = 'cumulative.infected',
                         outcome.metadata = NULL,
                         value.to.integrate = 'infected',
                         keep.dimensions = c('location','age','race','sex','risk'),
                         scale = 'non.negative.number',
                         save = F)

track.integrated.outcome(EHE.SPECIFICATION,
                         name = 'prep.indications',
                         outcome.metadata = create.outcome.metadata(display.name = 'PrEP Indications',
                                                                    description = "The Number of People with an Indication for PrEP",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Number indicated for PrEP',
                                                                    units = 'people',
                                                                    singular.unit = 'person'),
                         value.to.integrate = 'uninfected',
                         multiply.by = 'prep.indication', 
                         keep.dimensions = c('location','age','race','sex'),
                         corresponding.data.outcome = 'prep.indications',
                         save = T
)

track.integrated.outcome(EHE.SPECIFICATION,
                         name = 'prep.uptake',
                         outcome.metadata = create.outcome.metadata(display.name = 'PrEP Uptake',
                                                                    description = "The Number of People who Received a PrEP Prescription in the Past Year",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Number with PrEP prescription',
                                                                    units = 'people',
                                                                    singular.unit = 'person'),
                         value.to.integrate = 'uninfected',
                         multiply.by = 'proportion.receiving.prep', 
                         keep.dimensions = c('location','age','race','sex'),
                         corresponding.data.outcome = 'prep',
                         save = T) 

track.cumulative.outcome(EHE.SPECIFICATION,
                         name = 'prep.uptake.proportion',
                         outcome.metadata = create.outcome.metadata(display.name = 'PrEP Uptake',
                                                                    description = "The Proportion of People who Received a PrEP Prescription in the Past Year",
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion with PrEP prescription',
                                                                    units = '%',
                                                                    singular.unit = '%'),
                         value = 'prep.uptake',
                         value.is.numerator = T,
                         denominator.outcome = 'prep.indications',
                         keep.dimensions = c('location','age','race','sex'),
                         save = T) 

track.integrated.outcome(EHE.SPECIFICATION,
                         name = 'population',
                         outcome.metadata = create.outcome.metadata(display.name = 'Population',
                                                                    description = "The Number of Infected and Uninfected Individuals in the Population",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Population',
                                                                    units = 'people',
                                                                    singular.unit = 'person'),
                         value.to.integrate = 'point.population',
                         keep.dimensions = c('location','age','race','sex','risk'),
                         corresponding.data.outcome = 'adult.population',
                         save = T)

track.integrated.outcome(EHE.SPECIFICATION,
                         name = 'suppression',
                         outcome.metadata = create.outcome.metadata(display.name = 'Suppression',
                                                                    description = "The Proportion of People with Diagnosed HIV who are Virally Suppressed",
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion Suppressed',
                                                                    units = '%'),
                         subset.dimension.values = list(continuum='diagnosed.states'),
                         value.to.integrate = 'infected',
                         value.is.numerator = T,
                         multiply.by = 'suppression.of.diagnosed',
                         denominator.outcome = 'diagnosed.prevalence',
                         keep.dimensions = c('location','age','race','sex','risk'),
                         corresponding.data.outcome = 'suppression',
                         save = T)

register.model.element(EHE.SPECIFICATION,
                       'fraction.population.over.18',
                       scale = 'proportion',
                       get.value.function = get.fraction.over.age,
                       age = 18)

register.model.element(EHE.SPECIFICATION,
                       'fraction.tests.over.18',
                       scale = 'proportion',
                       dimensions = 'age',
                       dimension.values = list(age='all.ages'),
                       value = array(c(0.9,1,1,1,1), dim=c(age=5), dimnames=list(age=c('13-24 years','25-34 years','35-44 years','45-54 years','55+ years'))))

track.cumulative.outcome(EHE.SPECIFICATION,
                         name = 'cumulative.uninfected.over.18',
                         value = expression(cumulative.uninfected * fraction.population.over.18),
                         scale = 'non.negative.number',
                         keep.dimensions = c('location','age','race','sex','risk'),
                         rename.dimension.values = list(age=c('13-24 years'='18-24 years')),
                         save = F,
                         outcome.metadata = NULL
)

track.cumulative.proportion.from.rate(EHE.SPECIFICATION,
                                      name = 'proportion.general.population.tested.including.under.18',
                                      outcome.metadata = NULL,
                                      rate.value = 'general.population.testing',
                                      denominator.outcome = 'cumulative.uninfected',
                                      keep.dimensions = c('location','age','race','sex','risk'),
                                      save = F)

track.cumulative.outcome(EHE.SPECIFICATION,
                         name = 'proportion.general.population.tested',
                         outcome.metadata = create.outcome.metadata(display.name = 'Proportion Tested',
                                                                    description = "The Proportion of General Population who Received an HIV Test in the Past Year",
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion Tested',
                                                                    units = '%'),
                         value = expression(proportion.general.population.tested.including.under.18 * fraction.tests.over.18 / fraction.population.over.18),
                         value.is.numerator = F,
                         denominator.outcome = 'cumulative.uninfected.over.18',
                         rename.dimension.values = list(age=c('13-24 years'='18-24 years')),
                         corresponding.data.outcome = 'proportion.tested',
                         keep.dimensions = c('location','age','race','sex','risk'))

track.integrated.outcome(EHE.SPECIFICATION,
                         name = 'number.of.tests.in.uninfected',
                         outcome.metadata = create.outcome.metadata(display.name = 'Number of HIV Tests Among Uninfected',
                                                                    description = "Number of HIV Tests Done Among Uninfected in the Past Year",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Tests',
                                                                    units = 'tests',
                                                                    singular.unit = 'test'),
                         scale = 'non.negative.number',
                         value.to.integrate = 'uninfected',
                         multiply.by = 'general.population.testing',
                         save = F)

track.cumulative.outcome(EHE.SPECIFICATION,
                         name = 'total.hiv.tests',
                         outcome.metadata = create.outcome.metadata(display.name = 'Total Number of HIV Tests',
                                                                    description = "Number of HIV Tests Done in the Past Year",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Tests',
                                                                    units = 'tests',
                                                                    singular.unit = 'test'),
                         value = expression(number.of.tests.in.uninfected+new),
                         keep.dimensions = c("location","age","race","sex","risk"))

track.cumulative.outcome(EHE.SPECIFICATION,
                         name = 'total.hiv.tests.per.population',
                         outcome.metadata = create.outcome.metadata(display.name = 'Total Number of HIV Tests Per Population',
                                                                    description = "Number of HIV Tests Done in the Past Year Per Population",
                                                                    scale = 'proportion',
                                                                    axis.name = 'Tests Per Population',
                                                                    units = '%',
                                                                    singular.unit = '%'),
                         value = expression(total.hiv.tests/population),
                         denominator.outcome = 'population',
                         keep.dimensions = c("location"))

track.cumulative.outcome(EHE.SPECIFICATION,
                         name = 'cdc.hiv.test.positivity',
                         corresponding.data.outcome = 'cdc.hiv.test.positivity',
                         outcome.metadata = create.outcome.metadata(display.name = 'Proportion of Total CDC-funded HIV Tests that are Positive',
                                                                    description = "Proportion of Total CDC-funded HIV Tests that are Positive",
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion positive',
                                                                    units = '%',
                                                                    singular.unit = '%'),
                         value = expression(2.810587*(new/total.hiv.tests)), # ratio comes from cdc_positivity_bias.R"
                         denominator.outcome = 'total.hiv.tests',
                         keep.dimensions = c("location","age","race","sex","risk"))

track.cumulative.outcome(EHE.SPECIFICATION,
                         name = 'awareness',
                         outcome.metadata = create.outcome.metadata(display.name = 'The Proportion of People with HIV Aware of their Diagnosis',
                                                                    description = "The Proportion of People with HIV Aware of their Diagnosis",
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion aware',
                                                                    units = '%',
                                                                    singular.unit = '%'),
                         value = expression(diagnosed.prevalence/cumulative.infected),
                         corresponding.data.outcome = 'awareness',
                         denominator.outcome = 'cumulative.infected',
                         keep.dimensions = c("location"))

track.dynamic.outcome(EHE.SPECIFICATION,
                      name = 'hiv.mortality',
                      outcome.metadata = create.outcome.metadata(display.name = 'Mortality in PWH',
                                                                 description = "Number of People with Diagnosed HIV who Died of Any Cause in the Past Year",
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Deaths',
                                                                 units = 'deaths',
                                                                 singular.unit = 'death'),
                      dynamic.quantity.name = 'mortality',
                      corresponding.data.outcome = 'hiv.deaths',
                      groups = 'infected',
                      subset.dimension.values = list(continuum = "diagnosed.states"), # only those who were diagnosed, diff from aids deaths
                      exclude.tags = "emigration",
                      keep.dimensions = c('location','sex'))

track.integrated.outcome(EHE.SPECIFICATION,
                         name = 'diagnosed.prevalence',
                         outcome.metadata = create.outcome.metadata(display.name = 'Prevalence (of Diagnosed PWH)',
                                                                    description = "The Number of People with HIV Aware of their Diagnosis",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Prevalent Cases',
                                                                    units = 'cases',
                                                                    singular.unit = 'case'),
                         value.to.integrate = 'infected',
                         subset.dimension.values = list(continuum = "diagnosed.states"),
                         corresponding.data.outcome = 'diagnosed.prevalence',
                         keep.dimensions = c('location','age','race','sex','risk')
)



track.dynamic.outcome(EHE.SPECIFICATION,
                      name = 'immigration',
                      outcome.metadata = create.outcome.metadata(display.name = 'Immigration',
                                                                 description = "Number of People Immigrating into MSA in the Past Year",
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Number Immigrating',
                                                                 units = 'individuals'),
                      dynamic.quantity.name = 'births',
                      corresponding.data.outcome = 'adult.immigration',
                      include.tags = "immigration",
                      keep.dimensions = c('location','age','race','sex'))

track.dynamic.outcome(EHE.SPECIFICATION,
                      name = 'emigration',
                      outcome.metadata = create.outcome.metadata(display.name = 'Emigration',
                                                                 description = "Number of People Emigrating from MSA in the Past Year",
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Number Emigrating',
                                                                 units = 'individuals'),
                      dynamic.quantity.name = 'mortality',
                      corresponding.data.outcome = 'adult.emigration',
                      include.tags = "emigration",
                      keep.dimensions = c('location','age','race','sex'))

register.model.element(EHE.SPECIFICATION,
                       name = 'aids.to.new.diagnosis.ratio',
                       value = 1.044985, # see aids_diagnoses_multiplier.R
                       scale = 'ratio')


track.cumulative.outcome(EHE.SPECIFICATION,
                         name = 'aids.diagnoses',
                         outcome.metadata = create.outcome.metadata(display.name = 'AIDS Diagnoses',
                                                                    description = "Number of Individuals with an AIDS Diagnosis in the Past Year",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Cases',
                                                                    units = 'cases',
                                                                    singular.unit = 'case'),
                         value = expression(new*aids.to.new.diagnosis.ratio),
                         corresponding.data.outcome = "aids.diagnoses",
                         keep.dimensions = c("location","age","race","sex","risk"),
                         to.year = 2001)

track.dynamic.outcome(EHE.SPECIFICATION,
                      name = 'aids.deaths',
                      outcome.metadata = create.outcome.metadata(display.name = 'AIDS Deaths',
                                                                 description = "Number of People with HIV who Died of AIDS in the Past Year",
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Deaths',
                                                                 units = 'deaths',
                                                                 singular.unit = 'death'),
                      dynamic.quantity.name = 'mortality',
                      corresponding.data.outcome = 'aids.deaths',
                      groups = 'infected',
                      exclude.tags = "emigration",
                      keep.dimensions = c("location","race","sex","risk"),
                      subset.dimension.values = list(continuum = "diagnosed.states"), # maybe include this? 
                      from.year = 1980, 
                      to.year = 2001)

track.dynamic.outcome(EHE.SPECIFICATION,
                      name = 'total.mortality',
                      outcome.metadata = create.outcome.metadata(display.name = 'Total Deaths',
                                                                 description = "Number of People who Died of Any Cause in the Past Year",
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Deaths',
                                                                 units = 'deaths',
                                                                 singular.unit = 'death'),
                      dynamic.quantity.name = 'mortality',
                      corresponding.data.outcome = 'deaths',
                      groups = NULL,
                      exclude.tags = "emigration",
                      keep.dimensions = character())

track.cumulative.outcome(EHE.SPECIFICATION,
                         name = 'number.injecting.drugs',
                         outcome.metadata = create.outcome.metadata(display.name = 'Number Injecting Drugs',
                                                                    description = "Number of Individuals Injecting Drugs",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Number Injecting',
                                                                    units = 'people',
                                                                    singular.unit = 'person'),
                         value = "population",
                         subset.dimension.values = list(risk = "active_IDU"),
                         keep.dimensions = c("location","age","sex"),
                         save=F)

track.cumulative.outcome(EHE.SPECIFICATION,
                         name = 'proportion.injecting.drugs',
                         outcome.metadata = create.outcome.metadata(display.name = 'Proportion Injecting Drugs',
                                                                    description = "Proportion of Total Population Injecting Drugs",
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion Injecting',
                                                                    units = '%',
                                                                    singular.unit = '%'),
                         value = "number.injecting.drugs",
                         denominator.outcome = 'population',
                         value.is.numerator = T,
                         keep.dimensions = c("location","age","sex"))

track.cumulative.outcome(EHE.SPECIFICATION,
                         name = 'proportion.using.cocaine',
                         corresponding.data.outcome = "cocaine",
                         outcome.metadata = create.outcome.metadata(display.name = 'Proportion Using Cocaine',
                                                                    description = "Proportion of Total Population Using Cocaine",
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion Using Cocaine',
                                                                    units = '%',
                                                                    singular.unit = '%'),
                         value = expression(proportion.injecting.drugs*5.97), # multiplier from NSDUH, large metro, 2015-2018
                         denominator.outcome = 'population',
                         value.is.numerator = F,
                         keep.dimensions = c("location","age","sex"))

track.cumulative.outcome(EHE.SPECIFICATION,
                         name = 'proportion.using.heroin',
                         corresponding.data.outcome = "heroin",
                         outcome.metadata = create.outcome.metadata(display.name = 'Proportion Using Heroin',
                                                                    description = "Proportion of Total Population Using Heroin",
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion Using Heroin',
                                                                    units = '%',
                                                                    singular.unit = '%'),
                         value = expression(proportion.injecting.drugs*1.15), # multiplier from NSDUH, large metro, 2015-2018
                         denominator.outcome = 'population',
                         value.is.numerator = F,
                         keep.dimensions = c("location","age","sex"))

track.integrated.outcome(EHE.SPECIFICATION,
                         name = 'sexual.transmission.rates',
                         outcome.metadata = create.outcome.metadata(display.name = 'Sexual Transmission Rates',
                                                                    description = "Estimated rates of sexual transmission",
                                                                    scale = 'rate',
                                                                    axis.name = 'Rate',
                                                                    units = '/y',
                                                                    singular.unit = '/y'),
                         value.to.integrate = 'flattened.sexual.transmission.rates',
                         denominator.outcome = 'population',
                         dimension.alias.suffix = 'to',
                         keep.dimensions = c('location','age','race','sex')
)


##--------------------------------##
##--------------------------------##
##-- REGISTER THE SPECIFICATION --##
##--------------------------------##
##--------------------------------##

register.model.specification(EHE.SPECIFICATION)

source('../jheem_analyses/applications/EHE/ehe_parameters_helpers.R')
source('../jheem_analyses/applications/EHE/ehe_parameters.R')
source('../jheem_analyses/applications/EHE/ehe_parameter_mapping.R')

register.calibrated.parameters.for.version('ehe',
                                           distribution = EHE.PARAMETERS.PRIOR,
                                           apply.function = EHE.APPLY.PARAMETERS.FN,
                                           sampling.blocks = EHE.PARAMETER.SAMPLING.BLOCKS,
                                           calibrate.to.year = 2025,
                                           join.with.previous.version = F)
