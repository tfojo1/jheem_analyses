
#-- For now - source code and load data --#

# This 'source' call is equivalent to loading the jheem2 package
source('../jheem2/R/tests/source_jheem2_package.R')

    
    
# Source supporting files
source('../jheem_analyses/source_code.R')

 

##--------------------##
##--------------------##
##-- INITIAL SET-UP --##
##--------------------##
##--------------------##   
    

EHE.SPECIFICATION = create.jheem.specification(version = 'ehe',
                                               iteration = 1,
                                               description = "The initial JHEEM version, set up to address achieving EHE goals",
                                               
                                               age.endpoints=c(13,25,35,45,55,Inf),
                                               compartments.for.infected.only = list(
                                                   continuum = c('undiagnosed', 'undiagnosed_from_prep', 'diagnosed'),
                                                   stage = c('acute', 'chronic')
                                               ),
                                               compartments.for.uninfected.only = list(),
                                               compartments.for.infected.and.uninfected = list(
                                                   location = 'location',
                                                   age = 'all.ages',
                                                   race=c('black','hispanic','other'),
                                                   sex= c('heterosexual_male', 'msm', 'female'),
                                                   risk = c('never_IDU', 'active_IDU', 'IDU_in_remission')
                                               ),

                                               compartment.value.aliases = list(
                                                   location = function(location){location},
                                                   
                                                   first.diagnosed.states='diagnosed',
                                                   diagnosed.states='diagnosed',
                                                   undiagnosed.states=c('undiagnosed','undiagnosed_from_prep'),
                                                   undiagnosed.from.prep.states='undiagnosed_from_prep',
                                                   undiagnosed.no.prep.states='undiagnosed',

                                                   acute.stages = 'acute',
                                                   chronic.stages = 'chronic',
                                                   
                                                   active.idu.states = 'active_IDU',
                                                   non.active.idu.states = c('never_IDU','IDU_in_remission'),
                                                   prior.idu.states = 'IDU_in_remission',
                                                   never.idu.states = 'never_IDU',
                                                   idu.states = c('active_IDU','IDU_in_remission')
                                               )
                                               )

##----------------------##
##-- Fix Strata Sizes --##
##----------------------##

register.fixed.model.strata(EHE.SPECIFICATION,
                            applies.after.time = 2007,
                            applies.before.time = Inf,
                            fix.strata = T,
                            dimensions.to.fix = c('location','age','race','sex'))


##------------------------##
##-- Initial Population --##
##------------------------##

register.initial.population(EHE.SPECIFICATION,
                            group = 'infected',
                            value = 'initial.population.infected')

register.model.quantity(EHE.SPECIFICATION,
                        name = 'initial.population.infected',
                        value = 0)

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'initial.population.infected',
                               applies.to = list(continuum='undiagnosed',
                                                 stage = 1),
                               value = expression(base.initial.population * seed.rate.per.stratum))



register.initial.population(EHE.SPECIFICATION,
                            group = 'uninfected',
                            value = 'initial.population.uninfected')

register.model.quantity(EHE.SPECIFICATION,
                        name = 'initial.population.uninfected',
                        value = expression(base.initial.population * (1-seed.rate.per.stratum)))



register.model.quantity(EHE.SPECIFICATION,
                        name = 'base.initial.population.sans.risk',
                        value = 0)

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'base.initial.population.sans.risk',
                               applies.to = list(sex='female'),
                               value = 'base.initial.female.population')

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'base.initial.population.sans.risk',
                               applies.to = list(sex='msm'),
                               value = expression(base.initial.male.population * proportion.msm.of.male))

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'base.initial.population.sans.risk',
                               applies.to = list(sex='heterosexual_male'),
                               value = expression(base.initial.male.population * (1-proportion.msm.of.male)))

register.model.quantity(EHE.SPECIFICATION,
                        name = 'base.initial.population',
                        value = 0)

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'base.initial.population',
                               applies.to = list(risk='never_IDU'),
                               value = expression(base.initial.population.sans.risk * (1-prevalence.ever.idu)))

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'base.initial.population',
                               applies.to = list(risk='active_IDU'),
                               value = expression(base.initial.population.sans.risk * prevalence.active.idu))

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'base.initial.population',
                               applies.to = list(risk='IDU_in_remission'),
                               value = expression(base.initial.population.sans.risk * (prevalence.ever.idu-prevalence.active.idu)))

register.model.element(EHE.SPECIFICATION,
                       name = 'base.initial.female.population',
                       get.value.function = get.base.initial.female.population,
                       scale = 'non.negative.number')

register.model.element(EHE.SPECIFICATION,
                       name = 'base.initial.male.population',
                       get.value.function = get.base.initial.male.population,
                       scale = 'non.negative.number')

register.model.element(EHE.SPECIFICATION,
                       name = 'prevalence.ever.idu',
                       get.value.function = get.location.ever.idu.prevalence,
                       scale = 'proportion')

register.model.element(EHE.SPECIFICATION,
                       name = 'prevalence.active.idu',
                       get.value.function = get.location.active.idu.prevalence,
                       scale = 'proportion')

register.model.element(EHE.SPECIFICATION,
                       name = 'seed.rate.per.stratum',
                       get.value.function = get.seed.rate.per.stratum,
                       scale = 'proportion')

register.model.quantity(EHE.SPECIFICATION,
                        name = 'prevalence.prior.idu',
                        value = expression(prevalence.ever.idu-prevalence.active.idu))

register.model.quantity(EHE.SPECIFICATION,
                        name = 'prevalence.never.idu',
                        value = expression(1-prevalence.ever.idu))


##--------------------------------------##
##--------------------------------------##
##--        GENERAL QUANTITIES        --##
##--  (for multiple core components)  --##
##--------------------------------------##
##--------------------------------------##


##----------##
##-- PrEP --##
##----------##

##-- Oral PrEP --##
register.model.element(EHE.SPECIFICATION,
                       name = 'oral.prep',
                       scale = 'proportion',
                       get.functional.form.function = get.prep.model,
                       functional.form.from.time = 2014,
                       prep.manager = ALL.DATA.MANAGERS$prep,
                       ramp.times = 2011,
                       ramp.values = 0)

register.model.element(EHE.SPECIFICATION,
                       name = 'oral.prep.msm.rr',
                       scale = 'ratio',
                       functional.form = create.static.functional.form(value=EHE_BASE_PARAMETER_VALUES['prep.rr.msm'],
                                                                       link='log')
)


register.model.element(EHE.SPECIFICATION,
                       name = 'oral.prep.heterosexual.rr',
                       scale = 'ratio',
                       functional.form = create.static.functional.form(value=EHE_BASE_PARAMETER_VALUES['prep.rr.heterosexual'],
                                                                       link='log')
)

register.model.element(EHE.SPECIFICATION,
                       name = 'oral.prep.idu.rr',
                       scale = 'ratio',
                       functional.form = create.static.functional.form(value=EHE_BASE_PARAMETER_VALUES['prep.rr.idu'],
                                                                       link='log')
)

register.model.quantity(EHE.SPECIFICATION,
                        name = 'oral.prep.rr',
                        value = 'oral.prep.heterosexual.rr') 
    # ^the background value is heterosexual.rr, which we will overwrite with the msm and idu values

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'oral.prep.rr',
                               value = 'oral.prep.msm.rr',
                               applies.to = list(sex='msm',
                                                 risk='non.active.idu.states'))

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'oral.prep.rr',
                               value = 'oral.prep.idu.rr',
                               applies.to = list(risk='active.idu.states'))

register.model.element(EHE.SPECIFICATION,
                       name = 'oral.prep.persistence',
                       scale = 'proportion',
                       value = EHE_BASE_PARAMETER_VALUES['prep.persistence'])

##-- LAI PrEP --##
register.model.element(EHE.SPECIFICATION, 
                       name = 'lai.prep',
                       scale = 'proportion',
                       value = 0)

register.model.quantity(EHE.SPECIFICATION,
                        name = 'lai.prep.rr',
                        value = expression(oral.prep.rr * lai.vs.oral.prep.hr))

register.model.element(EHE.SPECIFICATION,
                       name = 'lai.vs.oral.prep.hr',
                       scale = 'ratio',
                       value = 0.34)

register.model.quantity(EHE.SPECIFICATION,
                        name = 'lai.prep.persistence',
                        value = expression(1 - (1-oral.prep.persistence) * lai.vs.oral.prep.discontinuation.rr))

register.model.element(EHE.SPECIFICATION,
                       name = 'lai.vs.oral.prep.discontinuation.rr',
                       scale = 'ratio',
                       value = 1)


##-- Common to All PrEP / Combinations of PrEP modalities --##
register.model.element(EHE.SPECIFICATION,
                       name = 'prep.screening.frequency',
                       scale = 'time',
                       value = 0.25) #3 months

register.model.quantity(EHE.SPECIFICATION,
                        name = 'all.prep.coverage',
                        value = expression(oral.prep + lai.prep),
                        scale = 'proportion')

register.model.quantity(EHE.SPECIFICATION,
                        name = 'all.prep.risk',
                        value = expression(oral.prep * oral.prep.rr + lai.prep * lai.prep.rr))

register.model.quantity(EHE.SPECIFICATION,
                        name = 'all.prep.discontinuation',
                        value = expression( (-log(1-oral.prep.persistence) * oral.prep +
                                                 -log(1-lai.prep.persistence) * lai.prep) /
                                                (oral.prep + lai.prep) ),
                        na.replacement = 0)


#-------------#
#-- Testing --#
#-------------#

TESTING.FIRST.YEAR.FRACTION.OF.RAMP = 0.5^(1993-1982)
register.model.element(EHE.SPECIFICATION,
                       name = 'testing',
                       scale = 'rate',
                       
                       get.functional.form.function = get.testing.model,
                       continuum.manager = ALL.DATA.MANAGERS$continuum,
                       functional.form.scale = 'proportion',
                       functional.form.from.time = 2010,
                       
                       ramp.scale = 'proportion',
                       ramp.times = c(1981,1982,1993),
                       ramp.values = c(0,0.5*TESTING.FIRST.YEAR.FRACTION.OF.RAMP,0.5),
                       ramp.interpolate.links = c('identity','log','identity'))


##---------------------##
##-- Needle Exchange --##
##---------------------##

register.model.element(EHE.SPECIFICATION, 
                       name = 'needle.exchange',
                       scale = 'proportion',
                       value = 0)

register.model.element(EHE.SPECIFICATION,
                       name='needle.exchange.rr',
                       scale = 'ratio',
                       value = EHE_BASE_PARAMETER_VALUES['needle.exchange.rr'])



##-----------------##
##-- Suppression --##                            
##-----------------##

register.model.quantity(EHE.SPECIFICATION,
                        name = 'suppression',
                        value = 0)

register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'suppression',
                               value = 'suppression.of.diagnosed',
                               applies.to = list(continuum='diagnosed'))

register.model.element(EHE.SPECIFICATION,
                       name = 'suppression.of.diagnosed',
                       scale = 'proportion',
                       
                       get.functional.form.function = get.suppression.model,
                       continuum.manager = ALL.DATA.MANAGERS$continuum,
                       functional.form.from.time = 2010,     
                       
                       ramp.times = 1996,
                       ramp.values = 0)


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
                    from.compartments = 'undiagnosed_from_prep',
                    to.compartments = 'diagnosed',
                    groups = 'infected',
                    value = expression( 1 / prep.screening.frequency))

register.transition(EHE.SPECIFICATION,
                    dimension = 'continuum',
                    from.compartments = 'undiagnosed_from_prep',
                    to.compartments = 'undiagnosed',
                    groups = 'infected',
                    value = 'all.prep.discontinuation')

register.transition(EHE.SPECIFICATION,
                    dimension = 'continuum',
                    from.compartments = 'undiagnosed',
                    to.compartments = 'diagnosed',
                    groups = 'infected',
                    value = 'testing')


##---------------------##
##-- IDU Transitions --##
##---------------------##

register.transition(EHE.SPECIFICATION,
                    dimension = 'risk',
                    groups = c('infected', 'uninfected'),
                    from.compartments = 'never_IDU',
                    to.compartments = 'active_IDU',
                    value = 'idu.incidence')

register.transition(EHE.SPECIFICATION,
                    dimension = 'risk',
                    groups = c('infected', 'uninfected'),
                    from.compartments = 'active_IDU',
                    to.compartments = 'IDU_in_remission',
                    value = expression(idu.remission * 
                                           (1 + needle.exchange * 
                                                (needle.exchange.remission.rate.ratio - 1))))

register.transition(EHE.SPECIFICATION,
                    dimension = 'risk',
                    groups = c('infected', 'uninfected'),
                    from.compartments = 'IDU_in_remission',
                    to.compartments = 'active_IDU',
                    value = 'idu.relapse')

register.model.element(EHE.SPECIFICATION,
                       name = 'idu.incidence',
                       scale = 'rate',
                       get.functional.form.function = get.incident.idu.model,
                       static=F,
                       functional.form.from.time = 1990,
                       idu.manager = ALL.DATA.MANAGERS$idu,
                       census = ALL.DATA.MANAGERS$census.full.msm)

register.model.element(EHE.SPECIFICATION,
                       name = 'idu.remission',
                       scale = 'rate',
                       get.functional.form.function = get.idu.remission.model,
                       static=T,
                       idu.manager = ALL.DATA.MANAGERS$idu,
                       census = ALL.DATA.MANAGERS$census.full.msm)

register.model.element(EHE.SPECIFICATION,
                       name = 'idu.relapse',
                       scale = 'rate',
                       get.functional.form.function = get.idu.relapse.model,
                       static=T,
                       idu.manager = ALL.DATA.MANAGERS$idu,
                       census = ALL.DATA.MANAGERS$census.full.msm)

register.model.element(EHE.SPECIFICATION,
                       name = 'needle.exchange.remission.rate.ratio',
                       scale = 'ratio',
                       value = EHE_BASE_PARAMETER_VALUES['needle.exchange.remission.rate.ratio'])


##-----------------------##
##-- Stage Transitions --##
##-----------------------##

register.transition(EHE.SPECIFICATION,
                    dimension = 'stage',
                    groups = 'infected',
                    from.compartments = 'acute',
                    to.compartments = 'chronic',
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
                       get.value.function = get.model.fertility.rates,
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
                       get.value.function = get.non.idu.general.mortality.rates,
                       scale = 'rate')

register.model.element(EHE.SPECIFICATION,
                       name = 'idu.mortality.rate',
                       value = EHE_BASE_PARAMETER_VALUES['idu.mortality'],
                       scale = 'rate')

##------------------------##
##------------------------##
##--        AGING       --##
##------------------------##
##------------------------##

##----------------------##
##-- Uninfected Aging --##
##----------------------##

#register.transition(EHE.SPECIFICATION,
#                    dimension = 'age',
#                    from.compartments = 1,
#                    to.compartments = 2,
#                    value=1/4,
#                    groups='all')

register.model.element(EHE.SPECIFICATION,
                       name = 'default.aging',
                       scale = 'rate',
                       get.value.function = get.default.aging.rates)

register.aging(EHE.SPECIFICATION,
               groups = 'uninfected',
               aging.rate.value = 'default.aging')

##--------------------##
##-- Infected Aging --##
##--------------------##

register.aging(EHE.SPECIFICATION,
               groups = 'infected',
               aging.rate.value = 'hiv.positive.aging.rates')

register.model.element(EHE.SPECIFICATION,
                       name = 'hiv.positive.aging.rates',
                       get.functional.form.function = function(location, specification.metadata){
                           rates = get.default.aging.rates(location, specification.metadata)
                           create.natural.spline.functional.form(knot.times=c(rate.pre.spike=1980, 
                                                                              rate0=2000, 
                                                                              rate1=2010, 
                                                                              rate2=2020, 
                                                                              rate3=2030),
                                                                 knot.values=list(rate.pre.spike=rates,
                                                                                  rate0=rates,
                                                                                  rate1=rates,
                                                                                  rate2=rates,
                                                                                  rate3=rates),
                                                                 knot.link = 'identity',
                                                                 min = 0,
                                                                 overwrite.knot.values.with.alphas = T
                           )
                       },
                       functional.form.from.time = 1980,
                       scale='rate')





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
                      all.new.infections.into.compartments = list(stage=1))

register.transmission(EHE.SPECIFICATION,
                      contact.value = 'idu.contact',
                      susceptibility.value = 'idu.susceptibility',
                      transmissibility.value = 'idu.transmissibility',
                      new.infection.proportions.value = 'new.infection.proportions',
                      tag = 'idu.transmission',
                      all.new.infections.into.compartments = list(stage=1),
                      from.applies.to = list(risk='active.idu.states'),
                      to.applies.to = list(risk='active.idu.states'))


##--------------------##
##-- Susceptibility --##
##--------------------##

register.model.quantity(EHE.SPECIFICATION,
                        name = 'sexual.susceptibility',
                        value = expression( base.sexual.susceptibility *
                                                (all.prep.risk + 1-all.prep.coverage) )
)

register.model.quantity(EHE.SPECIFICATION,
                        name='idu.susceptibility',
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
                       value = mean(ALL.DATA.MANAGERS$pairing$msm.sex.with.female.estimates), 
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
                       dimension.values = list(age=ALL.DATA.MANAGERS$census.full$age.names),
                       resolve.dimension.values.against.model = F,
                       scale='non.negative.number')

register.model.element(EHE.SPECIFICATION,
                       name = 'single.year.female.age.counts',
                       get.value.function = get.female.single.year.age.counts,
                       dimension.values = list(age=ALL.DATA.MANAGERS$census.full$age.names),
                       resolve.dimension.values.against.model = F,
                       scale='non.negative.number')

register.model.element(EHE.SPECIFICATION,
                       name = 'single.year.msm.age.counts',
                       get.value.function = get.msm.single.year.age.counts,
                       dimension.values = list(age=ALL.DATA.MANAGERS$census.full$age.names),
                       resolve.dimension.values.against.model = F,
                       scale='non.negative.number')

register.model.element(EHE.SPECIFICATION,
                       name = 'single.year.heterosexual.male.age.counts',
                       get.value.function = get.heterosexual.male.single.year.age.counts,
                       dimension.values = list(age=ALL.DATA.MANAGERS$census.full$age.names),
                       resolve.dimension.values.against.model = F,
                       scale='non.negative.number')

#----------------------------#
#-- Sexual Contact by Race --#
#----------------------------#

register.model.quantity(EHE.SPECIFICATION,
                        name = 'sexual.contact.by.race',
                        value = 0)

# To Black
register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.race',
                               applies.to = list(race.from='black', race.to='black'),
                               value = expression(black.population.count * black.black.sexual.oe /
                                                      (black.population.count * black.black.sexual.oe + hispanic.population.count + other.population.count)))
register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.race',
                               applies.to = list(race.from='hispanic', race.to='black'),
                               value = expression(hispanic.population.count /
                                                      (black.population.count * black.black.sexual.oe + hispanic.population.count + other.population.count)))
register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.race',
                               applies.to = list(race.from='other', race.to='black'),
                               value = expression(other.population.count /
                                                      (black.population.count * black.black.sexual.oe + hispanic.population.count + other.population.count)))

# To Hispanic
register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.race',
                               applies.to = list(race.from='black', race.to='hispanic'),
                               value = expression(black.population.count /
                                                      (black.population.count + hispanic.population.count * hispanic.hispanic.sexual.oe + other.population.count)))
register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.race',
                               applies.to = list(race.from='hispanic', race.to='hispanic'),
                               value = expression(hispanic.population.count * hispanic.hispanic.sexual.oe /
                                                      (black.population.count + hispanic.population.count * hispanic.hispanic.sexual.oe + other.population.count)))
register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.race',
                               applies.to = list(race.from='other', race.to='hispanic'),
                               value = expression(other.population.count /
                                                      (black.population.count + hispanic.population.count * hispanic.hispanic.sexual.oe + other.population.count)))

# To Other
register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.race',
                               applies.to = list(race.from='black', race.to='other'),
                               value = expression(black.population.count /
                                                      (black.population.count + hispanic.population.count + other.population.count * other.other.sexual.oe)))
register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.race',
                               applies.to = list(race.from='hispanic', race.to='other'),
                               value = expression(hispanic.population.count /
                                                      (black.population.count + hispanic.population.count + other.population.count * other.other.sexual.oe)))
register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'sexual.contact.by.race',
                               applies.to = list(race.from='other', race.to='other'),
                               value = expression(black.population.count * other.other.sexual.oe /
                                                      (black.population.count + hispanic.population.count + other.population.count * other.other.sexual.oe)))

# The race sub-values

register.model.element.values(EHE.SPECIFICATION,
                              scale = 'non.negative.number',
                              black.population.count = function(location, specification.metadata){sum(get.census.data(ALL.DATA.MANAGERS$census.full,
                                                                                                             races = 'black',
                                                                                                             years = DEFAULT.POPULATION.YEARS,
                                                                                                             ages = (specification.metadata$age.endpoints[1]):min(specification.metadata$age.endpoints[length(specification.metadata$age.endpoints)],
                                                                                                                                                       ALL.DATA.MANAGERS$census.full$age.lowers[length(ALL.DATA.MANAGERS$census.full$age.lowers)]),
                                                                                                             fips=get.sub.locations(location, 'county', limit.to.completely.enclosing = T)))/ length(DEFAULT.POPULATION.YEARS)},
                              hispanic.population.count = function(location, specification.metadata){sum(get.census.data(ALL.DATA.MANAGERS$census.full,
                                                                                                                races = 'hispanic',
                                                                                                                years=DEFAULT.POPULATION.YEARS,
                                                                                                                ages = (specification.metadata$age.endpoints[1]):min(specification.metadata$age.endpoints[length(specification.metadata$age.endpoints)],
                                                                                                                                                                 ALL.DATA.MANAGERS$census.full$age.lowers[length(ALL.DATA.MANAGERS$census.full$age.lowers)]),
                                                                                                                fips=get.sub.locations(location, 'county', limit.to.completely.enclosing = T)))/ length(DEFAULT.POPULATION.YEARS)},
                              other.population.count = function(location, specification.metadata){sum(get.census.data(ALL.DATA.MANAGERS$census.full,
                                                                                                             races = setdiff(ALL.DATA.MANAGERS$census.full$races, c('black','hispanic')),
                                                                                                             years=DEFAULT.POPULATION.YEARS,
                                                                                                             ages = (specification.metadata$age.endpoints[1]):min(specification.metadata$age.endpoints[length(specification.metadata$age.endpoints)],
                                                                                                                                                              ALL.DATA.MANAGERS$census.full$age.lowers[length(ALL.DATA.MANAGERS$census.full$age.lowers)]),
                                                                                                             fips=get.sub.locations(location, 'county', limit.to.completely.enclosing = T)))/ length(DEFAULT.POPULATION.YEARS)}
)

register.model.element.values(EHE.SPECIFICATION,
                              scale='ratio',
                              black.black.sexual.oe=mean(sapply( c(ALL.DATA.MANAGERS$pairing$msm.sex.by.race.oe, ALL.DATA.MANAGERS$pairing$het.sex.by.race.oe), function(oe){
                                  oe['black','black']
                              })),
                              hispanic.hispanic.sexual.oe=mean(sapply( c(ALL.DATA.MANAGERS$pairing$msm.sex.by.race.oe, ALL.DATA.MANAGERS$pairing$het.sex.by.race.oe), function(oe){
                                  oe['hispanic','hispanic']
                              })),
                              other.other.sexual.oe=mean(sapply( c(ALL.DATA.MANAGERS$pairing$msm.sex.by.race.oe, ALL.DATA.MANAGERS$pairing$het.sex.by.race.oe), function(oe){
                                  oe['other','other']
                              })))

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
                        value = 0)

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
                                                                               link = 'log',
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
                                                                               link = 'log',
                                                                               after.time = TRATE.AFTER.TIME,
                                                                               after.modifier = 1,
                                                                               modifiers.apply.to.change = T,
                                                                            #   after.modifier.application = 'additive.on.link.scale',
                                                                               overwrite.modifiers.with.alphas = T
                       ),
                       functional.form.from.time = 2000,
                       ramp.times = c(1970, 1980, 1990),
                       ramp.values = c(pre.peak=1, peak.start=1, peak.end=1))


##------------------------------------##
##-- IV Transmission Contact Arrays --##
##------------------------------------##

register.model.quantity(EHE.SPECIFICATION,
                        name = 'idu.contact',
                        value = expression(global.trate *
                                               idu.trates * 
                                               extra.idu.peak.multiplier *
                                               idu.contact.by.age *
                                               idu.contact.by.race *
                                               idu.contact.by.sex))


#-- IDU Contact by Age --#


register.model.element(EHE.SPECIFICATION,
                       name = 'single.year.age.idu.availability',
                       value = get.idu.availability(),
                       dimension.values = list(age=ALL.DATA.MANAGERS$census.full$age.names),
                       resolve.dimension.values.against.model = F,
                       scale='non.negative.number')

register.model.quantity(EHE.SPECIFICATION,
                        name = 'single.year.age.counts',
                        value = expression(single.year.female.age.counts +
                                               single.year.msm.age.counts +
                                               single.year.heterosexual.male.age.counts))

register.model.quantity(EHE.SPECIFICATION,
                        name = 'idu.contact.by.age',
                        value = get.idu.age.contact.proportions)

#-- IDU Contact by Race --#


register.model.quantity(EHE.SPECIFICATION,
                        name = 'idu.contact.by.race',
                        value = 0)

# To Black
register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'idu.contact.by.race',
                               applies.to = list(race.from='black', race.to='black'),
                               value = expression(black.population.count * black.black.idu.oe /
                                                      (black.population.count * black.black.idu.oe + hispanic.population.count + other.population.count)))
register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'idu.contact.by.race',
                               applies.to = list(race.from='hispanic', race.to='black'),
                               value = expression(hispanic.population.count /
                                                      (black.population.count * black.black.idu.oe + hispanic.population.count + other.population.count)))
register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'idu.contact.by.race',
                               applies.to = list(race.from='other', race.to='black'),
                               value = expression(other.population.count /
                                                      (black.population.count * black.black.idu.oe + hispanic.population.count + other.population.count)))

# To Hispanic
register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'idu.contact.by.race',
                               applies.to = list(race.from='black', race.to='hispanic'),
                               value = expression(black.population.count /
                                                      (black.population.count + hispanic.population.count * hispanic.hispanic.idu.oe + other.population.count)))
register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'idu.contact.by.race',
                               applies.to = list(race.from='hispanic', race.to='hispanic'),
                               value = expression(hispanic.population.count * hispanic.hispanic.idu.oe /
                                                      (black.population.count + hispanic.population.count * hispanic.hispanic.idu.oe + other.population.count)))
register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'idu.contact.by.race',
                               applies.to = list(race.from='other', race.to='hispanic'),
                               value = expression(other.population.count /
                                                      (black.population.count + hispanic.population.count * hispanic.hispanic.idu.oe + other.population.count)))

# To Other
register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'idu.contact.by.race',
                               applies.to = list(race.from='black', race.to='other'),
                               value = expression(black.population.count /
                                                      (black.population.count + hispanic.population.count + other.population.count * other.other.idu.oe)))
register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'idu.contact.by.race',
                               applies.to = list(race.from='hispanic', race.to='other'),
                               value = expression(hispanic.population.count /
                                                      (black.population.count + hispanic.population.count + other.population.count * other.other.idu.oe)))
register.model.quantity.subset(EHE.SPECIFICATION,
                               name = 'idu.contact.by.race',
                               applies.to = list(race.from='other', race.to='other'),
                               value = expression(other.population.count * other.other.idu.oe /
                                                      (black.population.count + hispanic.population.count + other.population.count * other.other.idu.oe)))

# The race sub-values

register.model.element.values(EHE.SPECIFICATION,
                              scale='ratio',
                              black.black.idu.oe=ALL.DATA.MANAGERS$pairing$idu.oe.race['black','black'],
                              hispanic.hispanic.idu.oe=ALL.DATA.MANAGERS$pairing$idu.oe.race['hispanic','hispanic'],
                              other.other.idu.oe=ALL.DATA.MANAGERS$pairing$idu.oe.race['other','other'])


#-- IDU Contact by Sex --#

register.model.element(EHE.SPECIFICATION,
                       name = 'idu.transmission.sex.oes',
                       scale = 'ratio',
                       value = ALL.DATA.MANAGERS$pairing$idu.oe.sex,
                       dimensions = c('sex.from','sex.to'))

register.model.quantity(EHE.SPECIFICATION,
                        name = 'idu.contact.by.sex',
                        value = get.idu.sex.contact.proportions)

#-- IDU Transmission Rates --#


register.model.element(EHE.SPECIFICATION,
                       name = 'idu.trates',
                       scale = 'rate',
                       functional.form = create.natural.spline.functional.form(knot.times=TRATE.KNOT.TIMES,
                                                                               knot.values = list(rate0=1,
                                                                                                  rate1=1,
                                                                                                  rate2=1),
                                                                               link = 'log',
                                                                               
                                                                               after.time = TRATE.AFTER.TIME,
                                                                               after.modifier = 1,
                                                                               modifiers.apply.to.change = T,
                                                                             #  after.modifier.application = 'multiplicative.of.change.on.value.scale',
                                                                               overwrite.modifiers.with.alphas = T
                       ),
                       functional.form.from.time = 2000,
                       ramp.times = c(1970, 1980, 1990),
                       ramp.values = c(pre.peak=1, peak.start=1, peak.end=1))

register.model.element(EHE.SPECIFICATION,
                       name = 'extra.idu.peak.multiplier',
                       scale = 'ratio',
                       functional.form = create.linear.spline.functional.form(knot.times=c(pre.peak=1970,
                                                                                           peak.start=1980,
                                                                                           peak.end=1990,
                                                                                           post.peak=as.numeric(TRATE.KNOT.TIMES[1])),
                                                                              knot.values = list(pre.peak=1,
                                                                                                 peak.start=1,
                                                                                                 peak.end=1,
                                                                                                 post.peak=1),
                                                                              link = 'identity',
                                                                              knot.link = 'log'
                                                                           ),
                       functional.form.from.time = 1970,
                       functional.form.to.time = TRATE.KNOT.TIMES[1]
)

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
                 corresponding.data.outcome = 'new')

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
                       'testing',
                       outcome.metadata = create.outcome.metadata(display.name = 'HIV Testing Rate',
                                                                  description = "The average number of HIV tests per year",
                                                                  scale = 'rate',
                                                                  axis.name = 'Tests per Year',
                                                                  units = 'tests/yr',
                                                                  singular.unit = 'test/yr'),
                       value = 'testing',
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
                         name = 'population',
                         outcome.metadata = create.outcome.metadata(display.name = 'Population',
                                                                    description = "The Number of Infected and Uninfected Individuals in the Population",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Population',
                                                                    units = 'people',
                                                                    singular.unit = 'person'),
                         value.to.integrate = 'point.population',
                         keep.dimensions = c('location','age','race','sex','risk'),
                         save = T)

track.cumulative.proportion.from.rate(EHE.SPECIFICATION,
                                      name = 'proportion.tested',
                                      outcome.metadata = create.outcome.metadata(display.name = 'Proportion Tested',
                                                                                 description = "The Proportion of People who Received an HIV Test in the Past Year",
                                                                                 scale = 'proportion',
                                                                                 axis.name = 'Proportion Tested',
                                                                                 units = '%'),
                                      rate.value = 'testing',
                                      denominator.outcome = 'cumulative.uninfected',
                                      corresponding.data.outcome = 'proportion.tested',
                                      keep.dimensions = c('location','age','race','sex','risk'))

track.dynamic.outcome(EHE.SPECIFICATION,
                      name = 'hiv.mortality',
                      outcome.metadata = create.outcome.metadata(display.name = 'Mortality in PWH',
                                                                 description = "Number of People with HIV who Died of Any Cause in the Past Year",
                                                                 scale = 'non.negative.number',
                                                                 axis.name = 'Deaths',
                                                                 units = 'deaths',
                                                                 singular.unit = 'death'),
                      dynamic.quantity.name = 'mortality',
                      corresponding.data.outcome = 'hiv.mortality',
                      groups = 'infected',
                      tags = NULL,
                      keep.dimensions = c('location','sex'))

track.integrated.outcome(EHE.SPECIFICATION,
                         name = 'diagnosed.prevalence',
                         outcome.metadata = create.outcome.metadata(display.name = 'Prevalence (of Diagnosed PWH)',
                                                                    description = "The Number of People with HIV Aware of their Diagnosis",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'PrevalenT Cases',
                                                                    units = 'cases',
                                                                    singular.unit = 'case'),
                         value.to.integrate = 'infected',
                         corresponding.data.outcome = 'diagnosed.prevalence',
                         keep.dimensions = c('location','age','race','sex','risk')
                         )

##--------------------------------##
##--------------------------------##
##-- REGISTER THE SPECIFICATION --##
##--------------------------------##
##--------------------------------##

register.model.specification(EHE.SPECIFICATION)



