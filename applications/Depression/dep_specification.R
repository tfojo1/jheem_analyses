## Depression and HIV analyses

## source dependencies
source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/Depression/dep_parameters.R')
source('../jheem_analyses/applications/Depression/dep_parameter_mapping.R')


DEP.SPECIFICATION <- create.jheem.specification(version="dep", 
                                                parent.version = "ehe", 
                                                description = "Model for evaluating the impact of treating depression for HIV control",
                                                iteration = 1, 
                                                compartments.for.infected.and.uninfected = list("depression"=c("not_depressed", "depressed")))

##--------------------------------##
##--------------------------------##
##--   Depression Transitions   --##
##--------------------------------##
##--------------------------------##

register.transition(DEP.SPECIFICATION, dimension="depression", from.compartments="depressed", to.compartments="not_depressed", 
                    groups = c('infected','uninfected'),
                    value=expression(1/depression.length))

register.transition(DEP.SPECIFICATION, dimension="depression", from.compartments="not_depressed", to.compartments="depressed", 
                    groups = c('infected','uninfected'),
                    value=expression(depression.incidence)) 


##--------------------------------------##
##--------------------------------------##
##--        GENERAL QUANTITIES        --##
##--------------------------------------##
##--------------------------------------##

register.model.element(DEP.SPECIFICATION, name="depression.length", 
                  #     functional.form = create.static.functional.form(value=6/12, link = "log"), 
                      value = 6/12,
                       scale="time") # average length of depressive episode

register.model.element(DEP.SPECIFICATION, name="depression.proportion.tx", 
                       functional.form = create.static.functional.form(value=0.18, link = "logit", # adults 18-24
                                                                       value.is.on.transformed.scale = F), 
                       scale = "proportion")

depression.inc.betas <- array(c(.42/.5, .34/.5, .34/.5, .34/.5, .18/.5,
                                .42/.5, .34/.5, .34/.5, .34/.5, .18/.5,
                                .26/.5, .22/.5, .22/.5, .22/.5, .12/.5), dim=c(sex=3,age=5),  ## add gender + interaction term ##
                              dimnames = list(sex=c("female","msm","heterosexual_male"),
                                              age=c("13-24 years", "25-34 years","35-44 years", "45-54 years", "55+ years")))

register.model.element(DEP.SPECIFICATION, name="depression.incidence",
                       functional.form = create.static.functional.form(value = depression.inc.betas, link = "logit",
                                                                       value.is.on.transformed.scale = F), 
                       scale = "rate") ## incidence among general population

##------------------------##
##-- INITIAL POPULATION --##
##------------------------##

register.model.element(DEP.SPECIFICATION,
                       name = 'init.proportion.depressed',
                       value = 0.05,
                       scale = 'proportion')

register.model.quantity(DEP.SPECIFICATION,
                        name = 'initial.depression.distribution',
                        value = 0)
register.model.quantity.subset(DEP.SPECIFICATION,
                               name = 'initial.depression.distribution',
                               value = 'init.proportion.depressed',
                               applies.to = list(depression='depressed'))
register.model.quantity.subset(DEP.SPECIFICATION,
                               name = 'initial.depression.distribution',
                               value = expression(1-init.proportion.depressed),
                               applies.to = list(depression='not_depressed'))

register.model.quantity(DEP.SPECIFICATION,
                        'base.initial.population',
                        value = expression(super.base.initial.population * initial.depression.distribution))

##-- BIRTHS --##

register.natality(specification = DEP.SPECIFICATION,
                  parent.groups = 'uninfected',
                  child.groups = 'uninfected',
                  fertility.rate.value = 'fertility',
                  birth.proportions.value = 'birth.proportions',
                  parent.child.concordant.dimensions = 'race',
                  all.births.into.compartments = list(age=1, risk=1, depression='not_depressed'))

register.natality(specification = DEP.SPECIFICATION,
                  parent.groups = 'infected',
                  child.groups = 'uninfected',
                  fertility.rate.value = 'fertility',
                  birth.proportions.value = 'birth.proportions',
                  parent.child.concordant.dimensions = 'race',
                  all.births.into.compartments = list(age=1, risk=1, depression='not_depressed'))

register.natality(specification = DEP.SPECIFICATION,
                  parent.groups = 'uninfected',
                  child.groups = 'uninfected',
                  fertility.rate.value = 'immigration',
                  birth.proportions.value = 'null.proportions', # because we're actually fixing all the strata below 
                  parent.child.concordant.dimensions = c('age','race','sex','risk','depression'),
                  all.births.into.compartments = list(),
                  tag = "immigration")

register.natality(specification = DEP.SPECIFICATION,
                  parent.groups = 'infected',
                  child.groups = 'infected',
                  fertility.rate.value = 'immigration',
                  birth.proportions.value = 'null.proportions', # because we're actually fixing all the strata below 
                  parent.child.concordant.dimensions = c('age','race','sex','risk','depression','continuum','stage'),
                  all.births.into.compartments = list(),
                  tag = "immigration")

##--------------------------------------##
##--------------------------------------##
##---  Effects of depression on HIV  ---##
##--------------------------------------##
##--------------------------------------##

##--------------------##
##--  Suppression   --##
##--------------------##

register.model.quantity(DEP.SPECIFICATION,
                        name = 'suppression',
                        value = 0)

register.model.quantity(DEP.SPECIFICATION, name = "suppression.of.diagnosed", 
                        value = expression(super.suppression.of.diagnosed* depression.effect.on.suppression))

register.model.quantity(DEP.SPECIFICATION, name = "depression.effect.on.suppression", 
                        value = 1)

register.model.quantity.subset(DEP.SPECIFICATION, name = "depression.effect.on.suppression", 
                               applies.to = list(depression="depressed"), 
                               value = expression(rr.suppression.dep.tx* depression.proportion.tx + rr.suppression.dep.notx* (1-depression.proportion.tx)))

register.model.element(DEP.SPECIFICATION, name="rr.suppression.dep.tx",
                       value = 1, scale = "ratio") # rate ratio

register.model.element(DEP.SPECIFICATION, name="rr.suppression.dep.notx",
                       value = 1, scale = "ratio") # rate ratio


##--------------------##
##-- Susceptibility --##
##--------------------##

## Sexual behaviour ##

register.model.quantity(DEP.SPECIFICATION, name = "sexual.susceptibility", 
                        value = expression(super.sexual.susceptibility* depression.effect.on.sexual.susceptibility))

register.model.quantity(DEP.SPECIFICATION, name = "depression.effect.on.sexual.susceptibility", 
                       value = 1)

register.model.quantity.subset(DEP.SPECIFICATION, name = "depression.effect.on.sexual.susceptibility", 
                               applies.to = list(depression="depressed"), 
                               value = "rr.sex.sus.dep")

register.model.element(DEP.SPECIFICATION, name = "rr.sex.sus.dep.hetmale", 
                       value = .88, scale= "ratio") 
register.model.element(DEP.SPECIFICATION, name = "rr.sex.sus.dep.msm", 
                       value = .88, scale="ratio") 
register.model.element(DEP.SPECIFICATION, name = "rr.sex.sus.dep.female", 
                       value = .72, scale="ratio") 

register.model.quantity(DEP.SPECIFICATION, name = "rr.sex.sus.dep", 
                        value = "rr.sex.sus.dep.female")

register.model.quantity.subset(DEP.SPECIFICATION, name = "rr.sex.sus.dep", 
                               applies.to = list(sex="msm"), 
                               value = "rr.sex.sus.dep.msm")

register.model.quantity.subset(DEP.SPECIFICATION, name = "rr.sex.sus.dep", 
                               applies.to = list(sex="heterosexual_male"), 
                               value = "rr.sex.sus.dep.hetmale")

## IDU ##

register.model.quantity(DEP.SPECIFICATION, name = "idu.susceptibility", 
                        value = expression(super.idu.susceptibility * depression.effect.on.idu.susceptibility))

register.model.quantity(DEP.SPECIFICATION, name = "depression.effect.on.idu.susceptibility", 
                        value = 1)

register.model.quantity.subset(DEP.SPECIFICATION, name = "depression.effect.on.idu.susceptibility", 
                               applies.to = list(depression="depressed"), 
                               value = expression(rr.idu.dep.tx * depression.proportion.tx + rr.idu.dep.notx * (1-depression.proportion.tx)))

register.model.element(DEP.SPECIFICATION, name="rr.idu.dep.tx",
                       value = 0.71, scale = "ratio") # rate ratio

register.model.element(DEP.SPECIFICATION, name="rr.idu.dep.notx",
                       value = 1.67, scale = "ratio") # rate ratio


##--------------------##
##--    Testing     --##
##--------------------##

register.model.quantity(DEP.SPECIFICATION, name = "general.population.testing", 
                        value = expression(super.general.population.testing*depression.effect.on.testing))

register.model.quantity(DEP.SPECIFICATION, name = "depression.effect.on.testing", 
                        value = 1)

register.model.quantity.subset(DEP.SPECIFICATION, name = "depression.effect.on.testing", 
                               applies.to = list(depression="depressed"), 
                               value = "rr.testing.depressed")

register.model.element(DEP.SPECIFICATION, name="rr.testing.depressed",
                       value = 1.4, scale = "ratio") # rate ratio




##--------------##
##-- OUTCOMES --##
##--------------##

track.point.outcome(DEP.SPECIFICATION,
                    'point.population',
                    outcome.metadata = NULL,
                    scale = 'non.negative.number',
                    value = expression(uninfected+infected),
                    keep.dimensions = c('location','age','race','sex','risk','depression'),
                    save = F)



##--------------##
##-- FINALIZE --##
##--------------##

register.model.specification(DEP.SPECIFICATION)



##

register.calibrated.parameters.for.version('dep',
                                           distribution = DEP.PARAMETERS.PRIOR,
                                           apply.function = DEP.APPLY.PARAMETERS.FN,
                                           sampling.blocks = DEP.PARAMETER.SAMPLING.BLOCKS,
                                           calibrate.to.year = 2025,
                                           join.with.previous.version = T)

