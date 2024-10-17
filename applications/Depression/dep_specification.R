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
                  parent.child.concordant.dimensions = c('age','race','sex','risk','depression','continuum'),
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

track.point.outcome(DEP.SPECIFICATION,
                    'point.population.inf',
                    outcome.metadata = NULL,
                    scale = 'non.negative.number',
                    value = expression(infected),
                    keep.dimensions = c('location','age','race','sex','risk','depression'),
                    save = F)

track.point.outcome(DEP.SPECIFICATION,
                    'point.population.noninf', ## HIV-ve population
                    outcome.metadata = NULL,
                    scale = 'non.negative.number',
                    value = expression(uninfected), 
                    keep.dimensions = c('location','age','race','sex','risk','depression'),
                    save = F)

track.cumulative.outcome(DEP.SPECIFICATION,
                         name = 'proportion.depressed', ## proportion depressed, HIV-ve and +ve
                         outcome.metadata = create.outcome.metadata(display.name = 'Proportion Depressed',
                                                                    description = "The Proportion of People who were depressed in the Past Year",
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion depressed',
                                                                    units = '%',
                                                                    singular.unit = '%'),
                         value = 'n.depressed',
                         value.is.numerator = T,
                         denominator.outcome = 'population',
                         keep.dimensions = c('location','age','race','sex'),
                         save = T)

track.cumulative.outcome(DEP.SPECIFICATION, ## number of depressed individuals in the population
                         name = 'n.depressed',
                         value = 'population',
                         outcome.metadata = NULL,
                         value.is.numerator = T,
                         denominator.outcome = NULL,
                         keep.dimensions = c('location','age','race','sex'),
                         save = F, 
                         scale = "non.negative.number", 
                         subset.dimension.values = list(depression='depressed'))

track.integrated.outcome(DEP.SPECIFICATION, ## total Population numbers
                         name = 'population',
                         outcome.metadata = create.outcome.metadata(display.name = 'Population',
                                                                    description = "The Number of Infected and Uninfected Individuals in the Population",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Population',
                                                                    units = 'people',
                                                                    singular.unit = 'person'),
                         value.to.integrate = 'point.population',
                         keep.dimensions = c('location','age','race','sex','risk','depression'),
                         corresponding.data.outcome = 'adult.population',
                         save = T)


track.integrated.outcome(DEP.SPECIFICATION, ## HIV-ve individuals only
                         name = 'population_noHIV',
                         outcome.metadata = create.outcome.metadata(display.name = 'Population',
                                                                    description = "The Number of Uninfected Individuals in the Population",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Population',
                                                                    units = 'people',
                                                                    singular.unit = 'person'),
                         value.to.integrate = 'uninfected',
                         keep.dimensions = c('location','age','race','sex','risk','depression'),
                         corresponding.data.outcome = 'adult.population',
                         save = T)

track.integrated.outcome(DEP.SPECIFICATION, ## HIV+ve individuals only
                         name = 'population.HIV',
                         outcome.metadata = create.outcome.metadata(display.name = 'Population',
                                                                    description = "The Number of Uninfected Individuals in the Population",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Population',
                                                                    units = 'people',
                                                                    singular.unit = 'person'),
                         value.to.integrate = 'infected',
                         keep.dimensions = c('location','age','race','sex','risk','depression'),
                         corresponding.data.outcome = 'adult.population',
                         save = T)

track.integrated.outcome(DEP.SPECIFICATION, ## HIV-ve individuals with depression
                         name = 'cumulative.uninfected.depressed',
                         outcome.metadata = NULL,
                         value.to.integrate = 'uninfected',
                         keep.dimensions = c('location','age','race','sex','risk'),
                         scale = 'non.negative.number',
                         save = F,
                         subset.dimension.values = list(depression='depressed'))

track.integrated.outcome(DEP.SPECIFICATION, ## HIV+ve individuals with depression
                         name = 'cumulative.infected.depressed',
                         outcome.metadata = NULL,
                         value.to.integrate = 'infected',
                         keep.dimensions = c('location','age','race','sex','risk'),
                         scale = 'non.negative.number',
                         save = F,
                         subset.dimension.values = list(depression='depressed'))

## -- Prevalence ratio HIV depressed vs. General Population
track.cumulative.outcome(DEP.SPECIFICATION,
                         name = 'prevRatio',
                         outcome.metadata = create.outcome.metadata(display.name = 'Prevalence Ratio of Depression',
                                                                    description = "Prevalence Ratio of Depression, PwH vs. General Population",
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion aware',
                                                                    units = '%',
                                                                    singular.unit = '%'),
                         value = expression(cumulative.infected.depressed/cumulative.uninfected.depressed), 
                         denominator.outcome = 'cumulative.uninfected.depressed',
                         keep.dimensions = c("location"), ## may add additional dimensions
                         save = T)


## -- Proportion of individuals with Depression on Tx
track.integrated.outcome(DEP.SPECIFICATION,
                         name = 'hiv.depression.treat', ## HIV+ on depression Tx
                         outcome.metadata = create.outcome.metadata(display.name = 'Prevalence of PwH with Depression on Treatment',
                                                                    description = "The Number of People with HIV and Depression on Treatment",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Prevalent Cases',
                                                                    units = 'cases',
                                                                    singular.unit = 'case'),
                         value.to.integrate = 'infected',
                         multiply.by = 'depression.proportion.tx',
                         keep.dimensions = c('location','age','race','sex','risk'),
                         save = T)

track.cumulative.outcome(DEP.SPECIFICATION,
                         name = 'proportion.HIV.depressed.treated', 
                         outcome.metadata = create.outcome.metadata(display.name = 'Proportion Depressed Treated',
                                                                    description = "The Proportion of PwH with Depression on Treatment",
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion Treated',
                                                                    units = '%'),
                         value = expression(hiv.depression.treat/cumulative.infected.depressed),
                         denominator.outcome = 'cumulative.infected.depressed',
                         keep.dimensions = c("location"), 
                         save = T)

track.cumulative.outcome(DEP.SPECIFICATION,
                         name = 'cumulative.uninfected.over.18',
                         value = expression(cumulative.uninfected * fraction.population.over.18),
                         scale = 'non.negative.number',
                         keep.dimensions = c('location','age','race','sex','risk','depression'),
                         rename.dimension.values = list(age=c('13-24 years'='18-24 years')),
                         save = F,
                         outcome.metadata = NULL)

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

