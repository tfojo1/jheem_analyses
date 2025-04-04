

source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/ryan_white/ryan_white_specification_helpers.R')
source('../jheem_analyses/applications/ryan_white/ryan_white_parameters.R')

source('../jheem_analyses/applications/ryan_white/process_rw_data_for_priors.R')
source('../jheem_analyses/applications/ryan_white/build_rw_priors.R')


RW.DATA.MANAGER = load.data.manager('../../cached/ryan.white.data.manager.rdata', set.as.default = F)

RW.SPECIFICATION = create.jheem.specification(version='rw',
                                              iteration = '1',
                                              description='Model to study the impacts of Ryan-White program on HIV transmission',
                                              parent.version = 'ehe')


## OVERVIEW
##
## This model presumes effects for 3 groups of PWH vis-a-vis Ryan White:
## (1) ADAP (AIDS Drug Assistance Program) recipients with/without any other Ryan White services
## (2) OAHS (Outpatient Ambulatory Health Services) recipients who do not receive any ADAP
## (3) Ryan White recipients who do not receive any ADAP or OAHS services (ie, supportive services from Ryan White)


##------------------------------------------##
##-- THE INPUT PROPORTIONS ON TYPES OF RW --##
##------------------------------------------##

register.model.element(RW.SPECIFICATION,
                       name = 'proportion.pwh.with.non.adap.rw',
                       scale = 'proportion',
                       functional.form = get.cached.object.for.version(name = "p.non.adap.functional.form", 
                                                                       version = 'rw'),
                       functional.form.from.time = 2010)


register.model.element(RW.SPECIFICATION,
                       name = 'proportion.non.adap.rw.with.oahs',
                       scale = 'proportion',
                       functional.form = get.cached.object.for.version(name = "p.oahs.functional.form", 
                                                                       version = 'rw'),
                       functional.form.from.time = 2010)

register.model.element(RW.SPECIFICATION,
                       name = 'proportion.non.adap.rw.with.adap',
                       scale = 'proportion',
                       functional.form = get.cached.object.for.version(name = "p.adap.functional.form", 
                                                                       version = 'rw'),
                       functional.form.from.time = 2010)

register.model.element(RW.SPECIFICATION,
                       name = 'proportion.adap.without.non.adap.rw',
                       scale = 'proportion',
                       value = 1/5)


##------------------------------------------------------##
##-- THE INPUT PROPORTIONS SUPPRESSED FOR TYPES OF RW --##
##------------------------------------------------------##

register.model.element(RW.SPECIFICATION,
                       name = 'proportion.adap.suppressed',
                       scale = 'proportion',
                       functional.form = get.cached.object.for.version(name = "p.suppression.oahs.functional.form", 
                                                                       version = 'rw'),
                       functional.form.from.time = 2010)

register.model.element(RW.SPECIFICATION,
                       name = 'proportion.oahs.without.adap.suppressed',
                       scale = 'proportion',
                       functional.form = get.cached.object.for.version(name = "p.suppression.oahs.functional.form", 
                                                                       version = 'rw'),
                       functional.form.from.time = 2010)

register.model.element(RW.SPECIFICATION,
                       name = 'proportion.rw.without.adap.or.oahs.suppressed',
                       scale = 'proportion',
                       functional.form = get.cached.object.for.version(name = "p.suppression.oahs.functional.form", 
                                                                       version = 'rw'),
                       functional.form.from.time = 2010)

##------------------------------------------------------------##
##-- THE OVERALL PROPORTIONS OF PEOPLE WITH EACH TYPE OF RW --##
##------------------------------------------------------------##

register.model.quantity(RW.SPECIFICATION,
                        name = 'proportion.pwh.with.adap',
                        value = expression(proportion.pwh.with.non.adap.rw * proportion.non.adap.rw.with.adap / (1-proportion.adap.without.non.adap.rw)) )

register.model.quantity(RW.SPECIFICATION,
                        name = 'proportion.pwh.with.oahs',
                        value = expression(proportion.pwh.with.non.adap.rw * proportion.non.adap.rw.with.oahs) )

register.model.quantity(RW.SPECIFICATION,
                        name = 'proportion.pwh.with.oahs.without.adap',
                        value = expression(proportion.pwh.with.oahs * (1-proportion.non.adap.rw.with.adap)) )

register.model.quantity(RW.SPECIFICATION,
                        name = 'proportion.pwh.with.rw.without.adap.or.oahs',
                        value = expression(proportion.pwh.with.non.adap.rw * (1-proportion.non.adap.rw.with.adap) * (1-proportion.non.adap.rw.with.oahs)))

register.model.quantity(RW.SPECIFICATION,
                        name = 'proportion.pwh.with.rw',
                        value = expression(proportion.pwh.with.adap + proportion.pwh.with.oahs.without.adap + proportion.pwh.with.rw.without.adap.or.oahs))


##-------------------------------------------------------------##
##-- THE OVERALL PROPORTIONS OF PWH SUPPRESSED BY TYPE OF RW --##
##-------------------------------------------------------------##

register.model.quantity(RW.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.with.adap',
                        value = expression(proportion.pwh.with.adap * proportion.adap.suppressed))

register.model.quantity(RW.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.with.oahs.without.adap',
                        value = expression(proportion.pwh.with.oahs.without.adap * proportion.oahs.without.adap.suppressed))

register.model.quantity(RW.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.with.rw.without.oahs.or.adap',
                        value = expression(proportion.pwh.with.rw.without.adap.or.oahs * proportion.rw.without.adap.or.oahs.suppressed))

register.model.quantity(RW.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.without.rw',
                        value = expression(super.suppression.of.diagnosed - proportion.pwh.who.are.suppressed.with.adap - proportion.pwh.who.are.suppressed.with.oahs.without.adap - proportion.pwh.who.are.suppressed.with.rw.without.oahs.or.adap) )

# needed for outcome
register.model.quantity(RW.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.with.oahs',
                        value = expression(proportion.pwh.with.oahs * (proportion.non.adap.rw.with.adap * proportion.adap.suppressed + (1-proportion.non.adap.rw.with.adap) * proportion.oahs.without.adap.suppressed)) )


##---------------------------------------##
##-- THE EFFECT ON OVERALL SUPPRESSION --##
##---------------------------------------##

register.model.element(RW.SPECIFICATION,
                       name = 'fraction.medicaid.expansion',
                       scale = 'proportion',
                       get.value.function = get.fraction.medicaid.expansion)

register.model.element(RW.SPECIFICATION,
                       name = 'adap.suppression.expansion.effect',
                       scale = 'proportion',
                       value = 1)

register.model.element(RW.SPECIFICATION,
                       name = 'oahs.suppression.expansion.effect',
                       scale = 'proportion',
                       value = 1)

register.model.element(RW.SPECIFICATION,
                       name = 'rw.support.suppression.expansion.effect',
                       scale = 'proportion',
                       value = 1)


register.model.element(RW.SPECIFICATION,
                       name = 'adap.suppression.nonexpansion.effect',
                       scale = 'proportion',
                       value = 1)

register.model.element(RW.SPECIFICATION,
                       name = 'oahs.suppression.nonexpansion.effect',
                       scale = 'proportion',
                       value = 1)

register.model.element(RW.SPECIFICATION,
                       name = 'rw.support.suppression.nonexpansion.effect',
                       scale = 'proportion',
                       value = 1)


register.model.quantity(RW.SPECIFICATION,
                       name = 'adap.suppression.effect',
                       scale = 'proportion',
                       value = expression(adap.suppression.expansion.effect * fraction.medicaid.expansion + adap.suppression.nonexpansion.effect * (1-fraction.medicaid.expansion)))

register.model.quantity(RW.SPECIFICATION,
                       name = 'oahs.suppression.effect',
                       scale = 'proportion',
                       value = expression(oahs.suppression.expansion.effect * fraction.medicaid.expansion + oahs.suppression.nonexpansion.effect * (1-fraction.medicaid.expansion)))

register.model.quantity(RW.SPECIFICATION,
                       name = 'rw.support.suppression.effect',
                       scale = 'proportion',
                       value = expression(rw.support.suppression.expansion.effect * fraction.medicaid.expansion + rw.support.suppression.nonexpansion.effect * (1-fraction.medicaid.expansion)))


register.model.quantity(RW.SPECIFICATION,
                        name= 'suppression.of.diagnosed',
                        value = expression(proportion.pwh.who.are.suppressed.without.rw +
                                             proportion.pwh.who.are.suppressed.with.adap * adap.suppression.effect +
                                             proportion.pwh.who.are.suppressed.with.oahs.without.adap * oahs.suppression.effect +
                                             proportion.pwh.who.are.suppressed.with.rw.without.oahs.or.adap * rw.support.suppression.effect)
)

##--------------##
##-- OUTCOMES --##
##--------------##


track.integrated.outcome(RW.SPECIFICATION,
                         name = 'rw.clients',
                         outcome.metadata = create.outcome.metadata(display.name = 'Ryan White Clients',
                                                                    description = "Number of Individuals Receiving any Ryan White Services",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Clients',
                                                                    units = 'people',
                                                                    singular.unit = 'person'),
                         value.to.integrate = 'infected',
                         multiply.by = 'proportion.pwh.with.rw',
                         subset.dimension.values = list(continuum='diagnosed.states'),
                         keep.dimensions = c('location','age','race','sex','risk'),
                         corresponding.data.outcome = NULL,
                         save = T)

track.integrated.outcome(RW.SPECIFICATION,
                         name = 'non.adap.clients',
                         outcome.metadata = create.outcome.metadata(display.name = 'Non-ADAP Clients',
                                                                    description = "Number of Individuals Receiving any non-ADAP Ryan White Services",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Clients',
                                                                    units = 'people',
                                                                    singular.unit = 'person'),
                         value.to.integrate = 'infected',
                         multiply.by = 'proportion.pwh.with.non.adap.rw',
                         subset.dimension.values = list(continuum='diagnosed.states'),
                         keep.dimensions = c('location','age','race','sex','risk'),
                         corresponding.data.outcome = 'non.adap.clients',
                         save = T)

track.integrated.outcome(RW.SPECIFICATION,
                         name = 'adap.clients',
                         outcome.metadata = create.outcome.metadata(display.name = 'ADAP Clients',
                                                                    description = "Number of Individuals Receiving AIDS Drug Assistance Program through Ryan White",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Clients',
                                                                    units = 'people',
                                                                    singular.unit = 'person'),
                         value.to.integrate = 'infected',
                         multiply.by = 'proportion.pwh.with.adap',
                         subset.dimension.values = list(continuum='diagnosed.states'),
                         keep.dimensions = c('location','age','race','sex','risk'),
                         corresponding.data.outcome = 'non.adap.clients',
                         save = T)

track.integrated.outcome(RW.SPECIFICATION,
                         name = 'oahs.clients',
                         outcome.metadata = create.outcome.metadata(display.name = 'OAHS Clients',
                                                                    description = "Number of Individuals Receiving any Outpatient Ambulatory Health Services through Ryan White",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Clients',
                                                                    units = 'people',
                                                                    singular.unit = 'person'),
                         value.to.integrate = 'infected',
                         multiply.by = 'proportion.pwh.with.oahs',
                         subset.dimension.values = list(continuum='diagnosed.states'),
                         keep.dimensions = c('location','age','race','sex','risk'),
                         corresponding.data.outcome = 'oahs.clients',
                         save = T)

track.integrated.outcome(RW.SPECIFICATION,
                         name = 'adap.proportion',
                         outcome.metadata = create.outcome.metadata(display.name = 'Proportion of Ryan White Clients on ADAP',
                                                                    description = "Proportion of Non-ADAP Ryan White Clients Receiving any AIDs Drug Assistance Program Services",
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion',
                                                                    units = '%',
                                                                    singular.unit = '%'),
                         value.to.integrate = 'infected',
                         multiply.by = 'proportion.pwh.with.adap',
                         denominator.outcome = 'non.adap.clients',
                         value.is.numerator = T,
                         subset.dimension.values = list(continuum='diagnosed.states'),
                         keep.dimensions = c('location','age','race','sex','risk'),
                         corresponding.data.outcome = 'adap.proportion',
                         save = T)

track.integrated.outcome(RW.SPECIFICATION,
                         name = 'adap.proportion.of.diagnosed',
                         outcome.metadata = create.outcome.metadata(display.name = 'Proportion of PWH on ADAP',
                                                                    description = "Proportion of People with Diagnosed HIV Receiving any AIDs Drug Assistance Program Services",
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion',
                                                                    units = '%',
                                                                    singular.unit = '%'),
                         value.to.integrate = 'infected',
                         multiply.by = 'proportion.pwh.with.adap',
                         denominator.outcome = 'diagnosed.prevalence',
                         value.is.numerator = T,
                         subset.dimension.values = list(continuum='diagnosed.states'),
                         keep.dimensions = c('location','age','race','sex'),
                         corresponding.data.outcome = 'adap.proportion.of.diagnosed',
                         save = T)

track.integrated.outcome(RW.SPECIFICATION,
                         name = 'oahs.suppression',
                         outcome.metadata = create.outcome.metadata(display.name = 'Suppression Among OAHS Clients',
                                                                    description = "Proportion of Ryan White Outpatient Ambulatory Health Services Clients who are Virally Suppressed",
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion',
                                                                    units = '%',
                                                                    singular.unit = '%'),
                         value.to.integrate = 'infected',
                         multiply.by = 'proportion.pwh.who.are.suppressed.with.oahs',
                         denominator.outcome = 'oahs.clients',
                         value.is.numerator = T,
                         subset.dimension.values = list(continuum='diagnosed.states'),
                         keep.dimensions = c('location','age','race','sex','risk'),
                         corresponding.data.outcome = 'oahs.suppression',
                         save = T)

track.integrated.outcome(RW.SPECIFICATION,
                         name = 'adap.suppression',
                         outcome.metadata = create.outcome.metadata(display.name = 'Suppression Among ADAP Clients',
                                                                    description = "Proportion of Ryan White AIDS Drug Assistance Program Clients who are Virally Suppressed",
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion',
                                                                    units = '%',
                                                                    singular.unit = '%'),
                         value.to.integrate = 'infected',
                         multiply.by = 'proportion.pwh.who.are.suppressed.with.adap',
                         denominator.outcome = 'adap.clients',
                         value.is.numerator = T,
                         subset.dimension.values = list(continuum='diagnosed.states'),
                         keep.dimensions = c('location','age','race','sex','risk'),
                         corresponding.data.outcome = 'adap.suppression',
                         save = T)

track.integrated.outcome(RW.SPECIFICATION,
                         name = 'adap.suppressed.proportion.of.diagnosed',
                         outcome.metadata = create.outcome.metadata(display.name = 'ADAP Receipt + Suppression Among PWH',
                                                                    description = "Proportion of People with Diagnosed HIV Receiving any AIDs Drug Assistance Program Services AND are Virally Suppressed",
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion',
                                                                    units = '%',
                                                                    singular.unit = '%'),
                         value.to.integrate = 'infected',
                         multiply.by = 'proportion.pwh.who.are.suppressed.with.adap',
                         denominator.outcome = 'diagnosed.prevalence',
                         value.is.numerator = T,
                         subset.dimension.values = list(continuum='diagnosed.states'),
                         keep.dimensions = c('location'),
                         corresponding.data.outcome = 'adap.suppressed.proportion.of.diagnosed',
                         save = T)

##-- ADD THE WEB SUB-VERSION --##

add.sub.version(RW.SPECIFICATION, 
                sub.version = 'w',
                description = "Web Tool version of Ryan White model",
                inherit.outcomes = F,
                can.seed.new.engine = T)

# General Outcomes
track.sub.version.outcomes(RW.SPECIFICATION,
                           sub.versions = 'w',
                           outcome.names = c(
                               'new',
                               'diagnosed.prevalence',
                               'incidence',
                               'prep.uptake',
                               'suppression','testing',
                               'awareness'))

#Ryan White Outcomes
track.sub.version.outcomes(RW.SPECIFICATION,
                           sub.versions = 'w',
                           outcome.names = c(
                               'rw.clients',
                               'non.adap.clients',
                               'adap.clients',
                               'oahs.clients',
                               'adap.proportion',
                               'oahs.suppression',
                               'adap.suppression'))

##-- REGISTER IT! --##

register.model.specification(RW.SPECIFICATION)
register.set.parameters.for.version('rw',
                                    parameter.names = RYAN.WHITE.PARAMETERS.PRIOR@var.names,
                                    apply.function = ryan.white.apply.set.parameters,
                                    join.with.previous.version = T)
