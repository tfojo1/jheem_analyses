
source('../jheem_analyses/applications/ehe/ehe_specification.R')
source('../jheem_analyses/applications/cdc_testing/cdc_testing_parameters.R')
source('../jheem_analyses/applications/cdc_testing/odds_ratio_estimation.R')


CDCT.SPECIFICATION = create.jheem.specification(version='cdct',
                                              iteration = '1',
                                              description='Model to study the impacts of cuts to CDC-funded HIV tests',
                                              parent.version = 'ehe')


#-- Basic Building Blocks --#
register.model.element(CDCT.SPECIFICATION, name = "cdc.effect",
                       value = 1,
                       scale = "proportion")

register.model.element(CDCT.SPECIFICATION, name = "proportion.tested.regardless",
                       value = 0.5,
                       scale = "proportion")


#-- The Diagnosis Rate --#
register.model.quantity(CDCT.SPECIFICATION, name = "testing.of.undiagnosed",
                        value = expression(cdc.funded.testing.of.undiagnosed + cdc.nonfunded.testing.of.undiagnosed))

register.model.quantity(CDCT.SPECIFICATION, name = "cdc.funded.testing.of.undiagnosed",
                       value = expression(super.testing.of.undiagnosed * fraction.diagnoses.from.cdc * cdc.effect),
                       scale = 'rate')

register.model.quantity(CDCT.SPECIFICATION, name = "cdc.nonfunded.testing.of.undiagnosed",
                        value = expression(super.testing.of.undiagnosed * 
                                               ( (1-fraction.diagnoses.from.cdc) +
                                                     fraction.diagnoses.from.cdc * (1-cdc.effect) * proportion.tested.regardless) )
)

#-- The logistic functional forms for key proportions --#

register.model.element(CDCT.SPECIFICATION, name = "fraction.diagnoses.from.cdc",
                       get.functional.form.function = get.fraction.diagnoses.from.cdc.model.spline,
                       scale = "proportion",
                       functional.form.from.time = 2010)

register.model.element(CDCT.SPECIFICATION, name = "fraction.tests.from.cdc",
                       functional.form = create.logistic.linear.functional.form(intercept = log(0.092800629)-log(0.9071994), slope = 0, anchor.year = 2020, parameters.are.on.logit.scale = TRUE),
                       scale = "proportion",
                       functional.form.from.time = 2010)

#-- The Testing Rate --#

register.model.quantity(CDCT.SPECIFICATION,
                        name = 'cdc.funded.general.population.testing',
                        scale = 'rate',
                        value = expression(super.general.population.testing * fraction.tests.from.cdc * cdc.effect)
)

register.model.quantity(CDCT.SPECIFICATION,
                        name = 'cdc.nonfunded.general.population.testing',
                        scale = 'rate',
                        value = expression(super.general.population.testing * 
                                               ( (1-fraction.tests.from.cdc) +
                                                  fraction.tests.from.cdc * (1-cdc.effect) * proportion.tested.regardless ) )
)

register.model.quantity(CDCT.SPECIFICATION,
                        name = 'general.population.testing',
                        scale = 'rate',
                        value = expression(cdc.funded.general.population.testing + cdc.nonfunded.general.population.testing))

#-- OUTCOMES --#

track.integrated.outcome(CDCT.SPECIFICATION,
                         name = 'cdc.funded.tests.in.uninfected',
                         outcome.metadata = NULL,
                         scale = 'non.negative.number',
                         value.to.integrate = 'uninfected',
                         multiply.by = 'cdc.funded.general.population.testing',
                         keep.dimensions = c('location'),
                         save = F)

track.integrated.outcome(CDCT.SPECIFICATION,
                         name = 'cumulative.fraction.diagnoses.from.cdc',
                         outcome.metadata = NULL,
                         scale = 'proportion',
                         value.to.integrate = 'fraction.diagnoses.from.cdc',
                         multiply.by = 'cdc.effect',
                         denominator.outcome = 'new',
                         keep.dimensions = c('location','age','race','sex','risk'),
                         save = F)

track.cumulative.outcome(CDCT.SPECIFICATION,
                         name = 'cdc.funded.diagnoses',
                         outcome.metadata = NULL,
                         scale = 'non.negative.number',
                         value = expression(new * cumulative.fraction.diagnoses.from.cdc),
                         keep.dimensions = c('location'),
                         save = F)

track.cumulative.outcome(CDCT.SPECIFICATION,
                         name = 'cdc.funded.tests',
                         outcome.metadata = create.outcome.metadata(display.name = 'Number of CDC-Funded HIV Tests',
                                                                    description = "Number of CDC-Funded cleaHIV Tests Done in the Past Year",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Tests',
                                                                    units = 'tests',
                                                                    singular.unit = 'test'),
                         scale = 'non.negative.number',
                         value = expression(cdc.funded.diagnoses + cdc.funded.tests.in.uninfected),
                         keep.dimensions = c('location'),
                         corresponding.data.outcome = "hiv.tests")

track.cumulative.outcome(CDCT.SPECIFICATION,
                         name = 'total.cdc.hiv.test.positivity',
                         corresponding.data.outcome = 'cdc.hiv.test.positivity',
                         outcome.metadata = create.outcome.metadata(display.name = 'Proportion of CDC-funded HIV Tests that are Positive',
                                                                    description = "Proportion of CDC-funded HIV Tests that are Positive",
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion positive',
                                                                    units = '%',
                                                                    singular.unit = '%'),
                         value = 'cdc.funded.diagnoses',
                         value.is.numerator = T,
                         denominator.outcome = 'cdc.funded.tests',
                         keep.dimensions = 'location')



##-- ADD THE WEB SUB-VERSION --##

add.sub.version(CDCT.SPECIFICATION, 
                sub.version = 'w',
                description = "Web Tool version of CDC Testing model",
                inherit.outcomes = F,
                can.seed.new.engine = F)

add.sub.version(CDCT.SPECIFICATION, 
                sub.version = 'ws',
                description = "Seed for Custom Scenarios for Web Tool version of CDC Testing model",
                inherit.outcomes = F,
                can.seed.new.engine = T)


# General Outcomes
track.sub.version.outcomes(CDCT.SPECIFICATION,
                           sub.versions = c('w','ws'),
                           outcome.names = 'awareness',
                           keep.dimensions = character())

track.sub.version.outcomes(CDCT.SPECIFICATION,
                           sub.versions = c('w','ws'),
                           outcome.names = c(
                               'new',
                               'diagnosed.prevalence',
                               'incidence'))

track.sub.version.outcomes(CDCT.SPECIFICATION,
                           sub.versions = c('w','ws'),
                           outcome.names = c(
                               'testing',
                               'total.hiv.tests'),
                           keep.dimensions = c('age','race','sex'))

#CDC Testing Outcomes
track.sub.version.outcomes(CDCT.SPECIFICATION,
                           sub.versions = c('w','ws'),
                           outcome.names = c('cdc.funded.tests',
                                             'total.cdc.hiv.test.positivity'),
                           keep.dimensions = character())



##-- REGISTER IT! --##

register.model.specification(CDCT.SPECIFICATION)
register.set.parameters.for.version('cdct',
                                    parameter.names = CDC.TESTING.PARAMETERS.PRIOR@var.names,
                                    apply.function = cdc.testing.apply.set.parameters,
                                    join.with.previous.version = T)

