
source('../jheem_analyses/applications/ehe/ehe_specification.R')
source('../jheem_analyses/applications/cdc_testing/cdc_testing_parameters.R')
source('../jheem_analyses/applications/cdc_testing/odds_ratio_estimation.R')


CDCT.SPECIFICATION = create.jheem.specification(version='cdct',
                                              iteration = '1',
                                              description='Model to study the impacts of cuts to CDC-funded HIV tests',
                                              parent.version = 'ehe')


register.model.quantity(CDCT.SPECIFICATION, name = "testing.of.undiagnosed",
                        value = expression(cdc.funded.testing.of.undiagnosed*cdc.effect+cdc.nonfunded.testing.of.undiagnosed))

register.model.element(CDCT.SPECIFICATION, name = "cdc.effect",
                       value = 1,
                       scale = "proportion")

register.model.quantity(CDCT.SPECIFICATION, name = "cdc.funded.testing.of.undiagnosed",
                       value = expression(super.testing.of.undiagnosed*fraction.diagnoses.from.cdc))

register.model.quantity(CDCT.SPECIFICATION, name = "cdc.nonfunded.testing.of.undiagnosed",
                        value = expression(super.testing.of.undiagnosed*(1-fraction.diagnoses.from.cdc)))

register.model.element(CDCT.SPECIFICATION, name = "fraction.diagnoses.from.cdc",
                       functional.form = create.logistic.linear.functional.form(intercept =diagnoses.intercept , slope = diagnoses.slope, anchor.year = 2020, parameters.are.on.logit.scale = TRUE),
                       scale = "proportion",
                       functional.form.from.time = 2010)

register.model.element(CDCT.SPECIFICATION, name = "fraction.tests.from.cdc",
                       functional.form = create.logistic.linear.functional.form(intercept = log(0.25)-log(0.75), slope = 0, anchor.year = 2020, parameters.are.on.logit.scale = TRUE),
                       scale = "proportion",
                       functional.form.from.time = 2010)

register.model.quantity(CDCT.SPECIFICATION,
                        name = 'cdc.funded.general.population.testing',
                        scale = 'rate',
                        value = expression(general.population.testing*fraction.tests.from.cdc)
)

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
                         keep.dimensions = c('location'))

track.cumulative.outcome(CDCT.SPECIFICATION,
                         name = 'cdc.hiv.test.positivity',
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

##-- REGISTER IT! --##

register.model.specification(CDCT.SPECIFICATION)
register.set.parameters.for.version('cdct',
                                    parameter.names = CDC.TESTING.PARAMETERS.PRIOR@var.names,
                                    apply.function = cdc.testing.apply.set.parameters,
                                    join.with.previous.version = T)

