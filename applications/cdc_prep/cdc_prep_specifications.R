source('../jheem_analyses/applications/ehe/ehe_specification.R')
source('../jheem_analyses/applications/cdc_testing/cdc_prep_parameters.R') #to create 
source('../jheem_analyses/applications/cdc_testing/odds_ratio_model.R') 


CDCP.SPECIFICATION = create.jheem.specification(version='cdcp',
                                                iteration = '1',
                                                description='Model to study the impacts of cuts to CDC Prevention Programs',
                                                parent.version = 'ehe')


#CDC Effect 

register.model.element(CDCP.SPECIFICATION, name = "cdc.effect",
                       value = 1,
                       scale = "proportion")



##----------##
##-- PrEP --##
##----------##

register.model.element(CDCP.SPECIFICATION,
                       name = "fraction.prep.from.cdc",
                       functional.form = create.logistic.linear.functional.form(intercept = log(0.2)-log(0.2), slope = 0, anchor.year = 2020, parameters.are.on.logit.scale = TRUE),
                       scale = "proportion",
                       functional.form.from.time = 2010)

register.model.quantity(CDCP.SPECIFICATION,
                        name = 'fraction.prep.not.from.cdc',
                        value = expression(1-fraction.prep.from.cdc))

register.model.quantity(CDCP.SPECIFICATION,
                        name = 'oral.prep.coverage',
                        value = expression(super.oral.prep.coverage * (fraction.prep.not.from.cdc + fraction.prep.from.cdc * cdc.effect)) )

register.model.quantity(CDCP.SPECIFICATION,
                        name = 'lai.prep.coverage',
                        value = expression(super.lai.prep.coverage * (fraction.prep.not.from.cdc + fraction.prep.from.cdc * cdc.effect)) )

##-------------##
##-- Testing --##
##-------------##

register.model.element(CDCP.SPECIFICATION, name = "proportion.tested.regardless",
                       value = 0.5,
                       scale = "proportion")


#-- The Diagnosis Rate --#
register.model.quantity(CDCP.SPECIFICATION, name = "testing.of.undiagnosed",
                        value = expression(cdc.funded.testing.of.undiagnosed + cdc.nonfunded.testing.of.undiagnosed))

register.model.quantity(CDCP.SPECIFICATION, name = "cdc.funded.testing.of.undiagnosed",
                        value = expression(super.testing.of.undiagnosed * fraction.diagnoses.from.cdc * cdc.effect),
                        scale = 'rate')

register.model.quantity(CDCP.SPECIFICATION, name = "cdc.nonfunded.testing.of.undiagnosed",
                        value = expression(super.testing.of.undiagnosed * 
                                               ( (1-fraction.diagnoses.from.cdc) +
                                                     fraction.diagnoses.from.cdc * (1-cdc.effect) * proportion.tested.regardless) )
)





#-- The logistic functional forms for key proportions --#


register.model.element(CDCP.SPECIFICATION, name = "fraction.diagnoses.from.cdc",
                       get.functional.form.function = get.fraction.diagnoses.from.cdc.model.spline,
                       scale = "proportion",
                       functional.form.from.time = 2010)

register.model.element(CDCP.SPECIFICATION, name = "fraction.tests.from.cdc",
                       functional.form = create.logistic.linear.functional.form(intercept = log(0.092800629)-log(0.9071994), slope = 0, anchor.year = 2020, parameters.are.on.logit.scale = TRUE),
                       scale = "proportion",
                       functional.form.from.time = 2010)


register.model.element(CDCP.SPECIFICATION, name = "fraction.unique",
                       get.functional.form.function = get.fraction.unique.from.cdc.model, #to specify 
                       scale = "proportion",
                       functional.form.from.time = 2010)

register.model.element(CDCP.SPECIFICATION, name = "fraction.eligible",
                       get.functional.form.function = get.fraction.prep.eligible,
                       scale = "proportion",
                       functional.form.from.time = 2010) 

register.model.element(CDCP.SPECIFICATION, name = "fraction.referred",
                       get.functional.form.function = get.fraction.prep.referred,
                       scale = "proportion",
                       functional.form.from.time = 2010)


#-- The Testing Rate --#

register.model.quantity(CDCP.SPECIFICATION,
                        name = 'cdc.funded.general.population.testing',
                        scale = 'rate',
                        value = expression(super.general.population.testing * fraction.tests.from.cdc * cdc.effect)
)

register.model.quantity(CDCP.SPECIFICATION,
                        name = 'cdc.nonfunded.general.population.testing',
                        scale = 'rate',
                        value = expression(super.general.population.testing * 
                                               ( (1-fraction.tests.from.cdc) +
                                                     fraction.tests.from.cdc * (1-cdc.effect) * proportion.tested.regardless ) )
)

register.model.quantity(CDCP.SPECIFICATION,
                        name = 'general.population.testing',
                        scale = 'rate',
                        value = expression(cdc.funded.general.population.testing + cdc.nonfunded.general.population.testing))



#-- OUTCOMES --#


track.integrated.outcome(CDCP.SPECIFICATION,
                         name = 'cdc.funded.tests.in.uninfected',
                         outcome.metadata = NULL,
                         scale = 'non.negative.number',
                         value.to.integrate = 'uninfected',
                         multiply.by = 'cdc.funded.general.population.testing',
                         keep.dimensions = c('location'),
                         save = F)

track.integrated.outcome(CDCP.SPECIFICATION,
                         name = 'cumulative.fraction.diagnoses.from.cdc',
                         outcome.metadata = NULL,
                         scale = 'proportion',
                         value.to.integrate = 'fraction.diagnoses.from.cdc',
                         multiply.by = 'cdc.effect',
                         denominator.outcome = 'new',
                         keep.dimensions = c('location','age','race','sex','risk'),
                         save = F)

track.cumulative.outcome(CDCP.SPECIFICATION,
                         name = 'cdc.funded.diagnoses',
                         outcome.metadata = NULL,
                         scale = 'non.negative.number',
                         value = expression(new * cumulative.fraction.diagnoses.from.cdc),
                         keep.dimensions = c('location'),
                         save = F)

track.cumulative.outcome(CDCP.SPECIFICATION,
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

track.cumulative.outcome(CDCP.SPECIFICATION,
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


track.integrated.outcome(CDCP.SPECIFICATION,
                         name = 'Prep',
                         outcome.metadata = NULL,
                         scale = 'non.negative.number',
                         value.to.integrate = 'prep', #unsure if prep is already tracked in the model 
                         keep.dimensions = c('location'),
                         save = F)

track.integrated.outcome(CDCP.SPECIFICATION,
                         name = 'cdc.funded.tests.in.uninfected.non.healthcare',
                         outcome.metadata = NULL,
                         scale = 'non.negative.number',
                         value.to.integrate = 'population',
                         multiply.by = expression(Prep*proportion.prep.cdc*1/fraction.unique* 1/fraction.eligible * 1/fraction.referred), #correct way to add? 
                         keep.dimensions = c('location'),
                         save = F)


##-- ADD THE WEB SUB-VERSION --##


add.sub.version(CDCP.SPECIFICATION, 
                sub.version = 'w',
                description = "Web Tool version of CDC Testing model",
                inherit.outcomes = F,
                can.seed.new.engine = F)

add.sub.version(CDCP.SPECIFICATION, 
                sub.version = 'ws',
                description = "Seed for Custom Scenarios for Web Tool version of CDC Testing model",
                inherit.outcomes = F,
                can.seed.new.engine = T)



# General Outcomes
track.sub.version.outcomes(CDCP.SPECIFICATION,
                           sub.versions = c('w','ws'),
                           outcome.names = 'awareness',
                           keep.dimensions = character())

track.sub.version.outcomes(CDCP.SPECIFICATION,
                           sub.versions = c('w','ws'),
                           outcome.names = c(
                               'new',
                               'diagnosed.prevalence',
                               'incidence'))

track.sub.version.outcomes(CDCP.SPECIFICATION,
                           sub.versions = c('w','ws'),
                           outcome.names = c(
                               'testing',
                               'total.hiv.tests'),
                           keep.dimensions = c('age','race','sex'))

#CDC Testing Outcomes
track.sub.version.outcomes(CDCP.SPECIFICATION,
                           sub.versions = c('w','ws'),
                           outcome.names = c('cdc.funded.tests',
                                             'total.cdc.hiv.test.positivity'),
                           keep.dimensions = character())



##-- REGISTER IT! --##

register.model.specification(CDCP.SPECIFICATION)
register.set.parameters.for.version('cdcp',
                                    parameter.names = CDC.PREP.PARAMETERS.PRIOR@var.names,
                                    apply.function = cdc.prep.apply.set.parameters,
                                    join.with.previous.version = T)