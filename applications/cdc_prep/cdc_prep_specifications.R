source('../jheem_analyses/applications/ehe/ehe_specification.R')
source('../jheem_analyses/applications/cdc_testing/cdc_prep_parameters.R') #to create 


CDCP.SPECIFICATION = create.jheem.specification(version='cdcp',
                                                iteration = '1',
                                                description='Model to study the impacts of cuts to CDC-funded PrEP referrals in non health-care settings',
                                                parent.version = 'ehe')


#-- Basic Building Blocks --#

register.model.element(CDCP.SPECIFICATION, name = "cdc.prep.effect", #proportionate scaling of cdc effect
                       value = 1,
                       scale = "proportion")

register.model.element(CDCP.SPECIFICATION, name = "prep", #proportion of population on prep (set?)
                       value = 1, #change 
                       scale = "proportion")

register.model.element(CDCP.SPECIFICATION, name = "prep.efficiacy", #efficacy of prep (set?)
                       value = 1, #change 
                       scale = "proportion")


register.model.quantity(CDCP.SPECIFICATION, name = "susceptibility", 
                        value = expression((prep*proportion.prep.cdc*cdc.prep.effect + prep*(1-proportion.prep.cdc))*(1-prep.efficacy) + (1-prep))) #necessary?

register.model.quantity(CDCP.SPECIFICATION, name = "prep",
                        value = expression(proportion.prep.cdc + proportion.prep.non.cdc)) #define proportion.prep.non.cdc? 

register.model.quantity(CDCT.SPECIFICATION, name = "testing.of.undiagnosed",
                        value = expression(cdc.funded.testing.of.undiagnosed + cdc.nonfunded.testing.of.undiagnosed),
                        scale = "rate") #all should be rates? 

register.model.quantity(CDCT.SPECIFICATION, name = "cdc.funded.testing.of.undiagnosed",
                        value = expression(super.testing.of.undiagnosed * fraction.diagnoses.from.cdc * cdc.prep.effect), 
                        scale = "rate") # should this effect be applied here?

register.model.quantity(CDCT.SPECIFICATION, name = "cdc.nonfunded.testing.of.undiagnosed",
                        value = expression(super.testing.of.undiagnosed * 
                                                (1-fraction.diagnoses.from.cdc) ),
                        scale = "rate")  #all should be rates? 


#-- The logistic functional forms for key proportions --#


register.model.element(CDCP.SPECIFICATION, name = "proportion.prep.cdc",
                       get.functional.form.function = get.proportion.prep.cdc.from.cdc.model,
                       scale = "proportion",
                       functional.form.from.time = 2010)

register.model.element(CDCP.SPECIFICATION, name = "fraction.unique",
                       get.functional.form.function = get.fraction.unique.from.cdc.model,
                       scale = "proportion",
                       functional.form.from.time = 2010)

register.model.element(CDCP.SPECIFICATION, name = "fraction.eligible",
                       get.functional.form.function = create.logistic.linear.functional.form(intercept = log(0.88)-log(0.88), slope = 0, anchor.year = 2021, parameters.are.on.logit.scale = TRUE),
                       scale = "proportion",
                       functional.form.from.time = 2010)

register.model.element(CDCP.SPECIFICATION, name = "fraction.reffered",
                       get.functional.form.function = create.logistic.linear.functional.form(intercept = log(0.35)-log(0.35), slope = 0, anchor.year = 2021, parameters.are.on.logit.scale = TRUE),
                       scale = "proportion",
                       functional.form.from.time = 2010)

register.model.element(CDCT.SPECIFICATION, name = "fraction.diagnoses.from.cdc",
                       get.functional.form.function = get.fraction.diagnoses.from.cdc.model,
                       scale = "proportion",
                       functional.form.from.time = 2010)


#-- OUTCOMES --#

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
                         value.to.integrate = 'proportion.prep.cdc',
                         multiply.by = expression(proportion.prep.cdc*1/fraction.unique* 1/fraction.eligible * 1/fraction.reffered), #correct way to add? 
                         keep.dimensions = c('location'),
                         save = F)

track.integrated.outcome(CDCP.SPECIFICATION,
                         name = 'cumulative.fraction.diagnoses.from.cdc',
                         outcome.metadata = NULL,
                         scale = 'proportion',
                         value.to.integrate = 'fraction.diagnoses.from.cdc',
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
                         value = expression(cdc.funded.diagnoses + cdc.funded.tests.in.uninfected.non.healthcare/fraction.cdc.non.healthcare),#define fraction.cdc.non.healthcare? 
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