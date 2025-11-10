source('../jheem_analyses/applications/ehe/ehe_specification.R') 
# source('../jheem_analyses/applications/cdc_testing/cdc_testing_specification.R')
source('../jheem_analyses/applications/cdc_prep/cdc_prep_parameters.R') 
source('../jheem_analyses/applications/cdc_prep/odds_ratio_model.R') 


CDCP.SPECIFICATION = create.jheem.specification(version='cdcp',
                                                iteration = '1',
                                                description='Model to study the impacts of cuts to CDC Prevention Programs',
                                                parent.version = 'ehe')



##----------------------##
##----------------------##
##-- THE MAJOR INPUTS --##
##----------------------##
##----------------------##

#-- PrEP --#
register.model.element(CDCP.SPECIFICATION,
                       name = "fraction.prep.from.cdc",
                       functional.form = create.logistic.linear.functional.form(intercept = log(0.2)-log(0.2), slope = 0, anchor.year = 2020, parameters.are.on.logit.scale = TRUE),
                       scale = "proportion", # per person
                       functional.form.from.time = 2010)

register.model.element(CDCP.SPECIFICATION,
                       name = 'p.prep.uptake.from.referral',
                       scale = 'proportion',
                       value = 0.21)

#-- Testing --#
register.model.element(CDCP.SPECIFICATION, name = "proportion.tested.regardless",
                       value = 0.5,
                       scale = "proportion")

register.model.element(CDCP.SPECIFICATION, name = 'fraction.cdc.funded.tests.in.non.healthcare.settings',
                       value = 0.5,
                       scale = "proportion")

register.model.element(CDCP.SPECIFICATION, name = "fraction.diagnoses.from.cdc",
                       get.functional.form.function = get.fraction.diagnoses.from.cdc.model.spline,
                       scale = "proportion",
                       functional.form.from.time = 2010)


#-- Relating Testing to PrEP --#

register.model.element(CDCP.SPECIFICATION, name = "fraction.cdc.tests.unique",
                       value = 0.5, 
                       scale = "proportion")

register.model.element(CDCP.SPECIFICATION, name = "fraction.cdc.tested.prep.eligible",
                       get.functional.form.function = get.fraction.prep.eligible,
                       scale = "proportion",
                       functional.form.from.time = 2010) 

register.model.element(CDCP.SPECIFICATION, name = "fraction.cdc.referred.to.prep",
                       get.functional.form.function = get.fraction.prep.referred,
                       scale = "proportion",
                       functional.form.from.time = 2010)

#-- Contact Tracing --#

register.model.element(
    CDCP.SPECIFICATION,
    name = 'mean.fraction.index.diagnoses.that.yield.a.positive.contact',
    get.value.function = function(location){
        if (location=="AL")
            0.03
        else if (location=='LA')
            0.006
        else
            stop("We are only set up to accomodate LA or AL at this time")
    },
    scale = 'proportion'
)

register.model.element(
    CDCP.SPECIFICATION,
    name = 'rr.fraction.index.diagnoses.that.yield.a.positive.contact',
    value = 1,
    scale = 'ratio'
)

register.model.quantity(
    CDCP.SPECIFICATION,
    name = 'fraction.index.diagnoses.that.yield.a.positive.contact',
    value = expression(mean.fraction.index.diagnoses.that.yield.a.positive.contact * rr.fraction.index.diagnoses.that.yield.a.positive.contact),
    scale = 'proportion'
)

#-- CDC Effects --#

register.model.element(CDCP.SPECIFICATION, name = "cdc.prep.effect",
                       value = 1,
                       scale = "proportion")

register.model.element(CDCP.SPECIFICATION, name = "cdc.testing.effect",
                       value = 1,
                       scale = "proportion")

register.model.element(CDCP.SPECIFICATION, name = "cdc.contact.tracing.effect",
                       value = 1,
                       scale = "proportion")


##----------------------------------------##
##----------------------------------------##
##-- QUANTITIES DERIVED FROM THE INPUTS --##
##----------------------------------------##
##----------------------------------------##

##----------##
##-- PrEP --##
##----------##


register.model.quantity(CDCP.SPECIFICATION,
                        name = 'fraction.prep.not.from.cdc',
                        value = expression(1-fraction.prep.from.cdc))

register.model.quantity(CDCP.SPECIFICATION,
                        name = 'oral.prep.coverage',
                        value = expression(super.oral.prep.coverage * (fraction.prep.not.from.cdc + fraction.prep.from.cdc * cdc.prep.effect)) )

register.model.quantity(CDCP.SPECIFICATION,
                        name = 'lai.prep.coverage',
                        value = expression(super.lai.prep.coverage * (fraction.prep.not.from.cdc + fraction.prep.from.cdc * cdc.prep.effect)) )

##-------------##
##-- Testing --##
##-------------##

#-- The Diagnosis Rate --#

register.model.quantity(CDCP.SPECIFICATION, name = "cdc.funded.testing.of.undiagnosed",
                        value = expression(super.testing.of.undiagnosed * fraction.diagnoses.from.cdc * cdc.testing.effect),
                        scale = 'rate')

register.model.quantity(CDCP.SPECIFICATION, name = 'fraction.diagnoses.from.contact.tracing',
                        scale = 'proportion',
                        value = expression(fraction.index.diagnoses.that.yield.a.positive.contact / (1+fraction.index.diagnoses.that.yield.a.positive.contact)))

register.model.quantity(CDCP.SPECIFICATION, name = "cdc.nonfunded.testing.of.undiagnosed",
                        value = expression(super.testing.of.undiagnosed * 
                                               ( (1-fraction.diagnoses.from.cdc) +
                                                     fraction.diagnoses.from.cdc * (1-cdc.testing.effect) * proportion.tested.regardless) )
)

register.model.quantity(CDCP.SPECIFICATION, name = "testing.of.undiagnosed",
                        value = expression( (cdc.funded.testing.of.undiagnosed + cdc.nonfunded.testing.of.undiagnosed) *
                                                (1-fraction.diagnoses.from.contact.tracing + fraction.diagnoses.from.contact.tracing * cdc.contact.tracing.effect)) )

#-- Derive cdc-funded testing rate from PrEP --#

register.model.quantity(CDCP.SPECIFICATION, name = 'cdc.prep.uptake.rate',
                        scale = 'rate', # per person per year
                        value = expression(proportion.receiving.prep * fraction.prep.from.cdc))

register.model.quantity(CDCP.SPECIFICATION, name = "cdc.prep.referral.rate",
                        scale = 'rate', # per person per year
                        value = expression(cdc.prep.uptake.rate / p.prep.uptake.from.referral / fraction.cdc.tests.unique))

register.model.quantity(CDCP.SPECIFICATION, name = "cdc.prep.eligible.rate",
                        scale = 'rate', # per person per year
                        value = expression(cdc.prep.referral.rate / fraction.cdc.referred.to.prep))

register.model.quantity(CDCP.SPECIFICATION, name = "cdc.funded.non.healthcare.testing.rate",
                        scale = 'rate', # per person per year
                        value = expression(cdc.prep.eligible.rate / fraction.cdc.tested.prep.eligible))

register.model.quantity(CDCP.SPECIFICATION, name = 'cdc.funded.testing.rate',
                        scale = 'rate', # per person per year,
                        value = expression(cdc.funded.non.healthcare.testing.rate / fraction.cdc.funded.tests.in.non.healthcare.settings))


#-- The Testing Rate --#

register.model.quantity(CDCP.SPECIFICATION,
                        name = 'cdc.funded.general.population.testing',
                        scale = 'rate',
                        value = expression(cdc.funded.testing.rate * cdc.testing.effect)
)

register.model.quantity(CDCP.SPECIFICATION,
                        name = 'cdc.nonfunded.general.population.testing',
                        scale = 'rate',
                        value = expression(super.general.population.testing - cdc.funded.testing.rate +
                                               cdc.funded.testing.rate * (1-cdc.testing.effect) * proportion.tested.regardless)
)

register.model.quantity(CDCP.SPECIFICATION,
                        name = 'general.population.testing',
                        scale = 'rate',
                        value = expression(cdc.funded.general.population.testing + cdc.nonfunded.general.population.testing))


##--------------##
##-- OUTCOMES --##
##--------------##


track.point.outcome(CDCP.SPECIFICATION,
                         name = 'cdc.nonfunded.general.population.testing',
                         outcome.metadata = create.outcome.metadata(display.name = 'CDC Nonfunded General Testing Rate',
                                                               description = "CDC Nonfunded General Testing Rate",
                                                               scale = 'rate',
                                                               axis.name = 'tests/person-year',
                                                               units = 'tests/person-year',
                                                               singular.unit = 'test/person-year'),
                         denominator.outcome = 'point.population',
                         scale = 'rate',
                         value = 'cdc.nonfunded.general.population.testing',
                         keep.dimensions = c('age','race','sex','risk','location'),
                         save = T)


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
                         multiply.by = 'cdc.testing.effect',
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

track.integrated.outcome(CDCP.SPECIFICATION,
                         name = 'cumulative.fraction.cdc.funded.tests.in.non.healthcare.settings',
                         outcome.metadata = NULL,
                         value.to.integrate = 'fraction.cdc.funded.tests.in.non.healthcare.settings',
                         scale = 'proportion',
                         keep.dimensions = c('location'),
                         denominator.outcome = 'cdc.funded.tests',
                         save = F)


track.cumulative.outcome(CDCP.SPECIFICATION,
                         name = 'cdc.funded.tests.nonhealthcare',
                         outcome.metadata = create.outcome.metadata(display.name = 'Number of CDC-Funded HIV Tests in Non Health Care Settings',
                                                                    description = "Number of CDC-Funded cleaHIV Tests Done in the past year in non health care settings",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Tests',
                                                                    units = 'tests',
                                                                    singular.unit = 'test'),
                         scale = 'non.negative.number',
                         value = expression(cdc.funded.tests * cumulative.fraction.cdc.funded.tests.in.non.healthcare.settings),
                         keep.dimensions = c('location'),
                         corresponding.data.outcome = "non.healthcare.hiv.tests")

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
                         name = 'cumulative.cdc.prep.referrals',
                         outcome.metadata = NULL,
                         scale = 'non.negative.number',
                         value.to.integrate = 'uninfected',
                         multiply.by = 'cdc.prep.referral.rate', 
                         keep.dimensions = c('location'),
                         save = F)

track.integrated.outcome(CDCP.SPECIFICATION,
                         name = 'cumulative.cdc.prep.eligible',
                         outcome.metadata = create.outcome.metadata(display.name = 'Number of Individuals Eligible for PrEP',
                                                                    description = "Number of Individuals Eligible for PrEP",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'People',
                                                                    units = 'people',
                                                                    singular.unit = 'person'),
                         scale = 'non.negative.number',
                         value.to.integrate = 'uninfected',
                         multiply.by = 'cdc.prep.eligible.rate', 
                         keep.dimensions = c('location'),
                         corresponding.data.outcome = "number.eligible"
                         )

track.integrated.outcome(CDCP.SPECIFICATION,
                         name = 'cdc.prep.uptake',
                         outcome.metadata = create.outcome.metadata(display.name = 'PrEP Uptake from CDC',
                                                                    description = "Number of Individuals who Start PrEP After a CDC-funded Referral",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Uptake',
                                                                    units = 'people',
                                                                    singular.unit = 'person'),
                         value.to.integrate = 'uninfected',
                         multiply.by = 'cdc.prep.uptake.rate', 
                         keep.dimensions = c('location'),
                         save = T)

track.cumulative.outcome(CDCP.SPECIFICATION,
                         name = 'cdc.fraction.prep.referred.of.eligible',
                         outcome.metadata = create.outcome.metadata(display.name = 'Proportion Referred for PrEP',
                                                                    description = "Proportion Eligible Individuals Referred for PrEP After CDC-Funded HIV Testing",
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion Referred',
                                                                    units = '%',
                                                                    singular.unit = '%'),
                         value = 'cumulative.cdc.prep.referrals',
                         value.is.numerator = T,
                         denominator.outcome = 'cumulative.cdc.prep.eligible',
                         keep.dimensions = c('location'),
                         corresponding.data.outcome = "proportion.referred",
                         save = T)

track.cumulative.outcome(CDCP.SPECIFICATION,
                         name = 'cdc.fraction.prep.eligible.of.tested',
                         outcome.metadata = create.outcome.metadata(display.name = 'Proportion Eligible for PrEP',
                                                                    description = "Proportion Individuals Receiving CDC-Funded HIV Testing Who Are Eligible for PrEP",
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion Eligible',
                                                                    units = '%',
                                                                    singular.unit = '%'),
                         value = 'cumulative.cdc.prep.eligible',
                         value.is.numerator = T,
                         denominator.outcome = 'cdc.prep.uptake',
                         keep.dimensions = c('location'),
                         save = T)



##-- ADD THE WEB SUB-VERSION --##

# 
# add.sub.version(CDCP.SPECIFICATION, 
#                 sub.version = 'w',
#                 description = "Web Tool version of CDC Testing model",
#                 inherit.outcomes = F,
#                 can.seed.new.engine = F)
# 
# add.sub.version(CDCP.SPECIFICATION, 
#                 sub.version = 'ws',
#                 description = "Seed for Custom Scenarios for Web Tool version of CDC Testing model",
#                 inherit.outcomes = F,
#                 can.seed.new.engine = T)
# 
# 
# 
# # General Outcomes
# track.sub.version.outcomes(CDCP.SPECIFICATION,
#                            sub.versions = c('w','ws'),
#                            outcome.names = 'awareness',
#                            keep.dimensions = character())
# 
# track.sub.version.outcomes(CDCP.SPECIFICATION,
#                            sub.versions = c('w','ws'),
#                            outcome.names = c(
#                                'new',
#                                'diagnosed.prevalence',
#                                'incidence'))
# 
# track.sub.version.outcomes(CDCP.SPECIFICATION,
#                            sub.versions = c('w','ws'),
#                            outcome.names = c(
#                                'testing',
#                                'total.hiv.tests'),
#                            keep.dimensions = c('age','race','sex'))
# 
# #CDC Testing Outcomes
# track.sub.version.outcomes(CDCP.SPECIFICATION,
#                            sub.versions = c('w','ws'),
#                            outcome.names = c('cdc.funded.tests',
#                                              'total.cdc.hiv.test.positivity'),
#                            keep.dimensions = character())
# 
# #CDC PrEP 
# track.sub.version.outcomes(CDCP.SPECIFICATION,
#                            sub.versions = c('w','ws'),
#                            outcome.names = c('cdc.funded.tests',
#                                              'total.cdc.hiv.test.positivity','cumulative.fraction.referred','cumulative.fraction.eligible','cdc.funded.tests.nonhealthcare'),
#                            keep.dimensions = character())
# 
# 

##-- REGISTER IT! --##

register.model.specification(CDCP.SPECIFICATION)
register.set.parameters.for.version('cdcp',
                                    parameter.names = CDC.PREP.PARAMETERS.PRIOR@var.names,
                                    apply.function = cdc.prep.apply.set.parameters,
                                    join.with.previous.version = T)