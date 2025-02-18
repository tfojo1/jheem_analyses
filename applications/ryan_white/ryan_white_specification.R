

source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/ryan_white/ryan_white_parameters.R')

RW.SPECIFICATION = create.jheem.specification(version='rw',
                                              iteration = '1',
                                              description='Model to study the impacts of Ryan-White program on HIV transmission',
                                              parent.version = 'ehe')


##------------------------------------------##
##-- THE INPUT PROPORTIONS ON TYPES OF RW --##
##------------------------------------------##

register.model.element(RW.SPECIFICATION,
                       name = 'proportion.pwh.with.non.adap.rw',
                       scale = 'proportion',
                       functional.form = create.logistic.linear.functional.form(
                         intercept = 1, # ie 0.5 on the logit scale
                         slope = 0, 
                         parameters.are.on.logit.scale = T,
                         anchor.year = 2020
                       ),
                       functional.form.from.time = 2010)


register.model.element(RW.SPECIFICATION,
                       name = 'proportion.non.adap.rw.with.oahs',
                       scale = 'proportion',
                       functional.form = create.logistic.linear.functional.form(
                         intercept = log(0.6)-log(0.4),
                         slope = 0, 
                         parameters.are.on.logit.scale = T,
                         anchor.year = 2020
                       ),
                       functional.form.from.time = 2010)

register.model.element(RW.SPECIFICATION,
                       name = 'proportion.non.adap.rw.with.adap',
                       scale = 'proportion',
                       functional.form = create.logistic.linear.functional.form(
                         intercept = log(1/3)-log(2/3),
                         slope = 0,
                         parameters.are.on.logit.scale = T,
                         anchor.year = 2020
                       ),
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
                       functional.form = create.logistic.linear.functional.form(
                         intercept = log(0.8)-log(0.2),
                         slope = 0, 
                         parameters.are.on.logit.scale = T,
                         anchor.year = 2020
                       ),
                       functional.form.from.time = 2010)

register.model.element(RW.SPECIFICATION,
                       name = 'proportion.oahs.without.adap.suppressed',
                       scale = 'proportion',
                       functional.form = create.logistic.linear.functional.form(
                         intercept = log(0.9)-log(0.1),
                         slope = 0, 
                         parameters.are.on.logit.scale = T,
                         anchor.year = 2020
                       ),
                       functional.form.from.time = 2010)

register.model.element(RW.SPECIFICATION,
                       name = 'proportion.rw.without.adap.or.oahs.suppressed',
                       scale = 'proportion',
                       functional.form = create.logistic.linear.functional.form(
                         intercept = log(0.4)-log(0.6),
                         slope = 0, 
                         parameters.are.on.logit.scale = T,
                         anchor.year = 2020
                       ),
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

# register.model.quantity(RW.SPECIFICATION,
#                         name = 'proportion.pwh.with.rw',
#                         value = expression(proportion.pwh.with.adap + proportion.pwh.with.oahs.without.adap + proportion.pwh.with.rw.without.adap.or.oahs))


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
                       name = 'adap.suppression.effect',
                       scale = 'proportion',
                       value = 1)

register.model.element(RW.SPECIFICATION,
                       name = 'oahs.suppression.effect',
                       scale = 'proportion',
                       value = 1)

register.model.element(RW.SPECIFICATION,
                       name = 'rw.support.suppression.effect',
                       scale = 'proportion',
                       value = 1)


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
                         name = 'non.adap.clients',
                         outcome.metadata = create.outcome.metadata(display.name = 'Ryan White Clients',
                                                                    description = "Number of Individuals Receiving any Ryan White Services",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Cases',
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
                         outcome.metadata = create.outcome.metadata(display.name = 'Ryan White Clients',
                                                                    description = "Number of Individuals Receiving any Ryan White Services",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Cases',
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
                         outcome.metadata = create.outcome.metadata(display.name = 'Ryan White Clients',
                                                                    description = "Number of Individuals Receiving any Ryan White Services",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Cases',
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
                         name = 'oahs.suppression',
                         outcome.metadata = create.outcome.metadata(display.name = 'Proportion of Ryan White OAHS Clients Suppressed',
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
                         corresponding.data.outcome = 'non.adap.viral.suppression',
                         save = T)

track.integrated.outcome(RW.SPECIFICATION,
                         name = 'adap.suppression',
                         outcome.metadata = create.outcome.metadata(display.name = 'Proportion of ADAP Suppressed',
                                                                    description = "Proportion of Ryan White ADAP Clients who are Virally Suppressed",
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion',
                                                                    units = '%',
                                                                    singular.unit = '%'),
                         value.to.integrate = 'infected',
                         multiply.by = 'proportion.adap.suppressed',
                         denominator.outcome = 'adap.clients',
                         value.is.numerator = T,
                         subset.dimension.values = list(continuum='diagnosed.states'),
                         keep.dimensions = c('location','age','race','sex','risk'),
                         corresponding.data.outcome = 'non.adap.viral.suppression',
                         save = T)

register.model.specification(RW.SPECIFICATION)
register.set.parameters.for.version('rw',
                                    parameter.names = RYAN.WHITE.PARAMETERS.PRIOR@var.names,
                                    apply.function = ryan.white.apply.set.parameters,
                                    join.with.previous.version = T)
