
source('../jheem_analyses/applications/EHE/ehe_specification.R')


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
                       value = create.logistic.linear.functional.form(intercept = log(0.25)-log(0.75), slope = 0, anchor.year = 2020, parameters.are.on.logit.scale = TRUE),
                       scale = "proportion")



##-- REGISTER IT! --##

register.model.specification(CDCT.SPECIFICATION)
register.set.parameters.for.version('rw',
                                    parameter.names = CDC.TESTING.PARAMETERS.PRIOR@var.names,
                                    apply.function = cdc.testing.apply.set.parameters,
                                    join.with.previous.version = T)