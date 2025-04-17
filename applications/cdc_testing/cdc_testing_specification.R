
source('../jheem_analyses/applications/EHE/ehe_specification.R')


CDCT.SPECIFICATION = create.jheem.specification(version='cdct',
                                              iteration = '1',
                                              description='Model to study the impacts of cuts to CDC-funded HIV tests',
                                              parent.version = 'ehe')


##-- REGISTER IT! --##

register.model.specification(CDCT.SPECIFICATION)
register.set.parameters.for.version('rw',
                                    parameter.names = CDC.TESTING.PARAMETERS.PRIOR@var.names,
                                    apply.function = cdc.testing.apply.set.parameters,
                                    join.with.previous.version = T)