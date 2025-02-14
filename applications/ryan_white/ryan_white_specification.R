

source('../jheem_analyses/applications/EHE/ehe_specification.R')

RW.SPECIFICATION = create.jheem.specification(version='rw',
                                              iteration = '1',
                                              description='Model to study the impacts of Ryan-White program on HIV transmission',
                                              parent.version = 'ehe')

register.model.element(RW.SPECIFICATION,
                       name = 'proportion.pwh.with.rw',
                       scale = 'proportion',
                       value = 0.5)


register.model.element(RW.SPECIFICATION,
                       name = 'proportion.rw.with.adap',
                       scale = 'proportion',
                       value = 0.5)


register.model.quantity(RW.SPECIFICATION,
                        name = 'proportion.pwh.without.rw',
                        value = expression(1 - proportion.pwh.with.rw))


register.model.quantity(RW.SPECIFICATION,
                        name = 'proportion.pwh.with.rw.and.adap',
                        value = expression(proportion.pwh.with.rw * proportion.rw.with.adap))

register.model.quantity(RW.SPECIFICATION,
                        name = 'proportion.pwh.with.rw.without.adap',
                        value = expression(proportion.pwh.with.rw * (1-proportion.rw.with.adap)))

register.model.element(RW.SPECIFICATION, 
                       name = 'proportion.rw.suppressed',
                       scale = 'proportion',
                       value = 0.9 )

register.model.quantity(RW.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.with.rw.without.adap',
                        value = expression(proportion.pwh.with.rw.without.adap * proportion.rw.suppressed))

register.model.quantity(RW.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.with.rw.and.adap',
                        value = expression(proportion.pwh.with.rw.and.adap * proportion.rw.suppressed))

register.model.quantity(RW.SPECIFICATION,
                        name = 'proportion.pwh.who.are.suppressed.without.rw',
                        value = expression(super.suppression.of.diagnosed - proportion.pwh.who.are.suppressed.with.rw.without.adap - proportion.pwh.who.are.suppressed.with.rw.and.adap))


register.model.element(RW.SPECIFICATION,
                       name = 'rw.without.adap.suppression.effect',
                       scale = 'proportion',
                       value = 1)

register.model.element(RW.SPECIFICATION,
                       name = 'adap.suppression.effect',
                       scale = 'proportion',
                       value = 1)


register.model.quantity(RW.SPECIFICATION,
                        name= 'suppression.of.diagnosed',
                        value = expression(proportion.pwh.who.are.suppressed.without.rw +
                                             proportion.pwh.who.are.suppressed.with.rw.without.adap * rw.without.adap.suppression.effect +
                                             proportion.pwh.who.are.suppressed.with.rw.and.adap * adap.suppression.effect)
)

track.integrated.outcome(RW.SPECIFICATION,
                         name = 'ryan.white.clients',
                         outcome.metadata = create.outcome.metadata(display.name = 'Ryan White Clients',
                                                                    description = "Number of Individuals Receiving any Ryan White Services",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Cases',
                                                                    units = 'people',
                                                                    singular.unit = 'person'),
                         value.to.integrate = 'infected',
                         multiply.by = 'proportion.pwh.with.rw',
                         subset.dimension.values = list(continuum='diagnosed.states'),
                         keep.dimensions = c('location','age','race','sex','risk'),
                         corresponding.data.outcome = 'non.adap.clients',
                         save = T)

register.model.specification(RW.SPECIFICATION)
