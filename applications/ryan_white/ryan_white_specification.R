

source('applications/EHE/ehe_specification.R')

RW.SPECIFICATION = create.jheem.specification(version='rw',
                                              iteration = '1',
                                              description='Model to study the impacts of Ryan-White program on HIV transmission',
                                              parent.version = 'ehe')

register.model.element(RW.SPECIFICATION,
                       name = 'proportion.pwh.receiving.amb.rw',
                       scale = 'proportion',
                       value = 0.5)


register.model.element(RW.SPECIFICATION,
                       name = 'proportion.amb.rw.receiving.adap',
                       scale = 'proportion',
                       value = 0.5)


register.model.quantity(RW.SPECIFICATION,
                        name = 'proportion.pwh.receiving.no.amb.rw',
                        value = expression(1 - proportion.pwh.receiving.amb.rw))


register.model.quantity(RW.SPECIFICATION,
                        name = 'proportion.pwh.receiving.adap',
                        value = expression(proportion.amb.rw.receiving.adap*proportion.pwh.receiving.amb.rw))

register.model.quantity(RW.SPECIFICATION,
                        name = 'proportion.pwh.receiving.amb.rw.non.adap',
                        value = expression(proportion.pwh.receiving.amb.rw*(1-proportion.amb.rw.receiving.adap)))


register.model.element(RW.SPECIFICATION, 
                       name = 'proportion.amb.rw.non.adap.suppressed',
                       scale = 'proportion',
                       value = 0.9 )

register.model.quantity(RW.SPECIFICATION,
                        name = 'proportion.suppressed.receiving.amb.rw.non.adap',
                        value = expression(proportion.pwh.receiving.amb.rw.non.adap*proportion.amb.rw.non.adap.suppressed / super.suppression.of.diagnosed))


register.model.element(RW.SPECIFICATION,
                        name = 'proportion.adap.suppressed',
                        scale = 'proportion',
                       value = 0.9)

register.model.quantity(RW.SPECIFICATION,
                        name = 'proportion.suppressed.receiving.adap',
                        value = expression(proportion.pwh.receiving.adap*proportion.adap.suppressed / super.suppression.of.diagnosed))

register.model.quantity(RW.SPECIFICATION,
                        name = 'proportion.suppressed.receiving.no.amb.rw',
                        value = expression(1 - proportion.suppressed.receiving.amb.rw.non.adap - proportion.suppressed.receiving.adap))


register.model.element(RW.SPECIFICATION,
                       name = 'amb.rw.no.adap.effect',
                       scale = 'proportion',
                       value = 1)

register.model.element(RW.SPECIFICATION,
                       name = 'adap.effect',
                       scale = 'proportion',
                       value = 1)


register.model.quantity(RW.SPECIFICATION,
                        name= 'suppression.of.diagnosed',
                        value = expression(super.suppression.of.diagnosed * 
                                             (proportion.suppressed.receiving.amb.rw.non.adap*amb.rw.no.adap.effect+
                                                proportion.suppressed.receiving.adap*adap.effect +
                                                proportion.suppressed.receiving.no.amb.rw*1)
                        )
                        
)

register.model.specification(RW.SPECIFICATION)
