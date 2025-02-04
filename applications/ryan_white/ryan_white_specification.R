

source('applications/EHE/ehe_specification.R')

RW.SPECIFICATION = create.jheem.specification(version='rw',
                                              iteration = '1',
                                              description='Model to study the impacts of Ryan-White program on HIV transmission',
                                              parent.version = 'ehe')


register.model.element(RW.SPECIFICATION,
                       name = 'proportion.pwh.receiving.adap',
                       scale = 'proportion',
                       value = 0.2)

register.model.element(RW.SPECIFICATION,
                       name = 'proportion.receiving.adap.who.are.suppressed',
                       scale = 'proportion',
                       value = 0.9)

register.model.element(RW.SPECIFICATION,
                       name = 'ryan.white.adap.effect',
                       scale = 'proportion',
                       value = 1)

register.model.quantity(RW.SPECIFICATION,
                       name = 'proportion.of.suppressed.on.adap',
                       scale = 'proportion',
                       value = expression(proportion.pwh.receiving.adap * proportion.receiving.adap.who.are.suppressed / super.suppression.of.diagnosed))

register.model.quantity(RW.SPECIFICATION,
                        name = 'proportion.of.suppressed.not.on.adap',
                        scale = 'proportion',
                        value = expression(1-proportion.of.suppressed.on.adap))

register.model.quantity(RW.SPECIFICATION,
                        name= 'suppression.of.diagnosed',
                        value = expression(super.suppression.of.diagnosed * 
                                             (proportion.of.suppressed.not.on.adap * 1 +
                                                proportion.of.suppressed.on.adap * ryan.white.adap.effect)
                        )
                        
)

register.model.specification(RW.SPECIFICATION)
