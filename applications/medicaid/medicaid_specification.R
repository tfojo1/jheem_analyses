source('../jheem_analyses/applications/ehe/ehe_specification.R')
source('../jheem_analyses/applications/cdc_prep/cdc_prep_parameters.R') 
source('../jheem_analyses/applications/cdc_prep/odds_ratio_model.R') 


MEDICAID.SPECIFICATION = create.jheem.specification(version='medicaid',
                                                    iteration = '1',
                                                    description='Model to study the impacts of cuts to Medicaid',
                                                    parent.version = 'ehe')

##-----------------------------##
##-- INTERVENTION PARAMETERS --##
##-----------------------------##

# proportion.suppressed.pwh.who.lose.medicaid

register.model.element.values(MEDICAID.SPECIFICATION,
                              scale = 'proportion',
                              medicaid.affects.suppression.switch = 1,
                              medicaid.affects.testing.switch = 1,
                              medicaid.affects.prep.switch = 1
                              )

##---------------------##
##-- INPUT PARMETERS --##
##---------------------##

register.model.element(MEDICAID.SPECIFICATION,
                       name = 'p.pwh.on.medicaid',
                       scale = 'proportion',
                       value = 0.1)


##---------------------##
##-- MEDICAID LOSSES --##
##---------------------##

register.model.element(MEDICAID.SPECIFICATION,
                       name = 'logit.mean.medicaid.loss',
                       scale = 'proportion',
                       value = log(0.1)-log(0.9))

register.model.element(MEDICAID.SPECIFICATION,
                       name = 'logit.sd.medicaid.loss',
                       scale = 'rate',
                       value = 0.025)

register.model.element(MEDICAID.SPECIFICATION,
                       name = 'z.medicaid.loss',
                       scale = 'number',
                       value = 0.1)

register.model.quantity(MEDICAID.SPECIFICATION,
                        name = 'medicaid.loss',
                        scale = 'proportion',
                        value = expression(1 / (1 + exp(-(logit.mean.medicaid.loss + z.medicaid.loss * logit.sd.medicaid.loss)))))

##-----------------------##
##-- VIRAL SUPPRESSION --##
##-----------------------##

register.model.quantity(MEDICAID.SPECIFICATION,
                        name= 'suppression.of.diagnosed',
                        value = expression(suppression.never.medicaid * p.never.medicaid.of.pwh +
                                               suppression.keep.medicaid * p.keep.medicaid.of.pwh +
                                               suppression.lose.medicaid.with.rw * p.lose.medicaid.with.rw.of.pwh +
                                               suppression.lose.medicaid.without.rw * p.lose.medicaid.without.rw.of.pwh
                                           ))

##-------------##
##-- TESTING --##
##-------------##


##----------##
##-- PREP --##
##----------##


##--------------##
##-- OUTCOMES --##
##--------------##

# Number all people on Medicaid
# Number PWH on Medicaid

# P tested if on Medicaid
# P tested if uninsured

##-----------------##
##-- REGISTER IT --##
##-----------------##

register.model.specification()

