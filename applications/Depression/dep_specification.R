## Depression and HIV analyses

## source dependencies
source('../jheem_analyses/applications/EHE/ehe_specification.R')

DEP.SPECIFICATION <- create.jheem.specification(version="dep", 
                                                parent.version = "ehe", 
                                                description = "Model for evaluating the impact of treating depression for HIV control",
                                                iteration = 1, 
                                                compartments.for.infected.and.uninfected = list("depression"=c("not_depressed", "depressed")))

##--------------------------------##
##--------------------------------##
##--   Depression Transitions   --##
##--------------------------------##
##--------------------------------##

register.transition(DEP.SPECIFICATION, dimension="depression", from.compartments="depressed", to.compartments="not_depressed", 
                    groups = c('infected','uninfected'),
                    value=expression(1/depression.length))

register.transition(DEP.SPECIFICATION, dimension="depression", from.compartments="not_depressed", to.compartments="depressed", 
                    groups = c('infected','uninfected'),
                    value=expression(depression.prevalence)) 


##--------------------------------------##
##--------------------------------------##
##--        GENERAL QUANTITIES        --##
##--------------------------------------##
##--------------------------------------##

register.model.element(DEP.SPECIFICATION, name="depression.length", 
                       functional.form = create.static.functional.form(value=6/12, link = "log"), 
                       scale="time") # average length of depressive episode

register.model.element(DEP.SPECIFICATION, name="depression.proportion.tx", 
                       functional.form = create.static.functional.form(value=0.18, link = "logit", 
                                                                       value.is.on.transformed.scale = F), 
                       scale = "proportion")

register.model.element(DEP.SPECIFICATION, name="depression.prevalence", ## CHANGE THIS TO INC RATE ##
                       functional.form = create.static.functional.form(value = 0.091, link = "logit",
                                                                       value.is.on.transformed.scale = F), 
                       scale = "proportion") # prevalence of depression in the general population ## can be stratified by age and race ## CHANGE THIS TO INC RATE ##


##--------------------------------------##
##--------------------------------------##
##---  Effects of depression on HIV  ---##
##--------------------------------------##
##--------------------------------------##

##--------------------##
##--  Suppression   --##
##--------------------##

register.model.quantity(DEP.SPECIFICATION,
                        name = 'suppression',
                        value = 0)

register.model.quantity(DEP.SPECIFICATION, name = "suppression.of.diagnosed", 
                        value = expression(super.suppression.of.diagnosed* depression.effect.on.suppression))

register.model.quantity(DEP.SPECIFICATION, name = "depression.effect.on.suppression", 
                        value = 1)

register.model.quantity.subset(DEP.SPECIFICATION, name = "depression.effect.on.suppression", 
                               applies.to = list(depression="depressed"), 
                               value = expression(rr.suppression.dep.tx* depression.proportion.tx + rr.suppression.dep.notx* (1-depression.proportion.tx)))

register.model.element(DEP.SPECIFICATION, name="rr.suppression.dep.tx",
                       value = 1, scale = "ratio") # rate ratio

register.model.element(DEP.SPECIFICATION, name="rr.suppression.dep.notx",
                       value = 1, scale = "ratio") # rate ratio


##--------------------##
##-- Susceptibility --##
##--------------------##

## Sexual behaviour ##

register.model.quantity(DEP.SPECIFICATION, name = "sexual.susceptibility", 
                        value = expression(super.sexual.susceptibility* depression.effect.on.sexual.susceptibility))

register.model.quantity(DEP.SPECIFICATION, name = "depression.effect.on.sexual.susceptibility", 
                       value = 1)

register.model.quantity.subset(DEP.SPECIFICATION, name = "depression.effect.on.sexual.susceptibility", 
                               applies.to = list(depression="depressed"), 
                               value = "rr.sex.sus.dep")

register.model.element(DEP.SPECIFICATION, name = "rr.sex.sus.dep.hetmale", 
                       value = .88, scale= "ratio") 
register.model.element(DEP.SPECIFICATION, name = "rr.sex.sus.dep.msm", 
                       value = .88, scale="ratio") 
register.model.element(DEP.SPECIFICATION, name = "rr.sex.sus.dep.female", 
                       value = .72, scale="ratio") 

register.model.quantity(DEP.SPECIFICATION, name = "rr.sex.sus.dep", 
                        value = "rr.sex.sus.dep.female")

register.model.quantity.subset(DEP.SPECIFICATION, name = "rr.sex.sus.dep", 
                               applies.to = list(sex="msm"), 
                               value = "rr.sex.sus.dep.msm")

register.model.quantity.subset(DEP.SPECIFICATION, name = "rr.sex.sus.dep", 
                               applies.to = list(sex="heterosexual_male"), 
                               value = "rr.sex.sus.dep.hetmale")

## IDU ##

register.model.quantity(DEP.SPECIFICATION, name = "idu.susceptibility", 
                        value = expression(super.idu.susceptibility * depression.effect.on.idu.susceptibility))

register.model.quantity(DEP.SPECIFICATION, name = "depression.effect.on.idu.susceptibility", 
                        value = 1)

register.model.quantity.subset(DEP.SPECIFICATION, name = "depression.effect.on.idu.susceptibility", 
                               applies.to = list(depression="depressed"), 
                               value = expression(rr.idu.dep.tx * depression.proportion.tx + rr.idu.dep.notx * (1-depression.proportion.tx)))

register.model.element(DEP.SPECIFICATION, name="rr.idu.dep.tx",
                       value = 0.71, scale = "ratio") # rate ratio

register.model.element(DEP.SPECIFICATION, name="rr.idu.dep.notx",
                       value = 1.67, scale = "ratio") # rate ratio


##--------------------##
##--    Testing     --##
##--------------------##

register.model.quantity(DEP.SPECIFICATION, name = "general.population.testing", 
                        value = expression(super.general.population.testing*depression.effect.on.testing))

register.model.quantity(DEP.SPECIFICATION, name = "depression.effect.on.testing", 
                        value = 1)

register.model.quantity.subset(DEP.SPECIFICATION, name = "depression.effect.on.testing", 
                               applies.to = list(depression="depressed"), 
                               value = "rr.testing.depressed")

register.model.element(DEP.SPECIFICATION, name="rr.testing.depressed",
                       value = 1.4, scale = "ratio") # rate ratio


register.model.specification(DEP.SPECIFICATION)

