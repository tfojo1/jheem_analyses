## Depression and HIV analyses

## source dependencies
source('../jheem_analyses/applications/EHE/ehe_specification.R')

DEP.SPECIFICATION <- create.jheem.specification(version="dep", 
                                                parent.version = "ehe", 
                                                description = "Model for evaluating the impact of treating depression for HIV control",
                                                iteration = 1, 
                                                compartments.for.infected.and.uninfected = list("depression"=c("not_depressed", "depressed")))


#depression transitions
# 1. add compartments
# 2. add movement between
# 3. add effect of depression on HIV transitions

# effect on testing
# effect on susceptibility
# effect on transmissibility

register.transition(DEP.SPECIFICATION, dimension="dep", from.compartments="depressed", to.compartments="not_depressed", value=expression(1/depression.length))
register.model.element(DEP.SPECIFICATION, name="depression.length", functional.form = create.static.functional.form(value=6/12, link = "log"), scale="time") # average length of dep epi
# add other model transitions

register.model.element(DEP.SPECIFICATION, name="depression.proportion.tx", functional.form = create.static.functional.form(value=.23, link = "logit", value.is.on.transformed.scale = F), scale = "proportion")


register.model.quantity(DEP.SPECIFICATION,
                        name = 'suppression',
                        value = 0)

register.model.quantity.subset(DEP.SPECIFICATION,
                               name = 'suppression',
                               value = 'suppression.of.diagnosed',
                               applies.to = list(continuum='diagnosed', depression="not_depressed"))

register.model.quantity.subset(DEP.SPECIFICATION,
                               name = 'suppression',
                               value = 'suppression.of.diagnosed.depressed',
                               applies.to = list(continuum='diagnosed', depression="depressed"))

register.model.quantity(DEP.SPECIFICATION, name="suppression.of.diagnosed.depressed",
                        value=expression(suppression.of.diagnosed*(depression.proportion.tx*rr.suppression.dep.tx+(1-depression.proportion.tx)*rr.suppression.dep.notx)))

#register.model.element(DEP.SPECIFICATION, name="suppression.of.diagnosed", ...)
#register.model.element(DEP.SPECIFICATION, name="rr.suppression.dep.tx", ...)
#register.model.element(DEP.SPECIFICATION, name="rr.suppression.dep.notx", ...)

#register.model.specification(DEP.SPECIFICATION)


