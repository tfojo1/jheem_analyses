# Tech2Check specification. Inherits from EHE and adds a 4-state intervention
# lifecycle within the diagnosed HIV compartment: diagnosed_chronic is retained
# as "never intervened"; three new compartments (on_intervention,
# recently_intervened, distantly_intervened) track the program lifecycle.
# Model design: see design.md in the tech2check working repo.

source('../jheem_analyses/applications/EHE/ehe_specification.R')


##-------------------##
##-- SPECIFICATION --##
##-------------------##

TECH2CHECK.SPECIFICATION = create.jheem.specification(
    version = 'tech2check',
    iteration = '1',
    description = 'Model to study the population-level impact of scaling up the Tech2Check intervention on viral suppression in youth with HIV.',
    parent.version = 'ehe',
    compartments.for.infected.only = list(
        continuum = c('undiagnosed_acute',
                      'undiagnosed_from_prep_acute',
                      'undiagnosed_chronic',
                      'diagnosed_chronic',       # never intervened
                      'on_intervention',
                      'recently_intervened',
                      'distantly_intervened')    # absorbing
    ),
    compartment.value.aliases = list(
        diagnosed.states = c('diagnosed_chronic', 'on_intervention', 'recently_intervened', 'distantly_intervened'),
        chronic.stages   = c('undiagnosed_chronic', 'diagnosed_chronic', 'on_intervention', 'recently_intervened', 'distantly_intervened')
    )
)


##-----------------##
##-- TRANSITIONS --##
##-----------------##

#-- Never intervened -> On intervention (recruitment; primary policy lever) --#
register.model.element(TECH2CHECK.SPECIFICATION,
                       name = 'tech2check.recruitment.rate',
                       scale = 'rate',
                       value = 0)

register.transition(TECH2CHECK.SPECIFICATION,
                    dimension = 'continuum',
                    groups = 'infected',
                    from.compartments = 'diagnosed_chronic',
                    to.compartments = 'on_intervention',
                    value = 'tech2check.recruitment.rate')


#-- On intervention -> Recently intervened (program completion) --#
# 1 / (6 months) = 2 per year
register.model.element(TECH2CHECK.SPECIFICATION,
                       name = 'tech2check.completion.rate',
                       scale = 'rate',
                       value = 2)

register.transition(TECH2CHECK.SPECIFICATION,
                    dimension = 'continuum',
                    groups = 'infected',
                    from.compartments = 'on_intervention',
                    to.compartments = 'recently_intervened',
                    value = 'tech2check.completion.rate')


#-- On intervention -> Distantly intervened (dropout during active phase) --#
# Trial: ~15% non-completion over 6 months; r = -ln(0.85) / 0.5yr
register.model.element(TECH2CHECK.SPECIFICATION,
                       name = 'tech2check.dropout.rate',
                       scale = 'rate',
                       value = 0.325)

register.transition(TECH2CHECK.SPECIFICATION,
                    dimension = 'continuum',
                    groups = 'infected',
                    from.compartments = 'on_intervention',
                    to.compartments = 'distantly_intervened',
                    value = 'tech2check.dropout.rate')


#-- Recently intervened -> Distantly intervened (waning; key sensitivity lever) --#
register.model.element(TECH2CHECK.SPECIFICATION,
                       name = 'tech2check.waning.rate',
                       scale = 'rate',
                       value = 1)

register.transition(TECH2CHECK.SPECIFICATION,
                    dimension = 'continuum',
                    groups = 'infected',
                    from.compartments = 'recently_intervened',
                    to.compartments = 'distantly_intervened',
                    value = 'tech2check.waning.rate')


##-----------------##
##-- SUPPRESSION --##
##-----------------##
#
# Apply the trial OR to the inherited baseline suppression proportion on the
# odds scale: p' = OR * p / (1 - p + OR * p), where p = suppression.of.diagnosed.
# Three independent ORs so the design.md sensitivity scenarios (reduced OR in
# Recently; residual OR in Distantly) are parameterizable without restructuring.
# diagnosed_chronic (never intervened) inherits the EHE baseline unchanged.

register.model.element(TECH2CHECK.SPECIFICATION,
                       name = 'tech2check.on.suppression.OR',
                       scale = 'ratio',
                       value = 2.0)

register.model.element(TECH2CHECK.SPECIFICATION,
                       name = 'tech2check.recently.suppression.OR',
                       scale = 'ratio',
                       value = 2.0)

register.model.element(TECH2CHECK.SPECIFICATION,
                       name = 'tech2check.distantly.suppression.OR',
                       scale = 'ratio',
                       value = 1.0)


register.model.quantity(TECH2CHECK.SPECIFICATION,
                        name = 'suppression',
                        value = 0)

register.model.quantity.subset(TECH2CHECK.SPECIFICATION,
                               name = 'suppression',
                               value = 'suppression.of.diagnosed',
                               applies.to = list(continuum = 'diagnosed_chronic'))


register.model.quantity(TECH2CHECK.SPECIFICATION,
                        name = 'suppression.on.intervention',
                        value = expression(tech2check.on.suppression.OR * suppression.of.diagnosed /
                                           (1 - suppression.of.diagnosed + tech2check.on.suppression.OR * suppression.of.diagnosed)),
                        scale = 'proportion')

register.model.quantity(TECH2CHECK.SPECIFICATION,
                        name = 'suppression.recently.intervened',
                        value = expression(tech2check.recently.suppression.OR * suppression.of.diagnosed /
                                           (1 - suppression.of.diagnosed + tech2check.recently.suppression.OR * suppression.of.diagnosed)),
                        scale = 'proportion')

register.model.quantity(TECH2CHECK.SPECIFICATION,
                        name = 'suppression.distantly.intervened',
                        value = expression(tech2check.distantly.suppression.OR * suppression.of.diagnosed /
                                           (1 - suppression.of.diagnosed + tech2check.distantly.suppression.OR * suppression.of.diagnosed)),
                        scale = 'proportion')


register.model.quantity.subset(TECH2CHECK.SPECIFICATION,
                               name = 'suppression',
                               value = 'suppression.on.intervention',
                               applies.to = list(continuum = 'on_intervention'))

register.model.quantity.subset(TECH2CHECK.SPECIFICATION,
                               name = 'suppression',
                               value = 'suppression.recently.intervened',
                               applies.to = list(continuum = 'recently_intervened'))

register.model.quantity.subset(TECH2CHECK.SPECIFICATION,
                               name = 'suppression',
                               value = 'suppression.distantly.intervened',
                               applies.to = list(continuum = 'distantly_intervened'))


##--------------##
##-- REGISTER --##
##--------------##

register.model.specification(TECH2CHECK.SPECIFICATION)
