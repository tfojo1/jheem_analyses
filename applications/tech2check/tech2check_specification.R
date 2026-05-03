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
# odds scale: p' = OR * p / (1 - p + OR * p), where p = super.suppression.of.diagnosed.
# Three independent ORs so the design.md sensitivity scenarios (reduced OR in
# Recently; residual OR in Distantly) are parameterizable without restructuring.
# diagnosed_chronic (never intervened) inherits the EHE baseline unchanged via
# the dispatcher's neutral default (OR = 1).
#
# Subtle: we redefine `suppression.of.diagnosed` (not `suppression`) because the
# inherited `suppression` tracked outcome reads `multiply.by = 'suppression.of.diagnosed'`,
# and the framework rejects re-registering an inherited outcome. Anchoring the
# OR effect on `suppression.of.diagnosed` lets the inherited outcome dispatch
# per-compartment automatically; EHE's mortality/transmissibility consume
# `suppression` (the dispatcher), which inherits the redefinition transparently.
# See jheem_spec_notes.md ("Model quantities vs. tracked outcomes") for full context.

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


# Per-compartment OR dispatcher: default 1 (neutral) everywhere; subsets override
# for the three intervention compartments. Same shape as EHE's `suppression`
# quantity — base value plus name-referencing subsets.
register.model.quantity(TECH2CHECK.SPECIFICATION,
                        name = 'tech2check.suppression.OR',
                        value = 1)

register.model.quantity.subset(TECH2CHECK.SPECIFICATION,
                               name = 'tech2check.suppression.OR',
                               value = 'tech2check.on.suppression.OR',
                               applies.to = list(continuum = 'on_intervention'))

register.model.quantity.subset(TECH2CHECK.SPECIFICATION,
                               name = 'tech2check.suppression.OR',
                               value = 'tech2check.recently.suppression.OR',
                               applies.to = list(continuum = 'recently_intervened'))

register.model.quantity.subset(TECH2CHECK.SPECIFICATION,
                               name = 'tech2check.suppression.OR',
                               value = 'tech2check.distantly.suppression.OR',
                               applies.to = list(continuum = 'distantly_intervened'))


# Redefine the diagnosed-suppression baseline with the per-compartment OR.
# diagnosed_chronic: dispatcher returns 1 -> expression collapses to
#   super.suppression.of.diagnosed (EHE baseline preserved, COVID multiplier intact).
# intervention compartments: dispatcher returns the compartment-specific OR ->
#   p' = OR * baseline / (1 - baseline + OR * baseline).
register.model.quantity(TECH2CHECK.SPECIFICATION,
                        name = 'suppression.of.diagnosed',
                        value = expression(tech2check.suppression.OR * super.suppression.of.diagnosed /
                                           (1 - super.suppression.of.diagnosed +
                                            tech2check.suppression.OR * super.suppression.of.diagnosed)))


##----------------------##
##-- TRACKED OUTCOMES --##
##----------------------##
#
# EHE outcomes are inherited; these are Tech2Check-specific additions for
# verification (simulation_verification.md) and downstream paper/cost analysis.

# Annual program enrollments per stratum.
track.transition(TECH2CHECK.SPECIFICATION,
                 name = 'tech2check.enrollments',
                 outcome.metadata = create.outcome.metadata(display.name = 'Tech2Check Enrollments',
                                                            description = "Number of Individuals Enrolling in Tech2Check in the Past Year",
                                                            scale = 'non.negative.number',
                                                            axis.name = 'Enrollments',
                                                            units = 'persons',
                                                            singular.unit = 'person'),
                 dimension = 'continuum',
                 from.compartments = 'diagnosed_chronic',
                 to.compartments = 'on_intervention',
                 keep.dimensions = c('location','age','race','sex','risk'))

# Population in each lifecycle compartment (continuum kept to break out by state).
track.point.outcome(TECH2CHECK.SPECIFICATION,
                    name = 'intervention.population',
                    outcome.metadata = create.outcome.metadata(display.name = 'Intervention Population',
                                                               description = "Number of Individuals in Each Intervention Lifecycle Compartment",
                                                               scale = 'non.negative.number',
                                                               axis.name = 'Persons',
                                                               units = 'persons',
                                                               singular.unit = 'person'),
                    value = 'infected',
                    subset.dimension.values = list(continuum = c('on_intervention',
                                                                 'recently_intervened',
                                                                 'distantly_intervened')),
                    keep.dimensions = c('location','age','race','sex','risk','continuum'))

# Person-years on active intervention -- direct cost-analysis input.
track.integrated.outcome(TECH2CHECK.SPECIFICATION,
                         name = 'person.years.on.intervention',
                         outcome.metadata = create.outcome.metadata(display.name = 'Person-Years on Intervention',
                                                                    description = "Person-Years Spent in the Active Intervention Compartment",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Person-Years',
                                                                    units = 'person-years',
                                                                    singular.unit = 'person-year'),
                         value.to.integrate = 'infected',
                         subset.dimension.values = list(continuum = 'on_intervention'),
                         keep.dimensions = c('location','age','race','sex','risk'))

# Per-state diagnosed prevalence -- denominator for the per-state suppression outcome.
track.integrated.outcome(TECH2CHECK.SPECIFICATION,
                         name = 'diagnosed.prevalence.by.intervention.state',
                         outcome.metadata = create.outcome.metadata(display.name = 'Prevalence by Intervention State',
                                                                    description = "The Number of Diagnosed PWH in Each Intervention Lifecycle State",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Prevalent Cases',
                                                                    units = 'cases',
                                                                    singular.unit = 'case'),
                         value.to.integrate = 'infected',
                         subset.dimension.values = list(continuum = 'diagnosed.states'),
                         keep.dimensions = c('location','age','race','sex','risk','continuum'))

# Suppression broken out by intervention state -- enables analytical verification
# of OR application (Step 4 of simulation_verification.md) and the per-stage
# suppression plot Melissa is anticipating.
track.integrated.outcome(TECH2CHECK.SPECIFICATION,
                         name = 'suppression.by.intervention.state',
                         outcome.metadata = create.outcome.metadata(display.name = 'Suppression by Intervention State',
                                                                    description = "The Proportion of People with Diagnosed HIV who are Virally Suppressed, by Intervention Lifecycle State",
                                                                    scale = 'proportion',
                                                                    axis.name = 'Proportion Suppressed',
                                                                    units = '%'),
                         value.to.integrate = 'infected',
                         value.is.numerator = T,
                         multiply.by = 'suppression.of.diagnosed',
                         subset.dimension.values = list(continuum = 'diagnosed.states'),
                         denominator.outcome = 'diagnosed.prevalence.by.intervention.state',
                         keep.dimensions = c('location','age','race','sex','risk','continuum'))


##--------------##
##-- REGISTER --##
##--------------##

register.model.specification(TECH2CHECK.SPECIFICATION)
