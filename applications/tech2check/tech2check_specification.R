# Tech2Check specification. Inherits from EHE and adds a 4-state intervention
# lifecycle within the diagnosed HIV compartment: diagnosed_chronic is retained
# as "never intervened"; three new compartments (on_intervention,
# recently_intervened, distantly_intervened) track the program lifecycle.
# Model design: see design.md in the tech2check working repo.

source('../jheem_analyses/applications/EHE/ehe_specification.R')


##---------------##
##-- CONSTANTS --##
##---------------##

# Trial-aligned youth eligible age range. JHEEM's first age band ("13-24 years")
# corresponds almost exactly to the Tech2Check trial's enrollment range
# (ages 12-25). 12-year-olds are not modeled by JHEEM (start age 13);
# 25-year-olds fall in band 2 — both edges are small modeling approximations.
TECH2CHECK.RECRUIT.YOUTH.AGES <- '13-24 years'

# Adult recruitment band for the broaden-the-pool sensitivity (#35). Adult
# recruitment defaults to 0 (youth-only base case), so the base reproduces the
# original single-band model. See docs/plan_broaden_pool.md.
TECH2CHECK.RECRUIT.ADULT.AGES <- '25-34 years'


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
# Split youth/adult for the broaden-the-pool sensitivity (#35) via an AGE
# DISPATCHER on one transition -- NOT two transitions. jheem2 keys transitions
# by dimension/from/to/group/tag and REJECTS a second transition on the same
# compartments regardless of `applies.to` (components.clash,
# SPECIFICATION_model_specification.R:7429 + parent :6661 -- applies.to is not
# part of the clash key). So one transition reads an age-subset dispatcher rate
# quantity: youth ages pick up the youth rate, adult ages the adult rate, all
# other ages 0 (the dispatcher default). Pattern A (separate underlying rate
# quantities) is still REQUIRED so the intervention can set youth/adult rates
# independently -- a single create.intervention rejects two effects on one
# quantity (INTERVENTIONS_main.R:60; jheem_spec_notes.md). Base case is
# youth-only (adult rate defaults to 0), so the base reproduces the original
# single-rate model (verified by the neutrality + base-preservation gates).
register.model.element(TECH2CHECK.SPECIFICATION,
                       name = 'tech2check.recruitment.rate.youth',
                       scale = 'rate',
                       value = 0)

register.model.element(TECH2CHECK.SPECIFICATION,
                       name = 'tech2check.recruitment.rate.adult',
                       scale = 'rate',
                       value = 0)

# Age dispatcher: default 0 (no recruitment); the youth/adult age bands override
# to their band-specific underlying rate element. Non-overlapping subsets
# (13-24 vs 25-34) -- overlapping-subset precedence is unverified in jheem2
# (jheem_spec_notes.md). Same dispatch shape as tech2check.suppression.OR, here
# consumed as a transition rate rather than inside an expression.
register.model.quantity(TECH2CHECK.SPECIFICATION,
                        name = 'tech2check.recruitment.rate',
                        scale = 'rate',
                        value = 0)

register.model.quantity.subset(TECH2CHECK.SPECIFICATION,
                               name = 'tech2check.recruitment.rate',
                               value = 'tech2check.recruitment.rate.youth',
                               applies.to = list(age = TECH2CHECK.RECRUIT.YOUTH.AGES))

register.model.quantity.subset(TECH2CHECK.SPECIFICATION,
                               name = 'tech2check.recruitment.rate',
                               value = 'tech2check.recruitment.rate.adult',
                               applies.to = list(age = TECH2CHECK.RECRUIT.ADULT.AGES))

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

# Annual program enrollments -- a total plus a youth/adult split (#35). Each is
# the integral over the year of (recruitment.rate * infected population)
# restricted to diagnosed_chronic in the relevant recruitment band(s) -- the same
# expression the ODE evaluates at the recruitment transition, integrated post-hoc.
#
# tech2check.enrollments (TOTAL) multiplies by the age DISPATCHER quantity
# (tech2check.recruitment.rate) over the full 13-34 recruitment range: the
# dispatcher returns the youth rate at 13-24 and the adult rate at 25-34, so the
# integral is youth-rate*youth-pop + adult-rate*adult-pop = the per-band split
# summed. Retained under its original name for backward compatibility (older
# consumers read `tech2check.enrollments`); in the youth-only base it equals the
# youth value exactly (adult rate 0). Verified total == youth + adult in
# verify_broaden_recruitment.R. The .youth / .adult splits each multiply by their
# band's underlying rate ELEMENT (tech2check.recruitment.rate.youth/.adult) -- the
# outcome's own age subset already restricts the source population, so the flat
# element is the right per-band rate; a single split outcome can't multiply two
# source populations by two different rates, hence the explicit pair.
#
# Implementation note: track.integrated.outcome (static) is used rather than
# track.transition (dynamic) so the spec remains transmutable from EHE.
# do.evaluate.can.transmute rejects new dynamic outcomes in the to-spec
# (JHEEM_transmutation.R::do.evaluate.can.transmute line ~123); the team's
# Ryan White spec follows the same pattern -- zero track.transition calls.
# For a constant within-year rate the integrated form is exact in the limit.
track.integrated.outcome(TECH2CHECK.SPECIFICATION,
                         name = 'tech2check.enrollments',
                         outcome.metadata = create.outcome.metadata(display.name = 'Tech2Check Enrollments (Total)',
                                                                    description = "Number of Individuals Enrolling in Tech2Check in the Past Year (all recruited ages)",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Enrollments',
                                                                    units = 'persons',
                                                                    singular.unit = 'person'),
                         value.to.integrate = 'infected',
                         multiply.by = 'tech2check.recruitment.rate',
                         subset.dimension.values = list(continuum = 'diagnosed_chronic',
                                                        age = c(TECH2CHECK.RECRUIT.YOUTH.AGES,
                                                                TECH2CHECK.RECRUIT.ADULT.AGES)),
                         keep.dimensions = c('location','age','race','sex','risk'))

track.integrated.outcome(TECH2CHECK.SPECIFICATION,
                         name = 'tech2check.enrollments.youth',
                         outcome.metadata = create.outcome.metadata(display.name = 'Tech2Check Enrollments (Youth 13-24)',
                                                                    description = "Number of Youth (13-24) Enrolling in Tech2Check in the Past Year",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Enrollments',
                                                                    units = 'persons',
                                                                    singular.unit = 'person'),
                         value.to.integrate = 'infected',
                         multiply.by = 'tech2check.recruitment.rate.youth',
                         subset.dimension.values = list(continuum = 'diagnosed_chronic',
                                                        age = TECH2CHECK.RECRUIT.YOUTH.AGES),
                         keep.dimensions = c('location','age','race','sex','risk'))

track.integrated.outcome(TECH2CHECK.SPECIFICATION,
                         name = 'tech2check.enrollments.adult',
                         outcome.metadata = create.outcome.metadata(display.name = 'Tech2Check Enrollments (Adult 25-34)',
                                                                    description = "Number of Adults (25-34) Enrolling in Tech2Check in the Past Year",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Enrollments',
                                                                    units = 'persons',
                                                                    singular.unit = 'person'),
                         value.to.integrate = 'infected',
                         multiply.by = 'tech2check.recruitment.rate.adult',
                         subset.dimension.values = list(continuum = 'diagnosed_chronic',
                                                        age = TECH2CHECK.RECRUIT.ADULT.AGES),
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

# Person-years in active OR recently-completed intervention -- the "active
# intervention" aggregate for headline policy claims. Distinct from
# person.years.on.intervention (on_intervention only, the program-delivery
# denominator for cost analysis) and from intervention.population (which
# includes distantly_intervened, where OR = 1.0 in the base case and so
# doesn't represent ongoing behavioral effect). Per-recruit dwell math gives
# ~93% of cumulative intervention.population person-years in distantly, so
# reaching for that aggregate overstates active program impact by roughly
# an order of magnitude.
track.integrated.outcome(TECH2CHECK.SPECIFICATION,
                         name = 'person.years.on.or.recently.intervened',
                         outcome.metadata = create.outcome.metadata(display.name = 'Person-Years On or Recently Intervened',
                                                                    description = "Person-Years Spent in the Active or Recently-Completed Intervention Compartments",
                                                                    scale = 'non.negative.number',
                                                                    axis.name = 'Person-Years',
                                                                    units = 'person-years',
                                                                    singular.unit = 'person-year'),
                         value.to.integrate = 'infected',
                         subset.dimension.values = list(continuum = c('on_intervention', 'recently_intervened')),
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
# of OR application (Step 4 of simulation_verification.md)
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
