# Tech2Check intervention definitions.
#
# Encodes the policy scenario: Tech2Check recruitment activates at the
# defined start year and runs through the end year (e.g., 2026-2030 for a
# 4-year scale-up window). Pre-start-year: no intervention -- baseline
# counterfactual is the spec default of tech2check.recruitment.rate = 0,
# so lifecycle compartments remain empty. Post-start-year: recruitment
# fires at the specified rate; spec-default ORs (on = 2.0, recently = 2.0,
# distantly = 1.0) drive the suppression effect through the dispatcher.
#
# Usage: source after the spec is registered, then apply to a base simset:
#
#   source('applications/tech2check/tech2check_specification.R')
#   source('applications/tech2check/tech2check_interventions.R')
#   sim         <- engine$run(params)  # base counterfactual
#   sim.scaleup <- tech2check.base.intervention$run(sim, start.year=2026, end.year=2030)
#
# Prototype as of 2026-05-15 -- single base intervention only. OR-sweep
# variants will follow the cdc_testing_interventions.R pattern (parallel
# intervention objects with different effect.values for the OR quantities)
# once the prototype confirms the API composes correctly with our spec.

TECH2CHECK.START.YEAR <- 2026
TECH2CHECK.END.YEAR   <- 2030


# Recruitment activation: set tech2check.recruitment.rate to a non-zero
# value at the start year. 0.5/yr is a placeholder; sweep parameter for
# the scale-up scenarios is recruitment rate (separate from the OR sweep).
tech2check.recruitment.activation <- create.intervention.effect(
    quantity.name = 'tech2check.recruitment.rate',
    start.time    = TECH2CHECK.START.YEAR,
    end.time      = TECH2CHECK.END.YEAR,
    effect.values = 0.5,
    times         = TECH2CHECK.START.YEAR,        # immediate; no ramp
    scale         = 'rate',
    apply.effects.as = 'value',
    allow.values.less.than.otherwise = F,
    allow.values.greater.than.otherwise = T
)


# Base intervention -- spec-default ORs (2.0 / 2.0 / 1.0) plus recruitment
# activation. No parameter sweep at this stage.
tech2check.base.intervention <- create.intervention(
    WHOLE.POPULATION,
    tech2check.recruitment.activation,
    code = 't2c.base',
    parameters = NULL
)
