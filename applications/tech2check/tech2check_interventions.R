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


# Factory: build a Tech2Check intervention for a given recruitment rate and
# (optionally) overridden suppression ORs. Defaults reproduce the base scenario
# (recruitment 0.5/yr sustained, spec-default ORs 2.0 / 2.0 / 1.0). Used for
# scenario sweeps -- vary recruitment.rate (scale-up intensity) and/or the ORs
# (effect-size sensitivity) -- following the cdc_testing_interventions pattern of
# parallel intervention objects built from a constructor.
#
# recruitment.rate : sustained rate applied from start.year.
# stop.year        : NULL = sustained (default); a year = time-limited (recruit
#                    at the rate from start.year to stop.year, then stop).
# on/recently/distantly.or : NULL = leave the spec default in place; a number
#                    overrides that compartment's OR.
#
# IMPORTANT (end.time gotcha, 2026-05-22): for SUSTAINED recruitment do NOT pass
# end.time. In jheem2, end.time is the time by which the quantity RETURNS to
# baseline -- the trajectory is built over union(start.time, times, end.time)
# with baseline at start.time/end.time and effect.values at times
# (INTERVENTIONS_intervention_effects.R ~L421). A single control point at
# start.time PLUS an end.time ramps the rate linearly DOWN to 0 over the window
# (the original bug). Omitting end.time (default Inf) holds it constant. The
# time-limited branch below pins both endpoints (times = c(start, stop),
# effect.values = c(r, r), end.time = stop) so the rate is held flat then stops.
make.tech2check.intervention <- function(recruitment.rate = 0.5,
                                         on.or        = NULL,
                                         recently.or  = NULL,
                                         distantly.or = NULL,
                                         start.year   = TECH2CHECK.START.YEAR,
                                         stop.year    = NULL,
                                         code         = NULL) {
    if (is.null(stop.year))
        recruitment.effect <- create.intervention.effect(
            quantity.name = 'tech2check.recruitment.rate',
            start.time    = start.year,
            times         = start.year,
            effect.values = recruitment.rate,
            scale         = 'rate',
            apply.effects.as = 'value',
            allow.values.less.than.otherwise = F,
            allow.values.greater.than.otherwise = T)
    else
        recruitment.effect <- create.intervention.effect(
            quantity.name = 'tech2check.recruitment.rate',
            start.time    = start.year,
            times         = c(start.year, stop.year),
            effect.values = c(recruitment.rate, recruitment.rate),
            end.time      = stop.year,
            scale         = 'rate',
            apply.effects.as = 'value',
            allow.values.less.than.otherwise = F,
            allow.values.greater.than.otherwise = T)

    effects <- list(recruitment.effect)

    # Optional OR overrides. scale = 'ratio' matches the OR model elements.
    # NOTE: the OR-override path is not yet exercised in a verified run -- before
    # relying on it in an OR sweep, confirm suppression.by.intervention.state
    # responds to these effects (the recruitment path IS verified).
    or.targets <- list('tech2check.on.suppression.OR'       = on.or,
                       'tech2check.recently.suppression.OR'  = recently.or,
                       'tech2check.distantly.suppression.OR' = distantly.or)
    for (qname in names(or.targets)) {
        v <- or.targets[[qname]]
        if (!is.null(v))
            effects <- c(effects, list(create.intervention.effect(
                quantity.name = qname,
                start.time    = start.year,
                times         = start.year,
                effect.values = v,
                scale         = 'ratio',
                apply.effects.as = 'value',
                allow.values.less.than.otherwise = T,
                allow.values.greater.than.otherwise = T)))
    }

    if (is.null(code))
        code <- sprintf('t2c.r%g', recruitment.rate)

    do.call(create.intervention,
            c(list(WHOLE.POPULATION), effects, list(code = code, parameters = NULL)))
}

# Base intervention: factory defaults (recruitment 0.5/yr sustained, spec ORs).
tech2check.base.intervention <- make.tech2check.intervention(code = 't2c.base')
