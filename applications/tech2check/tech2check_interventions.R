# Tech2Check intervention definitions.
#
# Encodes the policy scenario: Tech2Check recruitment activates at the
# defined start year and runs through the end year (e.g., 2026-2030 for a
# 4-year scale-up window). Pre-start-year: no intervention -- baseline
# counterfactual is the spec default of tech2check.recruitment.rate.youth/.adult = 0,
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
# recruitment.rate : legacy alias = YOUTH rate (adult defaults to 0). Use
#                    recruitment.rate.youth / .adult for explicit broadened scenarios.
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
make.tech2check.intervention <- function(recruitment.rate       = 0.5,   # legacy: YOUTH rate (adult defaults to 0)
                                         recruitment.rate.youth = NULL,
                                         recruitment.rate.adult = NULL,
                                         recruitment.rate.older = NULL,  # 35-44 band ("under-44" ladder rung)
                                         on.or        = NULL,   # legacy: sets BOTH youth + adult
                                         recently.or  = NULL,
                                         distantly.or = NULL,
                                         on.or.youth        = NULL,
                                         on.or.adult        = NULL,
                                         recently.or.youth  = NULL,
                                         recently.or.adult  = NULL,
                                         distantly.or.youth = NULL,
                                         distantly.or.adult = NULL,
                                         start.year   = TECH2CHECK.START.YEAR,
                                         stop.year    = NULL,
                                         code         = NULL) {
    # Resolve youth/adult recruitment (#35). Legacy `recruitment.rate` means the
    # YOUTH rate with adult defaulting to 0 -- so the base scenario stays youth-only
    # and is NOT silently broadened. Explicit `recruitment.rate.youth/.adult` override.
    youth.rate <- if (!is.null(recruitment.rate.youth)) recruitment.rate.youth else recruitment.rate
    adult.rate <- if (!is.null(recruitment.rate.adult)) recruitment.rate.adult else 0
    older.rate <- if (!is.null(recruitment.rate.older)) recruitment.rate.older else 0  # 35-44, default 0

    # Validate inputs up front (before scenario configs start passing these around):
    # a malformed rate should error loudly, not silently fall through the
    # `adult.rate > 0` guard (which would swallow a negative as "no adult arm" or
    # error opaquely on NA). Rates are non-negative; ORs (when given) are positive.
    require.scalar <- function(x, nm, allow.zero = TRUE) {
        if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
            (allow.zero && x < 0) || (!allow.zero && x <= 0))
            stop(sprintf(paste0("make.tech2check.intervention: '%s' must be a single finite ",
                                "%s number (got: %s)"),
                         nm, if (allow.zero) "non-negative" else "positive", deparse(x)))
    }
    require.scalar(youth.rate, 'recruitment.rate.youth')
    require.scalar(adult.rate, 'recruitment.rate.adult')
    require.scalar(older.rate, 'recruitment.rate.older')

    # Resolve youth/adult ORs (#35). Legacy `on.or` etc. set BOTH bands (harmless:
    # in the youth-only base no adult carries an OR, since adult recruitment is 0);
    # explicit `.youth`/`.adult` override per band. NULL leaves the spec default.
    on.youth.or        <- if (!is.null(on.or.youth))        on.or.youth        else on.or
    on.adult.or        <- if (!is.null(on.or.adult))        on.or.adult        else on.or
    recently.youth.or  <- if (!is.null(recently.or.youth))  recently.or.youth  else recently.or
    recently.adult.or  <- if (!is.null(recently.or.adult))  recently.or.adult  else recently.or
    distantly.youth.or <- if (!is.null(distantly.or.youth)) distantly.or.youth else distantly.or
    distantly.adult.or <- if (!is.null(distantly.or.adult)) distantly.or.adult else distantly.or

    or.targets <- list('tech2check.on.suppression.OR.youth'        = on.youth.or,
                       'tech2check.on.suppression.OR.adult'        = on.adult.or,
                       'tech2check.recently.suppression.OR.youth'  = recently.youth.or,
                       'tech2check.recently.suppression.OR.adult'  = recently.adult.or,
                       'tech2check.distantly.suppression.OR.youth' = distantly.youth.or,
                       'tech2check.distantly.suppression.OR.adult' = distantly.adult.or)
    for (nm in names(or.targets)) {
        v <- or.targets[[nm]]
        if (!is.null(v)) require.scalar(v, nm, allow.zero = FALSE)
    }

    # Sustained (stop.year = NULL) or time-limited rate effect on one quantity.
    # (See the end.time-gotcha note above: for SUSTAINED recruitment do NOT pass
    # end.time, or jheem2 ramps the rate linearly down to 0 over the window.)
    rate.effect <- function(qname, rate) {
        if (is.null(stop.year))
            create.intervention.effect(
                quantity.name = qname, start.time = start.year, times = start.year,
                effect.values = rate, scale = 'rate', apply.effects.as = 'value',
                allow.values.less.than.otherwise = F, allow.values.greater.than.otherwise = T)
        else
            create.intervention.effect(
                quantity.name = qname, start.time = start.year, times = c(start.year, stop.year),
                effect.values = c(rate, rate), end.time = stop.year, scale = 'rate',
                apply.effects.as = 'value', allow.values.less.than.otherwise = F,
                allow.values.greater.than.otherwise = T)
    }

    # Youth recruitment effect always; adult only when > 0 (0 = spec default, a
    # no-op -- omitting it keeps the youth-only base/neutrality scenario exactly
    # the original single-band model).
    effects <- list(rate.effect('tech2check.recruitment.rate.youth', youth.rate))
    if (adult.rate > 0)
        effects <- c(effects, list(rate.effect('tech2check.recruitment.rate.adult', adult.rate)))
    if (older.rate > 0)  # 35-44 band: the "under-44" ladder rung; 0 = no-op
        effects <- c(effects, list(rate.effect('tech2check.recruitment.rate.older', older.rate)))

    # Optional OR overrides on the six youth/adult x {on,recently,distantly}
    # quantities (resolved + validated above). scale = 'ratio' matches the OR
    # model elements. The youth-band override path is verified (verify_or_override.R,
    # #38); the multi-dimensional age x continuum path is verified by the
    # adult-positive OR gate (verify_broaden_or.R, #35).
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
        code <- sprintf('t2c.ry%g.ra%g', youth.rate, adult.rate)

    do.call(create.intervention,
            c(list(WHOLE.POPULATION), effects, list(code = code, parameters = NULL)))
}

# Base intervention: factory defaults (recruitment 0.5/yr sustained, spec ORs).
tech2check.base.intervention <- make.tech2check.intervention(code = 't2c.base')
