# =============================================================================
# OAKLAND TGA: REVIEW pop.ehe.oakland
# Run from code/jheem_analyses (same working directory as the calibration).
# =============================================================================

# Registers the Oakland calibrations AND applies the in-session data patches
# (e.g. Oakland 'deaths'), so the data show up on the plots
source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations_oakland.R')

LOCATION = 'TGA.OAKLAND'

location.style.manager = create.style.manager(color.data.by = "location.type")
source.style.manager = create.style.manager(color.data.by = "source")

load("prelim_results/pop.ehe.oakland_simset_2026-10-29_TGA.OAKLAND.Rdata")
sim.first = simset$first.sim()
sim.last = simset$last.sim()

#-- Likelihood: which components fit worst? ----
pop.lik = joint.pop.migration.total.trans.likelihood.instructions$instantiate.likelihood('ehe', LOCATION)
pop.lik$compute.piecewise(sim.first)
pop.lik$compute.piecewise(sim.last)

pop.lik$compare.sims(sim.first,sim.last)

#-- Population ----
simplot(sim.first, sim.last,
        outcomes = c("population"),
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030))

simplot(sim.first, sim.last,
        facet.by = "age",
        outcomes = c("population"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030))

simplot(sim.first, sim.last,
        facet.by = "race",
        outcomes = c("population"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030))

simplot(sim.first, sim.last,
        facet.by = "sex",
        outcomes = c("population"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030))

#-- Migration (Oakland data = TOTAL, all ages 1+; sim = model ages 13+) ----
simplot(sim.first, sim.last,
        outcomes = c("immigration", "emigration"),
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030))

#-- General mortality ----
simplot(sim.first, sim.last,
        outcomes = c("total.mortality"),
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030))

#-- HIV totals in the pop likelihood ----
simplot(sim.first, sim.last,
        outcomes = c("new", "diagnosed.prevalence"),
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030))
