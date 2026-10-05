# EHE26: simple example on ONE location with a FEW simulations
# Run this first, before the full run, to check that each intervention works.
# Nothing is saved to disk.

source('../jheem_analyses/applications/EHE26/ehe26_main.R')

# 1. LOAD A CALIBRATED SIMSET ----
simset = retrieve.simulation.set(version = VERSION,
                                 location = "C.12580",
                                 calibration.code = CALIBRATION.CODE,
                                 n.sim = N.SIM)

# Keep 20 simulations so it runs fast
simset = simset$thin(n = 20)

# 2. RUN NO INTERVENTION AND THE COMBINED INTERVENTION ----
sim.noint = noint$run(simset, start.year = BASELINE.YEAR, end.year = END.YEAR, verbose = T)
sim.msm.prep = msm.prep$run(simset, start.year = BASELINE.YEAR, end.year = END.YEAR, verbose = T)

# 3. CHECK THAT COVERAGE WENT UP AMONG MSM ----
# Each plot should show the intervention line rising from 2026 to 2030.
simplot(sim.noint, sim.all, 'prep.uptake', split.by = 'sex', dimension.values = list(year = 2015:2035))
simplot(sim.noint, sim.all, 'testing', split.by = 'sex', dimension.values = list(year = 2015:2035))
simplot(sim.noint, sim.all, 'suppression', split.by = 'sex', dimension.values = list(year = 2015:2035))

# 4. CHECK THE IMPACT ON INCIDENCE ----
simplot(sim.noint, sim.all, 'incidence', split.by = 'sex', dimension.values = list(year = 2015:2035))

# 5. A FIRST NUMBER: INFECTIONS AVERTED AMONG MSM, 2026-2035 ----
# Example: noint = 1,000 infections, intervention = 700 infections
#          averted = 300, percent reduction = 300/1000 = 30%
years = as.character(BASELINE.YEAR:END.YEAR)

inc.noint = sim.noint$get(outcomes = 'incidence',
                          keep.dimensions = 'year',
                          dimension.values = list(year = years, sex = 'msm'))
inc.all = sim.all$get(outcomes = 'incidence',
                      keep.dimensions = 'year',
                      dimension.values = list(year = years, sex = 'msm'))

# Sum over years -> one number per simulation
cum.noint = colSums(inc.noint)
cum.all = colSums(inc.all)

averted = cum.noint - cum.all
pct.reduction = 100 * averted / cum.noint

print("Infections averted among MSM, 2026-2035 (mean and 95% interval):")
print(c(mean = mean(averted), quantile(averted, c(0.025, 0.975))))

print("Percent reduction among MSM, 2026-2035 (mean and 95% interval):")
print(c(mean = mean(pct.reduction), quantile(pct.reduction, c(0.025, 0.975))))
