# depression test #
# source dep.sp

source('../jheem_analyses/applications/Depression/dep_specification.R')

joint.prior = join.distributions(DEP.PARAMETERS.PRIOR, EHE.PARAMETERS.PRIOR)
params = suppressWarnings(get.medians(joint.prior))
params["global.trate"] = 0.075

engine=create.jheem.engine("dep", "C.12580", end.year = 2025)
sim = engine$run(params)
simplot(sim,"new")


## intervention: example get 90% on depression tx
treatment90 <- create.intervention.effect(quantity.name = 'depression.proportion.tx', start.time = 2025, 
                                        effect.values = 0.9, 
                                        times = 2030, apply.effects.as = 'value', scale = 'proportion',
                                        allow.values.less.than.otherwise = F, allow.values.greater.than.otherwise = T)

intdep90  <- create.intervention(treatment90, WHOLE.POPULATION, code = 'depTx90')
sim.int   <- intdep90$run(sim,2025, 2035)
sim.noint <- get.null.intervention()$run(sim, 2025, 2035)

simplot(sim.noint, sim.int, 'incidence')

sim.noint$get('incidence', year=2035)
sim.int$get('incidence', year=2035)
