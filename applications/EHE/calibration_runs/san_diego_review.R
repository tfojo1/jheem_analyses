load("prelim_results/init.pop.ehe.2_simset_2025-02-18_C.41740.Rdata")
sim.pop = simset$last.sim()

trans.lik = 
  transmission.pop.idu.aware.aids.testing.likelihood.instructions$instantiate.likelihood('ehe','C.41740')

trans.lik$compute.piecewise(sim.pop)


test.race.risk.new.diagnoses.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "diagnoses",
                                       outcome.for.sim = "new",
                                       dimensions = c("race","risk"),
                                       levels.of.stratification = c(0,1,2), 
                                       from.year = 2015, 
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = DIAGNOSES.ERROR.TERM, 
                                       error.variance.type = 'cv',
                                       weights = (1*TRANSMISSION.WEIGHT), #list(0.3), # see prev_new_aware_weighting.R 
                                       equalize.weight.by.year = T
  )

tst.lik = test.race.risk.new.diagnoses.likelihood.instructions$instantiate.likelihood('ehe','C.41740')

tst.lik$compute.piecewise(sim.pop, debug = T)

load("prelim_results/init.pop.ehe_simset_2025-02-12_C.41740.Rdata")
sim.pop.old = simset$last.sim()


simplot(sim.pop.old,
        sim.pop.last, # has definitely improved
        outcomes = c("new"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim.pop.old,
        sim.pop.last,
        facet.by = "risk", split.by = "race", 
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 

