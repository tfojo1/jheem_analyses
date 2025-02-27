
simset = assemble.simulations.from.calibration('ehe','C.12580', 'full.with.covid2')
RW.DATA.MANAGER = load.data.manager('../../cached/ryan.white.data.manager.rdata', set.as.default = F)

source('applications/ryan_white/ryan_white_specification.R')
source('applications/ryan_white/ryan_white_mcmc.R')

simset = rerun.simulations(simset)

transmuter = create.jheem.transmuter(simset, to.version='rw')

sim = get.fitted.rw.simulation(sim.transmuter = transmuter,
                               sim.index = simset$n.sim)

simplot(sim, 'non.adap.clients', data.manager = RW.DATA.MANAGER)
simplot(sim, 'oahs.clients', data.manager = RW.DATA.MANAGER)
simplot(sim, 'oahs.suppression', data.manager = RW.DATA.MANAGER)
simplot(sim, 'adap.suppression', data.manager = RW.DATA.MANAGER)
simplot(sim, 'adap.proportion', data.manager = RW.DATA.MANAGER)

non.adap.lk = rw.non.adap.likelihood.instructions$instantiate.likelihood('rw', sim$location, data.manager = RW.DATA.MANAGER)
oahs.lk = rw.oahs.likelihood.instructions$instantiate.likelihood('rw', sim$location, data.manager = RW.DATA.MANAGER)
oahs.suppression.lk = rw.oahs.suppression.likelihood.instructions$instantiate.likelihood('rw', sim$location, data.manager = RW.DATA.MANAGER)
adap.lk = rw.adap.likelihood.instructions$instantiate.likelihood("rw",sim$location, data.manager = RW.DATA.MANAGER)
adap.suppression.lk = rw.adap.suppression.likelihood.instructions$instantiate.likelihood("rw",sim$location, data.manager = RW.DATA.MANAGER)

non.adap.lk$compute(sim)
oahs.lk$compute(sim)
oahs.suppression.lk$compute(sim)
adap.lk$compute(sim)


# QUICK AND DIRTY UNTIL ZOE GETS DATA UP

round(sim$get(outcomes = 'oahs.clients'))
RW.DATA.MANAGER$pull(outcome = 'ambulatory.care.past.year',
                     dimension.values = list(location=sim$location))


sim$get(outcomes = 'oahs.suppression')
RW.DATA.MANAGER$pull(outcome = 'non.adap.viral.suppression',
                     dimension.values = list(location=sim$location))




sim$get(outcomes='adap.proportion')

RW.DATA.MANAGER$pull(outcome = 'adap.clients',
                     dimension.values = list(location='MD')) / 
  RW.DATA.MANAGER$pull(outcome = 'non.adap.clients',
                       dimension.values = list(location='MD'))
