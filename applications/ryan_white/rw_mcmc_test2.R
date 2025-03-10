
if (1==2)
{
  source('applications/ryan_white/ryan_white_specification.R')
  RW.DATA.MANAGER = load.data.manager('../../cached/ryan.white.data.manager.rdata', set.as.default = F)
  
  source('applications/ryan_white/ryan_white_mcmc.R')
  source('commoncode/locations_of_interest.R')
  
  loc = 'C.12580'
  simset = retrieve.simulation.set('ehe',loc,'full.with.covid2',100)
  sim.ehe = simset$first.sim()
}

source('applications/ryan_white/ryan_white_likelihoods.R')

sim = fit.rw.simset(sim.ehe, 2000, verbose=T, track.mcmc = T)

sim0 = RW.MCMC.CHECKING$mcmc.runs[[1]]@simulations[[1]]

simplot(sim0, sim, 'adap.proportion', data.manager = RW.DATA.MANAGER)
simplot(sim0, sim, 'adap.proportion.of.diagnosed', data.manager = RW.DATA.MANAGER)
simplot(sim0, sim, 'adap.suppression', data.manager = RW.DATA.MANAGER)
simplot(sim0, sim, 'adap.suppressed.proportion.of.diagnosed', data.manager = RW.DATA.MANAGER)
simplot(sim0, sim, 'non.adap.clients', data.manager = RW.DATA.MANAGER)
simplot(sim0, sim, 'non.adap.clients', data.manager = RW.DATA.MANAGER, facet.by = 'race')
simplot(sim0, sim, 'non.adap.clients', data.manager = RW.DATA.MANAGER, facet.by = 'age')
simplot(sim0, sim, 'oahs.clients', data.manager = RW.DATA.MANAGER)
simplot(sim0, sim, 'oahs.clients', data.manager = RW.DATA.MANAGER, facet.by = 'race')
simplot(sim0, sim, 'oahs.clients', data.manager = RW.DATA.MANAGER, facet.by = 'age')

RW.DATA.MANAGER$data$oahs.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location[,sim$location]
apply(RW.DATA.MANAGER$data$oahs.clients$estimate$ryan.white.program$ryan.white.pdfs$year__location__age[,sim$location,],'year',sum)


adap.supp.lik = rw.adap.suppression.likelihood.instructions$instantiate.likelihood('rw',sim$location,data.manager=RW.DATA.MANAGER)
adap.supp.lik$compute(sim, debug=T)

adap.lik = rw.adap.likelihood.instructions$instantiate.likelihood('rw',sim$location,data.manager=RW.DATA.MANAGER)
adap.lik$compute(sim, debug=T)

lik = ryan.white.likelihood.instructions$instantiate.likelihood('rw','C.12580',data.manager = RW.DATA.MANAGER)
lik$compare.sims(sim0, sim)

oahs.lik = rw.oahs.likelihood.instructions$instantiate.likelihood('rw',sim$location,data.manager=RW.DATA.MANAGER)
oahs.lik$compute(sim, debug=T)
