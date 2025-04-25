
RW.DATA.MANAGER = load.data.manager('../../cached/ryan.white.data.manager.rdata', set.as.default = F)

source('applications/ryan_white/ryan_white_specification.R')
source('applications/ryan_white/ryan_white_mcmc.R')
source('applications/ryan_white/ryan_white_likelihoods.R')

ehe.simset = get(load('../jheem2/debug_sims/baltimore_simset_2025.02.25.Rdata'))

ehe.simset = ehe.simset$thin(keep = 5)

#simset = fit.rw.simset(ehe.simset, verbose=T)
fitted.sim = fit.rw.simset(ehe.simset$last.sim(), verbose=T)

simplot(simset, 'non.adap.clients', data.manager = RW.DATA.MANAGER)
simplot(simset, 'oahs.clients', data.manager = RW.DATA.MANAGER)
simplot(simset, 'oahs.suppression', data.manager = RW.DATA.MANAGER)
simplot(simset, 'adap.suppression', data.manager = RW.DATA.MANAGER)
simplot(simset, 'adap.proportion', data.manager = RW.DATA.MANAGER)


mcmc = RW.MCMC.CHECKING$mcmc.runs[[1]]

simplot(mcmc@simulations[[1]], mcmc@simulations[[length(mcmc@simulations)]], 'non.adap.clients', data.manager = RW.DATA.MANAGER)

mcmc1.simset = join.simulation.sets(mcmc@simulations)


simplot(mcmc1.simset, 'non.adap.clients', data.manager = RW.DATA.MANAGER)
bayesian.simulations::trace.plot(mcmc, 'non.adap')
