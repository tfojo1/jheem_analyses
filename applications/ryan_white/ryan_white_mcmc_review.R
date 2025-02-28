
#-- Set Up --#
source('applications/ryan_white/ryan_white_specification.R')
RW.DATA.MANAGER = load.data.manager('../../cached/ryan.white.data.manager.rdata', set.as.default = F)

source('applications/ryan_white/ryan_white_mcmc.R')
source('applications/ryan_white/ryan_white_likelihoods.R')

#-- Pull the EHE simulation and fit to RW --#
# @Ryan - load this from wherever you have it saved
ehe.simset = get(load('../jheem2/debug_sims/baltimore_simset_2025.02.25.Rdata'))
ehe.sim = ehe.simset$last.sim()

fitted.sim = fit.rw.simset(ehe.sim, verbose=T)

#-- Pull out the MCMC Object --#
mcmc = RW.MCMC.CHECKING$mcmc.runs[[1]]
mcmc.simset = join.simulation.sets(mcmc@simulations)

#-- Take a look at how it went --#
bayesian.simulations::likelihood.plot(mcmc, show.log.prior = F)

simplot(mcmc.simset, 'non.adap.clients', data.manager = RW.DATA.MANAGER)
bayesian.simulations::trace.plot(mcmc, 'non.adap')
