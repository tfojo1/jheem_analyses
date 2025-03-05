
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
mcmc.start.sim = mcmc@simulations[[1]]
#mcmc.simset = join.simulation.sets(mcmc@simulations)

#-- Take a look at how it went --#
bayesian.simulations::likelihood.plot(mcmc, show.log.prior = F)

#simplot(mcmc.simset, 'non.adap.clients', data.manager = RW.DATA.MANAGER)
simplot(mcmc.start.sim, fitted.sim, 'non.adap.clients', data.manager = RW.DATA.MANAGER)
simplot(mcmc.start.sim, fitted.sim, 'non.adap.clients', facet.by='race', data.manager = RW.DATA.MANAGER)
simplot(mcmc.start.sim, fitted.sim, 'non.adap.clients', facet.by='risk', split.by='sex', data.manager = RW.DATA.MANAGER)
simplot(mcmc.start.sim, fitted.sim, 'non.adap.clients', facet.by='sex', data.manager = RW.DATA.MANAGER)
simplot(mcmc.start.sim, fitted.sim, 'non.adap.clients', facet.by='age', data.manager = RW.DATA.MANAGER)
bayesian.simulations::trace.plot(mcmc, 'non.adap')

#-- Compare likelihoods --#
#lik = ryan.white.likelihood.instructions$instantiate.likelihood('rw',fitted.sim$location)
lik = RW.MCMC.CHECKING$likelihoods
lik$compare.sims(fitted.sim, mcmc.start.sim, piecewise = F)
exp(lik$compute(fitted.sim)-lik$compute(mcmc.start.sim))

calculate.density(RYAN.WHITE.PARAMETERS.PRIOR, fitted.sim$params) / calculate.density(RYAN.WHITE.PARAMETERS.PRIOR, mcmc.start.sim$params)

cbind(mcmc.start.sim$params, fitted.sim$params)[RYAN.WHITE.PARAMETERS.PRIOR@var.names,]

transmuter = create.jheem.transmuter(ehe.sim, 'rw')

params = fitted.sim$params
params['non.adap.idu.female.or']
params['non.adap.heterosexual.female.or'] = 1.1

sim2=transmuter$transmute(1, params)
simplot(fitted.sim, sim2, 'non.adap.clients', data.manager = RW.DATA.MANAGER)
simplot(fitted.sim, sim2, 'non.adap.clients', facet.by='sex', data.manager = RW.DATA.MANAGER)
simplot(fitted.sim, sim2, 'non.adap.clients', facet.by='race', data.manager = RW.DATA.MANAGER)
simplot(fitted.sim, sim2, 'non.adap.clients', facet.by='age', data.manager = RW.DATA.MANAGER)
simplot(fitted.sim, sim2, 'non.adap.clients', facet.by='risk', split.by='sex', data.manager = RW.DATA.MANAGER)


lik$compare.sims(fitted.sim, sim2, piecewise = T)


lik1 = rw.non.adap.likelihood.instructions$instantiate.likelihood('rw',fitted.sim$location, data.manager = RW.DATA.MANAGER)
lik1$compute(fitted.sim, debug=T)

non.adap.lik.instr2 = create.basic.likelihood.instructions(
  outcome.for.data = "non.adap.clients", 
  outcome.for.sim = "non.adap.clients",
  dimensions = c("age","sex","race","risk"),
  levels.of.stratification = c(0,1,2), # 0 = totals, 1 = 1-way stratification
  from.year = 2010,
  correlation.different.years = 0.5, # this is the default
  correlation.different.strata = 0.1, # this is the default
  correlation.different.sources = 0.3, # default
  correlation.same.source.different.details = 0.3, # default
  
  observation.correlation.form = 'compound.symmetry', 
  
  error.variance.term = 0.05,
  error.variance.type = 'cv',
  
  weights = 1,
  
  # if there are more datapoints for certain years, this will normalize
  # e.g., if there are a few years with only the totals 
  # before the stratifications are available
  equalize.weight.by.year = T
)

lik2 = non.adap.lik.instr2$instantiate.likelihood('rw', fitted.sim$location, data.manager = RW.DATA.MANAGER)
lik2$compare.sims(fitted.sim, sim2)
