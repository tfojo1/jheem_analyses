

RW.DATA.MANAGER = load.data.manager('../../cached/ryan.white.data.manager.rdata', set.as.default = F)
source('../jheem_analyses/applications/ryan_white/ryan_white_specification.R')
source('../jheem_analyses/applications/ryan_white/ryan_white_likelihoods.R')

lik = rw.adap.likelihood.instructions$instantiate.likelihood('rw','C.12580', data.manager = RW.DATA.MANAGER) # this one pulls me into a browser()


lik = rw.adap.suppression.likelihood.instructions$instantiate.likelihood('rw','C.12580', data.manager = RW.DATA.MANAGER) # this one instantiates
simset = retrieve.simulation.set('rw','C.12580','full.with.covid2',100) # you have to be logged into the VPN for this
lik$compute(simset$last.sim()) # evaluates to -Inf