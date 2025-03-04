#-- Set Up --#
source('applications/ryan_white/ryan_white_specification.R')
RW.DATA.MANAGER = load.data.manager('../../cached/ryan.white.data.manager.rdata', set.as.default = F)

source('applications/ryan_white/ryan_white_mcmc.R')
source('applications/ryan_white/ryan_white_likelihoods.R')

#-- Pull the EHE simulation and fit to RW --#
# @Ryan - load this from wherever you have it saved
ehe.simset = load.simulation.set("../../cached/Baltimore_simset_2025.02.25.Rdata")
ehe.sim = ehe.simset$last.sim()

fitted.sim = fit.rw.simset(ehe.sim, verbose=T)

source('applications/ryan_white/ryan_white_interventions.R')

no.int = get.null.intervention()

sim.no.int = no.int$run(fitted.sim,start.year = 2025, end.year = 2035)
