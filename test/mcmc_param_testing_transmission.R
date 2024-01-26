source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-01-25_C.12580.Rdata") # Baltimore

LOCATION = sim$location

sim.mcmc = sim 
params = sim$parameters

params.manual = c(params)
names(params.manual) = dimnames(params)[[1]]

engine = create.jheem.engine('ehe', LOCATION, start.year=1970, end.year=2025, max.run.time.seconds = 10)

# These are the transmission parameters we will likely end up varying 
EHE.PARAMETERS.PRIOR@var.names[grepl("trate",EHE.PARAMETERS.PRIOR@var.names) 
                               | grepl("susceptibility",EHE.PARAMETERS.PRIOR@var.names)] 

params.manual["global.trate"] = 0.07

sim.manual = engine$run(parameters = params.manual)

simplot(sim.mcmc, sim.manual, 
        outcomes = c("new","diagnosed.prevalence"),
        facet.by = "risk", split.by = "risk",
        dimension.values = list(year = 2000:2020))

simplot(sim.mcmc,sim.manual, 
        outcomes = c("new","diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020))

# not yet working 
full.lik = full.likelihood.instructions$instantiate.likelihood('ehe', location = LOCATION,
                                                               data.manager = SURVEILLANCE.MANAGER)


exp(full.lik$compute(sim.manual,check.consistency=F) - full.lik$compute(sim,check.consistency=F)) 
exp(full.lik$compute.piecewise(sim.manual,check.consistency=F) - full.lik$compute.piecewise(sim,check.consistency=F)) 
full.lik$compute(sim.manual,check.consistency=F)
full.lik$compute.piecewise(sim.manual,check.consistency=F)