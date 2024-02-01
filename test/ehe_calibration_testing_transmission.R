source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
library(bayesian.simulations)

# save(mcmc,file=paste0("prelim_results/init.transmission.mcmc_",Sys.Date(),"_",LOCATION,".Rdata")) # save mcmc 

load("../jheem_analyses/prelim_results/init.transmission.sim_2024-01-29_C.33100.Rdata") # Miami, two-way
sim.two.way = sim

load("../jheem_analyses/prelim_results/init.transmission.sim_2024-01-30_C.33100.Rdata") # Miami, one-way
sim.one.way = sim 

load("../jheem_analyses/prelim_results/init.transmission.mcmc_2024-01-31_C.33100.Rdata") # mcmc, to look at trace plot 

simplot(sim.two.way, 
        sim.one.way,
        outcomes = c("new","diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020))

simplot(sim.two.way, 
        sim.one.way,
        facet.by = "risk",
        outcomes = c("new","diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020))

simplot(#sim.two.way, 
        sim.one.way,
        facet.by = "risk",split.by = "race",
        outcomes = c("new","diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020))

simplot(sim.two.way, 
  sim.one.way,
  facet.by = "age",split.by = "race",
  outcomes = c("population"),
  dimension.values = list(year = 2000:2020))

round(sim.one.way$parameters[c(par.names.pop,par.names.transmission),],6)

trans.lik = one.way.transmission.likelihood.instructions$instantiate.likelihood('ehe', sim$location)
exp(trans.lik$compute.piecewise(sim.one.way,check.consistency=F) - trans.lik$compute.piecewise(sim.two.way,check.consistency=F)) 
exp(trans.lik$compute.piecewise(sim.one.way,check.consistency=F))

trace.plot(mcmc,var.names = "*trate")







