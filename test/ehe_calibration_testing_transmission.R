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

## 2/1 testing


simplot(sim,
        outcomes = c("new","diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020))

simplot(sim.nyc,
        facet.by = "risk",
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020))

simplot(sim,
        facet.by = "risk",split.by = "race",
        outcomes = c("new","diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020))

simplot(sim.nyc,
        facet.by = "age",split.by = "race",
        outcomes = c("population"),
        dimension.values = list(year = 2000:2020))

round(sim$parameters[par.names.transmission,],4)

new.lik = race.risk.one.way.new.diagnoses.likelihood.instructions$instantiate.likelihood('ehe', sim.nyc$location)

params = sim.nyc$parameters[,1]

engine = create.jheem.engine('ehe',sim.nyc$location,end.year = 2030)

sim.nyc.new = engine$run(parameters = params)

(new.lik$compute(sim.nyc.new,check.consistency=F) - new.lik$compute(sim.nyc,check.consistency=F))


simplot(sim.nyc, sim.nyc.new,
        facet.by = "risk", 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020))

simplot(sim,
        outcomes = c("aids.diagnoses"),
        dimension.values = list(year = 1980:2001))


msm.trate.mask = grepl("msm\\.trate",names(params))
msm.trate.0.mask = grepl("msm\\.trate\\.0",names(params))
msm.trate.1.mask = grepl("msm\\.trate\\.1",names(params))
msm.trate.2.mask = grepl("msm\\.trate\\.2",names(params))

het.trate.0.mask = grepl("heterosexual\\.trate\\.0",names(params))
het.trate.1.mask = grepl("heterosexual\\.trate\\.1",names(params))
het.trate.2.mask = grepl("heterosexual\\.trate\\.2",names(params))

params = sim.nyc$parameters[,1]
params.new = params
params.new[msm.trate.0.mask] = 1.15*params[msm.trate.0.mask]
params.new[msm.trate.1.mask] = 1.05*params[msm.trate.1.mask]
params.new[msm.trate.2.mask] = 2*params[msm.trate.2.mask]
params.new[het.trate.0.mask] = .5*params[het.trate.0.mask]
params.new[het.trate.1.mask] = .01*params[het.trate.1.mask]
params.new[het.trate.2.mask] = .01*params[het.trate.2.mask]
# params.new["fraction.heterosexual.male.pairings.with.male"] = 0 # try to tune without this 
sim.nyc.new = engine$run(parameters = params.new)





