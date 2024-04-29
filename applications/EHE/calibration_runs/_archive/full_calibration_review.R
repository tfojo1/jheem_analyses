source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

load("../jheem_analyses/prelim_results/init.full.minus.two.sim_2024-04-09_C.12580.Rdata")
sim.full = sim

load("../jheem_analyses/prelim_results/init.transmission.sim_2024-03-29_C.12580.Rdata")
sim.trans = sim

load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-03-29_C.12580.Rdata")
sim.pop = sim

cbind(round(sim.pop$parameters,3),
      round(sim.trans$parameters,3),
      round(sim.full$parameters,3))

#full.lik.without.supp = FULL.likelihood.instructions.minus.supp$instantiate.likelihood('ehe','C.12580')
round(exp(full.lik.without.supp$compute.piecewise(sim.full)),4)

# CREATING THE ENGINE CAUSES R TO ABORT???
# engine = create.jheem.engine('ehe',sim.full$location,end.year = 2030) 
params = sim.full$parameters
params.manual = params
sim.manual = engine$run(parameters = params.manual) 



# look until 2020 and then 2030 - takes off
simplot(sim.trans,
        #sim.full,
        facet.by = "risk", 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))

simplot(sim.trans,
        sim.full,
        facet.by = "race", 
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020)) + ggtitle(locations::get.location.name(sim$location))

simplot(sim.trans,
        sim.full,
        facet.by = "risk", 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020)) + ggtitle(locations::get.location.name(sim$location))

simplot(sim.trans,
        sim.full,
        facet.by = "race",
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020)) + ggtitle(locations::get.location.name(sim$location))

# this is obviously wrong
simplot(sim.pop,
        sim.trans,
        sim.full,
        facet.by = "age", split.by = "race", 
        outcomes = c("population"),
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))



# Two-way plots; prevalence, new (race/risk); population (age/race)
simplot(sim,
        facet.by = "risk", split.by = "race", 
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020)) + ggtitle(locations::get.location.name(sim$location))

simplot(sim,
        facet.by = "risk", split.by = "sex", 
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020)) + ggtitle(locations::get.location.name(sim$location))

simplot(sim,
        facet.by = "risk", split.by = "race", 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020)) + ggtitle(locations::get.location.name(sim$location))
