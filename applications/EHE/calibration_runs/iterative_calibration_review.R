source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

load("../jheem_analyses/prelim_results/pop_trans_mort_2024-04-13_C.12580.Rdata")
sim.pop.trans.mort = sim

load("../jheem_analyses/prelim_results/pop_trans_mort_idu_2024-04-13_C.12580.Rdata")
sim.pop.trans.mort.idu = sim

load("../jheem_analyses/prelim_results/pop_trans_mort_non_idu_2024-04-14_C.12580.Rdata")
sim.pop.trans.mort.non.idu = sim

load("../jheem_analyses/prelim_results/init.full.minus.supp_2024-04-14_C.12580.Rdata")
sim.full.minus.supp = sim

# all of the ones after pop.trans.mort are problematic
simplot(sim.pop.trans.mort,
        sim.pop.trans.mort.idu,
        facet.by = "age", split.by = "race", 
        outcomes = c("population"),
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))

simplot(sim.pop.trans.mort,
        sim.pop.trans.mort.idu,
        facet.by = "risk", 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))

simplot(sim.pop.trans.mort,
        sim.pop.trans.mort.idu,
        facet.by = "race", 
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))

simplot(sim.pop.trans.mort,
        sim.pop.trans.mort.idu,
        facet.by = "risk", 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))

simplot(sim.pop.trans.mort,
        sim.pop.trans.mort.idu,
        facet.by = "race",
        outcomes = c("new"),
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))


# Two-way plots; prevalence, new (race/risk); population (age/race)
simplot(sim.pop.trans.mort,
        sim.pop.trans.mort.idu,
        facet.by = "risk", split.by = "race", 
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))

simplot(sim.pop.trans.mort,
        sim.pop.trans.mort.idu,
        facet.by = "risk", split.by = "sex", 
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))

simplot(sim.pop.trans.mort,
        sim.pop.trans.mort.idu,
        facet.by = "risk", split.by = "race", 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))
