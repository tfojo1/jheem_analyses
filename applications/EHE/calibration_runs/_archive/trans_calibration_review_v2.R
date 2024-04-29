source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

# load("../jheem_analyses/prelim_results/init.transmission.sim_2024-04-02_C.12060.Rdata")
# sim.atlanta = sim
# 
# load("../jheem_analyses/prelim_results/init.transmission.sim_2024-04-02_C.33100.Rdata")
# sim.miami = sim
# 
# load("../jheem_analyses/prelim_results/init.transmission.sim_2024-04-02_C.37980.Rdata")
# sim.philadelphia = sim
# 
# load("../jheem_analyses/prelim_results/init.transmission.sim_2024-04-02_C.42660.Rdata")
# sim.seattle = sim

load("../jheem_analyses/prelim_results/init.transmission.sim_2024-03-29_C.12580.Rdata")


simplot(sim,
        facet.by = "risk", 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))

simplot(sim,
        facet.by = "race", 
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))

simplot(sim,
        facet.by = "risk", 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))

simplot(sim,
        facet.by = "race",
        outcomes = c("new"),
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))

# Two-way plots; prevalence, new (race/risk); population (age/race)
simplot(sim,
        facet.by = "risk", split.by = "race", 
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))

simplot(sim,
        facet.by = "risk", split.by = "sex", 
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))

simplot(sim,
        facet.by = "risk", split.by = "race", 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))

simplot(sim,
        facet.by = "age", split.by = "race", 
        outcomes = c("population"),
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))

# Extra
simplot(sim,
        facet.by = "risk", 
        outcomes = c("aids.diagnoses"),
        dimension.values = list(year = 1981:2030))+ ggtitle(locations::get.location.name(sim$location))

simplot(sim,
        facet.by = "race", 
        outcomes = c("aids.diagnoses"),
        dimension.values = list(year = 1981:2030))+ ggtitle(locations::get.location.name(sim$location))

simplot(sim,
        outcomes = c("aids.deaths"), 
        dimension.values = list(year = 1981:2030)) + ggtitle(locations::get.location.name(sim$location))


