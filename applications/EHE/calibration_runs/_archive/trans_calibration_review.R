source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

load("../jheem_analyses/prelim_results/init.transmission.sim_2024-03-21_C.37980.Rdata")
sim.philadelphia = sim

load("../jheem_analyses/prelim_results/init.transmission.sim_2024-03-21_C.12060.Rdata")
sim.atlanta = sim

load("../jheem_analyses/prelim_results/init.transmission.sim_2024-03-21_C.33100.Rdata")
sim.miami = sim


params.philadelphia = sim.philadelphia$parameters[,1]
params.atlanta = sim.atlanta$parameters[,1]
params.miami = sim.miami$parameters[,1]


round(cbind(params.philadelphia[c(par.names.pop,par.names.transmission)],
            params.atlanta[c(par.names.pop,par.names.transmission)],
            params.miami[c(par.names.pop,par.names.transmission)]),4)

sim = sim.miami

# One-way plots (by risk or race): prevalence, new, AIDS diagnoses 
simplot(sim,
        facet.by = "risk", 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2020)) + ggtitle(locations::get.location.name(sim$location))

simplot(sim,
        facet.by = "race", 
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020)) + ggtitle(locations::get.location.name(sim$location))

simplot(sim,
        facet.by = "risk", 
        outcomes = c("aids.diagnoses"),
        dimension.values = list(year = 1981:2020))+ ggtitle(locations::get.location.name(sim$location))

simplot(sim,
        facet.by = "race", 
        outcomes = c("aids.diagnoses"),
        dimension.values = list(year = 1981:2020))+ ggtitle(locations::get.location.name(sim$location))

simplot(sim,
        outcomes = c("aids.deaths"), 
        dimension.values = list(year = 1981:2020)) + ggtitle(locations::get.location.name(sim$location))

simplot(sim,
        facet.by = "risk", 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020)) + ggtitle(locations::get.location.name(sim$location))

simplot(sim,
        facet.by = "race",
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020)) + ggtitle(locations::get.location.name(sim$location))

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

simplot(sim,
        facet.by = "age", split.by = "race", 
        outcomes = c("population"),
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))




## 3/25: Checking which HIV parameter is affecting population so much

# Atlanta
engine.atlanta = create.jheem.engine('ehe',sim.atlanta$location,end.year = 2030) 
params.atlanta.manual = params.atlanta
params.atlanta.manual["other.incident.idu.multiplier.0"] = 1 #56.0986 - fixes it to an extent (but 55+ is worse)
sim.atlanta.manual = engine.atlanta$run(parameters = params.atlanta.manual) 

simplot(sim.atlanta,
        sim.atlanta.manual,
        facet.by = "age", split.by = "race", 
        outcomes = c("population"),
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim.atlanta$location))

# Miami
simplot(sim.miami, 
        facet.by = "age", split.by = "race", 
        outcomes = c("population"),
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim.miami$location))

engine.miami = create.jheem.engine('ehe',sim.miami$location,end.year = 2030) 

# load original population calibraiton file to check fit 
load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-03-19_C.33100.Rdata")
params.miami.pop.calibration = sim$parameters[,1]

sim.pop.calibration = engine.miami$run(parameters = params.miami.pop.calibration) 

# used to fit well! 
simplot(sim.pop.calibration, 
        facet.by = "age", split.by = "race", 
        outcomes = c("population"),
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim.miami$location))

# population parameters aren't the same - they should be though; not sure why this happened but set them back to original
round(cbind(params.miami[c(par.names.pop)],
            params.miami.pop.calibration[c(par.names.pop)]),4)

# MANUAL = population parameters from original population calibration; transmission parameters from this calibration 
params.miami.manual = params.miami
params.miami.manual[par.names.pop] = params.miami.pop.calibration[par.names.pop]
sim.miami.manual = engine.miami$run(parameters = params.miami.manual) 

round(cbind(params.miami.pop.calibration[c(par.names.pop,par.names.transmission)],
            params.miami[c(par.names.pop,par.names.transmission)],
            params.miami.manual[c(par.names.pop,par.names.transmission)]),4)

simplot(sim.pop.calibration, 
        sim.miami,
        sim.miami.manual,
        facet.by = "age", split.by = "race", 
        outcomes = c("population"),
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim.miami$location))

params.miami.manual["other.incident.idu.multiplier.2"] = 1 # 6.8758 tiny change among 25-34 and 35-44
params.miami.manual["msm.incident.idu.multiplier.0"] = 1 # 7.4139
params.miami.manual["hispanic.idu.trate.2"] = 1 # 73.4296
params.miami.manual["hispanic.idu.trate.1"] = 1 # 13.5155
params.miami.manual["hispanic.idu.trate.0"] = 1 # 4.2381
params.miami.manual["hispanic.incident.idu.multiplier.0"] = 1 # 1.0655
params.miami.manual["hispanic.sexual.assortativity.multiplier"] = 1.0143 # 0.4337
params.miami.manual["other.incident.idu.multiplier.0"] = 1 # 1.5477
params.miami.manual["hispanic.incident.idu.multiplier.2"] = 1 # 1.1388
params.miami.manual["black.sexual.assortativity.multiplier"] = 1.0143 # 8.9728
params.miami.manual["race.needle.sharing.assortativity.multiplier"] = 1.0143 # 2.7837
params.miami.manual["hispanic.msm.trate.2"] = 1 # 6.2038
params.miami.manual["idu.peak.trate.multiplier"] = 4.7000 # 3.1916
params.miami.manual["global.trate"] = 0.1 # 0.0692
params.miami.manual["black.msm.trate.0"] = 1 # 1.1642
params.miami.manual["black.msm.trate.1"] = 1 # 2.3977
params.miami.manual["msm.vs.heterosexual.male.idu.susceptibility.rr.peak"] = 3.3 # 1.5399
params.miami.manual["other.sexual.assortativity.multiplier"] = 1.0143 # 3.7893
params.miami.manual["msm.incident.idu.multiplier.2"] = 1 # 1.8988
params.miami.manual["hispanic.heterosexual.trate.0"] = 1 # 0
params.miami.manual["black.incident.idu.multiplier.0"] = 1 # 2.1704
params.miami.manual["other.idu.trate.0"] = 1 # 0
params.miami.manual["other.idu.trate.1"] = 1 # 0
params.miami.manual["other.idu.trate.2"] = 1 # 0
params.miami.manual["msm.vs.heterosexual.male.idu.susceptibility.rr.0"] = 3.3 # 5.7647
params.miami.manual["hispanic.msm.trate.1"] = 1 # 2.9421
params.miami.manual["black.msm.trate.2"] = 1 # 3.5820
params.miami.manual["msm.vs.heterosexual.male.idu.susceptibility.rr.1"] = 3.3 # 4.9760
params.miami.manual["other.msm.trate.0"] = 1 # 0.2109
params.miami.manual["heterosexual.peak.trate.multiplier"] = 2.2 # 0.0305 THIS MADE A DIFFERENCE
params.miami.manual["msm.peak.trate.multiplier"] = 3.1 # 3.2725 
params.miami.manual["other.msm.trate.2"] = 1 # 0.1714 
params.miami.manual["msm.vs.heterosexual.male.idu.susceptibility.rr.2"] = 3.3 # 1.2281 
params.miami.manual["black.heterosexual.trate.1"] = 1 # 0.3929 
params.miami.manual["other.heterosexual.trate.0"] = 1 # 0 THIS MADE A DIFFERENCE
params.miami.manual["other.heterosexual.trate.1"] = 0.5 # 0 THIS MADE A DIFFERENCE - overshot

sim.miami.manual = engine.miami$run(parameters = params.miami.manual) 


# Philadelphia
engine.philadelphia = create.jheem.engine('ehe',sim.philadelphia$location,end.year = 2030) # triggers an error


