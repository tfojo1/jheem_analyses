source('../jheem_analyses/applications/EHE/ehe_specification.R')

# start with population calibration parameters
load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-01-25_C.12580.Rdata")
load("../jheem_analyses/applications/EHE/calibration_runs/params.old.baltimore.Rdata")

sim.pop = sim
params.pop = sim.pop$parameters[,1]

shared.names = intersect(names(params.old),names(params.pop))

reference.params = params.pop
reference.params[shared.names] = params.old[shared.names]

engine = create.jheem.engine('ehe',sim$location,end.year = 2030) 

sim.old.params = engine$run(parameters = reference.params) 

params.manual = reference.params

params.manual[het.mask] = 0.125*reference.params[het.mask]
params.manual["other.other.sexual.oe"] = 50 # 1.593826
params.manual["black.black.sexual.oe"] = 50 # 3.16107

sim.manual = engine$run(parameters = params.manual) 


# One-way plots (by risk or race): AIDS, prevalence, new
simplot(sim.old.params,
        sim.manual,
        facet.by = "risk", 
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020))

simplot(sim.old.params,
        sim.manual,
        facet.by = "race",
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020)) 

simplot(sim,
        facet.by = "risk", 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020)) 

simplot(sim,
        facet.by = "race",
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020)) 

simplot(sim,
        sim.params.old,
        facet.by = "risk", 
        outcomes = c("aids.diagnoses"),
        dimension.values = list(year = 1981:2020))

simplot(sim,
        sim.params.old,
        facet.by = "race", 
        outcomes = c("aids.diagnoses"),
        dimension.values = list(year = 1981:2020))

# Two-way plots (race/risk); new + prevalence
simplot(sim,
        facet.by = "risk", split.by = "race",
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020)) 

simplot(sim,
        facet.by = "risk", split.by = "race",
        outcomes = c("incidence"),
        dimension.values = list(year = 2000:2020)) 

simplot(sim,
        sim.params.old,
        facet.by = "risk", split.by = "race",
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020)) 

simplot(sim,
        facet.by = "age", split.by = "race",
        outcomes = c("population"),
        dimension.values = list(year = 2000:2020)) 



