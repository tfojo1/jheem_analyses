source('../jheem_analyses/applications/EHE/ehe_specification.R')

# start with population calibration parameters
load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-01-25_C.12580.Rdata")
sim.pop = sim
params.pop = sim.pop$parameters[,1]
params.pop = params.pop[EHE.PARAMETERS.PRIOR@var.names]

# load transmission parameters from old calibration 
load("../jheem_analyses/applications/EHE/calibration_runs/params.old.baltimore.Rdata")
shared.names = intersect(names(params.old),names(params.pop))

reference.params = params.pop
reference.params[shared.names] = params.old[shared.names]

# run sim with old transmission parameters and new population parameters
engine = create.jheem.engine('ehe',sim$location,end.year = 2030) 
sim.old.params = engine$run(parameters = reference.params) 

# start manually tweaking old transmission parameters 
params.manual = reference.params

het.trate.0.mask = grepl("heterosexual\\.trate\\.0",names(params.manual))
het.trate.1.mask = grepl("heterosexual\\.trate\\.1",names(params.manual))
het.trate.2.mask = grepl("heterosexual\\.trate\\.2",names(params.manual))
het.mask = het.trate.0.mask | het.trate.1.mask | het.trate.2.mask

msm.trate.mask = grepl("msm\\.trate",names(params.manual))

params.manual[het.mask] = 0.05*reference.params[het.mask]
params.manual["black.heterosexual.trate.0"] = 0.05 # 0.07228892
params.manual["black.heterosexual.trate.1"] = 0.04 # 0.0620035
params.manual["black.heterosexual.trate.2"] = 0.005 # 0.0545
params.manual["other.heterosexual.trate.0"] = 0.0005 # 0.01551151
params.manual["other.heterosexual.trate.1"] = 0.0005 # 0.003613145
params.manual["other.heterosexual.trate.2"] = 0.0005 # 0.002512622
#params.manual[het.trate.1.mask] = 0.05*reference.params[het.trate.1.mask]
#params.manual[het.trate.2.mask] = 0.05*reference.params[het.trate.2.mask]
params.manual["black.idu.trate.0"] = 2.5 # 1.930838
params.manual[msm.trate.mask] = 0.55*reference.params[msm.trate.mask]
  
params.manual["other.other.sexual.oe"] = 3 # 1.593826
params.manual["black.black.sexual.oe"] = 15 # 3.16107

save(params.manual,file="applications/EHE/calibration_runs/params.manual_2024_02_19.Rdata")

sim.manual = engine$run(parameters = params.manual) 

# One-way plots (by risk or race): AIDS, prevalence, new
simplot(#sim.old.params,
        sim.manual,
        facet.by = "risk", 
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020))

simplot(#sim.old.params,
        sim.manual,
        facet.by = "race",
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020)) 

simplot(sim.manual,
        facet.by = "risk", 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020)) 

simplot(sim.manual,
        facet.by = "race",
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020)) 

simplot(sim.manual,
        facet.by = "risk", 
        outcomes = c("aids.diagnoses"),
        dimension.values = list(year = 1981:2020))

simplot(sim.manual,
        facet.by = "race", 
        outcomes = c("aids.diagnoses"),
        dimension.values = list(year = 1981:2020))

# Two-way plots (race/risk); new + prevalence
simplot(sim.manual,
        facet.by = "risk", split.by = "race",
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020)) 

simplot(#sim.old.params,
        sim.manual,
        facet.by = "risk", split.by = "race",
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020)) 

simplot(sim.manual,
        facet.by = "age", split.by = "race",
        outcomes = c("population"),
        dimension.values = list(year = 2000:2020)) 



