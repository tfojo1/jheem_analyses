source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

# load initial transmission mcmc for BALTIMORE 
load("../jheem_analyses/prelim_results/init.transmission.sim_2024-02-13_C.12580.Rdata") 
sim.new.mcmc = sim

load("../jheem_analyses/prelim_results/init.transmission.sim_2024-02-02_C.12580.Rdata") 
sim.old.mcmc = sim

# rerun old mcmc with the manual parameters
params.mcmc.0 = sim.old.mcmc$parameters[,1]
engine = create.jheem.engine('ehe',sim.old.mcmc$location,end.year = 2030) #comment out lines 1723-1737 of engine before running this
sim.mcmc.0 = engine$run(parameters = params.mcmc.0) 


# manually play with parameters, rerun, compare to sim.mcmc.0 
params.manual = params.mcmc.0

params.manual["black.msm.trate.0"] = 1.15*params.mcmc.0["black.msm.trate.0"]
params.manual["black.msm.trate.1"] = 5*params.mcmc.0["black.msm.trate.1"]
params.manual["black.msm.trate.2"] = 3*params.mcmc.0["black.msm.trate.2"]

params.manual["other.msm.trate.0"] = 0.03*params.mcmc.0["other.msm.trate.0"]
params.manual["other.msm.trate.1"] = 0.005*params.mcmc.0["other.msm.trate.1"]
params.manual["other.msm.trate.2"] = 0.005*params.mcmc.0["other.msm.trate.2"]

params.manual["black.heterosexual.trate.0"] = 10000*params.mcmc.0["black.heterosexual.trate.0"] # have to set very high 
params.manual["black.heterosexual.trate.1"] = 10000*params.mcmc.0["black.heterosexual.trate.1"] 
params.manual["black.heterosexual.trate.2"] = 500*params.mcmc.0["black.heterosexual.trate.2"] 

params.manual["other.heterosexual.trate.0"] = 0.5*params.mcmc.0["other.heterosexual.trate.0"] 
params.manual["other.heterosexual.trate.1"] = 0.1*params.mcmc.0["other.heterosexual.trate.1"] 
params.manual["other.heterosexual.trate.2"] = 0.05*params.mcmc.0["other.heterosexual.trate.2"] 

params.manual["black.idu.trate.0"] = 1.15*params.mcmc.0["black.idu.trate.0"]
params.manual["black.idu.trate.1"] = 0.5*params.mcmc.0["black.idu.trate.1"]
params.manual["black.idu.trate.2"] = 0.5*params.mcmc.0["black.idu.trate.2"]

#params.manual["idu.peak.trate.multiplier"] # mutliplier of trate0, but applied in peak year (IDU - 1990; MSM - 1980)
#params.manual["msm.peak.trate.multiplier"]

sim.manual = engine$run(parameters = params.manual)

params.new.mcmc = sim.new.mcmc$parameters[,1]
params.manual.new = params.new.mcmc

params.manual.new["idu.peak.trate.multiplier"] = 8*params.new.mcmc["idu.peak.trate.multiplier"]
params.manual.new["heterosexual.peak.trate.multiplier"] = 0.22*params.new.mcmc["heterosexual.peak.trate.multiplier"]
params.manual.new["msm.peak.trate.multiplier"] = 0.85*params.new.mcmc["msm.peak.trate.multiplier"]

params.manual.new["hispanic.msm.trate.0"] = 3*params.new.mcmc["hispanic.msm.trate.0"]
params.manual.new["hispanic.msm.trate.1"] = 20*params.new.mcmc["hispanic.msm.trate.1"]
params.manual.new["hispanic.msm.trate.2"] = 20*params.new.mcmc["hispanic.msm.trate.2"]

params.manual.new["black.msm.trate.0"] = 0.9*params.new.mcmc["black.msm.trate.0"]
params.manual.new["black.msm.trate.1"] = 3.5*params.new.mcmc["black.msm.trate.1"]

sim.manual.new = engine$run(parameters = params.manual.new)

save(params.manual.new,file = "applications/EHE/calibration_runs/params.manual_2024_02_14.Rdata")

# One-way plots (by risk or race): AIDS, prevalence, new
simplot(sim.manual,
        sim.new.mcmc,
        sim.manual.new,
        facet.by = "risk", 
        outcomes = c("aids.diagnoses"),
        dimension.values = list(year = 1981:2020))

simplot(sim.manual,
        sim.new.mcmc,
        sim.manual.new,
        facet.by = "race", 
        outcomes = c("aids.diagnoses"),
        dimension.values = list(year = 1981:2020))

simplot(sim.manual,
        sim.new.mcmc,
        sim.manual.new,
        facet.by = "risk", 
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020))

simplot(sim.manual,
        sim.new.mcmc,
        sim.manual.new,
        facet.by = "race",
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020)) 

simplot(sim.manual,
        sim.new.mcmc,
        sim.manual.new,
        facet.by = "risk", 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020)) 

simplot(sim.manual,
        sim.new.mcmc,
        sim.manual.new,
        facet.by = "race",
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020)) 

# Two-way plots (race/risk); new + prevalence
simplot(sim.manual,
        sim.new.mcmc,
        sim.manual.new,
        facet.by = "risk", split.by = "race",
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020)) 

simplot(sim.manual,
        sim.new.mcmc,
        sim.manual.new,
        facet.by = "risk", split.by = "race",
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020)) 




# compare likelihoods
trans.lik = one.way.transmission.likelihood.instructions$instantiate.likelihood('ehe', sim.new.mcmc$location)
(trans.lik$compute(sim.new.mcmc,check.consistency=F) - trans.lik$compute(sim.manual,check.consistency=F)) 
(trans.lik$compute.piecewise(sim.new.mcmc,check.consistency=F) - trans.lik$compute.piecewise(sim.manual,check.consistency=F)) 


SURVEILLANCE.MANAGER$pull(outcome = "aids.diagnoses", dimension.values = list(location = "C.35620"))
rowSums(SURVEILLANCE.MANAGER$pull(outcome = "aids.diagnoses", dimension.values = list(location = "C.35620"),
                                  keep.dimensions = c("year","risk")))

