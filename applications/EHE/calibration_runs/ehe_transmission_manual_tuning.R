source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

# load initial transmission mcmc for BALTIMORE - doing this instead of NYC 
load("../jheem_analyses/prelim_results/init.transmission.sim_2024-02-02_C.12580.Rdata") 

# rerun sim with parameters from mcmc, because we have changed parameters since the mcmc was run
params.mcmc.0 = sim$parameters[,1]
engine = create.jheem.engine('ehe',sim$location,end.year = 2030) #comment out lines 1723-1737 of engine before running this
sim.mcmc.0 = engine$run(parameters = params.mcmc.0) 

# check to make sure they have diff outputs, even though they use the same params - can't actually plot original sim anymore
simplot(#sim, 
        sim.mcmc.0, 
        facet.by = "risk", 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020))

simplot(sim.mcmc.0, facet.by = "risk", outcomes = "aids.diagnoses")
simplot(sim.mcmc.0, outcomes = c("aids.diagnoses","new","diagnosed.prevalence"), dimension.values = list(year = 1980:2020))

# manually play with parameters, rerun, compare to sim.mcmc.0 
msm.trate.0.mask = grepl("msm\\.trate\\.0",names(params.mcmc.0))
msm.trate.1.mask = grepl("msm\\.trate\\.1",names(params.mcmc.0))
msm.trate.2.mask = grepl("msm\\.trate\\.2",names(params.mcmc.0))

het.trate.0.mask = grepl("heterosexual\\.trate\\.0",names(params.mcmc.0))
het.trate.1.mask = grepl("heterosexual\\.trate\\.1",names(params.mcmc.0))
het.trate.2.mask = grepl("heterosexual\\.trate\\.2",names(params.mcmc.0))

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

save(params.manual,file = "applications/EHE/calibration_runs/params.manual_2024_02_13.Rdata")

sim.manual = engine$run(parameters = params.manual)

simplot(#sim.mcmc.0, 
        sim.manual,
        facet.by = "risk", 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020)) # look from 1981 to start, then zoom in from 2000

simplot(sim.mcmc.0, sim.manual,
        facet.by = "risk", 
        outcomes = c("aids.diagnoses"),
        dimension.values = list(year = 1981:2010))

simplot(sim.mcmc.0, sim.manual,
        facet.by = "risk", 
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020))

simplot(#sim.mcmc.0, 
        sim.manual,
        facet.by = "risk", split.by = "race", 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020)) 

simplot(sim.mcmc.0, sim.manual,
        facet.by = "risk", split.by = "race", 
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020)) 


# compare likelihoods
trans.lik = one.way.transmission.likelihood.instructions$instantiate.likelihood('ehe', sim$location)
new.lik = race.risk.one.way.new.diagnoses.likelihood.instructions$instantiate.likelihood('ehe', sim$location)
(new.lik$compute(sim.manual,check.consistency=F) - new.lik$compute(sim.mcmc.0,check.consistency=F)) 
(trans.lik$compute(sim.manual,check.consistency=F) - trans.lik$compute(sim.mcmc.0,check.consistency=F)) 
(trans.lik$compute.piecewise(sim.manual,check.consistency=F) - trans.lik$compute.piecewise(sim.mcmc.0,check.consistency=F)) 


SURVEILLANCE.MANAGER$pull(outcome = "aids.diagnoses", dimension.values = list(location = "C.35620"))
rowSums(SURVEILLANCE.MANAGER$pull(outcome = "aids.diagnoses", dimension.values = list(location = "C.35620"),
                          keep.dimensions = c("year","risk")))

# OLD NYC PARAMETERS - WHEN AIDS DIAGNOSES LOOKED WEIRD 
if(1==2){
  # params.manual[msm.trate.0.mask] = .75*params.mcmc.0[msm.trate.0.mask]
  # params.manual[msm.trate.1.mask] = 2.2*params.mcmc.0[msm.trate.1.mask]
  # params.manual[msm.trate.2.mask] = 2.65*params.mcmc.0[msm.trate.2.mask]
  
  params.manual["other.msm.trate.0"] = 0.25*params.mcmc.0["other.msm.trate.0"]
  params.manual["black.msm.trate.0"] = 0.75*params.mcmc.0["black.msm.trate.0"]
  params.manual["hispanic.msm.trate.0"] = 0.75*params.mcmc.0["hispanic.msm.trate.0"]
  
  params.manual["other.msm.trate.1"] = 0.1*params.mcmc.0["other.msm.trate.1"]
  params.manual["black.msm.trate.1"] = 2.25*params.mcmc.0["black.msm.trate.1"]
  params.manual["hispanic.msm.trate.1"] = 3.25*params.mcmc.0["hispanic.msm.trate.1"]
  
  params.manual["other.msm.trate.2"] = 0.5*params.mcmc.0["other.msm.trate.2"]
  params.manual["black.msm.trate.2"] = 3*params.mcmc.0["black.msm.trate.2"]
  params.manual["hispanic.msm.trate.2"] = 3.25*params.mcmc.0["hispanic.msm.trate.2"]
  
  params.manual[het.trate.0.mask] = .2*params.mcmc.0[het.trate.0.mask]
  params.manual[het.trate.1.mask] = .0000000000000001*params.mcmc.0[het.trate.1.mask]
  params.manual[het.trate.2.mask] = .0000000000000001*params.mcmc.0[het.trate.2.mask]
}