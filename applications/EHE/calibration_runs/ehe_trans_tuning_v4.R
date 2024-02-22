source('../jheem_analyses/applications/EHE/ehe_specification.R')

load("../jheem_analyses/prelim_results/init.transmission.sim_2024-02-22_C.12580.Rdata")
sim.new = sim

load("../jheem_analyses/prelim_results/init.transmission.sim_2024-02-21_C.12580.Rdata")
sim.old = sim

rm(sim)

params = sim.new$parameters[,1]

params.manual = params
engine = create.jheem.engine('ehe',sim.new$location,end.year = 2030) 

if(1==2){
  # I am having a hard time making any improvements with these - going to just give them to be sampled instead 
  save(params.manual,file="applications/EHE/calibration_runs/params.manual_2024_02_21.Rdata")
  params.manual["msm.incident.idu.multiplier.0"] = 0.9*params["msm.incident.idu.multiplier.0"]
  params.manual["black.incident.idu.multiplier.0"] = 1*params["black.incident.idu.multiplier.0"]
  params.manual["other.incident.idu.multiplier.0"] = 0.75*params["other.incident.idu.multiplier.0"]
  
  params.manual["msm.vs.heterosexual.male.idu.susceptibility.rr.0"] = 1*params["msm.vs.heterosexual.male.idu.susceptibility.rr.0"]
  params.manual["msm.vs.heterosexual.male.idu.susceptibility.rr.1"] = 1*params["msm.vs.heterosexual.male.idu.susceptibility.rr.1"]
  params.manual["msm.vs.heterosexual.male.idu.susceptibility.rr.peak"] = 1.05*params["msm.vs.heterosexual.male.idu.susceptibility.rr.peak"]
  
}

sim.manual = engine$run(parameters = params.manual) 

# EHE.PARAMETERS.PRIOR@var.names[grepl('incident.idu', EHE.PARAMETERS.PRIOR@var.names)]
# and msm.vs.het...

# One-way plots (by risk or race): prevalence, new, AIDS diagnoses 
simplot(sim.old,
        sim.new,
        facet.by = "risk", # idu and msm-idu undershooting 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2020))

simplot(sim.old,
        sim.new,
        facet.by = "race", # hispanic undershooting (and black historically)
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020)) 

simplot(sim.old,
        sim.new,
        facet.by = "risk", # pretty good 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020)) 

simplot(sim.old,
        sim.new,
        facet.by = "race", # hispanic undershooting (but small #s), other overshooting
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020)) 

simplot(sim.old,
        sim.new,
        facet.by = "risk", # heterosexual/msm overshooting
        outcomes = c("aids.diagnoses"),
        dimension.values = list(year = 1981:2020))

simplot(sim.old,
        sim.new,
        facet.by = "race", # hispanic undershooting (but small #s)
        outcomes = c("aids.diagnoses"),
        dimension.values = list(year = 1981:2020))

# Two-way plots; prevalence, new (race/risk); population (age/race)
simplot(sim.old,
        sim.new,
        facet.by = "risk", split.by = "race", # black msm overshooting; idu and msm-idu all undershooting
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020)) 

simplot(sim.old,
        sim.new,
        facet.by = "risk", split.by = "race", # not bad 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020)) 

simplot(sim.old,
        sim.new,
        facet.by = "age", split.by = "race",
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2020))

simplot(sim.old,
        sim.new,
        facet.by = "age", split.by = "race",
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020)) 

simplot(sim.old,
        sim.new,
        facet.by = "age", split.by = "race", # good 
        outcomes = c("population"),
        dimension.values = list(year = 2000:2020)) 

round(cbind(params[c(par.names.pop,par.names.transmission)]),2)

