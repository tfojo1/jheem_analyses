source('../jheem_analyses/applications/EHE/ehe_specification.R')

LOCATION = BALTIMORE.MSA # HOUSTON.MSA

load("../jheem_analyses/prelim_results/init.transmission.sim_2024-02-23_C.12580.Rdata")
sim.baltimore = sim

load("../jheem_analyses/prelim_results/init.transmission.sim_2024-02-23_C.26420.Rdata")
sim.houston = sim

rm(sim)

sim=sim.houston

params = sim.baltimore$parameters[,1]
# params.manual = params

engine.baltimore = create.jheem.engine('ehe',sim.baltimore$location,end.year = 2030) 
engine.houston = create.jheem.engine('ehe',sim.houston$location,end.year = 2030) 

# sim.manual = engine$run(parameters = params.manual) 

# EHE.PARAMETERS.PRIOR@var.names[grepl('incident.idu', EHE.PARAMETERS.PRIOR@var.names)]
# and msm.vs.het...

# One-way plots (by risk or race): prevalence, new, AIDS diagnoses 
simplot(sim,
        facet.by = "risk", # idu and msm-idu undershooting 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2020))

simplot(sim,
        facet.by = "race", # hispanic undershooting (and black historically)
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020)) 

simplot(sim,
        facet.by = "risk", # pretty good 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020)) 

simplot(sim,
        facet.by = "race", # hispanic undershooting (but small #s), other overshooting
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020)) 

simplot(sim,
        facet.by = "risk", # heterosexual/msm overshooting
        outcomes = c("aids.diagnoses"),
        dimension.values = list(year = 1981:2020))

simplot(sim,
        facet.by = "race", # hispanic undershooting (but small #s)
        outcomes = c("aids.diagnoses"),
        dimension.values = list(year = 1981:2020))

# Two-way plots; prevalence, new (race/risk); population (age/race)
simplot(sim,
        facet.by = "risk", split.by = "race", # black msm overshooting; idu and msm-idu all undershooting
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020)) 

simplot(sim,
        facet.by = "risk", split.by = "race", # not bad 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2020)) 

# simplot(sim,
#         facet.by = "age", split.by = "race",
#         outcomes = c("diagnosed.prevalence"), 
#         dimension.values = list(year = 2000:2020))
# 
# simplot(sim,
#         facet.by = "age", split.by = "race",
#         outcomes = c("new"),
#         dimension.values = list(year = 2000:2020)) 

simplot(sim,
        facet.by = "age", split.by = "race", # good 
        outcomes = c("population"),
        dimension.values = list(year = 2000:2020)) 

round(cbind(params[c(par.names.pop,par.names.transmission)]),2)

