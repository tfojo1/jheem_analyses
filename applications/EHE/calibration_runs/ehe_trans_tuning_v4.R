source('../jheem_analyses/applications/EHE/ehe_specification.R')

load("../jheem_analyses/prelim_results/init.transmission.sim_2024-02-23_C.12580.Rdata")
sim.old = sim

load("../jheem_analyses/prelim_results/init.transmission.sim_2024-02-24_C.12580.Rdata")
sim.new = sim

rm(sim)

params.old = sim.old$parameters[,1]
params.new = sim.new$parameters[,1]
# params.manual = params.new

round(cbind(params.old[c(par.names.pop,par.names.transmission)],params.new[c(par.names.pop,par.names.transmission)]),2)

engine = create.jheem.engine('ehe',sim.new$location,end.year = 2030) 

sim.test = engine$run(parameters = params.new) 

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
        facet.by = "age", split.by = "race", # good 
        outcomes = c("population"),
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
