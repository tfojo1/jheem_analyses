source('../jheem_analyses/applications/EHE/ehe_specification.R')

load("../jheem_analyses/prelim_results/init.transmission.sim_2024-02-28_C.12580.Rdata")
sim.old = sim

load("../jheem_analyses/prelim_results/init.transmission.sim_2024-02-29_C.12580.Rdata")
sim.new = sim

rm(sim)

params.old = sim.old$parameters[,1]
params.new = sim.new$parameters[,1]
# params.manual = params.new

round(cbind(params.old[c(par.names.pop,par.names.transmission)],params.new[c(par.names.pop,par.names.transmission)]),2)
(cbind(params.old[c(par.names.pop,par.names.transmission)],params.new[c(par.names.pop,par.names.transmission)]))

engine = create.jheem.engine('ehe',sim.new$location,end.year = 2030) 

params.manual = params.new
# other.idu.trate.mask = grepl("other\\.idu\\.trate",names(params.manual))
# other.het.trate.mask = grepl("other\\.heterosexual\\.trate",names(params.manual))
# params.manual[other.idu.trate.mask | other.het.trate.mask] = 0
# params.manual["peak.hiv.mortality"] = 0.95*params.new["peak.hiv.mortality"] # probably have to also decrease transmission
sim.manual = engine$run(parameters = params.manual) 

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
        facet.by = "risk", # heterosexual/msm overshooting
        outcomes = c("aids.diagnoses"),
        dimension.values = list(year = 1981:2020))

simplot(sim.old,
        sim.new,
        facet.by = "race", # hispanic undershooting (but small #s)
        outcomes = c("aids.diagnoses"),
        dimension.values = list(year = 1981:2020))

simplot(sim.old,
        sim.new,
        outcomes = c("aids.deaths"), 
        dimension.values = list(year = 1981:2020))

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

# Two-way plots; prevalence, new (race/risk); population (age/race)
simplot(sim.old,
        sim.new,
        facet.by = "risk", split.by = "race", # black msm overshooting; idu and msm-idu all undershooting
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020)) 

simplot(sim.old,
        sim.new,
        facet.by = "risk", split.by = "sex", # black msm overshooting; idu and msm-idu all undershooting
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


lik = two.way.transmission.pop.likelihood.instructions$instantiate.likelihood('ehe', sim.new$location)
(lik$compute(sim.new,check.consistency=F) - lik$compute(sim.old,check.consistency=F)) 
(lik$compute.piecewise(sim.new,check.consistency=F) - lik$compute.piecewise(sim.old,check.consistency=F)) 

# simplot(sim,
#         facet.by = "age", split.by = "race",
#         outcomes = c("diagnosed.prevalence"), 
#         dimension.values = list(year = 2000:2020))
# 
# simplot(sim,
#         facet.by = "age", split.by = "race",
#         outcomes = c("new"),
#         dimension.values = list(year = 2000:2020)) 
