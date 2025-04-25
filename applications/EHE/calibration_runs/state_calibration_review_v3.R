source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum")

load("prelim_results/full.ehe.state_simset_2025-04-18_NY.Rdata")
simset.ny.full = simset

load("prelim_results/trans.ehe.state_simset_2025-04-17_NY.Rdata")
simset.ny.trans = simset

load("prelim_results/full.ehe.state_simset_2025-04-18_FL.Rdata")
simset.fl.full = simset

load("prelim_results/trans.ehe.state_simset_2025-04-17_FL.Rdata")
simset.fl.trans = simset

# full.lik.fl = full.state.likelihood.instructions$instantiate.likelihood('ehe','FL')
# full.lik.ny = full.state.likelihood.instructions$instantiate.likelihood('ehe','NY')
# 
# trans.lik.fl = trans.state.likelihood.instructions$instantiate.likelihood('ehe','FL')
# trans.lik.ny = trans.state.likelihood.instructions$instantiate.likelihood('ehe','NY')

full.lik.fl$compare.sims(simset.fl.trans$last.sim(),simset.fl.full$last.sim())

engine = create.jheem.engine('ehe','FL',end.year = 2030)

sim.full = simset.fl.full$last.sim()
params.full = sim.full$params
params.manual = params.full
params.manual["male.vs.female.heterosexual.rr"] = 1 # 0.81239261

sim.manual = engine$run(parameters = params.manual)

full.lik.fl$compare.sims(sim.full,sim.manual,piecewise = F)

simplot(sim.full,
        sim.manual,
        facet.by = "age", split.by = "sex", 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

simplot(sim.full,
        sim.manual,
     #   facet.by = "age", split.by = "sex", 
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 

simplot(sim.full,
        sim.manual,
        facet.by = "risk", split.by = "race",
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

simplot(sim.full,
        sim.manual,
        facet.by = "risk", split.by = "race",
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 


simplot(simset.fl.trans$last.sim(),
        simset.fl.full$last.sim(),
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

# is this really 10^48x better? 
simplot(simset.fl.trans$last.sim(),
        simset.fl.full$last.sim(),
        outcomes = c("new"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 


# trans calibration ended in a pretty good place (good for FL, okay for NY)
simplot(simset.fl.trans$last.sim(),
        simset.fl.trans,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

# but then full calibration got worse - why? 
simplot(simset.fl.full$last.sim(),
        simset.fl.full,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 


# msm x race got better (by bringing down other); black heterosexual still under (+ idu, msm-idu)
simplot(simset.fl.trans$last.sim(),
        simset.fl.full$last.sim(),
        facet.by = "risk", split.by = "race", # race, risk, sex; 1- and 2-way 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

# female heterosexual still under (+ idu, msm-idu)
simplot(simset.fl.trans$last.sim(),
        simset.fl.full$last.sim(),
        facet.by = "risk", split.by = "sex", # race, risk, sex; 1- and 2-way 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

# improved male x age - because of these trends, can't have an early peak? has to be among women***
simplot(simset.fl.trans$last.sim(),
        simset.fl.full$last.sim(),
        facet.by = "age", split.by = "sex", # race, risk, sex; 1- and 2-way 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

