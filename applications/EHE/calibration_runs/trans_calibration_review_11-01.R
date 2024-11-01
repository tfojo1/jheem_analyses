load("~/jheem/code/jheem_analyses/prelim_results/init.transmission.ehe_simset_2024-10-31_C.35620.Rdata")
simset.cs = simset

load("~/jheem/code/jheem_analyses/prelim_results/init.transmission.ehe_simset_2024-11-01_C.35620.Rdata")
simset.ar = simset


simplot(simset.cs$last.sim(),
        simset.ar$last.sim(),
        outcomes = c("new"), 
        style.manager = source.style.manager, 
        dimension.values = list(year = 1980:2030)) 

simplot(simset.cs$last.sim(),
        simset.ar$last.sim(),
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 


simplot(simset.cs$last.sim(),
        simset.ar$last.sim(),
        outcomes = c("aids.diagnoses"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

engine = create.jheem.engine('ehe','C.35620', end.year = 2025)
params = simset$last.sim()$params
params["hiv.general.mortality.multiplier"] = 1
params["unsuppressed.hiv.mortality.0"] = 1.75*0.03581967
params["unsuppressed.hiv.mortality.1"] = 1.75*(23/1000)



sim.manual = engine$run(parameters = params)

simplot(simset$last.sim(),
        sim.manual,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset$last.sim(),
        sim.manual,
        facet.by = "risk", split.by = "race", # race, risk, sex; 1- and 2-way 
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 1980:2030)) 

simplot(simset$last.sim(),
        sim.manual,
        facet.by = "age", split.by = "sex", # race, risk, sex; 1- and 2-way 
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 1980:2030)) 

#trans.lik = transmission.pop.idu.aware.aids.testing.likelihood.instructions$instantiate.likelihood('ehe','C.35620')
trans.lik$compare.sims(simset$last.sim(),sim.manual)
