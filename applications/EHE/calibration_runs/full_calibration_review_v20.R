source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum")

load("../jheem_analyses/prelim_results/full.with.aids_simset_2024-11-09_C.12580.Rdata")
#simset = copy.simulation.set(simset)

engine = create.jheem.engine('ehe','C.12580',end.year = 2025)

params.mcmc = simset.new$get.params()
sim.mcmc = engine$run(parameters = params.mcmc)

params.manual = params.mcmc
sim.manual = engine$run(parameters = params.manual)

#params.manual["black.msm.trate.0"] = 1 # 0.1559
sim.manual = engine$run(parameters = params.manual)

#nyc.lik = transmission.pop.idu.aware.aids.testing.likelihood.instructions$instantiate.likelihood('ehe','C.35620')

nyc.lik$compare.sims(sim.mcmc,sim.manual) # compare.sims(nyc.lik, sim.mcmc,sim.manual) # still second minus first 
exp(nyc.lik$compute(sim.manual) - nyc.lik$compute(sim.mcmc))


simplot(#simset.old$last.sim(),
        #simset.new,
        sim.mcmc,
        sim.manual,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(#simset.old$last.sim(),
        #sim.mcmc,
        sim.manual,
        facet.by = "risk", split.by = "race", 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

simplot(#simset.old$last.sim(),,
        sim.mcmc,
        sim.manual,
        outcomes = c("new"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(#simset.old$last.sim(),
        #sim.mcmc,
        sim.manual,
        facet.by = "risk", split.by = "race", 
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 

simplot(#simset.old$last.sim(),
        sim.mcmc,
        sim.manual,
        outcomes = c("aids.diagnoses"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(#simset.new,
        sim.mcmc,
        sim.manual,
        facet.by = "risk", 
        outcomes = c("aids.diagnoses"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim.mcmc,
        sim.manual,
        #facet.by = "sex", # sex; 1-way 
        outcomes = c("hiv.mortality"),
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim.mcmc,
        sim.manual,
        outcomes = c("aids.deaths"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1981:2001)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        simset.new,
        #facet.by = "age", # age; 1-way 
        outcomes = c("proportion.using.heroin",
                     "proportion.using.cocaine"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simset.old$traceplot("black.sexual.assortativity.multiplier")
simset.new$traceplot("black.sexual.assortativity.multiplier")

