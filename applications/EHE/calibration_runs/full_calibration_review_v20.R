source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum")

load("../jheem_analyses/prelim_results/init.transmission.ehe_simset_2024-11-01_C.35620_AR_v1.Rdata")
simset= copy.simulation.set(simset)
simset.old = simset  

load("../jheem_analyses/prelim_results/init.transmission.ehe_simset_2024-11-04_C.35620.Rdata")
simset.new = simset

engine = create.jheem.engine('ehe','C.35620',end.year = 2025)

params.mcmc = simset.new$get.params()
sim.mcmc = engine$run(parameters = params.mcmc)

params.manual = params.mcmc
sim.manual = engine$run(parameters = params.manual)

cbind(sim.mcmc$get.params("prevalence"))
round(cbind(sim.mcmc$get.params("idu.trate")))
cbind(sim.mcmc$get.params("oe"))

params.manual["black.msm.trate.0"] = 1 # 0.1559

# these make heterosexual go up and msm go down; more than they change IDU? 
params.manual["black.active.idu.initial.prevalence.ratio"] = 2 # 1.42592516 
params.manual["hispanic.active.idu.initial.prevalence.ratio"] = 2.5 # 0.19291837 
params.manual["other.active.idu.initial.prevalence.ratio"] =.75 # 0.01671297
#params.manual["oe.never.idu.pairings.with.idu"] = 500 # 0.17134284 prior is at 0.2

#params.manual["black.idu.trate.peak"] = 20 # 20
#params.manual["black.idu.trate.0"] = 645 # 645
params.manual["black.idu.trate.1"] = 100 # 224
params.manual["black.idu.trate.2"] = 50 # 75

#params.manual["hispanic.idu.trate.peak"] = 120 # 120
params.manual["hispanic.idu.trate.0"] = 645 # 709
#params.manual["hispanic.idu.trate.1"] = 28 # 28
params.manual["hispanic.idu.trate.2"] = 30 # 86

params.manual["black.heterosexual.trate.peak"] = .15 # 0.6688023
#params.manual["black.heterosexual.trate.0"] # 0.0005632177
#params.manual["black.heterosexual.trate.1"] # 0.02997865
#params.manual["black.heterosexual.trate.2"] # 0.2110372


sim.manual = engine$run(parameters = params.manual)

#nyc.lik = transmission.pop.idu.aware.aids.testing.likelihood.instructions$instantiate.likelihood('ehe','C.35620')

# seems like benefit to prevalence should outweigh small changes to new/aids, but it doesn't; why does AIDS diagnoses score that much worse???
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


black.trates = intersect(names(params.manual[grepl("black",names(params.manual))]),
                         names(params.manual[grepl("trate",names(params.manual))]))

other.trates = intersect(names(params.manual[grepl("other",names(params.manual))]),
                         names(params.manual[grepl("trate",names(params.manual))]))

round(cbind(params.mcmc[black.trates],params.manual[black.trates]),4)
round(cbind(params.mcmc[other.trates],params.manual[other.trates]))
