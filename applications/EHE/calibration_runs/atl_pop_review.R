source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum")

load("../jheem_analyses/prelim_results/init.pop.ehe_simset_2024-09-24_C.12060.Rdata")
sim.mcmc = simset$last.sim()

atl.pop.lik = joint.pop.migration.total.trans.likelihood.instructions$instantiate.likelihood('ehe','C.12060')

engine = create.jheem.engine('ehe','C.12060',end.year = 2025)
params.mcmc = sim.mcmc$params
sim.mcmc = engine$run(params.mcmc)

cbind(sim.mcmc$get.params("birth"))
cbind(sim.mcmc$get.params("mortality"))
cbind(sim.mcmc$get.params("migration"))
cbind(sim.mcmc$get.params("hispanic.immigration.multiplier.time"))


params.manual = params.mcmc
#sim.manual = engine$run(params.manual)
params.manual["hispanic.birth.rate.multiplier"] = 3 # 1.0143249
#params.manual["hispanic.emigration.multiplier.time.2"] = 1 # 0.04113772
#params.manual["hispanic.emigration.multiplier.time.1"] = 1 # 0.04695921

sim.manual = engine$run(params.manual)

atl.pop.lik$compare.sims(sim.mcmc,sim.manual) 
exp(atl.pop.lik$compute(sim.manual) - atl.pop.lik$compute(sim.mcmc))

simplot(sim.engine.test,
        sim.mcmc,
        sim.manual,
        facet.by = "age", split.by = "race",
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.mcmc,
        sim.manual,
        outcomes = c("new"), 
        style.manager = source.style.manager, 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.mcmc,
        sim.manual,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager, 
        dimension.values = list(year = 2000:2030)) 


sim.engine.test = engine$run(parameters = params)