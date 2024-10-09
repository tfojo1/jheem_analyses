source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum")

atl.lik = FULL.likelihood.instructions.with.aids$instantiate.likelihood('ehe','C.12060')

## Manually playing with Atlanta
load(paste0("../jheem_analyses/prelim_results/full.with.aids_simset_2024-10-09_C.12060.Rdata"))
simset.atlanta = simset
engine = create.jheem.engine('ehe','C.12060',end.year = 2025)
params.mcmc = simset.atlanta$last.sim()$params
params.mcmc[grepl("max.covid.effect",names(params.mcmc))] = 1 # gives an error if I don't do this 
sim.mcmc = engine$run(params.mcmc)

cbind(sim.mcmc$get.params("suppress"))
 
params.manual = params.mcmc
sim.manual = engine$run(params.manual) # still getting warnings with this 
params.manual["msm.suppressed.slope.or"] = 1 # 2.08602200
sim.manual = engine$run(params.manual)

atl.lik$compare.sims(sim.mcmc,sim.manual) # biggest differences are new diagnoses and hiv mortality 
exp(atl.lik$compute(sim.manual) - atl.lik$compute(sim.mcmc))

simplot(simset.atlanta$last.sim(),
        sim.mcmc,
        sim.manual,
        #facet.by = "sex", # age, sex, race, risk; 1-way 
        outcomes = c("suppression"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.mcmc,
        sim.manual,
        outcomes = c("new"), 
        style.manager = source.style.manager, 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.mcmc,
        sim.manual,
        facet.by = "risk", split.by = "race",
        outcomes = c("new"),         
        dimension.values = list(year = 2000:2030)) 

simplot(sim.mcmc,
        sim.manual,
        facet.by = "age", split.by = "sex",
        outcomes = c("new"),         
        dimension.values = list(year = 2000:2030)) 

simplot(sim.mcmc,
        sim.manual,
        facet.by = "sex", 
        outcomes = c("hiv.mortality"),
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.mcmc,
        sim.manual,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 
