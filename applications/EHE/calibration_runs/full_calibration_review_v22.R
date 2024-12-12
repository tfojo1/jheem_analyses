source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum") # this is the default right now 

load("../jheem_analyses/prelim_results/full.with.aids_simset_2024-12-11_C.16980.Rdata")

engine = create.jheem.engine('ehe','C.16980',end.year = 2025)

params.mcmc = simset$get.params()
params.mcmc["max.covid.effect.prep.uptake.reduction"] = 1 # otherwise won't run 
sim.mcmc = engine$run(parameters = params.mcmc)

params.manual = params.mcmc
sim.manual = engine$run(parameters = params.manual)

cbind(simset$get.params("black.msm"))

params.manual["age1.black.msm.susceptibility.rr.01"] = 1 # 0.7616484
sim.manual = engine$run(parameters = params.manual)

#chi.lik = FULL.likelihood.instructions.with.aids$instantiate.likelihood('ehe','C.16980')
chi.lik$compare.sims(sim.mcmc,sim.manual)  # still second minus first 
exp(chi.lik$compute(sim.manual) - chi.lik$compute(sim.mcmc))

# makes new diagnoses better, but diagnosed prevalence and aids diagnoses worse (mostly among msm)

simplot(sim.mcmc,
        sim.manual,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim.mcmc,
        sim.manual,
        facet.by = "risk", #split.by = "race", 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

simplot(sim.mcmc,
        sim.manual,
        facet.by = "age", split.by = "sex", 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

simplot(sim.mcmc,
        sim.manual,
        outcomes = c("new"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim.mcmc,
        sim.manual,
        facet.by = "risk", split.by = "race", 
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 

simplot(sim.mcmc,
        sim.manual,
        facet.by = "age", split.by = "sex", 
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 


simplot(sim.mcmc,
        sim.manual,
        outcomes = c("aids.diagnoses"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim.mcmc,
        sim.manual,
        facet.by = "race", 
        outcomes = c("aids.diagnoses"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim.mcmc,
        sim.manual,
        facet.by = "risk", 
        outcomes = c("aids.diagnoses"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 1980:2030)) 
