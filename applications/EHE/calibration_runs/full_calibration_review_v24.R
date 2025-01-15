source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum") # this is the default right now 

load("../jheem_analyses/prelim_results/full.with.covid2_simset_2025-01-09_C.26420.Rdata")

engine = create.jheem.engine('ehe','C.26420',end.year = 2025)

params.mcmc = simset$get.params()
sim.mcmc = engine$run(parameters = params.mcmc)

params.manual = params.mcmc
sim.manual = engine$run(parameters = params.manual)

cbind(simset$get.params("testing.covid"))

# smaller number --> bigger dip during covid years; but this barely does anything! 
params.manual["age12.testing.covid.multiplier"] = 0 # 0.7128578
params.manual["age34.testing.covid.multiplier"] = 0 # 0.7715582
params.manual["age5.testing.covid.multiplier"] = 0 # 0.7054903

sim.manual = engine$run(parameters = params.manual)

#houston.lik = FULL.likelihood.instructions.with.covid$instantiate.likelihood('ehe','C.26420')
houston.lik$compare.sims(sim.mcmc,sim.manual)  # still second minus first 
exp(houston.lik$compute(sim.manual) - houston.lik$compute(sim.mcmc))


simplot(#simset$last.sim(),
        #simset,
        sim.mcmc,
        sim.manual,
        outcomes = c("new"), 
        style.manager = source.style.manager, 
        dimension.values = list(year = 1980:2030)) 

simplot(sim.mcmc,
        sim.manual,
        facet.by = "age", # age, sex, race, risk; 1-way 
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.mcmc,
        sim.manual,
        facet.by = "race", # age, sex, race, risk; 1-way 
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.mcmc,
        sim.manual,
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 
