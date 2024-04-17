source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
library(ggplot2)

# Have to rerun sims with new engine because of changes to simplot 
load("../jheem_analyses/prelim_results/pop_trans_mort_2024-04-13_C.12580.Rdata")
sim.pop.trans.mort = sim
params.pop.trans.mort = c(sim.pop.trans.mort$parameters)
names(params.pop.trans.mort) = dimnames(sim.pop.trans.mort$parameters)[[1]]
engine = create.jheem.engine('ehe',sim.pop.trans.mort$location,end.year = 2030) 
sim.pop.trans.mort = engine$run(parameters = params.pop.trans.mort) 

load("../jheem_analyses/prelim_results/pop_trans_mort_non_idu_2024-04-14_C.12580.Rdata")
sim.pop.trans.mort.non.idu = sim
params.pop.trans.mort.non.idu = c(sim.pop.trans.mort.non.idu$parameters)
names(params.pop.trans.mort.non.idu) = dimnames(sim.pop.trans.mort.non.idu$parameters)[[1]]
sim.pop.trans.mort.non.idu = engine$run(parameters = params.pop.trans.mort.non.idu) 

load("../jheem_analyses/prelim_results/base_plus_prop_tested_2024-04-16_C.12580.Rdata")
sim.prop.tested = sim
params.prop.tested = c(sim.prop.tested$parameters)
names(params.prop.tested) = dimnames(sim.prop.tested$parameters)[[1]]
sim.prop.tested = engine$run(parameters = params.prop.tested) 

load("../jheem_analyses/prelim_results/base_plus_positivity_2024-04-16_C.12580.Rdata")
sim.positivity = sim
params.positivity = c(sim.positivity$parameters)
names(params.positivity) = dimnames(sim.positivity$parameters)[[1]]
sim.positivity = engine$run(parameters = params.positivity) 

load("../jheem_analyses/prelim_results/base_plus_awareness_2024-04-17_C.12580.Rdata")
sim.awareness = sim
params.awareness = c(sim.awareness$parameters)
names(params.awareness) = dimnames(sim.awareness$parameters)[[1]]
sim.awareness = engine$run(parameters = params.awareness) 

params.manual = params.pop.trans.mort
sim.manual = engine$run(parameters = params.manual) 

#params.manual["female.non.idu.general.mortality.rate.multiplier"] = params.awareness["female.non.idu.general.mortality.rate.multiplier"]
sim.manual = engine$run(parameters = params.manual) 

# population 
simplot(sim.pop.trans.mort,
        sim.manual,
        #sim.pop.trans.mort.non.idu,
        #sim.prop.tested,
        #sim.positivity,
        sim.awareness, 
        facet.by = "age", split.by = "race", 
        outcomes = c("population"),
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))

# awareness
simplot(sim.pop.trans.mort,
        sim.manual,
        sim.pop.trans.mort.non.idu,
        sim.awareness, 
        outcomes = c("awareness"),
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))

# pop.trans.mort.non.idu.lik = pop.trans.mortality.non.idu.likelihood.instructions$instantiate.likelihood('ehe','C.12580')

cbind(round(sim.pop.trans.mort$parameters,3),
      round(sim.prop.tested$parameters,3),
      round(sim.positivity$parameters,3),
      round(sim.awareness$parameters,3))
