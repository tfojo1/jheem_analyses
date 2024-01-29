source("../jheem2/R/tests/ENGINE_test.R")
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')

# sim 2 - okay
load("../jheem_analyses/applications/EHE/temp.Rdata")
params.2 = params
params.2[names(rv$par)] = rv$par
sim2 = engine$run(parameters = params.2)

# sim 3 - much better 
params.3 = params
params.3['other.birth.rate.multiplier'] = 2
params.3['black.birth.rate.multiplier'] = 2
params.3['hispanic.birth.rate.multiplier'] = 2
params.3['age1.aging.multiplier'] = 1.2
sim3 = engine$run(parameters = params.3)

 
simplot(sim, sim2, sim3, "population",facet.by = "age",dimension.values = list(year = as.character(2000:2020)))
simplot(sim, sim2, sim3, "population",facet.by = "race",dimension.values = list(year = as.character(2000:2020)))
simplot(sim, sim2, sim3, "population",dimension.values = list(year = as.character(2000:2020)))

# TOTALS 
# pop.total.lik.test = population.total.likelihood.instructions$instantiate.likelihood('ehe','C.12580')
# exp(pop.total.lik.test$compute(sim2,check.consistency=F) - pop.total.lik.test$compute(sim,check.consistency=F))
 
# FULL LIKELIHOOD
pop.lik.test = population.likelihood.instructions$instantiate.likelihood('ehe','C.12580') # don't need to rerun this

exp(pop.lik.test$compute(sim2,check.consistency=F) - pop.lik.test$compute(sim,check.consistency=F)) # sim2 should be somewhat better
exp(pop.lik.test$compute(sim3,check.consistency=F) - pop.lik.test$compute(sim,check.consistency=F)) # sim3 should be way better

min(-pop.lik.test$compute(sim3,check.consistency=F), 
    -pop.lik.test$compute(sim2,check.consistency=F), 
    -pop.lik.test$compute(sim,check.consistency=F))

 