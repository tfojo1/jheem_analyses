

params
params.2 = params

params.2["global.trate"] = 0.10 # original value is 0.1

# MSM TRATES/MULTIPLIERS
params.2["black.msm.trate.0"] = 0.5 # 1
params.2["black.msm.trate.1"] = 10 # 1, moves a lot when >1, not much when <1
params.2["black.msm.trate.2"] = 10 # 1, moves a lot when >1, not much when <1

params.2["hispanic.msm.trate.0"] = 0.5 # 1
params.2["hispanic.msm.trate.1"] = 0.5 # 1, doesn't move much 
params.2["hispanic.msm.trate.2"] = 0.000001 # 1, doesn't move much, maybe at all? when <1?

params.2["other.msm.trate.0"] = 0.5 # 1
params.2["other.msm.trate.1"] = 10 # 1, moves when >1, not much when <1
params.2["other.msm.trate.2"] = 0.00000001 # 1, moves when >1, might be doing something weird to hit <1

params.2["msm.peak.trate.multiplier"] = 2 # 3.1 

# HETEROSEXUAL TRATES/MULTIPLIERS
params.2["black.heterosexual.trate.0"] = 0.5 # 1
params.2["black.heterosexual.trate.1"] = 0.5 # 1
params.2["black.heterosexual.trate.2"] = 0.5 # 1

params.2["hispanic.heterosexual.trate.0"] = 0.5 # 1
params.2["hispanic.heterosexual.trate.1"] = 0.1 # 1
params.2["hispanic.heterosexual.trate.2"] = 0.001 # 1, might be starting to do something weird

params.2["other.heterosexual.trate.0"] = 0.5 # 1
params.2["other.heterosexual.trate.1"] = 0.5 # 1
params.2["other.heterosexual.trate.2"] = 0.5 # 1

params.2["heterosexual.peak.trate.multiplier"] = 1.5 # 2.2

# IDU TRATES/MULTIPLIERS - next

sim2 = engine$run(parameters = params.2)
simplot(sim, sim2, "new")

params.2 = params

params.2['other.birth.rate.multiplier'] = 2
params.2['black.birth.rate.multiplier'] = 2
params.2['hispanic.birth.rate.multiplier'] = 2

params.2['age1.aging.multiplier'] = 1.2

#params.2['age2.non.idu.general.mortality.rate.multiplier'] = 1

params.2['other.birth.rate.slope.multiplier'] = 1
params.2['black.birth.rate.slope.multiplier'] = 1
params.2['hispanic.birth.rate.slope.multiplier'] = 1


# simplot(sim2,"population",split.by = "race")
# simplot(sim2,"population",split.by = "age")
# simplot(sim,"population",split.by = "race")

sim2 = engine$run(parameters = params.2)

simplot(sim, sim2, "population",facet.by = "age",dimension.values = list(year = as.character(2000:2020)))
simplot(sim, sim2, "population",facet.by = "race",dimension.values = list(year = as.character(2000:2020)))


## TESTING OUT AFTER OPTIM OUTPUT
# sim.optim = sim
# params.optim = sim.optim$parameters[names(params)]

sim.optim.new = sim.optim

simplot(sim.optim, "population",facet.by = "age",dimension.values = list(year = as.character(2000:2020)))
simplot(sim.optim, "population",facet.by = "race",dimension.values = list(year = as.character(2000:2020)))
simplot(sim.optim, "population",facet.by = "sex",dimension.values = list(year = as.character(2000:2020)))
simplot(sim.optim, "population",dimension.values = list(year = as.character(2000:2020)))

simplot(sim.optim, "population",facet.by = "age",split.by = "race",dimension.values = list(year = as.character(2000:2020)))
simplot(sim.optim, "population",facet.by = "age",split.by = "sex",dimension.values = list(year = as.character(2000:2020)))
simplot(sim.optim, "population",facet.by = "race",split.by = "sex",dimension.values = list(year = as.character(2000:2020)))

# manually tweaking after previous optim 
# params.manual = param.optim
# params.manual["black.birth.rate.multiplier"] = 2.3 # 1.868623
# params.manual["age1.non.idu.general.mortality.rate.multiplier"] = 5 # 0.22525397
# sim.manual = engine$run(parameters = params.manual)
# save(sim.manual, params.manual,file="prelim_results/ehe_manual_pop_result.Rdata")

# Plotting optim vs manual 
simplot(sim.optim, sim.manual,"population",facet.by = "age",split.by = "race",dimension.values = list(year = as.character(2000:2020)))
simplot(sim.optim, sim.manual,"population",facet.by = "age",split.by = "sex",dimension.values = list(year = as.character(2000:2020)))
simplot(sim.optim, sim.manual,"population",facet.by = "race",split.by = "sex",dimension.values = list(year = as.character(2000:2020)))

simplot(sim.optim, sim.manual,"population",facet.by = "age",dimension.values = list(year = as.character(2000:2020)))
simplot(sim.optim, sim.manual, "population",facet.by = "race",dimension.values = list(year = as.character(2000:2020)))
simplot(sim.optim, sim.manual, "population",facet.by = "sex",dimension.values = list(year = as.character(2000:2020)))
simplot(sim.optim, sim.manual,"population",dimension.values = list(year = as.character(2000:2020)))

# Creating likelihoods
pop.lik.test.total = population.total.likelihood.instructions$instantiate.likelihood('ehe',
                                                                                     location = 'C.12580')

pop.lik.test.one.way = population.one.way.likelihood.instructions$instantiate.likelihood('ehe',
                                                                                     location = 'C.12580')


# ANDREW RUN THIS
pop.lik.test.two.way = population.two.way.likelihood.instructions$instantiate.likelihood('ehe',
                                                                                         location = 'C.12580')
pop.lik.test.two.way$compute(sim.manual,check.consistency=F,debug = T)



# Comparing manual vs optim likelihood
exp(pop.lik.test.one.way$compute(sim.manual,check.consistency=F) - pop.lik.test.one.way$compute(sim.optim,check.consistency=F)) 
exp(pop.lik.test.two.way$compute(sim.manual,check.consistency=F) - pop.lik.test.two.way$compute(sim.optim,check.consistency=F)) 

# Comparing manual vs optim prior
exp(calculate.density(EHE.PARAMETERS.PRIOR, x = sim.manual$parameters, log = T) - 
      calculate.density(EHE.PARAMETERS.PRIOR, x = sim.optim$parameters, log = T))

# Comparing manual vs optim parameters
cbind(sim.manual$parameters[names(optim.result$par)],sim.optim$parameters[names(optim.result$par)])

# original sim 
params0 = suppressWarnings(get.medians(EHE.PARAMETERS.PRIOR))
params0['global.trate'] = 0.1
sim0 = engine$run(parameters = params0)

exp(pop.lik.test$compute(sim.optim,check.consistency=F) - pop.lik.test$compute(sim0,check.consistency=F))
simplot(sim.optim, sim0,"population",facet.by = "age",dimension.values = list(year = as.character(2000:2020)))
simplot(sim.optim, sim0,"population",facet.by = "race",dimension.values = list(year = as.character(2000:2020)))
simplot(sim.optim, sim0,"population",facet.by = "sex",dimension.values = list(year = as.character(2000:2020)))

# next step - 2-way plots



# Example calls to get input rates in model - fyi
# x = get.model.fertility.rates.functional.form("C.12580",specification.metadata = get.specification.metadata('ehe','C.12580'))
# y = get.empiric.aging.rates("C.12580",specification.metadata = get.specification.metadata('ehe','C.12580'))
# z = get.non.idu.general.mortality.rates.functional.form("C.12580",specification.metadata = get.specification.metadata('ehe','C.12580'))








