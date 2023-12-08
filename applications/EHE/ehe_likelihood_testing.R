# source engine test

load("../jheem_analyses/applications/EHE/temp.Rdata")

# test out total likelihood; then each of the one-way likelihoods and see if they make sense 
population.total.likelihood.instructions = create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                                                          outcome.for.sim = "population",
                                                                          dimensions = c("age","sex","race"),
                                                                          levels.of.stratification = c(0), # 0 = totals, 1 = 1-way stratification
                                                                          from.year = as.integer(2007),
                                                                          correlation.different.years = 0.5, # this is the default
                                                                          correlation.different.strata = 0.1, # this is the default
                                                                          correlation.different.sources = 0.3, # default
                                                                          correlation.same.source.different.details = 0.3, # default
                                                                          
                                                                          # assumes correlation between all combos of years is the same
                                                                          observation.correlation.form = 'compound.symmetry', 
                                                                          
                                                                          # should always be specified; describes how precise the estimates are; 
                                                                          # e.g., estimates can be off by 3% each year - LOOK THIS UP
                                                                          measurement.error.coefficient.of.variance = 0.03,
                                                                          
                                                                          # downweight because large population size; 
                                                                          # can get more specific with create.likelihood.weights 
                                                                          #(e.g., different weight for age X)
                                                                          weights = list(1/64),
                                                                          
                                                                          # if there are more datapoints for certain years, this will normalize
                                                                          # e.g., if there are a few years with only the totals 
                                                                          # before the stratifications are available
                                                                          equalize.weight.by.year = T 
)

params.2 = params
params.2[names(rv$par)] = rv$par

sim2 = engine$run(parameters = params.2)
 
simplot(sim, sim2, "population",facet.by = "age",dimension.values = list(year = as.character(2000:2020)))
simplot(sim, sim2, "population",facet.by = "race",dimension.values = list(year = as.character(2000:2020)))
simplot(sim, sim2, "population",dimension.values = list(year = as.character(2000:2020)))

# don't need to rerun this
pop.lik.test = population.likelihood.instructions$instantiate.likelihood('ehe','C.12580')
pop.total.lik.test = population.total.likelihood.instructions$instantiate.likelihood('ehe','C.12580')
 
exp(pop.lik.test$compute(sim2,check.consistency=F) - pop.lik.test$compute(sim,check.consistency=F))

# TOTALS 
# right now, this is giving us a value of 1 when sim 2 is clearly better 
exp(pop.total.lik.test$compute(sim2,check.consistency=F) - pop.total.lik.test$compute(sim,check.consistency=F))



test.mean.sim
test.mean.sim2


test.numerator.sim
test.numerator.sim2

 