pop.lik.instr.race = create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                                          outcome.for.sim = "population",
                                                          dimensions = c("race"),
                                                          levels.of.stratification = c(1), 
                                                          from.year = as.integer(2007),
                                                          observation.correlation.form = 'compound.symmetry', 
                                                          measurement.error.coefficient.of.variance = 0.03,
                                                          weights = 1,
                                                          equalize.weight.by.year = T)

simplot(sim.old, sim.new, "population",facet.by = "race", dimension.values = list(year = as.character(2000:2020)))
lik.race = pop.lik.instr.race$instantiate.likelihood('ehe', location = LOCATION)
exp(lik.race$compute(sim.new,check.consistency=F) - lik.race$compute(sim.old,check.consistency=F)) 



pop.lik.instr.black = create.basic.likelihood.instructions(outcome.for.data = "adult.population",
                                                           outcome.for.sim = "population",
                                                           dimensions = c("race"),
                                                           levels.of.stratification = c(1),
                                                           from.year = as.integer(2007),
                                                           observation.correlation.form = 'compound.symmetry',
                                                           measurement.error.coefficient.of.variance = 0.03,
                                                           weights = list(create.likelihood.weights(dimension.values = list(race=c("other","hispanic")),
                                                                                                    total.weight = .000000000001)),
                                                           equalize.weight.by.year = T)

lik.black = pop.lik.instr.black$instantiate.likelihood('ehe',location=LOCATION)
exp(lik.black$compute(sim.new,check.consistency=F) - lik.black$compute(sim.old,check.consistency=F))


pop.lik.instr.hispanic = create.basic.likelihood.instructions(outcome.for.data = "adult.population",
                                                              outcome.for.sim = "population",
                                                              dimensions = c("race"),
                                                              levels.of.stratification = c(1),
                                                              from.year = as.integer(2007),
                                                              observation.correlation.form = 'compound.symmetry',
                                                              measurement.error.coefficient.of.variance = 0.03,
                                                              weights = list(create.likelihood.weights(dimension.values = list(race=c("other","black")),
                                                                                                       total.weight = .000000000001)),
                                                              equalize.weight.by.year = T)

lik.hispanic = pop.lik.instr.hispanic$instantiate.likelihood('ehe',location=LOCATION)
exp(lik.hispanic$compute(sim.new,check.consistency=F, debug=T) - lik.hispanic$compute(sim.old,check.consistency=F))

pop.lik.instr.other = create.basic.likelihood.instructions(outcome.for.data = "adult.population",
                                                           outcome.for.sim = "population",
                                                           dimensions = c("race"),
                                                           levels.of.stratification = c(1),
                                                           from.year = as.integer(2007),
                                                           observation.correlation.form = 'compound.symmetry',
                                                           measurement.error.coefficient.of.variance = 0.03,
                                                           weights = list(create.likelihood.weights(dimension.values = list(race=c("hispanic","black")),
                                                                                                    total.weight = .000000000001)),
                                                           equalize.weight.by.year = T)

lik.other = pop.lik.instr.other$instantiate.likelihood('ehe',location=LOCATION)
exp(lik.other$compute(sim.new,check.consistency=F) - lik.other$compute(sim.old,check.consistency=F))