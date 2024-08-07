source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

# sim from population calibration: sim.pop
load('../jheem_analyses/prelim_results/init.pop.ehe_sim_2024-08-07_C.12580.Rdata')

engine = create.jheem.engine('ehe','C.12580',end.year = 2030)
params.pop = sim.pop$params

params.manual = params.pop
params.manual['other.age3.migration.multiplier.time.2'] = 2.5
sim.manual = engine$run(params.manual)

# SECOND sim minus the FIRST sim
pop.lik$compare.sims(sim.pop,sim.manual) # NEW LIKELIHOOD WEIGTHED BY YEAR
exp(pop.lik$compute(sim.manual, debug = T))
exp(pop.age.weighted.lik$compute(sim.manual, debug = T))

pop.heavy.lik$compare.sims(sim.pop,sim.manual) # overall, manual is worse - knew this already 
pop.tot.lik$compare.sims(sim.pop,sim.manual) # manual total scores and looks slightly better 
pop.age.lik$compare.sims(sim.pop,sim.manual) # manual age scores slightly WORSE but looks better
pop.sex.lik$compare.sims(sim.pop,sim.manual) # manual sex scores and looks slightly better 
pop.race.lik$compare.sims(sim.pop,sim.manual) # manual race scores and looks slightly better 
pop.two.way.lik$compare.sims(sim.pop,sim.manual) # manual TWO WAY is much worse?? 

# two-way individual comparisons
pop.age.race.lik$compare.sims(sim.pop,sim.manual) # age * race --> manual scores much WORSE but looks better **** ISSUE
pop.age.sex.lik$compare.sims(sim.pop,sim.manual) # age * sex --> manual scores WORSE but looks better **** ISSUE
pop.race.sex.lik$compare.sims(sim.pop,sim.manual) # race * sex --> manual scores and looks better 

# limited age comparisons
pop.age1.lik$compare.sims(sim.pop,sim.manual) # age 1 --> manual scores slightly better 
pop.age2.lik$compare.sims(sim.pop,sim.manual) # age 2 --> manual scores slightly WORSE (but very close)
pop.age3.lik$compare.sims(sim.pop,sim.manual) # age 3 --> manual scores slightly WORSE (but looks much better) **** ISSUE 
exp(pop.age3.lik$compute(sim.manual, debug = T))
pop.age4.lik$compare.sims(sim.pop,sim.manual) # age 4 --> manual scores slightly better
pop.age5.lik$compare.sims(sim.pop,sim.manual) # age 5 --> manual scores slightly WORSE (but very close)

# limited age*race 
pop.age3.other.lik$compare.sims(sim.pop,sim.manual) # age 3 * other --> manual scores WORSE (but looks much better) **** ISSUE
pop.age3.black.lik$compare.sims(sim.pop,sim.manual) # age 3 * black --> manual scores WORSE (but very close)
pop.age3.hispanic.lik$compare.sims(sim.pop,sim.manual) # age 3 * hispanic --> manual scores WORSE (but very close)

# limited age*sex 
pop.age3.male.lik$compare.sims(sim.pop,sim.manual) # age 3 * male --> manual scores and looks better
pop.age3.female.lik$compare.sims(sim.pop,sim.manual) # age 3 * female --> manual scores WORSE (but looks much) **** ISSUE 

pop.age3.2015.lik$compare.sims(sim.pop,sim.manual)
pop.2015.lik$compare.sims(sim.pop,sim.manual)

simplot(sim.pop,
        sim.manual,
        facet.by = "age", split.by = "sex", 
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

#####
pop.lik = population.likelihood.instructions$instantiate.likelihood('ehe','C.12580')
pop.heavy.lik = population.heavy.likelihood.instructions$instantiate.likelihood('ehe','C.12580')
pop.tot.lik = pop.total.instructions$instantiate.likelihood('ehe','C.12580')
pop.age.lik = pop.age.instructions$instantiate.likelihood('ehe','C.12580')
pop.sex.lik = pop.sex.instructions$instantiate.likelihood('ehe','C.12580')
pop.race.lik = pop.race.instructions$instantiate.likelihood('ehe','C.12580')
pop.two.way.lik = pop.two.way.instructions$instantiate.likelihood('ehe','C.12580')
pop.age.race.lik = pop.two.way.age.race.instructions$instantiate.likelihood('ehe','C.12580')
pop.age.sex.lik = pop.two.way.age.sex.instructions$instantiate.likelihood('ehe','C.12580')
pop.race.sex.lik = pop.two.way.race.sex.instructions$instantiate.likelihood('ehe','C.12580')
pop.age1.lik = pop.age1.instructions$instantiate.likelihood('ehe','C.12580')
pop.age2.lik = pop.age2.instructions$instantiate.likelihood('ehe','C.12580')
pop.age3.lik = pop.age3.instructions$instantiate.likelihood('ehe','C.12580')
pop.age4.lik = pop.age4.instructions$instantiate.likelihood('ehe','C.12580')
pop.age5.lik = pop.age5.instructions$instantiate.likelihood('ehe','C.12580')
pop.age3.other.lik = pop.age3.other.instructions$instantiate.likelihood('ehe','C.12580')
pop.age3.black.lik = pop.age3.black.instructions$instantiate.likelihood('ehe','C.12580')
pop.age3.hispanic.lik = pop.age3.hispanic.instructions$instantiate.likelihood('ehe','C.12580')
pop.age3.male.lik = pop.age3.male.instructions$instantiate.likelihood('ehe','C.12580')
pop.age3.female.lik = pop.age3.female.instructions$instantiate.likelihood('ehe','C.12580')
pop.age3.2015.lik = pop.age3.2015.instructions$instantiate.likelihood('ehe','C.12580')
pop.2015.lik = population.heavy.2015.likelihood.instructions$instantiate.likelihood('ehe','C.12580')
pop.age.weighted.lik = population.age.weighted.likelihood.instructions$instantiate.likelihood('ehe','C.12580')


#####
population.heavy.2015.likelihood.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                       outcome.for.sim = "population",
                                       dimensions = c("age","sex","race"),
                                       levels.of.stratification = c(0,1,2), # 0 = totals, 1 = 1-way stratification
                                       from.year = 2015,
                                       correlation.different.years = 0.5, # this is the default
                                       correlation.different.strata = 0.1, # this is the default
                                       correlation.different.sources = 0.3, # default
                                       correlation.same.source.different.details = 0.3, # default
                                       
                                       # assumes correlation between all combos of years is the same
                                       observation.correlation.form = 'compound.symmetry', 
                                       
                                       # should always be specified; describes how precise the estimates are; 
                                       # e.g., estimates can be off by 3% each year
                                       error.variance.term = 0.03,
                                       error.variance.type = 'cv',
                                       
                                       # downweight because large population size; 
                                       # can get more specific with create.likelihood.weights 
                                       #(e.g., different weight for age X)
                                       weights = 2, 
                                       
                                       # if there are more datapoints for certain years, this will normalize
                                       # e.g., if there are a few years with only the totals 
                                       # before the stratifications are available
                                       equalize.weight.by.year = F 
  )

# TOTAL
pop.total.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                       outcome.for.sim = "population",
                                       dimensions = character(),
                                       #dimensions = c("age","sex","race"),
                                       levels.of.stratification = c(0), # 0 = totals, 1 = 1-way stratification
                                       from.year = 2007,
                                       correlation.different.years = 0.5, # this is the default
                                       correlation.different.strata = 0.1, # this is the default
                                       correlation.different.sources = 0.3, # default
                                       correlation.same.source.different.details = 0.3, # default
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.03,
                                       error.variance.type = 'cv',
                                       weights = 2, 
                                       equalize.weight.by.year = F
  )

# ONE-WAY STRATIFICATIONS
pop.age.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                       outcome.for.sim = "population",
                                       dimensions = c("age"),
                                       levels.of.stratification = c(1), # 0 = totals, 1 = 1-way stratification
                                       from.year = 2007,
                                       correlation.different.years = 0.5, # this is the default
                                       correlation.different.strata = 0.1, # this is the default
                                       correlation.different.sources = 0.3, # default
                                       correlation.same.source.different.details = 0.3, # default
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.03,
                                       error.variance.type = 'cv',
                                       weights = 2, 
                                       equalize.weight.by.year = F
  )

pop.sex.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                       outcome.for.sim = "population",
                                       dimensions = c("sex"),
                                       levels.of.stratification = c(1), # 0 = totals, 1 = 1-way stratification
                                       from.year = 2007,
                                       correlation.different.years = 0.5, # this is the default
                                       correlation.different.strata = 0.1, # this is the default
                                       correlation.different.sources = 0.3, # default
                                       correlation.same.source.different.details = 0.3, # default
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.03,
                                       error.variance.type = 'cv',
                                       weights = 2, 
                                       equalize.weight.by.year = F
  )

pop.race.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                       outcome.for.sim = "population",
                                       dimensions = c("race"),
                                       levels.of.stratification = c(1), # 0 = totals, 1 = 1-way stratification
                                       from.year = 2007,
                                       correlation.different.years = 0.5, # this is the default
                                       correlation.different.strata = 0.1, # this is the default
                                       correlation.different.sources = 0.3, # default
                                       correlation.same.source.different.details = 0.3, # default
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.03,
                                       error.variance.type = 'cv',
                                       weights = 2, 
                                       equalize.weight.by.year = F
  )

# TWO-WAY STRATIFICATIONS
pop.two.way.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                       outcome.for.sim = "population",
                                       dimensions = c("age","sex","race"),
                                       levels.of.stratification = c(2), # 0 = totals, 1 = 1-way stratification
                                       from.year = 2007,
                                       correlation.different.years = 0.5, # this is the default
                                       correlation.different.strata = 0.1, # this is the default
                                       correlation.different.sources = 0.3, # default
                                       correlation.same.source.different.details = 0.3, # default
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.03,
                                       error.variance.type = 'cv',
                                       weights = 2, 
                                       equalize.weight.by.year = F
  )

pop.two.way.age.race.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                       outcome.for.sim = "population",
                                       dimensions = c("age","race"),
                                       levels.of.stratification = c(2), # 0 = totals, 1 = 1-way stratification
                                       from.year = 2007,
                                       correlation.different.years = 0.5, # this is the default
                                       correlation.different.strata = 0.1, # this is the default
                                       correlation.different.sources = 0.3, # default
                                       correlation.same.source.different.details = 0.3, # default
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.03,
                                       error.variance.type = 'cv',
                                       weights = 2, 
                                       equalize.weight.by.year = F
  )

pop.two.way.age.sex.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                       outcome.for.sim = "population",
                                       dimensions = c("age","sex"),
                                       levels.of.stratification = c(2), # 0 = totals, 1 = 1-way stratification
                                       from.year = 2007,
                                       correlation.different.years = 0.5, # this is the default
                                       correlation.different.strata = 0.1, # this is the default
                                       correlation.different.sources = 0.3, # default
                                       correlation.same.source.different.details = 0.3, # default
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.03,
                                       error.variance.type = 'cv',
                                       weights = 2, 
                                       equalize.weight.by.year = F
  )

pop.two.way.race.sex.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                       outcome.for.sim = "population",
                                       dimensions = c("race","sex"),
                                       levels.of.stratification = c(2), # 0 = totals, 1 = 1-way stratification
                                       from.year = 2007,
                                       correlation.different.years = 0.5, # this is the default
                                       correlation.different.strata = 0.1, # this is the default
                                       correlation.different.sources = 0.3, # default
                                       correlation.same.source.different.details = 0.3, # default
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.03,
                                       error.variance.type = 'cv',
                                       weights = 2, 
                                       equalize.weight.by.year = F
  )

# LIMITED AGE STRATIFICATIONS
pop.age1.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                       outcome.for.sim = "population",
                                       dimensions = c("age"),
                                       dimension.values = list(age = "13-24 years"),
                                       levels.of.stratification = c(1), # 0 = totals, 1 = 1-way stratification
                                       from.year = 2007,
                                       correlation.different.years = 0.5, # this is the default
                                       correlation.different.strata = 0.1, # this is the default
                                       correlation.different.sources = 0.3, # default
                                       correlation.same.source.different.details = 0.3, # default
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.03,
                                       error.variance.type = 'cv',
                                       weights = 2, 
                                       equalize.weight.by.year = F
  )

pop.age2.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                       outcome.for.sim = "population",
                                       dimensions = c("age"),
                                       dimension.values = list(age = "25-34 years"),
                                       levels.of.stratification = c(1), # 0 = totals, 1 = 1-way stratification
                                       from.year = 2007,
                                       correlation.different.years = 0.5, # this is the default
                                       correlation.different.strata = 0.1, # this is the default
                                       correlation.different.sources = 0.3, # default
                                       correlation.same.source.different.details = 0.3, # default
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.03,
                                       error.variance.type = 'cv',
                                       weights = 2, 
                                       equalize.weight.by.year = F
  )

pop.age3.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                       outcome.for.sim = "population",
                                       dimensions = c("age"),
                                       dimension.values = list(age = "35-44 years"),
                                       levels.of.stratification = c(1), # 0 = totals, 1 = 1-way stratification
                                       from.year = 2007,
                                       correlation.different.years = 0.5, # this is the default
                                       correlation.different.strata = 0.1, # this is the default
                                       correlation.different.sources = 0.3, # default
                                       correlation.same.source.different.details = 0.3, # default
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.03,
                                       error.variance.type = 'cv',
                                       weights = 2, 
                                       equalize.weight.by.year = F
  )

pop.age4.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                       outcome.for.sim = "population",
                                       dimensions = c("age"),
                                       dimension.values = list(age = "45-54 years"),
                                       levels.of.stratification = c(1), # 0 = totals, 1 = 1-way stratification
                                       from.year = 2007,
                                       correlation.different.years = 0.5, # this is the default
                                       correlation.different.strata = 0.1, # this is the default
                                       correlation.different.sources = 0.3, # default
                                       correlation.same.source.different.details = 0.3, # default
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.03,
                                       error.variance.type = 'cv',
                                       weights = 2, 
                                       equalize.weight.by.year = F
  )

pop.age5.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                       outcome.for.sim = "population",
                                       dimensions = c("age"),
                                       dimension.values = list(age = "55+ years"),
                                       levels.of.stratification = c(1), # 0 = totals, 1 = 1-way stratification
                                       from.year = 2007,
                                       correlation.different.years = 0.5, # this is the default
                                       correlation.different.strata = 0.1, # this is the default
                                       correlation.different.sources = 0.3, # default
                                       correlation.same.source.different.details = 0.3, # default
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.03,
                                       error.variance.type = 'cv',
                                       weights = 2, 
                                       equalize.weight.by.year = F
  )


# LIMITED AGE*RACE STRATIFICATIONS
pop.age3.other.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                       outcome.for.sim = "population",
                                       dimensions = c("age","race"),
                                       dimension.values = list(age = "35-44 years",
                                                               race = "other"),
                                       levels.of.stratification = c(2), # 0 = totals, 1 = 1-way stratification
                                       from.year = 2007,
                                       correlation.different.years = 0.5, # this is the default
                                       correlation.different.strata = 0.1, # this is the default
                                       correlation.different.sources = 0.3, # default
                                       correlation.same.source.different.details = 0.3, # default
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.03,
                                       error.variance.type = 'cv',
                                       weights = 2, 
                                       equalize.weight.by.year = F
  )

pop.age3.black.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                       outcome.for.sim = "population",
                                       dimensions = c("age","race"),
                                       dimension.values = list(age = "35-44 years",
                                                               race = "black"),
                                       levels.of.stratification = c(2), # 0 = totals, 1 = 1-way stratification
                                       from.year = 2007,
                                       correlation.different.years = 0.5, # this is the default
                                       correlation.different.strata = 0.1, # this is the default
                                       correlation.different.sources = 0.3, # default
                                       correlation.same.source.different.details = 0.3, # default
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.03,
                                       error.variance.type = 'cv',
                                       weights = 2, 
                                       equalize.weight.by.year = F
  )

pop.age3.hispanic.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                       outcome.for.sim = "population",
                                       dimensions = c("age","race"),
                                       dimension.values = list(age = "35-44 years",
                                                               race = "hispanic"),
                                       levels.of.stratification = c(2), # 0 = totals, 1 = 1-way stratification
                                       from.year = 2007,
                                       correlation.different.years = 0.5, # this is the default
                                       correlation.different.strata = 0.1, # this is the default
                                       correlation.different.sources = 0.3, # default
                                       correlation.same.source.different.details = 0.3, # default
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.03,
                                       error.variance.type = 'cv',
                                       weights = 2, 
                                       equalize.weight.by.year = F
  )


# LIMITED AGE*SEX STRATIFICATIONS
pop.age3.male.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                       outcome.for.sim = "population",
                                       dimensions = c("age","sex"),
                                       dimension.values = list(age = "35-44 years",
                                                               sex = "male"),
                                       levels.of.stratification = c(2), # 0 = totals, 1 = 1-way stratification
                                       from.year = 2007,
                                       correlation.different.years = 0.5, # this is the default
                                       correlation.different.strata = 0.1, # this is the default
                                       correlation.different.sources = 0.3, # default
                                       correlation.same.source.different.details = 0.3, # default
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.03,
                                       error.variance.type = 'cv',
                                       weights = 2, 
                                       equalize.weight.by.year = F
  )

pop.age3.female.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                       outcome.for.sim = "population",
                                       dimensions = c("age","sex"),
                                       dimension.values = list(age = "35-44 years",
                                                               sex = "female"),
                                       levels.of.stratification = c(2), # 0 = totals, 1 = 1-way stratification
                                       from.year = 2007,
                                       correlation.different.years = 0.5, # this is the default
                                       correlation.different.strata = 0.1, # this is the default
                                       correlation.different.sources = 0.3, # default
                                       correlation.same.source.different.details = 0.3, # default
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.03,
                                       error.variance.type = 'cv',
                                       weights = 2, 
                                       equalize.weight.by.year = F
  )


pop.age3.2015.instructions = 
  create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                       outcome.for.sim = "population",
                                       dimensions = c("age"),
                                       dimension.values = list(age = "35-44 years"),
                                       levels.of.stratification = c(1), # 0 = totals, 1 = 1-way stratification
                                       from.year = 2015,
                                       correlation.different.years = 0.5, # this is the default
                                       correlation.different.strata = 0.1, # this is the default
                                       correlation.different.sources = 0.3, # default
                                       correlation.same.source.different.details = 0.3, # default
                                       observation.correlation.form = 'compound.symmetry', 
                                       error.variance.term = 0.03,
                                       error.variance.type = 'cv',
                                       weights = 2, 
                                       equalize.weight.by.year = F
  )
