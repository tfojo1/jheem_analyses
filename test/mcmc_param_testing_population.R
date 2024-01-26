source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-01-13_C.26420.Rdata") # Houston
load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-01-12_C.12580.Rdata") # Baltimore
load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-01-13_C.33100.Rdata") # Miami

LOCATION = sim$location

sim.mcmc = sim 
params.mcmc = sim.mcmc$parameters

params.manual = c(params.mcmc)
names(params.manual) = dimnames(params.mcmc)[[1]]

engine = create.jheem.engine('ehe', LOCATION, start.year=1970, end.year=2025, max.run.time.seconds = 10)
sim.manual = engine$run(parameters = params.manual) 

simplot(sim.mcmc, sim.manual, "population",
        facet.by = "age",split.by = "race",
        dimension.values = list(year = as.character(2000:2020)))

cbind(sim.mcmc$parameters[par.names,],
      sim.manual$parameters[par.names,])

# HOUSTON PARAMETERS
params.manual["other.domino.aging.multiplier"] = 1.2 # 1.0069769
# likelihood much worse

# BALTIMORE PARAMETERS
params.manual["other.domino.aging.multiplier"] = 0.9 # 0.7152390
# likelihood much worse 

# MIAMI PARAMETERS
params.manual["hispanic.domino.aging.multiplier"] = 0.75 # 0.90860556 
# likelihood much worse but I don't understand why 

# New debugging 1/23
params.manual["age1.migration.multiplier.time.2"] = 1 # 0.04083471
params.manual["hispanic.age1.aging.multiplier"] = 1.2 # 0.93936679
params.manual["hispanic.domino.aging.multiplier"] = 1 # 1.91019700
params.manual["hispanic.birth.rate.multiplier"] = 2 # 2.20683254
params.manual["age2.migration.multiplier.time.1"] = 1 # 0.18348045
params.manual["hispanic.migration.multiplier.time.2"] = 1 # 0.27902878

sim.manual = engine$run(parameters = params.manual)
simplot(sim.mcmc, sim.manual, "population",
        facet.by = "age",split.by = "race",
        dimension.values = list(year = as.character(2000:2020)))


# Comparing manual vs mcmc likelihood - joint likelihood 
lik = joint.pop.migration.likelihood.instructions$instantiate.likelihood('ehe', location = LOCATION,
                                                                         data.manager = SURVEILLANCE.MANAGER)

exp(lik$compute(sim.manual,check.consistency=F) - lik$compute(sim.mcmc,check.consistency=F)) 

# Individual likelihood components
exp(lik$compute.piecewise(sim.manual,check.consistency=F) - lik$compute.piecewise(sim.mcmc,check.consistency=F)) 


## EXTRA DEBUGGING - MIAMI 
# Total 
pop.lik.instr.total = create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                                           outcome.for.sim = "population",
                                                           dimensions = c("age","race"),
                                                           levels.of.stratification = c(0), 
                                                           from.year = as.integer(2007),
                                                           observation.correlation.form = 'compound.symmetry', 
                                                           measurement.error.coefficient.of.variance = 0.03,
                                                           weights = 1,
                                                           equalize.weight.by.year = T)

simplot(sim.mcmc, sim.manual, "population",dimension.values = list(year = as.character(2000:2020)))
lik.total = pop.lik.instr.total$instantiate.likelihood('ehe', location = LOCATION)
exp(lik.total$compute(sim.manual,check.consistency=F) - lik.total$compute(sim.mcmc,check.consistency=F)) 
# 1.023586 - THIS IS FINE

# Age
pop.lik.instr.age = create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                                         outcome.for.sim = "population",
                                                         dimensions = c("age"),
                                                         levels.of.stratification = c(1), 
                                                         from.year = as.integer(2007),
                                                         observation.correlation.form = 'compound.symmetry', 
                                                         measurement.error.coefficient.of.variance = 0.03,
                                                         weights = 1,
                                                         equalize.weight.by.year = T)
simplot(sim.mcmc, sim.manual, "population",facet.by = "age", dimension.values = list(year = as.character(2000:2020)))
lik.age = pop.lik.instr.age$instantiate.likelihood('ehe', location = LOCATION)
exp(lik.age$compute(sim.manual,check.consistency=F) - lik.age$compute(sim.mcmc,check.consistency=F)) 
# 0.007489551 - SOMEWHAT PROBLEMATIC 

# Race
pop.lik.instr.race = create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                                          outcome.for.sim = "population",
                                                          dimensions = c("race"),
                                                          levels.of.stratification = c(1), 
                                                          from.year = as.integer(2007),
                                                          observation.correlation.form = 'compound.symmetry', 
                                                          measurement.error.coefficient.of.variance = 0.03,
                                                          weights = 1,
                                                          equalize.weight.by.year = T)

simplot(sim.mcmc, sim.manual, "population",facet.by = "race", dimension.values = list(year = as.character(2000:2020)))
lik.race = pop.lik.instr.race$instantiate.likelihood('ehe', location = LOCATION)
exp(lik.race$compute(sim.manual,check.consistency=F) - lik.race$compute(sim.mcmc,check.consistency=F)) 
# 1.01986 - THIS IS FINE

# Age-race
pop.lik.instr.age.race = create.basic.likelihood.instructions(outcome.for.data = "adult.population", 
                                                          outcome.for.sim = "population",
                                                          dimensions = c("age","race"),
                                                          levels.of.stratification = c(2), 
                                                          from.year = as.integer(2007),
                                                          observation.correlation.form = 'compound.symmetry', 
                                                          measurement.error.coefficient.of.variance = 0.03,
                                                          weights = 1,
                                                          equalize.weight.by.year = T)

simplot(sim.mcmc, sim.manual, "population",facet.by = "age",split.by = "race",
        dimension.values = list(year = as.character(2000:2020)))
lik.age.race = pop.lik.instr.age.race$instantiate.likelihood('ehe', location = LOCATION)
exp(lik.age.race$compute(sim.manual,check.consistency=F) - lik.age.race$compute(sim.mcmc,check.consistency=F)) 
# 4.23002e-10 - VERY PROBLEMATIC 



# Checking each stratum via mask within debug 
# Age only 
exp(lik.age$compute(sim.manual,check.consistency=F, debug=T) - lik.age$compute(sim.mcmc,check.consistency=F)) 
mask = grepl("35",lik.summary$stratum)

# Age-race
exp(lik.age.race$compute(sim.manual,check.consistency=F, debug=T) - lik.age.race$compute(sim.mcmc,check.consistency=F)) 
mask = grepl("13",lik.summary$stratum) & grepl("hispanic",lik.summary$stratum)

lik.summary[mask,] # check that only mask is showing 

# non-exponentiated likelihood for one of the sims
likelihood.test = mvtnorm::dmvnorm(private$i.obs.vector[mask],
                                   mean = mean[mask],
                                   sigma = matrix(sigma, nrow=length(private$i.obs.vector), ncol=length(private$i.obs.vector))[mask,mask],
                                   log=T,
                                   checkSymmetry = F)
lik.manual <<- likelihood.test
lik.mcmc <<- likelihood.test

exp(lik.manual - lik.mcmc)

(lik.summary$`sqrt(diag(sigma))`/lik.summary$mean)[mask]
((lik.summary$mean - lik.summary$`private$i.obs.vector`)/lik.summary$`sqrt(diag(sigma))`)[mask]

dnorm(-4.3012556,sd=2)/dnorm(-3.3060542,sd = 2) # age 35-44, manual relative to mcmc, 8th year, age 3 is 0.02 worse 
dnorm(0.9921381, sd = 2)/dnorm(-2.15060437, sd = 2) # age 13-24, manual relative to mcmc - age1 bracket is 6x better
# sd = 2 (downweighting) makes it not as extreme

# Age
# 13-24 = 8.876178
# 25-34 = 0.3777883
# 35-44 = 0.02634639 ## problematic 
# 45-54 = 0.2025573
# 55+ = 0.7802

# Age-race
# 13-24, hispanic = 190.3793
# 13-24, black = 1.178357
# 13-24, other = 1.230816

# 25-34, hispanic = 0.04547405
# 25-34, black = 0.2205604
# 25-34, other = 0.1435895

# 35-44, hispanic = 9.832497e-06 ## THIS IS THE PROBLEM 
# 35-44, black = 1.020122
# 35-44, other = 1.384699

# 45-54, hispanic = 0.0005323451 ## problematic 
# 45-54, black = 0.9227334
# 45-54, other = 1.154423

# 55+, hispanic = 1.105387
# 55+, black = 1.847819
# 55+, other = 1.135748


# EXTRA CODE 
cov.mat = matrix(sigma, nrow=length(private$i.obs.vector), ncol=length(private$i.obs.vector))
round(cov2cor(cov.mat[mask,mask]),3)
# range(lik.summary$`sqrt(diag(sigma))`[!mask])


# How to use weights in likelihood
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
