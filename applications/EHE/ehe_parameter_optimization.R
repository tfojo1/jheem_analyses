source('../jheem2/R/tests/ENGINE_test.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')

sim.metadata = get.simulation.metadata('ehe',location = 'C.12580')
pop.lik.test = population.two.way.likelihood.instructions$instantiate.likelihood('ehe','C.12580') # USING TWO-WAY LIKELIHOOD

## AGE TARGET
age.data = SURVEILLANCE.MANAGER$pull(outcome = "adult.population",
                              keep.dimensions = c("year","age"),
                              dimension.values = list(location = "C.12580",
                                                      year = 2007:2020))
age.data = apply(age.data,c("year","age"),sum,na.rm = T) # combine non-overlapping sources
age.mapping = get.ontology.mapping(from.ontology = dimnames(age.data),
                               to.ontology = sim.metadata$outcome.ontologies$population[c('year','age')])
target.data.age = age.mapping$apply(age.data)

## RACE TARGET
race.data = SURVEILLANCE.MANAGER$pull(outcome = "adult.population",
                                     keep.dimensions = c("year","race","ethnicity"),
                                     dimension.values = list(location = "C.12580",
                                                             year = 2007:2020))
race.data = apply(race.data,c("year","race","ethnicity"),sum,na.rm = T) # combine non-overlapping sources
race.mapping = get.ontology.mapping(from.ontology = dimnames(race.data),
                                    to.ontology = sim.metadata$outcome.ontologies$population[c('year','race')])
target.data.race = race.mapping$apply(race.data)




params = suppressWarnings(get.medians(EHE.PARAMETERS.PRIOR))
params['global.trate'] = 0.1

par.names = c("black.birth.rate.multiplier",
              "hispanic.birth.rate.multiplier",
              "other.birth.rate.multiplier",
              "black.birth.rate.slope.multiplier",
              "hispanic.birth.rate.slope.multiplier",
              "other.birth.rate.slope.multiplier",
              "age1.non.idu.general.mortality.rate.multiplier",
              "age2.non.idu.general.mortality.rate.multiplier",
              "age3.non.idu.general.mortality.rate.multiplier",
              "age4.non.idu.general.mortality.rate.multiplier",
              "age5.non.idu.general.mortality.rate.multiplier",
              "age1.aging.multiplier",
              "age2.aging.multiplier",
              "age3.aging.multiplier",
              "age4.aging.multiplier")

par = params[par.names]


score.sim = function(sim,
                     include.prior = T){
  
  sim.data.age = sim$get(outcomes = "population",keep.dimensions = c("year","age"),year=as.character(2007:2020))
  sim.data.race = sim$get(outcomes = "population",keep.dimensions = c("year","race"),year=as.character(2007:2020))
 
  if (any(is.na(sim.data.age)))
    browser()
  
  log.likelihood = pop.lik.test$compute(sim,check.consistency=F)
  
  if(include.prior){
    log.prior = calculate.density(EHE.PARAMETERS.PRIOR, x = sim$parameters, log = T)
    rv = -(log.likelihood+log.prior)
  } else
    rv = -(log.likelihood)
  
  if(rv>=.Machine$double.xmax | is.na(rv) | is.nan(rv)){
    rv = .Machine$double.xmax 
  }

  counter <<- counter + 1
  
  inc = 10
  if((counter%%inc)==0)
  {
    end.time = Sys.time()
    
    total.time.per = (as.numeric(end.time)-as.numeric(start.time)) / counter
    time.per.since.last = (as.numeric(end.time)-as.numeric(last.start.time)) / inc
    
    last.start.time <<- end.time
    print(paste0("ran ",counter," simulations, at a run time of ",
                 round(total.time.per, 1), " seconds (",
                 round(time.per.since.last, 1), " seconds per sim over the last ", inc, " simulations)"))
  }
  
  
  rv
   
}

run.and.score.sim = function(par,
                             include.prior = T) {
  
  params = suppressWarnings(get.medians(EHE.PARAMETERS.PRIOR))
  params['global.trate'] = 0.1
  params[names(par)] = exp(par)

  sim = engine$run(parameters = params)
  
  score.sim(sim, include.prior = include.prior)
  
}

set.seed(1234)


counter = 0
start.time = Sys.time()
last.start.time = Sys.time()
optim.result = optim(par = log(par), fn = run.and.score.sim, include.prior = F,
                     method = "BFGS",
           control = list(maxit = 10000)
)
end.time = Sys.time()
 
time.per.sim = (as.numeric(end.time) - as.numeric(start.time)) / counter

print(paste0("DONE! Took ", round(time.per.sim, 1), " seconds per sim for each of ", format(counter, big.mark = ','), " simulations"))

params.optim = params
params.optim[names(optim.result$par)] = exp(optim.result$par)
sim.optim = engine$run(parameters = params.optim)

save(sim.optim, params.optim, optim.result, start.time, end.time, counter, time.per.sim, 
     file=paste0("prelim_results/ehe_optim_pop_result_",Sys.Date(),".Rdata"))

# params.2 = params
# params.2[names(par)] = rv$par
# 
# sim2 = engine$run(parameters = params.2)
# simplot(sim, sim2, "population",facet.by = "age",dimension.values = list(year = as.character(2000:2020)))
# simplot(sim, sim2, "population",facet.by = "race",dimension.values = list(year = as.character(2000:2020)))
# simplot(sim, sim2, "population",dimension.values = list(year = as.character(2000:2020)))
