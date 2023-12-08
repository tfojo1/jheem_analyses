x = SURVEILLANCE.MANAGER$pull(outcome = "adult.population",
                              keep.dimensions = c("year","age"),
                              dimension.values = list(location = "C.12580",
                                                      year = 2007:2020))

x = apply(x,c("year","age"),sum,na.rm = T) # combine non-overlapping sources

sim.metadata = get.simulation.metadata('ehe',location = 'C.12580')

mapping = get.ontology.mapping(from.ontology = dimnames(x),
                               to.ontology = sim.metadata$outcome.ontologies$population[c('year','age')])
target.data = mapping$apply(x)

params = suppressWarnings(get.medians(EHE.PARAMETERS.PRIOR))
params['global.trate'] = 0.1

par.names = c("black.birth.rate.multiplier",
              "hispanic.birth.rate.multiplier",
              "other.birth.rate.multiplier",
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

counter = 0
score.sim = function(sim){
  #print("scoring sim")
  sim.data = sim$get(outcomes = "population",keep.dimensions = c("year","age"),year=as.character(2007:2020))
 
  rv = sum((sim.data - target.data)^2)
  
  if(rv>=.Machine$double.xmax | is.na(rv)){
    rv = 367743139602 # this is the score of the original sim 
  }

  counter <<- counter + 1
  
  if((counter%%10)==0)
    print(paste0("ran ",counter," simulations"))
  
  rv
   
}

run.and.score.sim = function(par) {
  
  params = suppressWarnings(get.medians(EHE.PARAMETERS.PRIOR))
  params['global.trate'] = 0.1
  params[names(par)] = par
  
  sim = engine$run(parameters = params)
  
  score.sim(sim)
  
}

set.seed(2468)
rv = optim(par = par, fn = run.and.score.sim, method = "L-BFGS-B",lower = 0,
           control = list(maxit = 6)
           )

# params.2 = params
# params.2[names(par)] = rv$par
# 
# sim2 = engine$run(parameters = params.2)
# simplot(sim, sim2, "population",facet.by = "age",dimension.values = list(year = as.character(2000:2020)))
# simplot(sim, sim2, "population",facet.by = "race",dimension.values = list(year = as.character(2000:2020)))
# simplot(sim, sim2, "population",dimension.values = list(year = as.character(2000:2020)))
