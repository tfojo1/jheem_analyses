source('../jheem_analyses/applications/EHE/ehe_specification.R')

# ALL CACHED
suppression.bias.estimates = get.p.bias.estimates(SURVEILLANCE.MANAGER,
                                                  dimensions = c("age","race","sex","risk"),
                                                  levels.of.stratification = c(0,1),
                                                  outcome.for.p = "suppression",
                                                  outcome.for.n = "diagnosed.prevalence",
                                                  sub.location.type = "COUNTY",
                                                  super.location.type = "STATE",
                                                  main.location.type = "CBSA",
                                                  main.location.type.p.source = "cdc.aggregated.proportion", 
                                                  main.location.type.n.source = "cdc.hiv")

proportion.tested.bias.estimates = get.p.bias.estimates(SURVEILLANCE.MANAGER,
                                                        dimensions = c("age","race","sex"),
                                                        levels.of.stratification = c(0,1),
                                                        outcome.for.p = "proportion.tested",
                                                        outcome.for.n = "adult.population",
                                                        sub.location.type = NULL, 
                                                        super.location.type = "STATE",
                                                        main.location.type = "CBSA",
                                                        main.location.type.n.source = "census.population") 

hiv.test.positivity.bias.estimates = get.p.bias.estimates(SURVEILLANCE.MANAGER,
                                             dimensions = character(),
                                             levels.of.stratification = c(0), 
                                             outcome.for.p = "hiv.test.positivity",
                                             outcome.for.n = "hiv.tests",
                                             sub.location.type = NULL, 
                                             super.location.type = "STATE",
                                             main.location.type = "CBSA")

awareness.bias.estimates = get.p.bias.estimates(SURVEILLANCE.MANAGER,
                                                        dimensions = c("age","race","sex","risk"),
                                                        levels.of.stratification = c(0,1),
                                                        outcome.for.p = "awareness",
                                                        outcome.for.n = "total.prevalence",
                                                        sub.location.type = NULL, 
                                                        super.location.type = "STATE",
                                                        main.location.type = "COUNTY",
                                                        main.location.type.n.source = "cdc.hiv")

cocaine.bias.estimates = get.p.bias.estimates(SURVEILLANCE.MANAGER,
                                              dimensions = c("age"),
                                              levels.of.stratification = c(0), # eventually 0,1 to add in age
                                              outcome.for.p = "cocaine",
                                              outcome.for.n = "adult.population",
                                              sub.location.type = NULL,
                                              super.location.type = "STATE",
                                              main.location.type = "NSDUH")

heroin.bias.estimates = get.p.bias.estimates(SURVEILLANCE.MANAGER,
                                              dimensions = c("age"),
                                              levels.of.stratification = c(0), # eventually 0,1 to add in age
                                              outcome.for.p = "heroin",
                                              outcome.for.n = "adult.population",
                                              sub.location.type = NULL, 
                                              super.location.type = "STATE",
                                              main.location.type = "NSDUH")

## NOT YET CACHED ## 
hiv.tests.per.population.bias.estimates = get.p.bias.estimates(SURVEILLANCE.MANAGER,
                                                          dimensions = character(),
                                                          levels.of.stratification = c(0), 
                                                          outcome.for.p = "hiv.tests.per.population",
                                                          outcome.for.n = "adult.population",
                                                          sub.location.type = NULL, 
                                                          super.location.type = "STATE",
                                                          main.location.type = "CBSA",
                                                          main.location.type.n.source = 'census.population',
                                                          super.location.type.n.source = 'census.population')

cache.object.for.version(object = awareness.bias.estimates, 
                         name = "heroin.bias.estimates", 
                         version = 'ehe', overwrite=T)

# ALL ALREADY CACHED
if(1==2){
  cache.object.for.version(object = suppression.bias.estimates, 
                           name = "suppression.bias.estimates", 
                           version = 'ehe', overwrite=T)  
  
  cache.object.for.version(object = proportion.tested.bias.estimates, 
                           name = "proportion.tested.bias.estimates", 
                           version = 'ehe', overwrite=T)
  
  cache.object.for.version(object = hiv.test.positivity.bias.estimates, 
                           name = "hiv.test.positivity.bias.estimates", 
                           version = 'ehe', overwrite=T)
  
  cache.object.for.version(object = hiv.tests.per.population.bias.estimates, 
                           name = "hiv.tests.per.population.bias.estimates", 
                           version = 'ehe', overwrite=T)
  
  cache.object.for.version(object = awareness.bias.estimates, 
                           name = "awareness.bias.estimates", 
                           version = 'ehe', overwrite=T)
  
  cache.object.for.version(object = cocaine.bias.estimates, 
                           name = "cocaine.bias.estimates", 
                           version = 'ehe', overwrite=T)
  
  cache.object.for.version(object = heroin.bias.estimates, 
                           name = "heroin.bias.estimates", 
                           version = 'ehe', overwrite=T)
}
