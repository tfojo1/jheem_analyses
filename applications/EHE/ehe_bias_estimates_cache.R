source('../jheem_analyses/applications/EHE/ehe_specification.R')

suppression.bias.estimates = get.p.bias.estimates(SURVEILLANCE.MANAGER,
                                                  dimensions = c("age","race","sex","risk"),
                                                  levels.of.stratification = c(0,1),
                                                  outcome.for.p = "suppression",
                                                  outcome.for.n = "diagnosed.prevalence",
                                                  sub.location.type = "COUNTY",
                                                  super.location.type = "STATE",
                                                  main.location.type = "CBSA",
                                                  main.location.type.n.source = "cdc.hiv")

proportion.tested.bias.estimates = get.p.bias.estimates(SURVEILLANCE.MANAGER,
                                                        dimensions = c("age","race","sex"),
                                                        levels.of.stratification = c(0,1),
                                                        outcome.for.p = "proportion.tested",
                                                        outcome.for.n = "adult.population",
                                                        sub.location.type = NULL, # want to be able to do this 
                                                        super.location.type = "STATE",
                                                        main.location.type = "CBSA",
                                                        main.location.type.n.source = "brfss")

awareness.bias.estimates = get.p.bias.estimates(SURVEILLANCE.MANAGER,
                                                        dimensions = c("age","race","sex","risk"),
                                                        levels.of.stratification = c(0,1),
                                                        outcome.for.p = "awareness",
                                                        outcome.for.n = "total.prevalence",
                                                        sub.location.type = NULL, # want to be able to do this 
                                                        super.location.type = "STATE",
                                                        main.location.type = "COUNTY",
                                                        main.location.type.n.source = "cdc.hiv")

cocaine.bias.estimates = get.p.bias.estimates(SURVEILLANCE.MANAGER,
                                              dimensions = c("age"),
                                              levels.of.stratification = c(0,1),
                                              outcome.for.p = "cocaine",
                                              outcome.for.n = "adult.population",
                                              sub.location.type = NULL, # want to be able to do this 
                                              super.location.type = "STATE",
                                              main.location.type = "NSDUH",
                                              main.location.type.n.source = "cdc.hiv")

heroin.bias.estimates = get.p.bias.estimates(SURVEILLANCE.MANAGER,
                                              dimensions = c("age"),
                                              levels.of.stratification = c(0,1),
                                              outcome.for.p = "heroin",
                                              outcome.for.n = "adult.population",
                                              sub.location.type = NULL, # want to be able to do this 
                                              super.location.type = "STATE",
                                              main.location.type = "NSDUH",
                                              main.location.type.n.source = "cdc.hiv")

# already cached
if(1==2){
  cache.object.for.version(object = suppression.bias.estimates, 
                           name = "suppression.bias.estimates", 
                           version = 'ehe', overwrite=T)  
}

# not yet cached
cache.object.for.version(object = proportion.tested.bias.estimates, 
                         name = "proportion.tested.bias.estimates", 
                         version = 'ehe', overwrite=T)

cache.object.for.version(object = awareness.bias.estimates, 
                         name = "awareness.bias.estimates", 
                         version = 'ehe', overwrite=T)
