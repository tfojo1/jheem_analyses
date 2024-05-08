source('../jheem_analyses/applications/DEP/dep_specification.R')

# ALL CACHED
propdep.bias.estimates = get.p.bias.estimates(SURVEILLANCE.MANAGER,
                                                  dimensions = c("age","race","sex","risk"),
                                                  levels.of.stratification = c(0,1),
                                                  outcome.for.p = "prop.dep", ## check names in surv manager 
                                                  outcome.for.n = "adult.population",
                                                  sub.location.type = "COUNTY",
                                                  super.location.type = "STATE",
                                                  main.location.type = "CBSA")


# ALL ALREADY CACHED
if(1==2){
  cache.object.for.version(object = propdep.bias.estimates, 
                           name = "propdep.bias.estimates", 
                           version = 'dep', overwrite=T) 
}