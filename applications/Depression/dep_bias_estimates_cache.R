source('applications/Depression/dep_specification.R')

# ALL CACHED
propdep.bias.estimates = get.p.bias.estimates(SURVEILLANCE.MANAGER,
                                                  dimensions = c("age"),
                                                  levels.of.stratification = c(0,1),
                                                  outcome.for.p = "depression", ## check name in surv manager 
                                                  outcome.for.n = "adult.population",
                                                  sub.location.type = NULL, #"COUNTY"
                                                  super.location.type = "STATE",
                                                  main.location.type = "NSDUH")


# ALL ALREADY CACHED
if(1==2){
  cache.object.for.version(object = propdep.bias.estimates, 
                           name = "propdep.bias.estimates", 
                           version = 'dep', overwrite=T) 
}
