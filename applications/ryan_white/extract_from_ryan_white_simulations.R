
print("Sourcing code prior to extracting from interventions")

source('../jheem_analyses/applications/ryan_white/ryan_white_main.R')

LOCATIONS = RW.LOCATIONS
FORCE.OVERWRITE = FORCE.REDO
INTERVENTION.CODES = c('noint','loseRW','temploseRW')


sim.collection=create.simset.collection(version="rw", calibration.code = CALIBRATION.CODE,
                                        locations = LOCATIONS, interventions = INTERVENTION.CODES, n.sim=N.SIM)

full.results = sim.collection$get(outcomes = c('incidence', 'diagnosed.prevalence', 'new',
                                               'non.adap.clients','oahs.clients','adap.clients',
                                               'oahs.suppression', 'adap.suppression'), 
                                  output = 'numerator',
                                  dimension.values=list(year=2010:2035), 
                                  keep.dimensions=c('year','age','race','sex','risk'), 
                                  verbose = VERBOSE)

all.parameters = sim.collection$get.parameters(verbose = VERBOSE)

full.incidence = full.results[,,,,,,'incidence',,]
total.incidence = apply(full.incidence, c('year','sim','location','intervention'), sum)
incidence.by.race = apply(full.incidence, c('year','race','sim','location','intervention'), sum)
incidence.by.age = apply(full.incidence, c('year','age','sim','location','intervention'), sum)
incidence.by.sex = apply(full.incidence, c('year','sex','sim','location','intervention'), sum)

total.new = apply(full.results[,,,,,,'new',,], c('year','sim','location','intervention'), sum)

save(total.incidence,
     incidence.by.race,
     incidence.by.age,
     incidence.by.sex,
     full.incidence,
     
     total.new,
     
     full.results,
     all.parameters,
     
     file=paste0('Q:results/ryan_white/ryan_white_results_', Sys.Date(), ".Rdata")
)
