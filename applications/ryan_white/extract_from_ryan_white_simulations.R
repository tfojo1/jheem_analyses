
print("Sourcing code prior to extracting from interventions")

source('../jheem_analyses/applications/ryan_white/ryan_white_main.R')

LOCATIONS = RW.LOCATIONS
FORCE.OVERWRITE = FORCE.REDO


sim.collection=create.simset.collection(version="rw", calibration.code = CALIBRATION.CODE,
                                        locations = LOCATIONS, interventions = RW.INTERVENTION.CODES, n.sim=N.SIM)

full.results = sim.collection$get(outcomes = c('incidence', 'diagnosed.prevalence', 'new',
                                               'rw.clients', 'non.adap.clients','oahs.clients','adap.clients',
                                               'oahs.suppression', 'adap.suppression',
                                               'suppression', 'population'), 
                                  output = 'numerator',
                                  dimension.values=list(year=2010:2035), 
                                  keep.dimensions=c('year','age','race','sex','risk'), 
                                  verbose = VERBOSE)

other.totals = sim.collection$get(outcomes = c('sexual.transmission.rates','prep.uptake','testing'), 
                                  output = 'numerator',
                                  dimension.values=list(year=2010:2035), 
                                  keep.dimensions=c('year'), 
                                  verbose = VERBOSE)

total.sexual.transmission = other.totals[,,'sexual.transmission.rates',,]

all.parameters = sim.collection$get.parameters(verbose = VERBOSE)

full.incidence = full.results[,,,,,,'incidence',,]
total.incidence = apply(full.incidence, c('year','sim','location','intervention'), sum)
incidence.by.race = apply(full.incidence, c('year','race','sim','location','intervention'), sum)
incidence.by.age = apply(full.incidence, c('year','age','sim','location','intervention'), sum)
incidence.by.sex = apply(full.incidence, c('year','sex','sim','location','intervention'), sum)

total.new = apply(full.results[,,,,,,'new',,], c('year','sim','location','intervention'), sum)
total.pop = apply(full.results[,,,,,,'population',,], c('year','sim','location','intervention'), sum)

save(total.incidence,
     incidence.by.race,
     incidence.by.age,
     incidence.by.sex,
     full.incidence,
     
     other.totals,
     
     total.new,
     total.pop,
     
     total.sexual.transmission,
     
     full.results,
     all.parameters,
     
     file=paste0('Q:results/ryan_white/ryan_white_results_', Sys.Date(), ".Rdata")
)
