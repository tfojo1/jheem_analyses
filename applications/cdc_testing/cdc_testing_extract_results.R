
print("Sourcing code prior to extracting from interventions")

source('../jheem_analyses/applications/cdc_testing/cdc_testing_main.R')

LOCATIONS = CDC.TESTING.LOCATIONS


sim.collection=create.simset.collection(version="cdct", calibration.code = CALIBRATION.CODE,
                                        locations = LOCATIONS, 
                                        interventions = c('baseline', CDC.TESTING.INTERVENTION.CODES),
                                        n.sim=N.SIMS)

print("PULLING TOTALS...")
total.results = sim.collection$get(outcomes = c('incidence', 'diagnosed.prevalence', 'new',
                                                'cdc.funded.tests', 'total.cdc.hiv.test.positivity', 'total.hiv.tests', 'testing',
                                                'suppression', 'population',
                                                'sexual.transmission.rates','prep.uptake','testing'),
                                   output = 'numerator',
                                   dimension.values=list(year=2010:2035),
                                   keep.dimensions=c('year'),
                                   aggregate.total = T,
                                   verbose = VERBOSE)



print("PULLING FULL INCIDENCE...")
full.incidence = sim.collection$get(outcomes = 'incidence',
                                    output = 'numerator',
                                    dimension.values=list(year=2020:2035),
                                    keep.dimensions=c('year','age','race','sex','risk'),
                                    aggregate.total = T,
                                    verbose = VERBOSE)

print("PULLING PARAMETERS...")
all.parameters = sim.collection$get.parameters(verbose = VERBOSE)


print("DONE - SAVING...")
save(total.results,
     full.incidence,
     all.parameters,
     
     file=paste0('Q:results/cdc_testing/cdc_testing_results_', Sys.Date(), ".Rdata")
)
