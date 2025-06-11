
LOCATIONS = LOCATION
print(paste0("Running interventions for locations: ", paste0(LOCATIONS, collapse=', ')))


source("applications/cdc_testing/cdc_testing_main.R")



coll = create.simset.collection('cdct', 
                                calibration.code = 'final.ehe.state',
                                locations = LOCATIONS, 
                                interventions = CDC.TESTING.INTERVENTION.CODES,
                                n.sim = N.SIMS)

coll$run(start.year=2025, end.year=2035, verbose = T, overwrite.prior = F, stop.for.errors = T)
