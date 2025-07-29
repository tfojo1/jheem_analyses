
#LOCATIONS = LOCATION
LOCATION.INDICES = 2*6 + 1:6
print(paste0("Running interventions for location indices: ", paste0(LOCATION.INDICES, collapse=', ')))


source("applications/cdc_testing/cdc_testing_main.R")
LOCATIONS = CDC.TESTING.LOCATIONS[LOCATION.INDICES]
print(paste0("Running interventions for locations: ", paste0(LOCATIONS, collapse=', ')))

coll = create.simset.collection('cdct', 
                                calibration.code = 'final.ehe.state',
                                locations = LOCATIONS, 
                                interventions = 'cdct.intr', # CDC.TESTING.INTERVENTION.CODES,
                                n.sim = N.SIMS)

coll$run(start.year=2025, keep.from.year = 2024, end.year=2035, verbose = T, overwrite.prior = T, stop.for.errors = T)

source('applications/cdc_testing/cdc_testing_extract_results.R')