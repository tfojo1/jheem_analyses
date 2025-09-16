
#LOCATIONS = LOCATION
LOCATION.INDICES = 2*6 + 1:6
print(paste0("Running interventions for location indices: ", paste0(LOCATION.INDICES, collapse=', ')))


source("applications/cdc_testing/cdc_testing_main.R")
LOCATIONS = CDC.TESTING.LOCATIONS[LOCATION.INDICES]
print(paste0("Running interventions for locations: ", paste0(LOCATIONS, collapse=', ')))

coll = create.simset.collection('cdct', 
                                calibration.code = 'final.ehe.state',
                                locations = LOCATIONS, 
                                interventions = setdiff(CDC.TESTING.INTERVENTION.CODES, 'noint'),
                                n.sim = N.SIMS)

coll$run(start.year=CDC.TESTING.ANCHOR.YEAR, keep.from.year = CDC.TESTING.ANCHOR.YEAR-1, end.year=CDC.TESTING.ANCHOR.YEAR+10, verbose = T, overwrite.prior = T, stop.for.errors = T)


coll.noint = create.simset.collection('cdct', 
                                calibration.code = 'final.ehe.state',
                                locations = LOCATIONS, 
                                interventions = 'noint',
                                n.sim = N.SIMS)

coll$run(start.year=2034, keep.from.year = 1970, end.year=CDC.TESTING.ANCHOR.YEAR+10, verbose = T, overwrite.prior = T, stop.for.errors = T)

source('applications/cdc_testing/cdc_testing_extract_results.R')