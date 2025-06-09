
source("applications/cdc_testing/cdc_testing_main.R")

LOCATION = "AL"

set.up.transmute.calibration('cdct', 
                             from.calibration.code = 'final.ehe.state', 
                             location = LOCATION, 
                             n.chunks = N.CHUNKS,
                             n.sim = N.SIMS,
                             allow.overwrite.cache = TRUE,
                             return.simulations = F)

print(paste0("RUNNNING"))
run.transmute.calibration("cdct",location = LOCATION, n.sim = N.SIMS, chunks = 1:N.CHUNKS,verbose =  T, ignore.errors = F)

simset = assemble.transmuted.simulations('cdct', LOCATION, n.sim = N.SIMS)

