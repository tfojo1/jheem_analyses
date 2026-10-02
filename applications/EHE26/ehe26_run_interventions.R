# EHE26: run all interventions for all locations and save the simsets
# Based on applications/cdc_testing/cdc_run_interventions.R
#
# On the cluster: set LOCATION.INDICES before sourcing this file
# to run a chunk of locations per job.
# Example: LOCATION.INDICES = 1:3 runs the first 3 locations.

source('../jheem_analyses/applications/EHE26/ehe26_main.R')

# 1. PICK THE LOCATIONS FOR THIS RUN ----
if (!exists('LOCATION.INDICES'))
    LOCATION.INDICES = 1:length(LOCATIONS)

LOCATIONS.TO.RUN = LOCATIONS[LOCATION.INDICES]
print(paste0("Running EHE26 interventions for: ", paste0(LOCATIONS.TO.RUN, collapse = ', ')))

# 2. SET UP THE COLLECTION ----
# One simset per location x intervention.
collection = create.simset.collection(version = VERSION,
                                      calibration.code = CALIBRATION.CODE,
                                      locations = LOCATIONS.TO.RUN,
                                      interventions = INTERVENTION.CODES,
                                      n.sim = N.SIM)

# 3. RUN AND SAVE ----
# overwrite.prior = F skips anything that was already run and saved.
collection$run(start.year = BASELINE.YEAR,
               end.year = END.YEAR,
               keep.from.year = KEEP.FROM.YEAR,
               overwrite.prior = F,
               verbose = VERBOSE,
               stop.for.errors = T)

print("DONE running EHE26 interventions")
