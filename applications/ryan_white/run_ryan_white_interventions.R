
print("Sourcing code prior to running interventions")

source('../jheem_analyses/applications/ryan_white/ryan_white_specification.R')
source("../jheem_analyses/applications/ryan_white/ryan_white_interventions.R")
source('../jheem_analyses/commoncode/locations_of_interest.R')

LOCATIONS = BALTIMORE.MSA#MSAS.OF.INTEREST
VERBOSE = T
CALIBRATION.CODE = 'full.with.covid2'
N.SIM = 100
FORCE.OVERWRITE = F
INTERVENTION.CODES = c('loseRW','temploseRW')

noint.collection=create.simset.collection(version="rw", calibration.code = CALIBRATION.CODE, 
                                    locations = LOCATIONS, interventions = 'noint', n.sim=N.SIM)

noint.collection$run(2030, 2035, verbose=TRUE, stop.for.errors=T, overwrite.prior=FORCE.OVERWRITE, keep.from.year = 2010)


int.collection=create.simset.collection(version="rw", calibration.code = CALIBRATION.CODE, 
                                          locations = LOCATIONS, interventions = INTERVENTION.CODES, n.sim=N.SIM)

int.collection$run(2025, 2035, verbose=TRUE, stop.for.errors=T, overwrite.prior=FORCE.OVERWRITE, keep.from.year = 2024)
