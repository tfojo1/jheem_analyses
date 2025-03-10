
print("Sourcing code prior to running interventions")

source('../jheem_analyses/applications/ryan_white/ryan_white_main.R')

LOCATIONS = LOCATIONS.TO.TRANSMUTE #MSAS.OF.INTEREST
FORCE.OVERWRITE = FORCE.REDO
INTERVENTION.CODES = c('loseRW','temploseRW')

noint.collection=create.simset.collection(version="rw", calibration.code = CALIBRATION.CODE, 
                                    locations = LOCATIONS, interventions = 'noint', n.sim=N.SIM)

noint.collection$run(2030, 2035, verbose=TRUE, stop.for.errors=F, overwrite.prior=FORCE.OVERWRITE, keep.from.year = 2010)


int.collection=create.simset.collection(version="rw", calibration.code = CALIBRATION.CODE, 
                                          locations = LOCATIONS, interventions = INTERVENTION.CODES, n.sim=N.SIM)

int.collection$run(2025, 2035, verbose=TRUE, stop.for.errors=F, overwrite.prior=FORCE.OVERWRITE, keep.from.year = 2024)
