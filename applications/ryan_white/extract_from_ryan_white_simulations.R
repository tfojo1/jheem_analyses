
print("Sourcing code prior to extracting from interventions")

source('../jheem_analyses/applications/ryan_white/ryan_white_specification.R')
source("../jheem_analyses/applications/ryan_white/ryan_white_interventions.R")
source('../jheem_analyses/commoncode/locations_of_interest.R')

LOCATIONS = BALTIMORE.MSA#MSAS.OF.INTEREST
VERBOSE = T
CALIBRATION.CODE = 'full.with.covid2'
N.SIM = 100
FORCE.OVERWRITE = F
INTERVENTION.CODES = c('noint','loseRW','temploseRW')


sim.collection=create.simset.collection(version="rw", calibration.code = CALIBRATION.CODE, 
                                        locations = LOCATIONS, interventions = INTERVENTION.CODES, n.sim=N.SIM)


print("getting total incidence...")
total.incidence = sim.collection$get('incidence', dimension.values=list(year=2010:2035), verbose = VERBOSE)
print("getting incidence by race...")
incidence.by.race = sim.collection$get('incidence', dimension.values=list(year=2010:2035), keep.dimensions='race', verbose = VERBOSE)
print("getting total incidence by age...")
incidence.by.age = sim.collection$get('incidence', dimension.values=list(year=2010:2035), keep.dimensions='age', verbose = VERBOSE)
print("getting total incidence by sex...")
incidence.by.sex = sim.collection$get('incidence', dimension.values=list(year=2010:2035), keep.dimensions='sex', verbose = VERBOSE)

save(total.incidence,
     incidence.by.race,
     incidence.by.age,
     incidence.by.sex,
     file='ryan_white_results.Rdata')