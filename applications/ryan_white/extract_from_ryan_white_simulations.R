
print("Sourcing code prior to extracting from interventions")

source('../jheem_analyses/applications/ryan_white/ryan_white_specification.R')
source("../jheem_analyses/applications/ryan_white/ryan_white_interventions.R")
source('../jheem_analyses/commoncode/locations_of_interest.R')

LOCATIONS = c(BALTIMORE.MSA, NYC.MSA, DALLAS.MSA)#MSAS.OF.INTEREST
VERBOSE = T
CALIBRATION.CODE = 'full.with.covid2'
N.SIM = 100
FORCE.OVERWRITE = F
INTERVENTION.CODES = c('noint','loseRW','temploseRW')


sim.collection=create.simset.collection(version="rw", calibration.code = CALIBRATION.CODE,
                                        locations = LOCATIONS, interventions = INTERVENTION.CODES, n.sim=N.SIM)


print("getting total incidence...")
total.incidence = sim.collection$get('incidence', dimension.values=list(year=2010:2035), verbose = VERBOSE, stop.for.errors = F)
print("getting incidence by race...")
incidence.by.race = sim.collection$get('incidence', dimension.values=list(year=2010:2035), keep.dimensions='race', verbose = VERBOSE)
print("getting incidence by age...")
incidence.by.age = sim.collection$get('incidence', dimension.values=list(year=2010:2035), keep.dimensions='age', verbose = VERBOSE)
print("getting incidence by sex...")
incidence.by.sex = sim.collection$get('incidence', dimension.values=list(year=2010:2035), keep.dimensions='sex', verbose = VERBOSE)
print("getting fully stratified incidence...")
full.incidence = sim.collection$get('incidence', dimension.values=list(year=2010:2035), keep.dimensions=c('age','race','sex','risk'), verbose = VERBOSE)

save(total.incidence,
     incidence.by.race,
     incidence.by.age,
     incidence.by.sex,
     full.incidence,
     file='applications/ryan_white/results/ryan_white_results.Rdata')
