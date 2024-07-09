
### Source model specification, define interventions and parameters
source("applications/EHE/ehe_specification.R")

CALIBRATION.CODE= "full.with.aids" #either "full.with.aids" or "init.transmission.ehe"
LOCATIONS=c("C.12580") #Baltimore 
INTERVENTIONS=c("noint", "black.prep")


### Load calibrated simset - repeat next two lines for each location/simset that you have
load("../jheem_analyses/prelim_results/full.with.aids_simset_2024_07-09_C.12580.Rdata") 
simset$save()


### Run interventions and select relevant results
collection=create.simset.collection(version="ehe", calibration.code = CALIBRATION.CODE, 
                                    locations = LOCATIONS, interventions = INTERVENTIONS, n.sim=50)

collection$run(2025, 2035, verbose=TRUE) # stop.for.errors = T if I want to check error messages 

results = collection$get(outcomes = c("new", "population"),
                         dimension.values = list(year=2035),
                         keep.dimensions = c("race"))
