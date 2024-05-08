#Run a test simulation and get results for 2 MSAs
source("applications/EHE/ehe_specification.R")
source("../jheem_analyses/applications/ehe_disparities/ehe_disparities_interventions.R")

CALIBRATION.CODE=NULL
LOCATIONS=c("C.12580","C.33100") #Baltimore & Miami
INTERVENTIONS=c("noint","testdisp")

collection=create.simset.collection(version="ehe", calibration.code = CALIBRATION.CODE, locations = LOCATIONS, interventions = INTERVENTIONS, n.sim=2)

collection$run(2025, 2035, verbose=TRUE)

results = collection$get(outcomes = c("incidence", "population"),
                         dimension.values = list(year=2035),
                         keep.dimensions = c("race"))


#With actual calibrated simulations
source("applications/EHE/ehe_specification.R")
source("../jheem_analyses/applications/ehe_disparities/ehe_disparities_interventions.R")

CALIBRATION.CODE="full.with.aids"
LOCATIONS=c("C.12580")
#LOCATIONS=c("C.12580","C.33100") #Baltimore & Miami
INTERVENTIONS=c("noint",
                "testdisp")

load("../jheem_analyses/applications/ehe_disparities/simset_2024-05-06_C.12580.Rdata")
#load("../jheem_analyses/prelim_results/simset_2024-05-06_C.12580.Rdata") # for Melissa to run on her computer 
simset$save()
simset

simplot(simset, "new", split.by="race")

collection=create.simset.collection(version="ehe", calibration.code = CALIBRATION.CODE, 
                                    locations = LOCATIONS, interventions = INTERVENTIONS, n.sim=50)

collection$run(2025, 2035, verbose=TRUE)

results = collection$get(outcomes = c("new", "population"),
                         dimension.values = list(year=2035),
                         keep.dimensions = c("race"))

