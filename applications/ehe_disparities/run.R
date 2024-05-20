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

CALIBRATION.CODE= "init.transmission.ehe" # "full.with.aids" or "init.transmission.ehe"
LOCATIONS=c("C.12580","C.12060") #Baltimore & Atlanta
#LOCATIONS=c("C.12580","C.12060","C.33100") #Baltimore, Atlanta & Miami
INTERVENTIONS=c("noint",
                "testdisp")

noint=get.null.intervention()

load("../jheem_analyses/applications/ehe_disparities/simset_2024-05-08_C.12580.Rdata")
load("../jheem_analyses/applications/ehe_disparities/simset_2024-05-19_C.12060.Rdata")
#load("../jheem_analyses/prelim_results/init.transmission.ehe_simset_2024-05-08_C.12580.Rdata") # for Melissa 
#rerun.simulations(simset) # this line takes ~5 min to run; don't need to run this on the "init.transmission.ehe" one 
simset$save()

#simplot(simset, "new", split.by="race")

collection=create.simset.collection(version="ehe", calibration.code = CALIBRATION.CODE, 
                                    locations = LOCATIONS, interventions = INTERVENTIONS, n.sim=50)

collection$run(2025, 2035, verbose=TRUE) # include stop.for.errors = T if I want to check error messages 

results = collection$get(outcomes = c("new", "population"),
                         dimension.values = list(year=2035),
                         keep.dimensions = c("race"))

