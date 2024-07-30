
### Source model specification, define interventions and parameters
source("applications/prep_PK/prep_interventions_PK.R") 



### Load calibrated simset - repeat next two lines for each location/simset that you have
load("prelim_results/full.with.aids_simset_2024-07-09_C.12580.Rdata") 
# check the name of the calibration code 
simset$calibration.code 
# save it in the corretc directory so it's accessible by the code 
simset$save()
#

CALIBRATION.CODE= "full.with.aids" #either "full.with.aids" or "init.transmission.ehe"
LOCATIONS=c("C.12580") #Baltimore 
INTERVENTIONS=c("noint", "black.prep.fix")
#we can see a list of current interventions here:
# INTERVENTION.MANAGER$interventions$...

### Run interventions and select relevant results
collection=create.simset.collection(version="ehe", 
                                    calibration.code = CALIBRATION.CODE, 
                                    locations = LOCATIONS, 
                                    interventions = INTERVENTIONS, 
                                    n.sim=50 #what's this? is it set to 50 because we have 50 calibrated sims? dont we randomly sample them?
                                    )

collection$run(start.year = 2025, 
               end.year = 2035, 
               verbose=TRUE,
               overwrite.prior = F, #saves the previous runs so if you mess up one scenario, you dont need to run the other ones again
               ) # stop.for.errors = T if I want to check error messages 

results = collection$get(outcomes = c("new"),
                         dimension.values = list(year=2035),
                         keep.dimensions = c("race"))
results
