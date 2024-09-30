source('../jheem_analyses/applications/SHIELD//shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')

# Calibration population size and demographics ------
CALIBRATION.CODE.POPULATION = 'init.pop.shield'


N.ITER.TEST = 10000
N.ITER = 15000
N.ITER.FULL = 35000

# load params manual
load("../jheem_analyses/applications/EHE/calibration_runs/params.manual_2024_02_21.Rdata")## @Todd: what's this?

print("REGISTERING CALIBRATIONS")
#-- REGISTER POPULATION CALIBRATION  --#
par.names.pop = c(
  POPULATION.PARAMETERS.PRIOR@var.names
)

# Preliminary calibration for population parameters
# The purpose of this is to search the parameter space with larger step sizes to get to a good region and then do a more deep search with multiple chains
# likelihood is defined for each location, joint.pop.migration.total.trans.likelihood.instructions provides the instruction to build the likelihood for a given location
# fixed.initial.parameter.values = c(global.trate=0.03), this parameter has an inproper uniform distribution (0-Inf), so we need a starting value
# is.preliminary = T: if true, we use a smaller acceptance threshold for the calibration at 10% (larger step size), all paramters are set to median value for the starting point (unless stated otherwise)
register.calibration.info(CALIBRATION.CODE.POPULATION,
                          likelihood.instructions = joint.pop.migration.total.trans.likelihood.instructions,  
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030, 
                          parameter.names = par.names.pop,
                          n.iter = N.ITER, #chain size
                          thin = 50, # freq to save results
                          
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          description = "A quick run to get population parameters in the general vicinity"
)
