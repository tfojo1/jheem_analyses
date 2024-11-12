cat("*** Running Shiled_register_calibration.R ***\n")
# DO NOT SOURCE THIS FILE. IT'S CALLED FROM ELSEWHERE



################################################################################################
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')

CALIBRATION.NAME = 'pop.demog.shield.wAging2'
N.ITER = 15000
# which subset of parameters should we use for calibration?
PARAMETER.NAMES = c(
  POPULATION.PARAMETERS.PRIOR@var.names,
  AGING.PARAMETERS.PRIOR@var.names
)
LIKELIHOOD.INSTRUCTIONS= likelihood.instructions.demographics

register.calibration.info(CALIBRATION.NAME, 
                          likelihood.instructions = LIKELIHOOD.INSTRUCTIONS,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  # the most efficeint way is to run it to the last year of data; but it's also helpful to review projections for start
                          parameter.names = PARAMETER.NAMES, # can include a subset of parameters
                          n.iter = N.ITER,
                          thin = 50, 
                          # fixed.initial.parameter.values = c(global.trate=0), #if there is a previous calibration, it will take values from there.othwise median of priors 
                          is.preliminary = T, # it's set to optimization mode with a smaller acceptance rate of 10% to move more quickly 
                          max.run.time.seconds = 10, 
                          description = "A quick run to get population parameters in the general vicinity"
)

# it will save the runs in the root directory : get.jheem.root.directory()
cat("Calibration code registered for ", CALIBRATION.NAME,"  with ",N.ITER," itterations \n")
# cat("*** Shiled_register_calibration.R completed!***\n")

