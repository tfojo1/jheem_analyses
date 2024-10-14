cat("*** Running Shiled_register_calibration.R ***\n")

source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')


CALIBRATION.CODE.POPULATION = 'init.pop.shield'


N.ITER.TEST = 10000
N.ITER = 15000
N.ITER.FULL = 50000

#-- REGISTER POPULATION CALIBRATION  --#
par.names.pop = c(
  POPULATION.PARAMETERS.PRIOR@var.names,
  "global.trate"#,
)

register.calibration.info(CALIBRATION.CODE.POPULATION, 
                          likelihood.instructions = POPULATION.likelihood.instructions,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  # the most efficeint way is to run it to the last year of data; but it's also helpful to review projections for start
                          parameter.names = par.names.pop, # can include a subset of parameters
                          n.iter = N.ITER,
                          thin = 50, 
                          fixed.initial.parameter.values = c(global.trate=0.03), #if there is a previous calibration, it will take values from there.othwise median of priors 
                          is.preliminary = T, # it's set to optimization mode with a smaller acceptance rate of 10% to move more quickly 
                          max.run.time.seconds = 10, 
                          description = "A quick run to get population parameters in the general vicinity"
)

# it will save the runs in the root directory : get.jheem.root.directory()
cat("*** Shiled_register_calibration.R completed!***\n")