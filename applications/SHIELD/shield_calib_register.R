cat("*** Running Shiled_register_calibration.R ***\n")

register.calibration.info('pop.demog.shield', 
                          likelihood.instructions = likelihood.instructions.demographics,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  # the most efficient way is to run it to the last year of data; but it's also helpful to review projections for start
                          parameter.names = c(# can include a subset of parameters
                              POPULATION.PARAMETERS.PRIOR@var.names,
                              AGING.PARAMETERS.PRIOR@var.names 
                          ), 
                          n.iter = 15000,
                          thin = 50, 
                          # fixed.initial.parameter.values = c(global.transmission.rate=3),
                          is.preliminary = T, # it's set to optimization mode with a smaller acceptance rate of 10% to move more quickly 
                          max.run.time.seconds = 10, 
                          description = "A quick run to get population parameters in the general vicinity"
)
# cat("*** Shiled_register_calibration.R completed!***\n")

