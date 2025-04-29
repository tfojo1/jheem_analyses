cat("*** Running Shiled_register_calibration.R ***\n")
N.ITER=15000
#SUMMARY -----
# simsets are saved in: /Volumes/jheem$/shield/pop.demog.1.Rdata
#04.14: PK: I removed initial infected population to speed up the sims #check base_params prp.of.initial.population.infected.syphilis=0
#04.18: Ryan added immigration parameters
#04.23: <pop.demog.1> we found discrepancies in the population size, Zoe is working on a fix
#04.25: <pop.demog.wEmigration> same chain run with 6 parameters by Ryan
#04.29: <pop.demog.2> Zoe has update the survillance data. we have a bad fit to older agegroups, and the fertility in Baltimore. Trying the fit in NY

register.calibration.info('pop.demog.2', 
                          likelihood.instructions = likelihood.instructions.demographics,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030,  # the most efficient way is to run it to the last year of data; but it's also helpful to review projections for start
                          parameter.names = c(# can include a subset of parameters
                              POPULATION.PARAMETERS.PRIOR@var.names,
                              AGING.PARAMETERS.PRIOR@var.names 
                          ), 
                          n.iter = N.ITER,
                          thin = 50, 
                          # fixed.initial.parameter.values = c(global.transmission.rate=3),
                          is.preliminary = T, # it's set to optimization mode with a smaller acceptance rate of 10% to move more quickly 
                          max.run.time.seconds = 10, 
                          description = "A quick run to get population parameters in the general vicinity"
)
# cat("*** Shiled_register_calibration.R completed!***\n")


# get.contained.locations('US', 'CBSA')