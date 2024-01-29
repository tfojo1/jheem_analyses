print("SOURCING CODE")
source('../jheem_analyses/applications/EHE/ehe_init_pop_calibration_register.R')

print("SETTING UP MCMC")
CALIBRATION.CODE.TRANSMISSION = 'init.transmission.ehe'
N.ITER = 10000

par2 = EHE.PARAMETERS.PRIOR@var.names[grepl('trate', EHE.PARAMETERS.PRIOR@var.names)]

register.calibration.info(CALIBRATION.CODE.TRANSMISSION,
                          likelihood.instructions = transmission.likelihood.instructions,
                          data.manager = SURVEILLANCE.MANAGER,
                          start.year = 1970,
                          end.year = 2030, 
                          parameter.names = c(par.names, par2),
                          n.iter = N.ITER,
                          thin = 10, 
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          description = "A test run",
                          preceding.calibration.codes = CALIBRATION.CODE.POPULATION
)
