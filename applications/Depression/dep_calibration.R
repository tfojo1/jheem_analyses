## Depression calibration


source("applications/EHE/calibration_runs/ehe_register_calibrations.R")
source('../jheem_analyses/applications/Depression/dep_specification.R')
source('applications/Depression/depression_likelihood.R')
#-- REGISTER FULL CALIBRATION WITH COVID-RELATED --#
register.calibration.info('dep.init',
                          likelihood.instructions = dep_likelihood_full,
                          data.manager = SURVEILLANCE.MANAGER,
                          end.year = 2030, 
                          parameter.names = DEP.PARAMETERS.PRIOR@var.names, 
                          n.iter = N.ITER.FULL, 
                          thin = 200, 
                          is.preliminary = T,
                          max.run.time.seconds = 10,
                          preceding.calibration.codes = 'full.with.aids',
                          description = "Initial depression calibration", 
                          draw.from.parent.version = T
)