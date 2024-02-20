source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations.R')

ONE.LOCATION = BALTIMORE.MSA
CALIBRATION.CODE.TO.PROCESS = CALIBRATION.CODE.TRANSMISSION # or CALIBRATION.CODE.POPULATION

mcmc = assemble.mcmc.from.calibration(version = 'ehe',
                                      location = ONE.LOCATION,
                                      calibration.code = CALIBRATION.CODE.TO.PROCESS,
                                      allow.incomplete=T)

# Pull all the simulations into a simset
simset = join.simulation.sets(as.numeric(mcmc@simulations[mcmc@simulation.indices]))

# Pull just the last simulatin
sim = mcmc@simulations[[length(mcmc@simulations)]]