source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations.R')

ONE.LOCATION = CHICAGO.MSA
CALIBRATION.CODE.TO.PROCESS = CALIBRATION.CODE.TRANSMISSION # or CALIBRATION.CODE.POPULATION

mcmc = assemble.mcmc.from.calibration(version = 'ehe',
                                      location = ONE.LOCATION,
                                      calibration.code = CALIBRATION.CODE.TO.PROCESS,
                                      allow.incomplete=T)

source('../jheem_analyses/applications/EHE/ehe_specification.R')

# Pull all the simulations into a simset
simset = join.simulation.sets((mcmc@simulations[mcmc@simulation.indices]))
simset = simset$burn(keep = 0.5)
simset = simset$thin(keep = 100)

save(simset,file=paste0("prelim_results/init.transmission.SIMSET_",Sys.Date(),"_",ONE.LOCATION,".Rdata"))

# Pull just the last simulation
# sim = mcmc@simulations[[length(mcmc@simulations)]]
# 
# save(sim,file=paste0("prelim_results/init.transmission.sim_",Sys.Date(),"_",ONE.LOCATION,".Rdata"))