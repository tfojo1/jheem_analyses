source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

LOCATION = BALTIMORE.MSA

CALIBRATION.CODE.TO.PROCESS = CALIBRATION.CODE.FULL.PLUS.COVID 

# Pull just the last simulation
# mcmc = assemble.mcmc.from.calibration(version = 'ehe',
#                                       location = LOCATION,
#                                       calibration.code = CALIBRATION.CODE.TO.PROCESS,
#                                       allow.incomplete=T)
# sim = mcmc@simulations[[length(mcmc@simulations)]]
# save(sim,file=paste0("prelim_results/",CALIBRATION.CODE.TO.PROCESS,"_",Sys.Date(),"_",location,".Rdata"))


# Save simset
simset = assemble.simulations.from.calibration(version = 'ehe',
                                               location = LOCATION,
                                               calibration.code = CALIBRATION.CODE.TO.PROCESS,
                                               allow.incomplete = T)

simset = simset$burn(keep = 0.5)
simset = simset$thin(keep = 50)

save(simset,file=paste0("prelim_results/",CALIBRATION.CODE.TO.PROCESS,"_simset_",Sys.Date(),"_",LOCATION,".Rdata"))

