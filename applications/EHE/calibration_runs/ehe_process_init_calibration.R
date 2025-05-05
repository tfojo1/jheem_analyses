source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

LOCATION = "CA"

CALIBRATION.CODE.TO.PROCESS = c(CALIBRATION.CODE.POPULATION, # 1
                                CALIBRATION.CODE.TRANSMISSION, # 2
                                CALIBRATION.CODE.FULL.PLUS.COVID, # 3
                                CALIBRATION.CODE.EHE.FINAL, # 4
                                # STATE-SPECIFIC
                                CALIBRATION.CODE.POP.STATE, # 5
                                CALIBRATION.CODE.TRANS.STATE, # 6
                                CALIBRATION.CODE.FULL.STATE, # 7
                                CALIBRATION.CODE.EHE.FINAL.STATE # 8
                                )[c(7)]


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
sim = extract.last.simulation.from.calibration('ehe',
                                               LOCATION,
                                               CALIBRATION.CODE.TO.PROCESS,
                                               allow.incomplete = T)
                                               
# simset = simset$burn(keep = 0.5)
# simset = simset$thin(keep = 50)

save(simset,file=paste0("prelim_results/",CALIBRATION.CODE.TO.PROCESS,"_simset_",Sys.Date(),"_",LOCATION,".Rdata"))

