source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations.R')

LOCATIONS = c('C.12580')

CALIBRATION.CODE.TO.PROCESS = CALIBRATION.CODE.TRANSMISSION 

for(location in LOCATIONS){
  mcmc = assemble.mcmc.from.calibration(version = 'ehe',
                                        location = location,
                                        calibration.code = CALIBRATION.CODE.TO.PROCESS,
                                        allow.incomplete=T)
  
  source('../jheem_analyses/applications/EHE/ehe_specification.R')
  
  # Pull just the last simulation
  sim = mcmc@simulations[[length(mcmc@simulations)]]
  
  save(sim,file=paste0("prelim_results/",CALIBRATION.CODE.TO.PROCESS,"_",Sys.Date(),"_",location,".Rdata"))


}


# trying to assemble a simset
if(1==2){
  source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations.R')
  
  LOCATIONS = c('C.12580')
  
  CALIBRATION.CODE.TO.PROCESS = CALIBRATION.CODE.TRANSMISSION 
  
  for(location in LOCATIONS){
    simset = assemble.simulations.from.calibration(version = 'ehe',
                                                   location = location,
                                                   calibration.code = CALIBRATION.CODE.TO.PROCESS)
    
    simset = simset$burn(keep = 0.5)
    simset = simset$thin(keep = 50)
    
    save(simset,file=paste0("prelim_results/",CALIBRATION.CODE.TO.PROCESS,"_simset_",Sys.Date(),"_",location,".Rdata"))
    
    
  }
  
  
  
  
}
