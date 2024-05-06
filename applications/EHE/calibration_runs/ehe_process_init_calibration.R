source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations.R')

LOCATIONS = c('C.12580')

CALIBRATION.CODE.TO.PROCESS = CALIBRATION.CODE.FULL.PLUS.AIDS 

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
  
  CALIBRATION.CODE.TO.PROCESS = CALIBRATION.CODE.FULL.PLUS.AIDS 
  
  for(location in LOCATIONS){
    simset = assemble.simulations.from.calibration(version = 'ehe',
                                                   location = location,
                                                   calibration.code = CALIBRATION.CODE.TO.PROCESS)
    
    # I think this is how I will thin/burn?
    # simset = simset$thin
    
    save(sim,file=paste0("prelim_results/",CALIBRATION.CODE.TO.PROCESS,"_",Sys.Date(),"_",location,".Rdata"))
    
    
  }
  
  
  
  
}
