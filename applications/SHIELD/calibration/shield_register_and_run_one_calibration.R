source('../jheem_analyses/applications/SHIELD/calibration/shield_register_calibrations.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

LOCATION = BALTIMORE.MSA
CALIBRATION.CODES.TO.RUN = c(CALIBRATION.CODE.POPULATION 
)[c(2,4)]

for (CALIBRATION.CODE.TO.RUN in CALIBRATION.CODES.TO.RUN)
{
  set.seed(12345)
  print(paste0("Setting up ", LOCATION, " (", locations::get.location.name(LOCATION), ")"))
  # clear Cache ----
  clear.calibration.cache(version='shield',
                          location=LOCATION,
                          calibration.code = CALIBRATION.CODE.TO.RUN,
                          allow.remove.incomplete = T)
  # Setup calibration ----
  set.up.calibration(version='shield',
                     location=LOCATION,
                     calibration.code = CALIBRATION.CODE.TO.RUN,
                     cache.frequency = 250 #save the results periodically to make sure that we dont lose them and we can review them along the way too
                     )  
  
  print(paste0("DONE setting up ", LOCATION, " (", locations::get.location.name(LOCATION), ")"))
  start.time = Sys.time()
  # Plot ----
  print(ggplot2::qplot(1,1) + 
          ggplot2::ggtitle(paste0(LOCATION, " - ", 
                                  locations::get.location.name(LOCATION), " - ",
                                  CALIBRATION.CODE.TO.RUN)))
  
  print(paste0("STARTING MCMC RUN OF ", LOCATION, " (", locations::get.location.name(LOCATION), ") AT ", Sys.time()))
  # Run Calibration ----
  mcmc = run.calibration(version = 'ehe',
                         location = LOCATION,
                         calibration.code = CALIBRATION.CODE.TO.RUN,
                         chains = 1,
                         update.frequency = 100,
                         update.detail = 'med')
  end.time = Sys.time()
  run.time = as.numeric(end.time) - as.numeric(start.time)
   
  # Save simset ----
  simset = assemble.simulations.from.calibration(version = 'ehe',
                                                 location = LOCATION,
                                                 calibration.code = CALIBRATION.CODE.TO.RUN)
  # 
  # simset = simset$burn(keep = 0.5)
  # simset = simset$thin(keep = 50)
  # 
  save(simset,file=paste0("prelim_results/",CALIBRATION.CODE.TO.RUN,"_simset_",Sys.Date(),"_",LOCATION,".Rdata"))
}
