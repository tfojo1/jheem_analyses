source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations.R')

LOCATIONS = c(SEATTLE.MSA,
              BALTIMORE.MSA
  #MIAMI.MSA,
              #HOUSTON.MSA,
              #CHICAGO.MSA,
              #ATLANTA.MSA,
              )

CALIBRATION.CODE.TO.PROCESS = CALIBRATION.CODE.POPULATION # or CALIBRATION.CODE.POPULATION

for(location in LOCATIONS){
  mcmc = assemble.mcmc.from.calibration(version = 'ehe',
                                        location = location,
                                        calibration.code = CALIBRATION.CODE.TO.PROCESS,
                                        allow.incomplete=T)
  
  source('../jheem_analyses/applications/EHE/ehe_specification.R')
  
  # Pull just the last simulation
  sim = mcmc@simulations[[length(mcmc@simulations)]]
  
  save(sim,file=paste0("prelim_results/init.pop.migration.sim_",Sys.Date(),"_",location,".Rdata"))
  
  # Pull all the simulations into a simset
  # simset = join.simulation.sets((mcmc@simulations[mcmc@simulation.indices]))
  # simset = simset$burn(keep = 0.5)
  # simset = simset$thin(keep = 100)
  # 
  # save(simset,file=paste0("prelim_results/init.transmission.SIMSET_",Sys.Date(),"_",location,".Rdata"))

}
