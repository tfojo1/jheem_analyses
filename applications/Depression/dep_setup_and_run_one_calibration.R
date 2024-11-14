source('../jheem_analyses/applications/Depression/dep_calibration.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

LOCATION = BALTIMORE.MSA
CALIBRATION.CODES.TO.RUN = "dep.init"

for (CALIBRATION.CODE.TO.RUN in CALIBRATION.CODES.TO.RUN)
{
  set.seed(12345)
  
  print(paste0("Setting up ", LOCATION, " (", locations::get.location.name(LOCATION), ")"))
  
  clear.calibration.cache(version='dep',
                          location=LOCATION,
                          calibration.code = CALIBRATION.CODE.TO.RUN,
                          allow.remove.incomplete = T)
  
  set.up.calibration(version='dep',
                     location=LOCATION,
                     calibration.code = CALIBRATION.CODE.TO.RUN,
                     cache.frequency = 250)  
  
  print(paste0("DONE setting up ", LOCATION, " (", locations::get.location.name(LOCATION), ")"))
  start.time = Sys.time()
  
  print(ggplot2::qplot(1,1) + 
          ggplot2::ggtitle(paste0(LOCATION, " - ", 
                                  locations::get.location.name(LOCATION), " - ",
                                  CALIBRATION.CODE.TO.RUN)))
  
  print(paste0("STARTING MCMC RUN OF ", LOCATION, " (", locations::get.location.name(LOCATION), ") AT ", Sys.time()))
  #Rprof()
  mcmc = run.calibration(version = 'dep',
                         location = LOCATION,
                         calibration.code = CALIBRATION.CODE.TO.RUN,
                         chains = 1,
                         update.frequency = 100,
                         update.detail = 'med')
  end.time = Sys.time()
  run.time = as.numeric(end.time) - as.numeric(start.time)
  # Rprof(NULL)
  # summ = summaryRprof()
  # print(paste0("DONE RUNNING MCMC: Took ",
  #              round(run.time/60, 0), " minutes to run ",
  #              format(N.ITER, big.mark = ","),
  #              " simulations (",
  #              round(run.time / N.ITER, 1), " seconds per simulation on average)"))
  
  # Save sim 
  # sim = mcmc@simulations[[length(mcmc@simulations)]]
  # save(sim,file=paste0("prelim_results/",CALIBRATION.CODE.TO.RUN,"_",Sys.Date(),"_",LOCATION,".Rdata"))
  
  # Save simset
  simset = assemble.simulations.from.calibration(version = 'dep',
                                                 location = LOCATION,
                                                 calibration.code = CALIBRATION.CODE.TO.RUN)
  # 
  # simset = simset$burn(keep = 0.5)
  # simset = simset$thin(keep = 50)
  # 
  save(simset,file=paste0("prelim_results/",CALIBRATION.CODE.TO.RUN,"_simset_",Sys.Date(),"_",LOCATION,".Rdata"))
}
