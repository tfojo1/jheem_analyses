
# Get the command-line arguments passed to the script
args <- commandArgs(trailingOnly = TRUE)

# Check if an argument was provided (in this case, checking if it's not empty)
if (length(args) > 0 && args[1] != "rockfish") {
  # If the first argument is not empty, use it as the working directory
  JHEEM.DIR="/home/pkasaie1/scratch4-pkasaie1/jheem_analyses"
} else {
  # If no argument is provided, print a message and don't change the working directory
  JHEEM.DIR="~/OneDrive - Johns Hopkins/SHIELD/Simulation/code/jheem_analyses/"
}
setwd(JHEEM.DIR)
cat("Working directory set to:", getwd(), "\n")


###############################################
{
  LOCATION='C.12580'
  set.seed(12345)
  CALIBRATION.NAME = 'pop.demog.shield.wAging2' 
  print(paste0("Setting up ",CALIBRATION.NAME," code for ", LOCATION, " (", locations::get.location.name(LOCATION), ")"))
  #
  source('../jheem_analyses/applications/SHIELD/calibration/shield_calib_register.R')
  source('../jheem_analyses/commoncode/locations_of_interest.R') #provides aliases for locations C.12580=Blatimore MSA
    #
  clear.calibration.cache(version='shield',
                          location=LOCATION,
                          calibration.code = CALIBRATION.NAME,
                          allow.remove.incomplete = T)
  print("Cashe is cleared")
  
  set.up.calibration(version='shield',
                     location=LOCATION,
                     calibration.code = CALIBRATION.NAME,
                     cache.frequency = 500 #how often write to disk 
  )  
  print(paste0("DONE setting up ", LOCATION, " (", locations::get.location.name(LOCATION), ")"))
}

# Run calibration ----
{
  start.time = Sys.time()
  print(ggplot2::qplot(1,1) + 
          ggplot2::ggtitle(paste0(LOCATION, " - ", 
                                  locations::get.location.name(LOCATION), " - ",
                                  CALIBRATION.NAME)))
  
  print(paste0("STARTING MCMC RUN OF ", LOCATION, " (", locations::get.location.name(LOCATION), ") AT ", Sys.time()))
  mcmc = run.calibration(version = 'shield',
                         location = LOCATION,
                         calibration.code = CALIBRATION.NAME,
                         chains = 1,
                         update.frequency = 20,
                         update.detail = 'med')
  end.time = Sys.time()
  run.time = as.numeric(end.time) - as.numeric(start.time)
  print(paste0("DONE RUNNING MCMC: Took ",
               round(run.time/60, 0), " minutes to run ",
               format(N.ITER, big.mark = ","),
               " simulations (",
               round(run.time / N.ITER, 1), " seconds per simulation on average)"))
}
# # Save sim
# sim = mcmc@simulations[[length(mcmc@simulations)]]
# save(sim,file=paste0("prelim_results/",CALIBRATION.NAME,"_",Sys.Date(),"_",LOCATION,".Rdata"))

# Save simset
simset = assemble.simulations.from.calibration(version = 'shield',
                                               location = LOCATION,
                                               calibration.code = CALIBRATION.NAME,
                                               allow.incomplete = T)
# 
# simset = simset$burn(keep = 0.5)
# simset = simset$thin(keep = 50)
# 
save(simset,file=paste0("prelim_results/",CALIBRATION.NAME,"_simset_",Sys.Date(),"_",LOCATION,".Rdata"))

