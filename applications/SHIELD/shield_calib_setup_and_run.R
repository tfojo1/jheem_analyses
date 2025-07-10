
# # Get the command-line arguments passed to the script
# args <- commandArgs(trailingOnly = TRUE)
# 
# # Check if an argument was provided (in this case, checking if it's not empty)
# if (length(args) > 0 && args[1] != "rockfish") {
#   # If the first argument is not empty, use it as the working directory
#   JHEEM.DIR="/home/pkasaie1/scratch4-pkasaie1/jheem_analyses"
# } else {
#   # If no argument is provided, print a message and don't change the working directory
#   JHEEM.DIR="~/OneDrive - Johns Hopkins/SHIELDR01/Simulation/code/jheem_analyses/"
# }
# setwd(JHEEM.DIR)
# cat("Working directory set to:", getwd(), "\n")


##----
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
source('../jheem_analyses/applications/SHIELD/shield_calib_register.R')
source('../jheem_analyses/commoncode/locations_of_interest.R') #provides aliases for locations C.12580=Blatimore MSA

#
VERSION='shield'
LOCATION='C.12580' #Baltimore MSA 
set.seed(00000)
CACHE.FREQ= 100 # how often should write the results to disk (Default: 100)
UPDATE.FREQ= 50 # how often to print messages (Default: 50)

CALIBRATION.NAME = 'calib.07.10.pk0' 


#################
print(paste0("Setting up ",CALIBRATION.NAME," code for ", LOCATION, " (", locations::get.location.name(LOCATION), ")"))
#
clear.calibration.cache(version=VERSION,
                        location=LOCATION,
                       calibration.code = CALIBRATION.NAME,
                        allow.remove.incomplete = T)
print("Cashe is cleared")
#
set.up.calibration(version=VERSION,
                   location=LOCATION,
                   calibration.code = CALIBRATION.NAME,
                   cache.frequency = CACHE.FREQ #100 #how often write the results to disk
)
print(paste0("Calibration is set up for ", LOCATION, " (", locations::get.location.name(LOCATION), ")"))

# Run calibration ----
start.time = Sys.time()
print(paste0("STARTING MCMC RUN OF ", LOCATION, " (", locations::get.location.name(LOCATION), ") AT ", Sys.time()))
mcmc = run.calibration(version = VERSION,
                       location = LOCATION,
                       calibration.code = CALIBRATION.NAME,
                       chains = 1,
                       update.frequency = UPDATE.FREQ,#50,
                       update.detail = 'med')
end.time = Sys.time()
run.time = as.numeric(end.time) - as.numeric(start.time)

# Print run time ----
print(paste0("DONE RUNNING MCMC: Took ",
             round(run.time/60, 0), " minutes to run ",
             format(N.ITER, big.mark = ","),
             " simulations (",
             round(run.time / N.ITER, 1), " seconds per simulation on average)"))


# Save simset
simset = assemble.simulations.from.calibration(version = VERSION,
                                               location = LOCATION,
                                               calibration.code = CALIBRATION.NAME,
                                               allow.incomplete = T)
# filename=paste0("prelim_results/",CALIBRATION.NAME,"_simset_",Sys.Date(),"_",LOCATION,".Rdata")
# save(simset,file=filename)

filename=paste0(get.jheem.root.directory(),"/shield/",CALIBRATION.NAME,"_simset_",Sys.Date(),"_",LOCATION,".Rdata")
save(simset,file =filename )

print(paste0("Simet was saved on disk as:   ", filename))
