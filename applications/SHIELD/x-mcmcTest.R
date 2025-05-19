# sample run for Nick to test on the servers
##----
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
source('../jheem_analyses/applications/SHIELD/shield_calib_register.R')
source('../jheem_analyses/commoncode/locations_of_interest.R') #provides aliases for locations C.12580=Blatimore MSA

#
VERSION='shield'
LOCATION='C.12580' #Baltimore MSA
set.seed(00000)

CALIBRATION.NAME = 'pop.demog.test' 
print(paste0("Setting up ",CALIBRATION.NAME," code for ", LOCATION, " (", locations::get.location.name(LOCATION), ")"))
#
# clear.calibration.cache(version=VERSION,
#                         location=LOCATION,
#                        calibration.code = CALIBRATION.NAME,
#                         allow.remove.incomplete = T)
# print("Cashe is cleared")

set.up.calibration(version=VERSION,
                   location=LOCATION,
                   calibration.code = CALIBRATION.NAME,
                   cache.frequency = 1 #how often write the results to disk
)
print(paste0("Calibration is set up for ", LOCATION, " (", locations::get.location.name(LOCATION), ")"))


# Run calibration ----
start.time = Sys.time()
print(paste0("STARTING MCMC RUN OF ", LOCATION, " (", locations::get.location.name(LOCATION), ") AT ", Sys.time()))
mcmc = run.calibration(version = VERSION,
                       location = LOCATION,
                       calibration.code = CALIBRATION.NAME,
                       chains = 1,
                       update.frequency = 1,
                       update.detail = 'med')
end.time = Sys.time()
run.time = as.numeric(end.time) - as.numeric(start.time)
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
filename=paste0("prelim_results/",CALIBRATION.NAME,"_simset_",Sys.Date(),"_",LOCATION,".Rdata")
save(simset,file=filename)
print(paste0("Simet was saved on disk as:   ", filename))
