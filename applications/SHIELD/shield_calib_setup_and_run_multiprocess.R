#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
LOCATION <- as.character(args[1])

# Multi-location calibration

print(paste0("Starting ", LOCATION, " at ", Sys.time()))

setwd("C:/Users/azalesa1/Documents/JHEEM/code/jheem_analyses")

source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
source('../jheem_analyses/applications/SHIELD/shield_calib_register_new.R')
source('../jheem_analyses/commoncode/locations_of_interest.R') #provides aliases for locations C.12580=Blatimore MSA

VERSION='shield'
set.seed(00000)
CACHE.FREQ= 500 # how often should write the results to disk (Default: 100)
UPDATE.FREQ= 50 # how often to print messages (Default: 50)

CALIBRATIONS.TO.RUN = c("calib.2.19.stage0.az", "calib.2.19.stage1.az")
START_FROM_SCRATCH = T

for (CALIBRATION.NAME in CALIBRATIONS.TO.RUN) {
    ################
    if (START_FROM_SCRATCH) {
        print(paste0("Setting up ",CALIBRATION.NAME," code for ", LOCATION, " (", locations::get.location.name(LOCATION), ") at ", Sys.time()))
        #
        clear.calibration.cache(version=VERSION,
                                location=LOCATION,
                                calibration.code = CALIBRATION.NAME,
                                allow.remove.incomplete = T)
        print("Cache is cleared")
        #
        set.up.calibration(version=VERSION,
                           location=LOCATION,
                           calibration.code = CALIBRATION.NAME,
                           cache.frequency = CACHE.FREQ #100 #how often write the results to disk
        )
        print(paste0("Calibration is set up for ", LOCATION, " (", locations::get.location.name(LOCATION), ")"))
    }
    
    # Run calibration ----
    start.time = Sys.time()
    print(paste0("STARTING MCMC RUN OF ", LOCATION, " (", locations::get.location.name(LOCATION), ") AT ", Sys.time()))
    mcmc = run.calibration(version = VERSION,
                           location = LOCATION,
                           calibration.code = CALIBRATION.NAME,
                           chains = 1,
                           update.frequency = UPDATE.FREQ,
                           update.detail = 'med')
    end.time = Sys.time()
    run.time = as.numeric(end.time) - as.numeric(start.time)
    
    # Print run time ----
    print(paste0("DONE RUNNING MCMC: Took ",
                 round(run.time/60, 0), " minutes to run ",
                 format(N_ITER, big.mark = ","),
                 " simulations (",
                 round(run.time / N_ITER, 1), " seconds per simulation on average)"))
    
    
    # # Save simset
    simset = assemble.simulations.from.calibration(version = VERSION,
                                                   location = LOCATION,
                                                   calibration.code = CALIBRATION.NAME,
                                                   allow.incomplete = T)
    filename=paste0("prelim_results/",CALIBRATION.NAME,"_simset_",Sys.Date(),"_",LOCATION,".Rdata")
    save(simset,file=filename)
    
    # filename=paste0(get.jheem.root.directory(),"/shield/",CALIBRATION.NAME,"_simset_",Sys.Date(),"_",LOCATION,".Rdata")
    # save(simset,file =filename )
    save.simulation.set(simset)
    
    print(paste0("Simset was saved on disk at ", Sys.time(), " as:   ", filename))
    
}

print(paste0("Done with thread ", THREAD_NUMBER, " at ", Sys.time()))