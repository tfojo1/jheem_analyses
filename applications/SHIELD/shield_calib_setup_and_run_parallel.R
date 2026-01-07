setwd("jheem/code/jheem_analyses")
cat("Working directory set to:", getwd(), "\n")

##----
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
source('../jheem_analyses/applications/SHIELD/shield_calib_register.R')
source('../jheem_analyses/commoncode/locations_of_interest.R') #provides aliases for locations C.12580=Blatimore MSA


run_shield_calibration <- function(LOCATION, CALIBRATION.NAME) {
    
    
    VERSION='shield' 
    set.seed(00000)
    CACHE.FREQ= 100 # how often should write the results to disk (Default: 100)
    UPDATE.FREQ= 50 # how often to print messages (Default: 50)
    N.ITER = 15000
    
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
    
    filename=paste0(get.jheem.root.directory(),"/shield/",CALIBRATION.NAME,"_simset_",Sys.Date(),"_",LOCATION,".Rdata")
    save(simset,file =filename )
    
    print(paste0("Simet was saved on disk as:   ", filename))
}


library(parallel)

# Put logs in the home directory
log_dir <- path.expand("~")
dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)

# Fixed log filenames — no timestamps inside the worker
jobs <- list(
    list(loc = "C.12060", calib = "calib.demog.ATL.08.08",
         log = file.path(log_dir, "calib.demog.ATL.08.08_C.12060.log")),
    list(loc = "C.12580", calib = "calib.demog.BLT.08.08",
         log = file.path(log_dir, "calib.demog.BLT.08.08_C.12580.log")),
    list(loc = "C.33100", calib = "calib.demog.MIA.08.08",
         log = file.path(log_dir, "calib.demog.MIA.08.08_C.33100.log")),
    list(loc = "C.35620", calib = "calib.demog.NYC.08.08",
         log = file.path(log_dir, "calib.demog.NYC.08.08_C.35620.log"))
)

# Run in parallel
mclapply(jobs, function(job) {
    log_con <- file(job$log, open = "wt")
    sink(log_con, type = "output")
    sink(log_con, type = "message")
    on.exit({
        sink(type = "message")
        sink(type = "output")
        close(log_con)
    }, add = TRUE)
    
    run_shield_calibration(job$loc, job$calib)
    
    return(job$log)
}, mc.cores = 4)  # adjust for your machine
