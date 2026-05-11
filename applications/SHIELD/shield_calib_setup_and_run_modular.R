# Get location and calibration stage from command-line arguments ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) stop("Usage: Rscript script.R <location> <calibration.stage> <run.stage> [chain]")

LOCATION         <- as.character(args[1])
CALIBRATION.NAME <- as.character(args[2])
RUN.STAGE        <- as.character(args[3])          # setup | run | assemble
CHAIN            <- if (length(args) >= 4) as.integer(args[4]) else 1

cat("Location:", LOCATION, "\n")
cat("Calibration stage:", CALIBRATION.NAME, "\n")
cat("Run stage:", RUN.STAGE, "\n")
cat("Chain:", CHAIN, "\n")

# Source requirements ----
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
source('../jheem_analyses/applications/SHIELD/shield_calib_register.R')
source('../jheem_analyses/commoncode/locations_of_interest.R') #provides aliases for locations C.12580=Blatimore MSA

VERSION<- 'shield'
CACHE.FREQ= 500 # how often should write the results to disk (Default: 100)
UPDATE.FREQ= 50 # how often to print messages (Default: 50)

#SECTION1: SETUP ----
if (RUN.STAGE == 'setup') {
    set.seed(00000)
    START_FROM_SCRATCH <- TRUE
    if (START_FROM_SCRATCH) {
        print(paste0("Setting up ",CALIBRATION.NAME," code for ", LOCATION, " (", locations::get.location.name(LOCATION), ")"))
        #
        clear.calibration.cache(version=VERSION,
                                location=LOCATION,
                                calibration.code = CALIBRATION.NAME,
                                allow.remove.incomplete = T)
        print("Cache is cleared")
    }
    #
    set.up.calibration(version=VERSION,
                       location=LOCATION,
                       calibration.code = CALIBRATION.NAME,
                       cache.frequency = CACHE.FREQ )
    print(paste0("Setup complete for ", LOCATION))
}

#SECTION2: RUN ----
if (RUN.STAGE == 'run') {
    
    start.time <- Sys.time()
    print(paste0("STARTING CHAIN ", CHAIN, " FROM ", CALIBRATION.NAME, " CODE FOR ", LOCATION,
                 " (", locations::get.location.name(LOCATION), ") AT ", Sys.time()))
    
    # Wrap this in a loop that will re-try it if a write step ever gets interrupted
    attempts <- 1
    while (attempts < 100) {
        finished <- F
        tryCatch({
            mcmc <- run.calibration(version = VERSION,
                                    location = LOCATION,
                                    calibration.code = CALIBRATION.NAME,
                                    chains = CHAIN,
                                    update.frequency = UPDATE.FREQ,
                                    update.detail = 'med')
            finished <- T
        },
        error = function(e) {
            print(paste0("MCMC chain ", CHAIN, " was probably interrupted during write step. Sleeping 5 minutes before retrying..."))
            Sys.sleep(60 * 5)
        })
        if (finished) break
        attempts <- attempts + 1
        print(paste0("Chain ", CHAIN, " retry attempt #", attempts))
    }
    
    end.time <- Sys.time()
    
    print(paste0("CHAIN ", CHAIN, " DONE: took ",
                 round(as.numeric(end.time - start.time) / 60, 1), " minutes"))
}

#SECTION3: ASSEMBLE ----
if (RUN.STAGE == 'assemble') {
    print(paste0("Assembling all chains from ", CALIBRATION.NAME, " code for ", LOCATION,
                 " (", locations::get.location.name(LOCATION), ")"))
    # Save simset
    simset <- assemble.simulations.from.calibration(version = VERSION,
                                                    location = LOCATION,
                                                    calibration.code = CALIBRATION.NAME,
                                                    allow.incomplete = T)
    save.simulation.set(simset)
    print(paste0("Assembly complete for ", LOCATION))
}


