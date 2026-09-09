# Get location and calibration stage from command-line arguments ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) stop("Usage: Rscript script.R <location> <calibration.stage> <run.stage> [chain]")

LOCATION         <- as.character(args[1])
CALIBRATION.NAME <- as.character(args[2])
RUN.STAGE        <- as.character(args[3])          # setup | run | assemble | all
CHAIN            <- if (length(args) >= 4) as.integer(args[4]) else 1

cat("Location:", LOCATION, "\n")
cat("Calibration stage:", CALIBRATION.NAME, "\n")
cat("Run stage:", RUN.STAGE, "\n")
cat("Chain:", CHAIN, "\n")

# Source requirements ----
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
source('../jheem_analyses/applications/SHIELD/shield_calib_register.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')  

VERSION<- 'shield'
CACHE.FREQ= 500 # how often should write the results to disk (Default: 100)
UPDATE.FREQ= 50 # how often to print messages (Default: 50)
RUN.ROOT.DIR <- get.jheem.root.directory()

#SECTION1: SETUP ----
if (RUN.STAGE == 'setup' | RUN.STAGE == 'all') {
    set.seed(00000)
    #
    print(paste0("Setting up ",CALIBRATION.NAME," code for ", LOCATION, " (", locations::get.location.name(LOCATION), ")"))
    #
    clear.calibration.cache(version=VERSION,
                            location=LOCATION,
                            calibration.code = CALIBRATION.NAME,
                            root.dir = RUN.ROOT.DIR,
                            allow.remove.incomplete = T)
    print("Cache is cleared")
    #
    set.up.calibration(version=VERSION,
                       location=LOCATION,
                       calibration.code = CALIBRATION.NAME,
                       cache.frequency = CACHE.FREQ,
                       root.dir = RUN.ROOT.DIR)
    capture.jheem.provenance.safely(
        start.calibration.provenance(
            version = VERSION,
            location = LOCATION,
            calibration.code = CALIBRATION.NAME,
            root.dir = RUN.ROOT.DIR,
            application = "SHIELD",
            managers = list(
                census = CENSUS.MANAGER,
                syphilis = SURVEILLANCE.MANAGER
            )
        )
    )
    print(paste0("Setup complete for ", LOCATION))
}

#SECTION2: RUN ----
if (RUN.STAGE == 'run'| RUN.STAGE == 'all') {
    #
    start.time <- Sys.time()
    print(paste0("STARTING CHAIN ", CHAIN, " FROM ", CALIBRATION.NAME, " CODE FOR ", LOCATION,
                 " (", locations::get.location.name(LOCATION), ") AT ", Sys.time()))
    #
    # Wrap this in a loop that will re-try it if a write step ever gets interrupted
    attempts <- 1
    while (attempts < 100) {
        finished <- F
        capture.jheem.provenance.safely(
            record.calibration.provenance.event(
                version = VERSION,
                location = LOCATION,
                calibration.code = CALIBRATION.NAME,
                root.dir = RUN.ROOT.DIR,
                status = "attempt_started",
                chain = CHAIN,
                attempt = attempts
            )
        )
        tryCatch({
            mcmc <- run.calibration(version = VERSION,
                                    location = LOCATION,
                                    calibration.code = CALIBRATION.NAME,
                                    root.dir = RUN.ROOT.DIR,
                                    chains = CHAIN,
                                    update.frequency = UPDATE.FREQ,
                                    update.detail = 'med')
            finished <- T
            capture.jheem.provenance.safely(
                record.calibration.provenance.event(
                    version = VERSION,
                    location = LOCATION,
                    calibration.code = CALIBRATION.NAME,
                    root.dir = RUN.ROOT.DIR,
                    status = "attempt_completed",
                    chain = CHAIN,
                    attempt = attempts
                )
            )
        },
        error = function(e) {
            capture.jheem.provenance.safely(
                record.calibration.provenance.event(
                    version = VERSION,
                    location = LOCATION,
                    calibration.code = CALIBRATION.NAME,
                    root.dir = RUN.ROOT.DIR,
                    status = "attempt_failed",
                    chain = CHAIN,
                    attempt = attempts,
                    details = list(error = conditionMessage(e))
                )
            )
            print(paste0("MCMC chain ", CHAIN, " was probably interrupted during write step. Sleeping 5 minutes before retrying..."))
            Sys.sleep(60 * 5)
        })
        if (finished) break
        attempts <- attempts + 1
        print(paste0("Chain ", CHAIN, " retry attempt #", attempts))
    }
    #
    end.time <- Sys.time()
    #
    print(paste0("CHAIN ", CHAIN, " DONE: took ",
                 round(as.numeric(end.time - start.time) / 60, 1), " minutes"))
}

#SECTION3: ASSEMBLE ----
if (RUN.STAGE == 'assemble'| RUN.STAGE == 'all') {
    print(paste0("Assembling all chains from ", CALIBRATION.NAME, " code for ", LOCATION,
                 " (", locations::get.location.name(LOCATION), ")"))
    # Save simset
    simset <- assemble.simulations.from.calibration(version = VERSION,
                                                    location = LOCATION,
                                                    calibration.code = CALIBRATION.NAME,
                                                    root.dir = RUN.ROOT.DIR,
                                                    allow.incomplete = T)
    save.simulation.set(simset, root.dir = RUN.ROOT.DIR)
    capture.jheem.provenance.safely(
        finalize.calibration.provenance(simset, root.dir = RUN.ROOT.DIR)
    )
    print(paste0("Assembly complete for ", LOCATION))
}
