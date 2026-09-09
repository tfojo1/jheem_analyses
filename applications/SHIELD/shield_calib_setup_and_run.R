# Get location and calibration stage from command-line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("Usage: Rscript script.R <location> <calibration.stage>")

LOCATION         <- as.character(args[1])
CALIBRATION.NAME <- as.character(args[2])

cat("Location:", LOCATION, "\n")
cat("Calibration stage:", CALIBRATION.NAME, "\n")
##----
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
source('../jheem_analyses/applications/SHIELD/shield_calib_register.R')
source('../jheem_analyses/commoncode/locations_of_interest.R') #provides aliases for locations C.12580=Blatimore MSA

VERSION<- 'shield'
START_FROM_SCRATCH <- TRUE
set.seed(00000)
CACHE.FREQ= 500 # how often should write the results to disk (Default: 100)
UPDATE.FREQ= 50 # how often to print messages (Default: 50)
RUN.ROOT.DIR <- get.jheem.root.directory()

#SECTION1: SETUP
if (START_FROM_SCRATCH) {
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
                       cache.frequency = CACHE.FREQ, #100 #how often write the results to disk
                       root.dir = RUN.ROOT.DIR
    )
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
    print(paste0("Calibration is set up for ", LOCATION, " (", locations::get.location.name(LOCATION), ")"))
}

#SECTION2: RUN
start.time <- Sys.time()
print(paste0("STARTING MCMC RUN OF ", LOCATION, " (", locations::get.location.name(LOCATION), ") AT ", Sys.time()))

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
            chain = 1,
            attempt = attempts
        )
    )
    tryCatch({
        mcmc <- run.calibration(version = VERSION,
                                location = LOCATION,
                                calibration.code = CALIBRATION.NAME,
                                root.dir = RUN.ROOT.DIR,
                                chains = 1,
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
                chain = 1,
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
                chain = 1,
                attempt = attempts,
                details = list(error = conditionMessage(e))
            )
        )
        print("MCMC was probably interrupted during write step. Sleeping 5 minutes before retrying...")
        Sys.sleep(60 * 5)
    })
    if (finished) break
    attempts <- attempts + 1
    print(paste0("Retrying (attempt #", attempts, ")..."))
}

end.time <- Sys.time()
run.time <- as.numeric(end.time) - as.numeric(start.time)

#SECTION3: ASSEMBLE
print(paste0("DONE RUNNING MCMC: Took ",
             round(run.time/60, 0), " minutes to run "))


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

