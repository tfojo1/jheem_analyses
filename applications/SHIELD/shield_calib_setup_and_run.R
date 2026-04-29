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

################
if (START_FROM_SCRATCH) {
    print(paste0("Setting up ",CALIBRATION.NAME," code for ", LOCATION, " (", locations::get.location.name(LOCATION), ")"))
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
start.time <- Sys.time()
print(paste0("STARTING MCMC RUN OF ", LOCATION, " (", locations::get.location.name(LOCATION), ") AT ", Sys.time()))

# Wrap this in a loop that will re-try it if a write step ever gets interrupted
attempts <- 1
while (attempts < 100) {
    finished <- F
    tryCatch({
        mcmc <- run.calibration(version = VERSION,
                                location = LOCATION,
                                calibration.code = CALIBRATION.NAME,
                                chains = 1,
                                update.frequency = UPDATE.FREQ,
                                update.detail = 'med')
        finished <- T
    },
    error = function(e) {
        print("MCMC was probably interrupted during write step. Sleeping 5 minutes before retrying...")
        Sys.sleep(60 * 5)
    })
    if (finished) break
    attempts <- attempts + 1
    print(paste0("Retrying (attempt #", attempts, ")..."))
}

end.time <- Sys.time()
run.time <- as.numeric(end.time) - as.numeric(start.time)

# Print run time ----
print(paste0("DONE RUNNING MCMC: Took ",
             round(run.time/60, 0), " minutes to run ",
             format(N_ITER, big.mark = ","),
             " simulations (",
             round(run.time / N_ITER, 1), " seconds per simulation on average)"))


# Save simset
simset <- assemble.simulations.from.calibration(version = VERSION,
                                               location = LOCATION,
                                               calibration.code = CALIBRATION.NAME,
                                               allow.incomplete = T)
save.simulation.set(simset)



