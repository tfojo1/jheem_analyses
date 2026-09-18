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

#SECTION1: SETUP ----
if (RUN.STAGE == 'setup' | RUN.STAGE == 'all') {
    set.seed(00000)
    #
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
                       cache.frequency = CACHE.FREQ )
    print(paste0("Setup complete for ", LOCATION))
}

#SECTION2: RUN ----
if (RUN.STAGE == 'run'| RUN.STAGE == 'all') {
    #
    start.time <- Sys.time()
    print(paste0("STARTING CHAIN ", CHAIN, " FROM ", CALIBRATION.NAME, " CODE FOR ", LOCATION,
                 " (", locations::get.location.name(LOCATION), ") AT ", Sys.time()))
    #
    # Wrap this in a loop that will re-try it if a write step ever gets interrupted.
    # Retrying only ever helps for that transient case: every other failure (OOM, a
    # bug in a likelihood, an unregistered calibration code) is deterministic and
    # will fail again identically. So report the real error, give up as soon as the
    # same error repeats, and exit non-zero when attempts run out so the launcher
    # sees the chain as FAILED instead of DONE.
    MAX.ATTEMPTS        <- 10
    RETRY.SLEEP.SECONDS <- 15   # keep the message and the sleep tied to one value

    mcmc <- NULL
    last.error.message <- NULL

    for (attempt in seq_len(MAX.ATTEMPTS)) {

        error.stack <- NULL
        err <- tryCatch(
            withCallingHandlers({
                mcmc <- run.calibration(version = VERSION,
                                        location = LOCATION,
                                        calibration.code = CALIBRATION.NAME,
                                        chains = CHAIN,
                                        update.frequency = UPDATE.FREQ,
                                        update.detail = 'med')
                NULL  # NULL == success
            },
            # grab the call stack here, while it still exists - by the time the
            # tryCatch handler below runs, it has already been unwound
            error = function(e) error.stack <<- sys.calls()),
            error = function(e) e)

        if (is.null(err)) break

        this.error.message <- conditionMessage(err)
        this.error.call    <- conditionCall(err)

        message("ERROR on attempt ", attempt, " of ", MAX.ATTEMPTS, " for chain ", CHAIN,
                " (", LOCATION, ", ", CALIBRATION.NAME, "): ", this.error.message)
        if (!is.null(this.error.call))
            message("  in call: ", paste(deparse(this.error.call), collapse = " "))
        if (length(error.stack) > 0)
            message("  call stack (innermost last):\n    ",
                    paste(utils::tail(vapply(error.stack,
                                             function(cl) deparse(cl, nlines = 1),
                                             character(1)), 15),
                          collapse = "\n    "))

        if (identical(last.error.message, this.error.message))
            stop("Chain ", CHAIN, " failed twice in a row with the same error, so this is ",
                 "not a transient write interruption. Giving up. Last error: ",
                 this.error.message, call. = FALSE)
        last.error.message <- this.error.message

        if (attempt == MAX.ATTEMPTS)
            stop("Chain ", CHAIN, " failed on all ", MAX.ATTEMPTS, " attempts. Last error: ",
                 this.error.message, call. = FALSE)

        message("Sleeping ", RETRY.SLEEP.SECONDS, " seconds before retry attempt ",
                attempt + 1, " of ", MAX.ATTEMPTS, "...")
        Sys.sleep(RETRY.SLEEP.SECONDS)
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
                                                    allow.incomplete = T)
    save.simulation.set(simset)
    print(paste0("Assembly complete for ", LOCATION))
}


