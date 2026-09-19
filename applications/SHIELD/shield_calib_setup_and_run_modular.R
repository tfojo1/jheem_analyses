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

# Locate the repo ----
# Resolved from this script's own path, so the run depends on neither the
# working directory nor the checkout being named "jheem_analyses".
if (!exists("JHEEM.ANALYSES.PATH")) {
    .arg   <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    .ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
    .d <- if (length(.arg))          dirname(normalizePath(sub("^--file=", "", .arg[1])))
          else if (!is.null(.ofile)) dirname(normalizePath(.ofile))
          else                       normalizePath(getwd())
    while (!dir.exists(file.path(.d, "commoncode")) ||
           !dir.exists(file.path(.d, "applications", "SHIELD"))) {
        if (identical(dirname(.d), .d))
            stop("Could not locate the jheem_analyses repo root above ", .d)
        .d <- dirname(.d)
    }
    JHEEM.ANALYSES.PATH <- .d
    rm(.arg, .ofile, .d)
}
SHIELD.DIR <- file.path(JHEEM.ANALYSES.PATH, "applications", "SHIELD")
cat("Repo root:", JHEEM.ANALYSES.PATH, "\n")

# Source requirements ----
source(file.path(SHIELD.DIR, "shield_specification.R"))
source(file.path(SHIELD.DIR, "shield_likelihoods.R"))
source(file.path(SHIELD.DIR, "shield_calib_register.R"))
source(file.path(JHEEM.ANALYSES.PATH, "commoncode", "locations_of_interest.R"))

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
    # =============================================================================
    # RUN THE CHAIN, RETRYING IF THE NAS DRIVE DROPS OUT
    #
    # The problem: the run writes to the NAS drive as it goes. If the drive drops
    # offline mid-write, the run dies even though nothing is wrong with the model.
    # The drive usually comes back within a few minutes.
    #
    # So: if the drive is what broke, wait for it and try again (up to ~95 minutes).
    # If anything else broke, print the error and stop - retrying a real bug 20
    # times only hides it and makes the log impossible to read.
    # =============================================================================

    # --- Settings -----------------------------------------------------------------
    MAX.ATTEMPTS        <- 20       # how many times to try the chain in total
    RETRY.SLEEP.SECONDS <- 60 * 5   # how long to wait between tries (20 tries = ~95 min)
    NAS.POLL.SECONDS    <- 15       # while waiting, how often to check if the drive is back
    RUN.LABEL           <- paste0("Chain ", CHAIN)  # how to refer to this run in the log

    # --- Error messages that mean "the drive went away", not "the model is broken" -
    # This is a backup check. It catches the case where the drive drops out and
    # comes back so fast that the drive test below already sees it as healthy again.
    TRANSIENT.ERROR.PATTERNS <- paste(c(
        "cannot open the connection",
        "cannot open compressed file",
        "cannot open file",
        "error writing to connection",
        "error reading from connection",
        "Input/output error",
        "Operation timed out",
        "Resource temporarily unavailable",
        "Stale file handle",
        "Transport endpoint is not connected",
        "Device not configured",
        "No such file or directory",
        "Permission denied",
        "Interrupted system call",
        "Network is (unreachable|down)",
        "Host is down",
        "Connection (reset|refused|timed out)",
        "bad restore file magic number",         # a file left truncated by a mid-write drop
        "invalid or incomplete compressed data",
        "unexpected end of (file|input)"
    ), collapse = "|")

    # --- Is the NAS drive working right now? --------------------------------------
    # We cannot just ask whether the folder exists, because a disconnected network
    # drive usually still looks like it is there. The only reliable test is to write
    # to it. So: write a tiny file, check it landed, delete it. TRUE = drive is fine.
    nas.is.reachable <- function() {
        tryCatch({
            root <- get.jheem.root.directory()
            if (!dir.exists(root)) return(FALSE)
            probe <- file.path(root, paste0(".nas_probe_", Sys.getpid()))
            on.exit(unlink(probe), add = TRUE)   # always clean up the test file
            cat("probe", file = probe)
            isTRUE(file.exists(probe))
        },
        error = function(e) FALSE, warning = function(w) FALSE)
    }

    # --- Try the chain, up to MAX.ATTEMPTS times ----------------------------------
    mcmc <- NULL
    retry.start.time <- Sys.time()

    for (attempt in seq_len(MAX.ATTEMPTS)) {

        # STEP 1 - Run the chain. If it fails, catch the error instead of letting it
        # crash the script, so we can look at it and decide what to do.
        # Afterwards: err is NULL if the run worked, or the error object if it failed.
        error.stack <- NULL
        err <- tryCatch(
            withCallingHandlers({
                mcmc <- run.calibration(version = VERSION,
                                        location = LOCATION,
                                        calibration.code = CALIBRATION.NAME,
                                        chains = CHAIN,
                                        update.frequency = UPDATE.FREQ,
                                        update.detail = 'med')
                NULL   # reaching this line means the run succeeded
            },
            # Save the list of function calls that led to the error, while it still
            # exists. By the time the tryCatch below runs, R has thrown it away.
            error = function(e) error.stack <<- sys.calls()),
            error = function(e) e)

        # STEP 2 - No error means the chain finished. Stop looping.
        if (is.null(err)) break

        # STEP 3 - Print what actually went wrong: the error message, the line that
        # failed, and the trail of function calls that got there. Without this,
        # every different failure looks identical in the log file.
        this.error.message <- conditionMessage(err)
        this.error.call    <- conditionCall(err)

        message("ERROR on ", RUN.LABEL, ", attempt ", attempt, " of ", MAX.ATTEMPTS,
                " (", LOCATION, ", ", CALIBRATION.NAME, "): ", this.error.message)
        if (!is.null(this.error.call))
            message("  in call: ", paste(deparse(this.error.call), collapse = " "))
        if (length(error.stack) > 0)
            message("  call stack (innermost last):\n    ",
                    paste(utils::tail(vapply(error.stack,
                                             function(cl) deparse(cl, nlines = 1),
                                             character(1)), 15),
                          collapse = "\n    "))

        # STEP 4 - Decide whether trying again is worth it. Three cases:
        #   drive is down                        -> wait for it, then retry
        #   drive is fine, but a storage error   -> a blip that already healed, retry
        #   drive is fine, some other error      -> a real bug, so stop here
        if (!nas.is.reachable())
            message("  the jheem root directory is NOT writable - treating this as a dropped NAS mount")
        else if (grepl(TRANSIENT.ERROR.PATTERNS, this.error.message, ignore.case = TRUE))
            message("  the drive is reachable now, but this error looks like a storage problem - will retry")
        else
            stop(RUN.LABEL, " failed and the jheem root directory is writable, so this is not a ",
                 "NAS dropout. Not retrying - see the error and call stack above. Error: ",
                 this.error.message, call. = FALSE)

        # STEP 5 - Out of attempts. Stop with an error, so that Rscript exits with a
        # non-zero code and the launcher script reports this run as FAILED, instead
        # of carrying on as if the chain had finished.
        if (attempt == MAX.ATTEMPTS)
            stop(RUN.LABEL, " failed on all ", MAX.ATTEMPTS, " attempts over ",
                 round(as.numeric(difftime(Sys.time(), retry.start.time, units = "mins")), 1),
                 " minutes. Last error: ", this.error.message, call. = FALSE)

        # STEP 6 - Wait for the drive to come back. Check every NAS.POLL.SECONDS and
        # start the next attempt as soon as it is back, rather than always sitting
        # out the full 5 minutes.
        message("  waiting up to ", round(RETRY.SLEEP.SECONDS / 60), " minutes for the drive, ",
                "then starting attempt ", attempt + 1, " of ", MAX.ATTEMPTS, "...")
        wait.until <- Sys.time() + RETRY.SLEEP.SECONDS
        repeat {
            seconds.left <- as.numeric(difftime(wait.until, Sys.time(), units = "secs"))
            if (seconds.left <= 0) break
            Sys.sleep(min(NAS.POLL.SECONDS, seconds.left))
            if (nas.is.reachable()) {
                message("  drive is reachable again - retrying now")
                break
            }
        }
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


