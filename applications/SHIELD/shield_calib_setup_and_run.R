# Get location and calibration stage from command-line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("Usage: Rscript script.R <location> <calibration.stage>")

LOCATION         <- as.character(args[1])
CALIBRATION.NAME <- as.character(args[2])
recorded.flag <- tolower(trimws(Sys.getenv("SHIELD_RECORDED_RUN", unset = "false")))
if (!recorded.flag %in% c("true", "false")) {
    stop("SHIELD_RECORDED_RUN must be true or false", call. = FALSE)
}
SHIELD.RECORDED.RUN <- identical(recorded.flag, "true")
rm(recorded.flag)
if (!SHIELD.RECORDED.RUN &&
    (identical(tolower(Sys.getenv("SHIELD_CONTAINER_PROFILE")), "recorded") ||
     identical(tolower(Sys.getenv("SHIELD_REQUIRE_IMMUTABLE_INPUTS")), "true"))) {
    stop("Recorded SHIELD profile requires SHIELD_RECORDED_RUN=true; refusing the destructive ordinary path",
         call. = FALSE)
}

if (SHIELD.RECORDED.RUN) {
    recorded.analyses.path <- trimws(Sys.getenv("JHEEM_ANALYSES_PATH"))
    if (!nzchar(recorded.analyses.path)) {
        stop("Recorded SHIELD run requires JHEEM_ANALYSES_PATH", call. = FALSE)
    }
    recorded.source <- file.path(recorded.analyses.path,
                                 "applications/SHIELD/R/shield_recorded_runtime.R")
    source(recorded.source)
    SHIELD.RECORDED.CONFIG <- shield.recorded.config()
    shield.recorded.assert.checkout(SHIELD.RECORDED.CONFIG$analyses_path,
                                    SHIELD.RECORDED.CONFIG$analyses_ref)
    shield.recorded.assert.checkout(SHIELD.RECORDED.CONFIG$jheem2_path,
                                    SHIELD.RECORDED.CONFIG$jheem2_ref)
    shield.recorded.assert.state(SHIELD.RECORDED.CONFIG, LOCATION, CALIBRATION.NAME)
    SHIELD.DIR <- file.path(SHIELD.RECORDED.CONFIG$analyses_path,
                            "applications/SHIELD")
    # The current specification and likelihood files still use repo-root
    # relative sources. Make that assumption explicit for recorded runs.
    setwd(SHIELD.RECORDED.CONFIG$analyses_path)
    rm(recorded.source, recorded.analyses.path)
}

cat("Location:", LOCATION, "\n")
cat("Calibration stage:", CALIBRATION.NAME, "\n")
##----
if (SHIELD.RECORDED.RUN) {
    source(file.path(SHIELD.DIR, "shield_specification.R"))
    source(file.path(SHIELD.DIR, "shield_likelihoods.R"))
    source(file.path(SHIELD.DIR, "shield_calib_register.R"))
    source(file.path(SHIELD.RECORDED.CONFIG$analyses_path,
                     "commoncode/locations_of_interest.R"))
} else {
    source('../jheem_analyses/applications/SHIELD/shield_specification.R')
    source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
    source('../jheem_analyses/applications/SHIELD/shield_calib_register.R')
    source('../jheem_analyses/commoncode/locations_of_interest.R') #provides aliases for locations C.12580=Blatimore MSA
}

VERSION<- 'shield'
START_FROM_SCRATCH <- if (SHIELD.RECORDED.RUN) {
    identical(SHIELD.RECORDED.CONFIG$run_mode, "fresh")
} else TRUE
set.seed(if (SHIELD.RECORDED.RUN) SHIELD.RECORDED.CONFIG$random_seed else 00000)
CACHE.FREQ <- if (SHIELD.RECORDED.RUN) SHIELD.RECORDED.CONFIG$cache_frequency else 500
UPDATE.FREQ <- if (SHIELD.RECORDED.RUN) SHIELD.RECORDED.CONFIG$update_frequency else 50

if (SHIELD.RECORDED.RUN) {
    recorded.inputs <- shield.recorded.inputs(
        SHIELD.RECORDED.CONFIG,
        get.data.manager.resolution(CENSUS.MANAGER),
        get.data.manager.resolution(SURVEILLANCE.MANAGER))
    shield.recorded.check.receipt(SHIELD.RECORDED.CONFIG, LOCATION,
                                  CALIBRATION.NAME, recorded.inputs)
    if (!START_FROM_SCRATCH) {
        progress <- get.calibration.progress(
            version = VERSION, locations = LOCATION,
            calibration.code = CALIBRATION.NAME,
            root.dir = SHIELD.RECORDED.CONFIG$root_dir)
        if (all(is.na(progress))) {
            stop("Resume checkpoint could not be read as calibration progress",
                 call. = FALSE)
        }
    }
}

#SECTION1: SETUP
if (START_FROM_SCRATCH) {
    print(paste0("Setting up ",CALIBRATION.NAME," code for ", LOCATION, " (", locations::get.location.name(LOCATION), ")"))
    #
    if (SHIELD.RECORDED.RUN) {
        # The recorded path never clears pre-existing calibration state.
        shield.recorded.assert.state(SHIELD.RECORDED.CONFIG, LOCATION,
                                     CALIBRATION.NAME)
        shield.recorded.write.receipt(SHIELD.RECORDED.CONFIG, LOCATION,
                                      CALIBRATION.NAME, recorded.inputs)
    } else {
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
                       cache.frequency = CACHE.FREQ #100 #how often write the results to disk
    )
    print(paste0("Calibration is set up for ", LOCATION, " (", locations::get.location.name(LOCATION), ")"))
}

#SECTION2: RUN
start.time <- Sys.time()
print(paste0("STARTING MCMC RUN OF ", LOCATION, " (", locations::get.location.name(LOCATION), ") AT ", Sys.time()))

# *****************************************************************************
# RUN THE CHAIN, RETRYING IF THE NAS DRIVE DROPS OUT
#
# The problem: the run writes to the NAS drive as it goes. If the drive drops
# offline mid-write, the run dies even though nothing is wrong with the model.
# The drive usually comes back within a few minutes.
#
# So: if the drive is what broke, wait for it and try again (up to ~95 minutes).
# If anything else broke, print the error and stop - retrying a real bug 20
# times only hides it and makes the log impossible to read.
# *****************************************************************************

# --- Settings -----------------------------------------------------------------
MAX.ATTEMPTS        <- if (SHIELD.RECORDED.RUN) 1L else 20L
RETRY.SLEEP.SECONDS <- 60 * 5   # how long to wait between tries (20 tries = ~95 min)
NAS.POLL.SECONDS    <- 15       # while waiting, how often to check if the drive is back
RUN.LABEL           <- "the MCMC run"  # how to refer to this run in the log

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

## nas.is.reachable ----
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
                                    chains = 1,
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

end.time <- Sys.time()
run.time <- as.numeric(end.time) - as.numeric(start.time)

#SECTION3: ASSEMBLE
print(paste0("DONE RUNNING MCMC: Took ",
             round(run.time/60, 0), " minutes to run "))


# Save simset
simset <- assemble.simulations.from.calibration(version = VERSION,
                                                location = LOCATION,
                                                calibration.code = CALIBRATION.NAME,
                                                allow.incomplete = T)
save.simulation.set(simset)
