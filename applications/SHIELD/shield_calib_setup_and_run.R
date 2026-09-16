# Run one SHIELD calibration chain from setup through assembly.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
    stop("Usage: Rscript shield_calib_setup_and_run.R <location> <calibration.stage>")
}

LOCATION <- as.character(args[[1]])
CALIBRATION.NAME <- as.character(args[[2]])
JHEEM.ANALYSES.PATH <- trimws(Sys.getenv("JHEEM_ANALYSES_PATH"))
if (!nzchar(JHEEM.ANALYSES.PATH)) JHEEM.ANALYSES.PATH <- "../jheem_analyses"

cat("Location:", LOCATION, "\n")
cat("Calibration stage:", CALIBRATION.NAME, "\n")

source(file.path(JHEEM.ANALYSES.PATH, "applications/SHIELD/shield_specification.R"))
source(file.path(JHEEM.ANALYSES.PATH, "applications/SHIELD/shield_likelihoods.R"))
source(file.path(JHEEM.ANALYSES.PATH, "applications/SHIELD/shield_calib_register.R"))
if (shield.env.flag("SHIELD_ENABLE_CONTAINER_SMOKE", FALSE)) {
    source(file.path(
        JHEEM.ANALYSES.PATH,
        "applications/SHIELD/shield_calib_register_container_smoke.R"
    ))
}
source(file.path(JHEEM.ANALYSES.PATH, "commoncode/locations_of_interest.R"))

VERSION <- "shield"
RUN.ROOT.DIR <- SHIELD.RUNTIME.CONFIG$root_dir
set.seed(SHIELD.RUNTIME.CONFIG$seed)

if (identical(SHIELD.RUNTIME.CONFIG$run_mode, "fresh")) {
    message("Fresh mode requested; removing prior calibration state for this location and code")
    clear.calibration.cache(
        version = VERSION,
        location = LOCATION,
        calibration.code = CALIBRATION.NAME,
        root.dir = RUN.ROOT.DIR,
        allow.remove.incomplete = TRUE
    )
    set.up.calibration(
        version = VERSION,
        location = LOCATION,
        calibration.code = CALIBRATION.NAME,
        cache.frequency = SHIELD.RUNTIME.CONFIG$cache_frequency,
        root.dir = RUN.ROOT.DIR
    )
    capture.jheem.provenance.safely(
        start.calibration.provenance(
            version = VERSION,
            location = LOCATION,
            calibration.code = CALIBRATION.NAME,
            root.dir = RUN.ROOT.DIR,
            application = "SHIELD",
            managers = list(census = CENSUS.MANAGER, syphilis = SURVEILLANCE.MANAGER)
        )
    )
} else {
    assert.shield.resume.state(VERSION, LOCATION, CALIBRATION.NAME, RUN.ROOT.DIR)
    message("Resume mode verified an existing calibration checkpoint")
}

start.time <- Sys.time()
message("Starting MCMC chain 1 at ", start.time)

record.attempt <- function(status, attempt, error = NULL) {
    details <- list(run_id = SHIELD.RUNTIME.CONFIG$run_id)
    if (!is.null(error)) details$error <- conditionMessage(error)
    capture.jheem.provenance.safely(
        record.calibration.provenance.event(
            version = VERSION,
            location = LOCATION,
            calibration.code = CALIBRATION.NAME,
            root.dir = RUN.ROOT.DIR,
            status = status,
            chain = 1,
            attempt = attempt,
            details = details
        )
    )
}

mcmc <- run.shield.with.retry(
    operation = function() run.calibration(
        version = VERSION,
        location = LOCATION,
        calibration.code = CALIBRATION.NAME,
        root.dir = RUN.ROOT.DIR,
        chains = 1,
        update.frequency = SHIELD.RUNTIME.CONFIG$update_frequency,
        update.detail = "med"
    ),
    max.attempts = SHIELD.RUNTIME.CONFIG$max_attempts,
    retry.delay.seconds = SHIELD.RUNTIME.CONFIG$retry_delay_seconds,
    on.event = record.attempt
)

message(
    "MCMC chain 1 completed in ",
    round(as.numeric(difftime(Sys.time(), start.time, units = "mins")), 1),
    " minutes"
)

simset <- assemble.simulations.from.calibration(
    version = VERSION,
    location = LOCATION,
    calibration.code = CALIBRATION.NAME,
    root.dir = RUN.ROOT.DIR,
    allow.incomplete = SHIELD.RUNTIME.CONFIG$allow_incomplete
)
save.simulation.set(simset, root.dir = RUN.ROOT.DIR)
capture.jheem.provenance.safely(
    finalize.calibration.provenance(simset, root.dir = RUN.ROOT.DIR)
)
