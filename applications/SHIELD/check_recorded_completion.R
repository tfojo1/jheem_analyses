# Read-only completion check for the container pipeline; no model is loaded.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Usage: check_recorded_completion.R <location> <calibration-code>")
analyses <- Sys.getenv("JHEEM_ANALYSES_PATH")
source(file.path(analyses, "applications/SHIELD/R/shield_recorded_runtime.R"))
config <- shield.recorded.config()
shield.recorded.assert.checkout(config$analyses_path, config$analyses_ref)
shield.recorded.assert.checkout(config$jheem2_path, config$jheem2_ref)
result <- shield.recorded.validate.outputs(config, args[[1]], args[[2]])
manager <- function(name, tag) {
    directory <- file.path(config$cache_dir, "data-managers", name, tag)
    resolution <- jsonlite::fromJSON(file.path(directory, "resolution.json"))
    if (!identical(shield.recorded.sha256(file.path(directory, name)), resolution$sha256)) {
        stop("Current manager fails SHA-256 verification: ", name)
    }
    resolution
}
expected <- shield.recorded.inputs(config,
    manager("census.manager.rdata", config$census_tag),
    manager("syphilis.manager.rdata", config$syphilis_tag), result$inputs$preceding)
if (!identical(result$inputs, expected)) {
    stop("Completed stage inputs differ from the requested code, managers, or seed: ", args[[2]])
}
cat("Verified completed SHIELD stage:", args[[2]], "for", args[[1]], "\n")
