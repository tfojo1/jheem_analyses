# Run from the jheem_analyses repository root:
# Rscript applications/SHIELD/tests/test-recorded-runtime.R
source("applications/SHIELD/R/shield_recorded_runtime.R")

expect.error <- function(expr, pattern) {
    error <- tryCatch({ force(expr); NULL }, error = identity)
    stopifnot(inherits(error, "error"), grepl(pattern, conditionMessage(error)))
}

test.root <- tempfile("shield-recorded-runtime-")
dir.create(test.root)
on.exit(unlink(test.root, recursive = TRUE), add = TRUE)
for (name in c("jheem_analyses", "jheem2", "cache", "state")) {
    dir.create(file.path(test.root, name))
}

values <- c(
    JHEEM_ANALYSES_PATH = file.path(test.root, "jheem_analyses"),
    JHEEM2_PATH = file.path(test.root, "jheem2"),
    JHEEM_CACHE_DIR = file.path(test.root, "cache"),
    JHEEM_ROOT_DIR = file.path(test.root, "state"),
    JHEEM_ANALYSES_REF = paste(rep("a", 40L), collapse = ""),
    JHEEM2_REF = paste(rep("b", 40L), collapse = ""),
    LOCATIONS_REF = paste(rep("c", 40L), collapse = ""),
    SHIELD_RANDOM_SEED = "20260916",
    JHEEM_CENSUS_MANAGER_TAG = "data-managers-v2026.08.26",
    JHEEM_SYPHILIS_MANAGER_TAG = "syphilis-manager-v2026.07.27"
)
getenv <- function(name, unset = "") {
    if (name %in% names(values)) values[[name]] else unset
}

config <- shield.recorded.config(getenv)
stopifnot(identical(config$run_mode, "resume"))
values[["JHEEM_ANALYSES_PATH"]] <- file.path(test.root, "cache")
expect.error(shield.recorded.config(getenv), "must be named jheem_analyses")
values[["JHEEM_ANALYSES_PATH"]] <- file.path(test.root, "jheem_analyses")
expect.error(shield.recorded.assert.state(config, "C.12580", "stage0"),
             "no nonempty chain-1 checkpoint")

directory <- shield.recorded.calibration.dir(config, "C.12580", "stage0")
dir.create(file.path(directory, "cache"), recursive = TRUE)
writeLines("checkpoint", file.path(directory, "cache", "chain1_control.Rdata"))
stopifnot(identical(shield.recorded.assert.state(config, "C.12580", "stage0"),
                    directory))
expect.error(shield.recorded.assert.state(config, "../other", "stage0"),
             "path-safe")

values[["SHIELD_RUN_MODE"]] <- "fresh"
fresh <- shield.recorded.config(getenv)
expect.error(shield.recorded.assert.state(fresh, "C.12580", "stage0"),
             "would replace existing")
stopifnot(identical(shield.recorded.assert.state(fresh, "C.12580", "stage1"),
                    shield.recorded.calibration.dir(fresh, "C.12580", "stage1")))

values[["JHEEM_CENSUS_MANAGER_TAG"]] <- "data-managers-latest"
expect.error(shield.recorded.config(getenv), "immutable dated release")
values[["JHEEM_CENSUS_MANAGER_TAG"]] <- "data-managers-v2026.08.26"
values[["JHEEM_ANALYSES_REF"]] <- "main"
expect.error(shield.recorded.config(getenv), "full 40-character")
values[["JHEEM_ANALYSES_REF"]] <- paste(rep("a", 40L), collapse = "")
values[["JHEEM_CACHE_DIR"]] <- values[["JHEEM_ROOT_DIR"]]
expect.error(shield.recorded.config(getenv), "separate trees")

values[["JHEEM_CACHE_DIR"]] <- file.path(test.root, "cache")
values[["SHIELD_RUN_MODE"]] <- "fresh"
fresh <- shield.recorded.config(getenv)
inputs <- shield.recorded.inputs(fresh,
    list(manager = "census.manager.rdata", resolved_tag = fresh$census_tag,
         sha256 = paste(rep("d", 64L), collapse = "")),
    list(manager = "syphilis.manager.rdata", resolved_tag = fresh$syphilis_tag,
         sha256 = paste(rep("e", 64L), collapse = "")))
expect.error(shield.recorded.inputs(fresh, NULL, NULL), "no matching verified")
shield.recorded.write.receipt(fresh, "C.12580", "stage1", inputs)
expect.error(shield.recorded.write.receipt(fresh, "C.12580", "stage1", inputs),
             "already has an input receipt")
values[["SHIELD_RUN_MODE"]] <- "resume"
resume <- shield.recorded.config(getenv)
shield.recorded.check.receipt(resume, "C.12580", "stage1", inputs)
inputs$analyses_ref <- paste(rep("f", 40L), collapse = "")
expect.error(shield.recorded.check.receipt(resume, "C.12580", "stage1", inputs),
             "differ from")

if (nzchar(Sys.which("git"))) {
    checkout <- file.path(test.root, "jheem_analyses")
    run.git <- function(...) {
        result <- suppressWarnings(system2("git", c("-C", shQuote(checkout), ...),
                                           stdout = TRUE, stderr = TRUE))
        stopifnot(is.null(attr(result, "status")))
        result
    }
    run.git("init", "-q")
    writeLines("original", file.path(checkout, "source.txt"))
    run.git("add", "source.txt")
    run.git("-c", "user.name=Test", "-c", "user.email=test@example.invalid",
            "commit", "-qm", "test")
    revision <- tolower(run.git("rev-parse", "HEAD")[[1L]])
    stopifnot(isTRUE(shield.recorded.assert.checkout(checkout, revision)))
    expect.error(shield.recorded.assert.checkout(checkout,
                 paste(rep("0", 40L), collapse = "")), "does not match")
    writeLines("modified", file.path(checkout, "source.txt"))
    expect.error(shield.recorded.assert.checkout(checkout, revision),
                 "uncommitted changes")
}

cat("Recorded SHIELD runtime preflight tests passed\n")
