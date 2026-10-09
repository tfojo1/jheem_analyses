# Run from the jheem_analyses repository root:
# Rscript applications/SHIELD/tests/test-recorded-runtime.R
source("applications/SHIELD/R/shield_recorded_runtime.R")

# Stand-in for jheem2's get.calibration.dir(), as a sourced or load_all()
# session exposes it; current jheem2 orders the path calibration, then location.
get.calibration.dir <- function(version, location, calibration.code, root.dir) {
    file.path(root.dir, "mcmc_runs", version, calibration.code, location)
}
get.mcmc.summary.file <- function(version, location, calibration.code, root.dir) {
    file.path(root.dir, "mcmc_summaries", version, calibration.code,
              paste0("summary_", version, "_", location, "_", calibration.code, ".Rdata"))
}
registered <- list(
    stage0 = list(n.chains = 1, preceding.calibration.codes = character()),
    stage1 = list(n.chains = 1, preceding.calibration.codes = "stage0"),
    stage3 = list(n.chains = 4, preceding.calibration.codes = "stage1")
)
get.calibration.info <- function(code) registered[[code]]

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
    BAYESIAN_SIMULATIONS_REF = paste(rep("1", 40L), collapse = ""),
    DISTRIBUTIONS_REF = paste(rep("2", 40L), collapse = ""),
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
stopifnot(identical(directory, file.path(config$root_dir, "mcmc_runs", "shield",
                                         "stage0", "C.12580")))
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
values[["JHEEM_CENSUS_MANAGER_TAG"]] <- "census-manager-latest"
expect.error(shield.recorded.config(getenv), "immutable dated release")
values[["JHEEM_CENSUS_MANAGER_TAG"]] <- "syphilis-manager-v2026.09.09"
expect.error(shield.recorded.config(getenv), "immutable dated release")
values[["JHEEM_CENSUS_MANAGER_TAG"]] <- "census-manager-v2026.10.08"
stopifnot(identical(shield.recorded.config(getenv)$census_tag, "census-manager-v2026.10.08"))
values[["JHEEM_CENSUS_MANAGER_TAG"]] <- "data-managers-v2026.08.26"
values[["JHEEM_ANALYSES_REF"]] <- "main"
expect.error(shield.recorded.config(getenv), "full 40-character")
values[["JHEEM_ANALYSES_REF"]] <- paste(rep("a", 40L), collapse = "")
values[["BAYESIAN_SIMULATIONS_REF"]] <- ""
expect.error(shield.recorded.config(getenv), "requires BAYESIAN_SIMULATIONS_REF")
values[["BAYESIAN_SIMULATIONS_REF"]] <- paste(rep("1", 40L), collapse = "")
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

# The single-process launcher takes single-chain calibrations; phases take any.
stopifnot(identical(shield.recorded.calibration.info("stage1")$n.chains, 1))
expect.error(shield.recorded.calibration.info("stage3"), "single-chain")
stopifnot(identical(shield.recorded.calibration.info("stage3", allow.multiple.chains = TRUE)$n.chains, 4))

# Phases: setup starts a calibration; each chain and assembly continue it.
stopifnot(identical(shield.recorded.phase(getenv), list(phase = "all", chain = NA_integer_)))
values[["SHIELD_RECORDED_PHASE"]] <- "run"
expect.error(shield.recorded.phase(getenv), "SHIELD_RECORDED_CHAIN")
values[["SHIELD_RECORDED_CHAIN"]] <- "3"
stopifnot(identical(shield.recorded.phase(getenv), list(phase = "run", chain = 3L)))
values[["SHIELD_RECORDED_PHASE"]] <- "sample"
expect.error(shield.recorded.phase(getenv), "must be all, setup, run, or assemble")
expect.error(shield.recorded.assert.phase.mode(fresh, list(phase = "run", chain = 1L)),
             "SHIELD_RUN_MODE=resume")
expect.error(shield.recorded.assert.phase.mode(resume, list(phase = "setup", chain = NA)),
             "SHIELD_RUN_MODE=fresh")
stopifnot(isTRUE(shield.recorded.assert.phase.mode(resume, list(phase = "assemble", chain = NA))),
          isTRUE(shield.recorded.assert.phase.mode(fresh, list(phase = "all", chain = NA))))
values[["SHIELD_RECORDED_PHASE"]] <- ""
values[["SHIELD_RECORDED_CHAIN"]] <- ""

# A chain resumes from its own checkpoint, and setup records the chain count.
chained <- shield.recorded.calibration.dir(resume, "C.12580", "stage3")
dir.create(file.path(chained, "cache"), recursive = TRUE)
writeLines("checkpoint", file.path(chained, "cache", "chain2_control.Rdata"))
stopifnot(identical(shield.recorded.assert.state(resume, "C.12580", "stage3", chain = 2L), chained))
expect.error(shield.recorded.assert.state(resume, "C.12580", "stage3", chain = 3L),
             "no nonempty chain-3 checkpoint")
dir.create(dirname(shield.recorded.record.path(fresh, "C.12580", "stage3", "chains.txt")),
           recursive = TRUE, showWarnings = FALSE)
shield.recorded.write.chains(fresh, "C.12580", "stage3", 4L)
stopifnot(identical(readLines(shield.recorded.record.path(fresh, "C.12580", "stage3", "chains.txt")), "4"))

# Assembly refuses until every chain is finished.
progress <- c(100, 100, 100, 100)
get.calibration.progress <- function(version, locations, calibration.code, root.dir, ...) {
    matrix(progress, nrow = 1, dimnames = list(location = locations,
                                               chain = paste0("chain", seq_along(progress))))
}
stopifnot(isTRUE(shield.recorded.assert.chains.complete(resume, "C.12580", "stage3", 4L)))
progress <- c(100, 50, 100, NA)
expect.error(shield.recorded.assert.chains.complete(resume, "C.12580", "stage3", 4L),
             "Not every chain.*50%.*missing")
progress <- c(100, 100)
expect.error(shield.recorded.assert.chains.complete(resume, "C.12580", "stage3", 4L),
             "Not every chain")

# A later stage names the recorded outputs of the stage it starts from.
stopifnot(identical(shield.recorded.preceding(fresh, "C.12580", registered$stage0),
                    list()))
expect.error(shield.recorded.preceding(fresh, "C.12580", registered$stage1),
             "stage0, which has no recorded outputs")
summary.file <- shield.recorded.summary.file(fresh, "C.12580", "stage0")
simset.file <- file.path(fresh$root_dir, "simulations", "shield", "stage0-2",
                         "C.12580", "simset.Rdata")
dir.create(dirname(summary.file), recursive = TRUE)
dir.create(dirname(simset.file), recursive = TRUE)
writeLines("summary", summary.file)
writeLines("simset", simset.file)
stage0.inputs <- shield.recorded.inputs(fresh,
    list(manager = "census.manager.rdata", resolved_tag = fresh$census_tag,
         sha256 = paste(rep("d", 64L), collapse = "")),
    list(manager = "syphilis.manager.rdata", resolved_tag = fresh$syphilis_tag,
         sha256 = paste(rep("e", 64L), collapse = "")))
outputs.path <- shield.recorded.write.outputs(fresh, "C.12580", "stage0", stage0.inputs,
    list(mcmc_summary = summary.file, simulation_set = simset.file))
shield.recorded.write.receipt(fresh, "C.12580", "stage0", stage0.inputs)
written <- jsonlite::fromJSON(outputs.path, simplifyVector = FALSE)
stopifnot(identical(written$inputs, stage0.inputs),
          identical(written$outputs[[1]]$role, "mcmc_summary"),
          identical(written$outputs[[1]]$path,
                    "mcmc_summaries/shield/stage0/summary_shield_C.12580_stage0.Rdata"),
          identical(written$outputs[[2]]$path,
                    "simulations/shield/stage0-2/C.12580/simset.Rdata"),
          identical(written$outputs[[2]]$sha256, shield.recorded.sha256(simset.file)))
expect.error(shield.recorded.write.outputs(fresh, "C.12580", "stage0", stage0.inputs,
    list(simulation_set = file.path(test.root, "cache", "elsewhere.Rdata"))),
    "missing")
writeLines("outside", file.path(test.root, "cache", "elsewhere.Rdata"))
expect.error(shield.recorded.write.outputs(fresh, "C.12580", "stage0", stage0.inputs,
    list(simulation_set = file.path(test.root, "cache", "elsewhere.Rdata"))),
    "outside JHEEM_ROOT_DIR")
# Assembling again rewrites the record to describe the files now on disk.
writeLines("simset, assembled again", simset.file)
shield.recorded.write.outputs(fresh, "C.12580", "stage0", stage0.inputs,
    list(mcmc_summary = summary.file, simulation_set = simset.file))
rewritten <- jsonlite::fromJSON(outputs.path, simplifyVector = FALSE)
stopifnot(identical(rewritten$outputs[[2]]$sha256, shield.recorded.sha256(simset.file)),
          !identical(rewritten$outputs[[2]]$sha256, written$outputs[[2]]$sha256))

preceding <- shield.recorded.preceding(fresh, "C.12580", registered$stage1)
stopifnot(identical(preceding, list(list(calibration_code = "stage0",
                                         outputs_sha256 = shield.recorded.sha256(outputs.path)))))

# A completed record never substitutes for missing or modified artifacts.
shield.recorded.validate.outputs(fresh, "C.12580", "stage0", stage0.inputs)
writeLines("tampered", simset.file)
expect.error(shield.recorded.preceding(fresh, "C.12580", registered$stage1), "verification")
unlink(simset.file)
expect.error(shield.recorded.validate.outputs(fresh, "C.12580", "stage0"), "missing")
writeLines("simset, assembled again", simset.file)
changed.inputs <- stage0.inputs
changed.inputs$random_seed <- "99"
expect.error(shield.recorded.validate.outputs(fresh, "C.12580", "stage0", changed.inputs), "inputs differ")
changed.record <- rewritten
changed.record$location <- "C.99999"
shield.recorded.write.json(changed.record, outputs.path)
expect.error(shield.recorded.validate.outputs(fresh, "C.12580", "stage0"), "invalid recorded")
shield.recorded.write.json(rewritten, outputs.path)
shield.recorded.validate.outputs(fresh, "C.12580", "stage0")
changed.record <- rewritten
changed.record$outputs[[2]]$path <- "../cache/elsewhere.Rdata"
shield.recorded.write.json(changed.record, outputs.path)
expect.error(shield.recorded.validate.outputs(fresh, "C.12580", "stage0"), "outside")
shield.recorded.write.json(rewritten, outputs.path)

# Interrupted setup leaves its receipt intact: neither resume without a
# checkpoint nor another fresh start may silently replace that state.
shield.recorded.write.receipt(fresh, "C.12580", "setup.interrupted", stage0.inputs)
setup.receipt <- shield.recorded.receipt.path(fresh, "C.12580", "setup.interrupted")
setup.digest <- shield.recorded.sha256(setup.receipt)
expect.error(shield.recorded.assert.state(resume, "C.12580", "setup.interrupted"), "no nonempty")
expect.error(shield.recorded.check.receipt(fresh, "C.12580", "setup.interrupted", stage0.inputs), "already has")
stopifnot(identical(shield.recorded.sha256(setup.receipt), setup.digest))
# Preceding outputs are part of a later stage's inputs, so resume compares them.
stage1.inputs <- shield.recorded.inputs(fresh,
    list(manager = "census.manager.rdata", resolved_tag = fresh$census_tag,
         sha256 = paste(rep("d", 64L), collapse = "")),
    list(manager = "syphilis.manager.rdata", resolved_tag = fresh$syphilis_tag,
         sha256 = paste(rep("e", 64L), collapse = "")),
    preceding)
shield.recorded.write.receipt(fresh, "C.12580", "stage1b", stage1.inputs)
shield.recorded.check.receipt(resume, "C.12580", "stage1b", stage1.inputs)
stage1.inputs$preceding[[1]]$outputs_sha256 <- paste(rep("0", 64L), collapse = "")
expect.error(shield.recorded.check.receipt(resume, "C.12580", "stage1b", stage1.inputs),
             "differ from")

# A later completed stage must still verify the earlier outputs it names.
stage1.inputs$preceding <- preceding
shield.recorded.write.outputs(fresh, "C.12580", "stage1b", stage1.inputs,
    list(mcmc_summary = summary.file, simulation_set = simset.file))
shield.recorded.validate.outputs(fresh, "C.12580", "stage1b")
changed.record <- rewritten
changed.record$created_at_utc <- "2000-01-01T00:00:00Z"
shield.recorded.write.json(changed.record, outputs.path)
expect.error(shield.recorded.validate.outputs(fresh, "C.12580", "stage1b"), "preceding-stage outputs changed")
shield.recorded.write.json(rewritten, outputs.path)

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
