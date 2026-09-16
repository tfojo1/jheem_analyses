script.argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script.argument) != 1) stop("Could not locate this test script")
script.path <- normalizePath(sub("^--file=", "", script.argument))
repository.root <- normalizePath(file.path(dirname(script.path), "../.."))
original.working.directory <- setwd(repository.root)
on.exit(setwd(original.working.directory), add = TRUE)

source("commoncode/run_provenance.R")

get.calibration.dir <- function(version, location, calibration.code, root.dir) {
    file.path(root.dir, "mcmc_runs", version, location, calibration.code)
}

stopifnot(identical(
    sanitize.git.remote.url("https://user:secret@example.org/team/repo.git"),
    "https://example.org/team/repo.git"
))
stopifnot(identical(
    sanitize.git.remote.url("git@example.org:team/repo.git"),
    "git@example.org:team/repo.git"
))

declared.commit <- paste(rep("a", 40), collapse = "")
copied.source <- tempfile("copied-source-")
dir.create(copied.source)
declared.identity <- collect.git.repository.identity(
    "jheem_analyses", copied.source, declared.ref = declared.commit
)
stopifnot(
    identical(declared.identity$identity, "exact"),
    identical(declared.identity$commit, declared.commit),
    identical(declared.identity$identity_source, "immutable_image_declaration")
)
invalid.declaration <- collect.git.repository.identity(
    "jheem_analyses", copied.source, declared.ref = "main"
)
stopifnot(
    identical(invalid.declaration$identity, "unknown"),
    identical(invalid.declaration$declared_ref, "main")
)
worktree.with.wrong.declaration <- collect.git.repository.identity(
    "jheem_analyses", repository.root, declared.ref = declared.commit
)
stopifnot(identical(worktree.with.wrong.declaration$identity, "mismatch"))

set.seed(20260909)
random.seed.before <- .Random.seed

exact.manager <- new.env(parent = emptyenv())
attr(exact.manager, "jheem.manager.resolution") <- list(
    repository = "example/managers",
    requested_tag = "manager-v1",
    resolved_tag = "manager-v1",
    asset = "manager.rdata",
    sha256 = paste(rep("a", 64), collapse = ""),
    published_at = "2026-09-09T00:00:00Z",
    local_path = "/cache/manager-v1/manager.rdata"
)
floating.manager <- list(
    name = "Legacy manager",
    creation.date = as.POSIXct("2026-01-01", tz = "UTC"),
    last.modified.date = as.POSIXct("2026-08-01", tz = "UTC")
)

context <- collect.jheem.run.context(
    application = "SHIELD",
    operation = "calibration",
    version = "shield",
    location = "C.12580",
    calibration.code = "test-stage",
    root.dir = tempfile("nonexistent-root-"),
    repositories = list(
        jheem_analyses = repository.root,
        missing = file.path(repository.root, "does-not-exist")
    ),
    repository.refs = list(),
    packages = c("base", "package.that.does.not.exist"),
    managers = list(exact = exact.manager, floating = floating.manager)
)

stopifnot(identical(.Random.seed, random.seed.before))
stopifnot(inherits(context, "jheem.run.context"))
stopifnot(identical(context$schema_version, "0.1.0"))
stopifnot(identical(context$request$location, "C.12580"))
stopifnot(context$repositories$jheem_analyses$identity %in% c("exact", "modified"))
stopifnot(nchar(context$repositories$jheem_analyses$commit) == 40)
stopifnot(identical(context$repositories$missing$identity, "unknown"))
stopifnot(identical(context$packages$base$identity, "version_only"))
stopifnot(identical(
    context$packages[["package.that.does.not.exist"]]$identity, "unknown"
))
stopifnot(identical(context$data_managers$exact$identity, "exact"))
stopifnot(identical(context$data_managers$exact$resolved_tag, "manager-v1"))
stopifnot(identical(context$data_managers$floating$identity, "floating"))

serialized <- jsonlite::toJSON(unclass(context), auto_unbox = TRUE, null = "null")
stopifnot(grepl('"schema_version":"0.1.0"', serialized, fixed = TRUE))

printed <- capture.output(print(context))
stopifnot(any(grepl("JHEEM run context", printed, fixed = TRUE)))
stopifnot(any(grepl("manager-v1", printed, fixed = TRUE)))

## Exercise the setup -> event -> artifact receipt lifecycle without running a
## model. The fixture mirrors only the cache slots read by the collector.
methods::setClass(
    "provenance_test_control",
    slots = c(method = "character", var.names = "character",
              burn = "integer", thin = "integer")
)
methods::setClass(
    "provenance_test_chain_state",
    slots = c(current.parameters = "numeric")
)
methods::setClass(
    "provenance_test_chain_control",
    slots = c(chain.id = "character", seeds = "integer",
              chain.state = "provenance_test_chain_state")
)
methods::setClass(
    "provenance_test_global_control",
    slots = c(id = "character", n.chains = "integer", n.chunks = "integer",
              chunk.size = "integer", save.chunk = "logical",
              chain.control.filenames = "character",
              control = "provenance_test_control")
)

lifecycle.root <- file.path(fixture.directory <- tempfile("provenance-lifecycle-"),
                            "files")
dir.create(lifecycle.root, recursive = TRUE)
on.exit(unlink(fixture.directory, recursive = TRUE), add = TRUE)
calibration.directory <- file.path(
    lifecycle.root, "mcmc_runs", "shield", "C.12580", "test-stage"
)
cache.directory <- file.path(calibration.directory, "cache")
dir.create(cache.directory, recursive = TRUE)
cache.control <- methods::new(
    "provenance_test_control", method = "adaptive blockwise metropolis",
    var.names = c("alpha", "beta"), burn = 100L, thin = 10L
)
global.control <- methods::new(
    "provenance_test_global_control", id = "global-test", n.chains = 1L,
    n.chunks = 2L, chunk.size = c(500L, 500L),
    save.chunk = c(TRUE, TRUE), chain.control.filenames = "chain1_control.Rdata",
    control = cache.control
)
chain.state <- methods::new(
    "provenance_test_chain_state",
    current.parameters = c(alpha = 1, beta = 2)
)
chain.control <- methods::new(
    "provenance_test_chain_control", chain.id = "chain-test",
    seeds = c(101L, 202L), chain.state = chain.state
)
save(global.control, file = file.path(cache.directory, "global_control.Rdata"))
save(chain.control, file = file.path(cache.directory, "chain1_control.Rdata"))

original.spool <- Sys.getenv("JHEEM_PROVENANCE_SPOOL_DIR", unset = NA_character_)
test.spool <- file.path(fixture.directory, "spool")
Sys.setenv(JHEEM_PROVENANCE_SPOOL_DIR = test.spool)
on.exit({
    if (is.na(original.spool)) Sys.unsetenv("JHEEM_PROVENANCE_SPOOL_DIR")
    else Sys.setenv(JHEEM_PROVENANCE_SPOOL_DIR = original.spool)
}, add = TRUE)

lifecycle.seed.before <- .Random.seed
started <- start.calibration.provenance(
    version = "shield", location = "C.12580",
    calibration.code = "test-stage", root.dir = lifecycle.root,
    application = "SHIELD", managers = list(syphilis = exact.manager),
    repositories = list(jheem_analyses = repository.root)
)
stopifnot(identical(.Random.seed, lifecycle.seed.before))
stopifnot(identical(started$calibration_cache$identity, "exact"))
stopifnot(identical(started$calibration_cache$chains[[1]]$seeds,
                    c(101L, 202L)))

pointer <- read.current.provenance.run(calibration.directory)
run.paths <- provenance.run.paths(calibration.directory, pointer$run_id)
stopifnot(file.exists(run.paths$context), file.exists(run.paths$summary))
stopifnot(file.exists(file.path(test.spool, "runs", pointer$run_id,
                                "context.json")))

record.calibration.provenance.event(
    version = "shield", location = "C.12580",
    calibration.code = "test-stage", root.dir = lifecycle.root,
    status = "attempt_started", chain = 1, attempt = 1
)
stopifnot(length(list.files(run.paths$events, pattern = "[.]json$")) == 1)
if (.Platform$OS.type == "unix") {
    parallel::mclapply(1:2, function(chain) {
        record.calibration.provenance.event(
            version = "shield", location = "C.12580",
            calibration.code = "test-stage", root.dir = lifecycle.root,
            status = "attempt_completed", chain = chain, attempt = 1
        )
    }, mc.cores = 2)
    stopifnot(length(list.files(run.paths$events, pattern = "[.]json$")) == 3)
}

artifact <- file.path(fixture.directory, "shield_simset.Rdata")
writeBin(charToRaw("simulation set fixture\n"), artifact)
simset <- list(
    version = "shield", sub.version = NULL,
    calibration.code = "test-stage", n.sim = 1,
    location = "C.12580", intervention.code = NULL
)
class(simset) <- c("jheem.simulation.set", "list")
receipt <- finalize.calibration.provenance(
    simset, root.dir = lifecycle.root, artifact.path = artifact
)
stopifnot(identical(.Random.seed, lifecycle.seed.before))
stopifnot(identical(receipt$status, "complete"))
stopifnot(identical(receipt$artifact$sha256, provenance.sha256.file(artifact)))
stopifnot(file.exists(paste0(artifact, ".provenance.json")))
stopifnot(file.exists(paste0(artifact, ".RUN_INFO.txt")))
stopifnot(any(grepl(
    "JHEEM run context",
    readLines(paste0(artifact, ".RUN_INFO.txt"), warn = FALSE),
    fixed = TRUE
)))

inspection <- inspect.jheem.artifact.provenance(artifact)
stopifnot(identical(inspection$found_by, "adjacent_sidecar"))
unlink(paste0(artifact, ".provenance.json"))
digest.inspection <- inspect.jheem.artifact.provenance(artifact)
stopifnot(identical(digest.inspection$found_by, "artifact_digest"))
stopifnot(identical(digest.inspection$receipt$run_id, pointer$run_id))

## A digest may legitimately have more than one provenance origin. Preserve all
## records rather than allowing a later run to overwrite the earlier receipt.
second.record <- digest.inspection
second.record$found_by <- NULL
second.record$receipt$run_id <- "second-origin"
second.digest.record <- file.path(
    test.spool, "by-digest", receipt$artifact$sha256, "second-origin.json"
)
atomic.write.provenance.json(second.record, second.digest.record)
multiple.inspection <- inspect.jheem.artifact.provenance(artifact)
stopifnot(identical(multiple.inspection$found_by, "artifact_digest"))
stopifnot(identical(multiple.inspection$match_count, 2L))
stopifnot(setequal(
    vapply(multiple.inspection$matches, function(record) record$receipt$run_id,
           character(1)),
    c(pointer$run_id, "second-origin")
))

## Disabling capture is a true no-op.
disabled.root <- file.path(fixture.directory, "disabled-root")
dir.create(file.path(disabled.root, "mcmc_runs", "shield", "C.12580",
                     "disabled-stage", "cache"), recursive = TRUE)
original.enabled <- Sys.getenv("JHEEM_PROVENANCE_ENABLED", unset = NA_character_)
Sys.setenv(JHEEM_PROVENANCE_ENABLED = "false")
disabled.result <- start.calibration.provenance(
    version = "shield", location = "C.12580",
    calibration.code = "disabled-stage", root.dir = disabled.root
)
if (is.na(original.enabled)) {
    Sys.unsetenv("JHEEM_PROVENANCE_ENABLED")
} else {
    Sys.setenv(JHEEM_PROVENANCE_ENABLED = original.enabled)
}
stopifnot(is.null(disabled.result))
stopifnot(!dir.exists(file.path(
    disabled.root, "mcmc_runs", "shield", "C.12580", "disabled-stage",
    "provenance"
)))

## An unavailable local spool must not erase a valid primary record.
unavailable.spool <- file.path(fixture.directory, "not-a-directory")
writeLines("occupied", unavailable.spool)
Sys.setenv(JHEEM_PROVENANCE_SPOOL_DIR = unavailable.spool)
spool.failure <- suppressWarnings(start.calibration.provenance(
    version = "shield", location = "C.12580",
    calibration.code = "test-stage", root.dir = lifecycle.root,
    application = "SHIELD", repositories = list()
))
spool.failure.paths <- provenance.run.paths(
    calibration.directory, spool.failure$run_id
)
stopifnot(file.exists(spool.failure.paths$context))
stopifnot(file.exists(provenance.run.paths(calibration.directory)$current))

cat("run provenance collector tests passed\n")
