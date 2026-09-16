## Pure tests for SHIELD runtime configuration and retry semantics.
script.argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script.argument) != 1) stop("Could not locate this test script")
script.path <- normalizePath(sub("^--file=", "", script.argument))
repository.root <- normalizePath(file.path(dirname(script.path), "../.."))
source(file.path(repository.root, "applications/SHIELD/R/shield_runtime.R"))

assert.error <- function(expression, pattern = NULL) {
    error <- tryCatch({
        force(expression)
        NULL
    }, error = identity)
    stopifnot(inherits(error, "error"))
    if (!is.null(pattern)) stopifnot(grepl(pattern, conditionMessage(error)))
    invisible(error)
}

make.getenv <- function(values) {
    force(values)
    function(name, unset = "") {
        if (name %in% names(values)) values[[name]] else unset
    }
}

stopifnot(!shield.env.flag(
    "SHIELD_ENABLE_CONTAINER_SMOKE",
    FALSE,
    getenv = make.getenv(character())
))
stopifnot(shield.env.flag(
    "SHIELD_ENABLE_CONTAINER_SMOKE",
    FALSE,
    getenv = make.getenv(c(SHIELD_ENABLE_CONTAINER_SMOKE = "true"))
))

fixture <- tempfile("shield-runtime-")
dir.create(fixture)
on.exit(unlink(fixture, recursive = TRUE), add = TRUE)
paths <- file.path(fixture, c("analyses", "jheem2", "root", "cache"))
invisible(vapply(paths, dir.create, logical(1)))

base.values <- c(
    JHEEM_ANALYSES_PATH = paths[[1]],
    JHEEM2_PATH = paths[[2]],
    JHEEM_ROOT_DIR = paths[[3]],
    JHEEM_CACHE_DIR = paths[[4]]
)
config <- resolve.shield.runtime.config(make.getenv(base.values))
stopifnot(
    identical(config$run_mode, "resume"),
    identical(config$jheem2_mode, "source"),
    identical(config$input_offline, TRUE),
    identical(config$require_immutable_inputs, FALSE),
    is.null(config$census_manager_tag),
    is.null(config$syphilis_manager_tag),
    identical(config$max_attempts, 1L),
    identical(config$seed, 0L),
    identical(config$allow_incomplete, FALSE),
    identical(config$root_dir, normalizePath(paths[[3]]))
)

fresh.config <- resolve.shield.runtime.config(make.getenv(c(
    base.values,
    SHIELD_RUN_MODE = "fresh",
    SHIELD_ALLOW_INCOMPLETE = "yes",
    SHIELD_MAX_ATTEMPTS = "3",
    SHIELD_RETRY_DELAY_SECONDS = "0",
    SHIELD_RANDOM_SEED = "42",
    SHIELD_RUN_ID = "test-run"
)))
stopifnot(
    identical(fresh.config$run_mode, "fresh"),
    identical(fresh.config$allow_incomplete, TRUE),
    identical(fresh.config$max_attempts, 3L),
    identical(fresh.config$seed, 42L),
    identical(fresh.config$run_id, "test-run")
)

assert.error(
    resolve.shield.runtime.config(make.getenv(base.values[names(base.values) != "JHEEM_ROOT_DIR"])),
    "JHEEM_ROOT_DIR"
)
assert.error(
    resolve.shield.runtime.config(make.getenv(c(base.values, SHIELD_RUN_MODE = "overwrite"))),
    "fresh or resume"
)
assert.error(
    resolve.shield.runtime.config(make.getenv(c(base.values, SHIELD_MAX_ATTEMPTS = "0"))),
    "at least 1"
)
assert.error(
    resolve.shield.runtime.config(make.getenv(c(
        base.values,
        SHIELD_REQUIRE_IMMUTABLE_INPUTS = "true",
        JHEEM_CENSUS_MANAGER_TAG = "data-managers-v1"
    ))),
    "JHEEM_CENSUS_MANAGER_TAG and JHEEM_SYPHILIS_MANAGER_TAG"
)
assert.error(
    resolve.shield.runtime.config(make.getenv(c(
        base.values,
        SHIELD_REQUIRE_IMMUTABLE_INPUTS = "true",
        JHEEM_SYPHILIS_MANAGER_TAG = "syphilis-manager-v1"
    ))),
    "JHEEM_CENSUS_MANAGER_TAG and JHEEM_SYPHILIS_MANAGER_TAG"
)
immutable.config <- resolve.shield.runtime.config(make.getenv(c(
    base.values,
    SHIELD_REQUIRE_IMMUTABLE_INPUTS = "true",
    JHEEM_CENSUS_MANAGER_TAG = "data-managers-v1",
    JHEEM_SYPHILIS_MANAGER_TAG = "syphilis-manager-v1"
)))
stopifnot(
    identical(immutable.config$census_manager_tag, "data-managers-v1"),
    identical(immutable.config$syphilis_manager_tag, "syphilis-manager-v1")
)

## Deterministic failures are never retried, even if max.attempts is larger.
deterministic.attempts <- 0L
deterministic.events <- character()
error <- assert.error(run.shield.with.retry(
    operation = function() {
        deterministic.attempts <<- deterministic.attempts + 1L
        stop("invalid model parameter")
    },
    max.attempts = 3L,
    retry.delay.seconds = 0L,
    on.event = function(status, attempt, error = NULL) {
        deterministic.events <<- c(deterministic.events, paste(status, attempt))
    }
), "invalid model parameter")
stopifnot(
    deterministic.attempts == 1L,
    identical(deterministic.events, c("attempt_started 1", "attempt_failed 1")),
    identical(conditionMessage(error), "invalid model parameter")
)

## A narrowly classified transient storage failure may retry and then succeed.
transient.attempts <- 0L
sleep.delays <- integer()
value <- run.shield.with.retry(
    operation = function() {
        transient.attempts <<- transient.attempts + 1L
        if (transient.attempts == 1L) {
            condition <- simpleError("stale file handle while writing checkpoint")
            class(condition) <- c("shield.transient.storage.error", class(condition))
            stop(condition)
        }
        "completed"
    },
    max.attempts = 2L,
    retry.delay.seconds = 7L,
    sleep = function(seconds) sleep.delays <<- c(sleep.delays, seconds)
)
stopifnot(
    identical(value, "completed"),
    transient.attempts == 2L,
    identical(sleep.delays, 7L)
)

## Exhaustion preserves the original condition and exits with an error.
exhausted.attempts <- 0L
error <- assert.error(run.shield.with.retry(
    operation = function() {
        exhausted.attempts <<- exhausted.attempts + 1L
        stop("input/output error on checkpoint volume")
    },
    max.attempts = 2L,
    retry.delay.seconds = 0L,
    sleep = function(seconds) NULL
), "input/output error")
stopifnot(exhausted.attempts == 2L)

## Source and cache initialization must not mutate Git or install packages.
source.code <- paste(readLines(
    file.path(repository.root, "applications/SHIELD/shield_source_code.R"),
    warn = FALSE
), collapse = "\n")
cache.code <- paste(readLines(
    file.path(repository.root, "commoncode/cache_manager.R"),
    warn = FALSE
), collapse = "\n")
stopifnot(
    !grepl("git[[:space:]]+(pull|fetch|reset|checkout)", source.code, ignore.case = TRUE),
    !grepl("google_mobility_data", source.code, fixed = TRUE),
    !grepl("install\\.packages\\(", cache.code)
)

cat("SHIELD runtime pure tests passed\n")
