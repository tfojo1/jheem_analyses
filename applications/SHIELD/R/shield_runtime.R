## Runtime configuration and failure semantics shared by SHIELD entrypoints.
## This file is intentionally dependency-free so it can be tested before the
## model specification or cached data managers are loaded.

shield.env.value <- function(name, default = NULL, required = FALSE, getenv = Sys.getenv) {
    value <- trimws(getenv(name, unset = ""))
    if (!nzchar(value)) value <- default
    if (required && (is.null(value) || !nzchar(value))) {
        stop("Required environment variable ", name, " is not set", call. = FALSE)
    }
    value
}

shield.env.flag <- function(name, default = FALSE, getenv = Sys.getenv) {
    value <- shield.env.value(name, default = if (default) "true" else "false", getenv = getenv)
    normalized <- tolower(value)
    if (!normalized %in% c("true", "false", "1", "0", "yes", "no")) {
        stop(name, " must be one of true, false, 1, 0, yes, or no", call. = FALSE)
    }
    normalized %in% c("true", "1", "yes")
}

shield.env.integer <- function(name, default, minimum = NULL, getenv = Sys.getenv) {
    value <- shield.env.value(name, default = as.character(default), getenv = getenv)
    parsed <- suppressWarnings(as.integer(value))
    if (is.na(parsed) || !identical(as.character(parsed), value)) {
        stop(name, " must be an integer", call. = FALSE)
    }
    if (!is.null(minimum) && parsed < minimum) {
        stop(name, " must be at least ", minimum, call. = FALSE)
    }
    parsed
}

shield.require.directory <- function(name, path, writable = FALSE) {
    if (!dir.exists(path)) {
        stop(name, " does not exist or is not a directory: ", path, call. = FALSE)
    }
    normalized <- normalizePath(path, mustWork = TRUE)
    if (writable && file.access(normalized, mode = 2) != 0) {
        stop(name, " is not writable: ", normalized, call. = FALSE)
    }
    normalized
}

resolve.shield.runtime.config <- function(getenv = Sys.getenv) {
    input.offline <- shield.env.flag("SHIELD_INPUT_OFFLINE", TRUE, getenv)
    require.immutable.inputs <- shield.env.flag(
        "SHIELD_REQUIRE_IMMUTABLE_INPUTS", FALSE, getenv
    )
    syphilis.manager.tag <- shield.env.value(
        "JHEEM_SYPHILIS_MANAGER_TAG", NULL, getenv = getenv
    )
    if (require.immutable.inputs && is.null(syphilis.manager.tag)) {
        stop(
            "JHEEM_SYPHILIS_MANAGER_TAG is required when ",
            "SHIELD_REQUIRE_IMMUTABLE_INPUTS=true",
            call. = FALSE
        )
    }
    analyses.path <- shield.require.directory(
        "JHEEM_ANALYSES_PATH",
        shield.env.value("JHEEM_ANALYSES_PATH", "../jheem_analyses", getenv = getenv)
    )
    jheem2.path <- shield.require.directory(
        "JHEEM2_PATH",
        shield.env.value("JHEEM2_PATH", "../jheem2", getenv = getenv)
    )
    root.dir <- shield.require.directory(
        "JHEEM_ROOT_DIR",
        shield.env.value("JHEEM_ROOT_DIR", required = TRUE, getenv = getenv),
        writable = TRUE
    )
    cache.dir <- shield.require.directory(
        "JHEEM_CACHE_DIR",
        shield.env.value("JHEEM_CACHE_DIR", required = TRUE, getenv = getenv),
        writable = !input.offline
    )

    jheem2.mode <- shield.env.value("JHEEM2_MODE", "source", getenv = getenv)
    if (!jheem2.mode %in% c("source", "package")) {
        stop("JHEEM2_MODE must be either source or package", call. = FALSE)
    }

    run.mode <- shield.env.value("SHIELD_RUN_MODE", "resume", getenv = getenv)
    if (!run.mode %in% c("fresh", "resume")) {
        stop("SHIELD_RUN_MODE must be either fresh or resume", call. = FALSE)
    }

    list(
        analyses_path = analyses.path,
        jheem2_path = jheem2.path,
        root_dir = root.dir,
        cache_dir = cache.dir,
        jheem2_mode = jheem2.mode,
        run_mode = run.mode,
        input_offline = input.offline,
        require_immutable_inputs = require.immutable.inputs,
        syphilis_manager_tag = syphilis.manager.tag,
        allow_incomplete = shield.env.flag("SHIELD_ALLOW_INCOMPLETE", FALSE, getenv),
        max_attempts = shield.env.integer("SHIELD_MAX_ATTEMPTS", 1L, 1L, getenv),
        retry_delay_seconds = shield.env.integer("SHIELD_RETRY_DELAY_SECONDS", 30L, 0L, getenv),
        seed = shield.env.integer("SHIELD_RANDOM_SEED", 0L, 0L, getenv),
        cache_frequency = shield.env.integer("SHIELD_CACHE_FREQUENCY", 500L, 1L, getenv),
        update_frequency = shield.env.integer("SHIELD_UPDATE_FREQUENCY", 50L, 1L, getenv),
        run_id = shield.env.value(
            "SHIELD_RUN_ID",
            paste0(format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), "-", Sys.getpid()),
            getenv = getenv
        )
    )
}

shield.retryable.storage.error <- function(error) {
    if (inherits(error, "shield.transient.storage.error")) return(TRUE)
    message <- tolower(conditionMessage(error))
    grepl(
        paste(c(
            "stale file handle",
            "resource temporarily unavailable",
            "device or resource busy",
            "input/output error",
            "connection reset by peer"
        ), collapse = "|"),
        message,
        perl = TRUE
    )
}

run.shield.with.retry <- function(operation,
                                  max.attempts = 1L,
                                  retry.delay.seconds = 30L,
                                  retryable = shield.retryable.storage.error,
                                  on.event = function(status, attempt, error = NULL) NULL,
                                  sleep = Sys.sleep) {
    stopifnot(is.function(operation), is.function(retryable), is.function(on.event))
    if (max.attempts < 1L) stop("max.attempts must be at least 1", call. = FALSE)

    for (attempt in seq_len(max.attempts)) {
        on.event("attempt_started", attempt)
        result <- tryCatch(
            list(ok = TRUE, value = operation()),
            error = function(error) list(ok = FALSE, error = error)
        )
        if (isTRUE(result$ok)) {
            on.event("attempt_completed", attempt)
            return(result$value)
        }

        on.event("attempt_failed", attempt, result$error)
        can.retry <- attempt < max.attempts && isTRUE(retryable(result$error))
        if (!can.retry) stop(result$error)

        message(
            "Retryable storage failure on attempt ", attempt, "/", max.attempts,
            ": ", conditionMessage(result$error),
            "; retrying in ", retry.delay.seconds, " seconds"
        )
        sleep(retry.delay.seconds)
    }

    stop("unreachable retry state", call. = FALSE)
}

assert.shield.resume.state <- function(version, location, calibration.code, root.dir) {
    progress <- suppressWarnings(get.calibration.progress(
        version = version,
        locations = location,
        calibration.code = calibration.code,
        root.dir = root.dir
    ))
    if (length(progress) == 0 || all(is.na(progress))) {
        stop(
            "SHIELD_RUN_MODE=resume but no calibration checkpoint exists for ",
            location, " / ", calibration.code, " under ", root.dir,
            call. = FALSE
        )
    }
    invisible(progress)
}
