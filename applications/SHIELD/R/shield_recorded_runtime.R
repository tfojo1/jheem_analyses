# Preflight for opt-in, recorded SHIELD runs. No model code or packages are
# loaded here, and validation must not create, clear, or repair run state.

shield.recorded.env <- function(name, getenv = Sys.getenv) {
    value <- trimws(getenv(name, unset = ""))
    if (!nzchar(value)) {
        stop("Recorded SHIELD run requires ", name, call. = FALSE)
    }
    value
}

shield.recorded.directory <- function(name, getenv = Sys.getenv, writable = FALSE) {
    path <- shield.recorded.env(name, getenv)
    if (!dir.exists(path)) {
        stop(name, " is not an existing directory: ", path, call. = FALSE)
    }
    path <- normalizePath(path, mustWork = TRUE)
    if (writable && file.access(path, 2L) != 0L) {
        stop(name, " is not writable: ", path, call. = FALSE)
    }
    path
}

shield.recorded.revision <- function(name, getenv = Sys.getenv) {
    revision <- shield.recorded.env(name, getenv)
    if (!grepl("^[0-9a-fA-F]{40}$", revision)) {
        stop(name, " must be a full 40-character commit SHA", call. = FALSE)
    }
    tolower(revision)
}

shield.recorded.tag <- function(name, prefix, getenv = Sys.getenv) {
    tag <- shield.recorded.env(name, getenv)
    if (!grepl(paste0("^", prefix, "-v[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}([.-][0-9A-Za-z]+)*$"), tag)) {
        stop(name, " must name an immutable dated release, not an alias: ", tag,
             call. = FALSE)
    }
    tag
}

shield.recorded.integer <- function(name, default = NULL, minimum = 0L,
                                    getenv = Sys.getenv) {
    value <- trimws(getenv(name, unset = if (is.null(default)) "" else as.character(default)))
    if (!grepl("^[0-9]+$", value)) {
        stop(name, " must be a nonnegative integer", call. = FALSE)
    }
    parsed <- suppressWarnings(as.integer(value))
    if (is.na(parsed) || parsed < minimum) {
        stop(name, " must be an integer at least ", minimum, call. = FALSE)
    }
    parsed
}

shield.recorded.config <- function(getenv = Sys.getenv) {
    analyses <- shield.recorded.directory("JHEEM_ANALYSES_PATH", getenv)
    jheem2 <- shield.recorded.directory("JHEEM2_PATH", getenv)
    # Existing SHIELD model files still contain ../jheem_analyses references.
    # Keep this layout requirement explicit until those paths are refactored.
    if (!identical(basename(analyses), "jheem_analyses")) {
        stop("Recorded SHIELD source directory must be named jheem_analyses",
             call. = FALSE)
    }
    cache <- shield.recorded.directory("JHEEM_CACHE_DIR", getenv)
    root <- shield.recorded.directory("JHEEM_ROOT_DIR", getenv, writable = TRUE)
    if (identical(cache, root) || startsWith(root, paste0(cache, "/")) ||
        startsWith(cache, paste0(root, "/"))) {
        stop("JHEEM_CACHE_DIR and JHEEM_ROOT_DIR must be separate trees",
             call. = FALSE)
    }

    mode <- trimws(getenv("SHIELD_RUN_MODE", unset = "resume"))
    if (!mode %in% c("fresh", "resume")) {
        stop("SHIELD_RUN_MODE must be fresh or resume", call. = FALSE)
    }
    package.mode <- trimws(getenv("JHEEM2_MODE", unset = "package"))
    if (!package.mode %in% c("package", "source")) {
        stop("JHEEM2_MODE must be package or source", call. = FALSE)
    }

    list(
        analyses_path = analyses,
        jheem2_path = jheem2,
        cache_dir = cache,
        root_dir = root,
        analyses_ref = shield.recorded.revision("JHEEM_ANALYSES_REF", getenv),
        jheem2_ref = shield.recorded.revision("JHEEM2_REF", getenv),
        locations_ref = shield.recorded.revision("LOCATIONS_REF", getenv),
        census_tag = shield.recorded.tag("JHEEM_CENSUS_MANAGER_TAG", "data-managers", getenv),
        syphilis_tag = shield.recorded.tag("JHEEM_SYPHILIS_MANAGER_TAG", "syphilis-manager", getenv),
        run_mode = mode,
        jheem2_mode = package.mode,
        random_seed = shield.recorded.integer("SHIELD_RANDOM_SEED", getenv = getenv),
        cache_frequency = shield.recorded.integer("SHIELD_CACHE_FREQUENCY", 500L, 1L, getenv),
        update_frequency = shield.recorded.integer("SHIELD_UPDATE_FREQUENCY", 50L, 1L, getenv)
    )
}

shield.recorded.calibration.dir <- function(config, location, calibration.code) {
    for (item in list(location = location, calibration.code = calibration.code)) {
        if (!is.character(item) || length(item) != 1L || is.na(item) ||
            !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", item)) {
            stop("Location and calibration code must be single path-safe names",
                 call. = FALSE)
        }
    }
    file.path(config$root_dir, "mcmc_runs", "shield", location, calibration.code)
}

shield.recorded.assert.state <- function(config, location, calibration.code) {
    directory <- shield.recorded.calibration.dir(config, location, calibration.code)
    if (identical(config$run_mode, "fresh")) {
        if (file.exists(directory)) {
            stop("Fresh recorded run would replace existing calibration state: ",
                 directory, call. = FALSE)
        }
    } else {
        control <- file.path(directory, "cache", "chain1_control.Rdata")
        if (!file.exists(control) || is.na(file.info(control)$size) ||
            file.info(control)$size <= 0L) {
            stop("Resume requested but no nonempty chain-1 checkpoint exists: ",
                 control, call. = FALSE)
        }
    }
    invisible(directory)
}

shield.recorded.assert.checkout <- function(path, revision) {
    # Container images omit Git metadata; their build must attest the pinned
    # context. For a checkout-based run, verify the claim before loading code.
    if (!file.exists(file.path(path, ".git"))) return(invisible(FALSE))
    git <- function(...) {
        result <- suppressWarnings(system2(
            "git", c("-C", shQuote(path), ...), stdout = TRUE, stderr = TRUE
        ))
        if (!is.null(attr(result, "status"))) {
            stop("Could not inspect recorded checkout: ", path, call. = FALSE)
        }
        result
    }
    actual <- tolower(git("rev-parse", "HEAD")[[1L]])
    if (!identical(actual, revision)) {
        stop("Recorded source revision does not match checkout ", path,
             ": expected ", revision, ", found ", actual, call. = FALSE)
    }
    if (length(git("status", "--porcelain"))) {
        stop("Recorded source checkout has uncommitted changes: ", path,
             call. = FALSE)
    }
    invisible(TRUE)
}

shield.recorded.inputs <- function(config, census.resolution, syphilis.resolution) {
    manager <- function(resolution, tag, name) {
        if (is.null(resolution) ||
            !identical(resolution$resolved_tag, tag) ||
            !identical(resolution$manager, name) ||
            is.null(resolution$sha256) ||
            !grepl("^[0-9a-fA-F]{64}$", resolution$sha256)) {
            stop("Recorded manager has no matching verified release identity: ",
                 name, call. = FALSE)
        }
        list(tag = tag, sha256 = tolower(resolution$sha256))
    }
    list(
        analyses_ref = config$analyses_ref,
        jheem2_ref = config$jheem2_ref,
        locations_ref = config$locations_ref,
        random_seed = as.character(config$random_seed),
        census = manager(census.resolution, config$census_tag, "census.manager.rdata"),
        syphilis = manager(syphilis.resolution, config$syphilis_tag, "syphilis.manager.rdata")
    )
}

shield.recorded.receipt.path <- function(config, location, calibration.code) {
    shield.recorded.calibration.dir(config, location, calibration.code)
    file.path(config$root_dir, "run_records", "shield", location,
              calibration.code, "inputs.json")
}

shield.recorded.check.receipt <- function(config, location, calibration.code,
                                          inputs) {
    path <- shield.recorded.receipt.path(config, location, calibration.code)
    if (identical(config$run_mode, "fresh")) {
        if (file.exists(path)) {
            stop("Fresh recorded run already has an input receipt: ", path,
                 call. = FALSE)
        }
    } else {
        if (!file.exists(path)) {
            stop("Resume requested but no input receipt exists: ", path,
                 call. = FALSE)
        }
        previous <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE),
                             error = function(e) NULL)
        if (is.null(previous) || !identical(previous$inputs, inputs)) {
            stop("Resume inputs differ from the recorded calibration inputs: ",
                 path, call. = FALSE)
        }
    }
    invisible(path)
}

shield.recorded.write.receipt <- function(config, location, calibration.code,
                                          inputs) {
    if (!identical(config$run_mode, "fresh")) {
        stop("Only a fresh recorded run can create an input receipt", call. = FALSE)
    }
    path <- shield.recorded.check.receipt(config, location, calibration.code,
                                          inputs)
    directory <- dirname(path)
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    lock <- filelock::lock(paste0(path, ".lock"), timeout = 30000)
    if (is.null(lock)) stop("Could not lock recorded input receipt", call. = FALSE)
    on.exit(filelock::unlock(lock), add = TRUE)
    if (file.exists(path)) stop("Recorded input receipt appeared concurrently", call. = FALSE)
    temporary <- tempfile("inputs-", tmpdir = directory, fileext = ".json")
    on.exit(unlink(temporary), add = TRUE)
    jsonlite::write_json(list(
        schema_version = 1L,
        created_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        location = location,
        calibration_code = calibration.code,
        inputs = inputs
    ), temporary, auto_unbox = TRUE, pretty = TRUE)
    if (!file.rename(temporary, path)) {
        stop("Could not persist recorded input receipt: ", path, call. = FALSE)
    }
    invisible(path)
}
