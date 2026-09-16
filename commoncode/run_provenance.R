## Read-only collection of the code, environment, and scientific inputs visible
## to a JHEEM run. This file must not pull repositories, install packages,
## download inputs, mutate caches, or advance R's random-number generator.

provenance.utc.time <- function(value = Sys.time()) {
    format(value, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
}

provenance.value.or.null <- function(value) {
    if (is.null(value) || length(value) == 0 || all(is.na(value))) NULL
    else as.character(value[[1]])
}

run.git.command <- function(repository.path, arguments) {
    output <- suppressWarnings(system2(
        "git",
        c("-C", shQuote(repository.path), arguments),
        stdout = TRUE,
        stderr = TRUE
    ))
    status <- attr(output, "status")
    if (is.null(status)) status <- 0L
    list(status = as.integer(status), output = as.character(output))
}

sanitize.git.remote.url <- function(value) {
    value <- provenance.value.or.null(value)
    if (is.null(value)) return(NULL)
    ## A remote URL is useful provenance, but HTTPS remotes can contain a
    ## username or access token. Retain the repository address, not userinfo.
    sub("^([A-Za-z][A-Za-z0-9+.-]*://)[^/@]+@", "\\1", value, perl = TRUE)
}

is.full.git.commit <- function(value) {
    !is.null(value) && length(value) == 1 && !is.na(value) &&
        grepl("^[0-9a-fA-F]{40}$", value)
}

declared.repository.identity <- function(name, path, declared.ref, reason) {
    if (!is.full.git.commit(declared.ref)) {
        return(list(
            name = name,
            identity = "unknown",
            reason = reason,
            path = provenance.value.or.null(path),
            declared_ref = provenance.value.or.null(declared.ref)
        ))
    }
    list(
        name = name,
        identity = "exact",
        commit = tolower(declared.ref),
        dirty = FALSE,
        identity_source = "immutable_image_declaration",
        path = provenance.value.or.null(path)
    )
}

collect.git.repository.identity <- function(name, path, declared.ref = NULL) {
    if (is.null(path) || length(path) != 1 || is.na(path) || !dir.exists(path)) {
        return(declared.repository.identity(
            name, path, declared.ref, "repository path is unavailable"
        ))
    }

    normalized.path <- normalizePath(path, mustWork = TRUE)
    inside <- run.git.command(normalized.path, c("rev-parse", "--is-inside-work-tree"))
    if (inside$status != 0L || !identical(inside$output[[1]], "true")) {
        return(declared.repository.identity(
            name, normalized.path, declared.ref, "path is not a Git worktree"
        ))
    }

    head <- run.git.command(normalized.path, c("rev-parse", "HEAD"))
    branch <- run.git.command(normalized.path, c("rev-parse", "--abbrev-ref", "HEAD"))
    status <- run.git.command(normalized.path, c("status", "--porcelain", "--untracked-files=normal"))
    remote <- run.git.command(normalized.path, c("remote", "get-url", "origin"))

    if (head$status != 0L || length(head$output) != 1) {
        return(declared.repository.identity(
            name, normalized.path, declared.ref, "Git HEAD could not be read"
        ))
    }

    dirty.entries <- if (status$status == 0L) status$output[nzchar(status$output)] else character()
    normalized.head <- tolower(head$output[[1]])
    normalized.declared <- if (is.full.git.commit(declared.ref)) tolower(declared.ref) else NULL
    declaration.mismatch <- !is.null(normalized.declared) &&
        !identical(normalized.head, normalized.declared)
    list(
        name = name,
        identity = if (declaration.mismatch) "mismatch"
            else if (length(dirty.entries) == 0) "exact" else "modified",
        commit = normalized.head,
        declared_ref = normalized.declared,
        identity_source = "git_worktree",
        branch = if (branch$status == 0L) branch$output[[1]] else NULL,
        dirty = length(dirty.entries) > 0,
        dirty_entry_count = length(dirty.entries),
        origin = if (remote$status == 0L && length(remote$output) == 1) {
            sanitize.git.remote.url(remote$output[[1]])
        } else {
            NULL
        },
        path = normalized.path
    )
}

collect.installed.package.identity <- function(package) {
    description <- tryCatch(
        utils::packageDescription(package),
        warning = function(warning) NULL,
        error = function(error) NULL
    )
    if (is.null(description)) {
        return(list(
            package = package,
            identity = "unknown",
            reason = "package is not installed"
        ))
    }

    remote.sha <- provenance.value.or.null(description$RemoteSha)
    if (is.null(remote.sha)) remote.sha <- provenance.value.or.null(description$GithubSHA1)
    list(
        package = package,
        identity = if (is.null(remote.sha)) "version_only" else "exact",
        version = provenance.value.or.null(description$Version),
        remote_sha = remote.sha,
        remote_ref = provenance.value.or.null(description$RemoteRef),
        remote_repository = if (!is.null(description$RemoteUsername) &&
                                !is.null(description$RemoteRepo)) {
            paste0(description$RemoteUsername, "/", description$RemoteRepo)
        } else {
            NULL
        },
        built = provenance.value.or.null(description$Built),
        library_path = tryCatch(
            normalizePath(system.file(package = package), mustWork = TRUE),
            error = function(error) system.file(package = package)
        )
    )
}

read.manager.field <- function(manager, field) {
    tryCatch(manager[[field]], error = function(error) NULL)
}

collect.data.manager.identity <- function(name, manager) {
    if (is.null(manager)) {
        return(list(
            name = name,
            identity = "unknown",
            reason = "manager is not loaded"
        ))
    }

    resolution <- attr(manager, "jheem.manager.resolution", exact = TRUE)
    if (!is.null(resolution) &&
        !is.null(resolution$resolved_tag) &&
        !is.null(resolution$sha256)) {
        return(list(
            name = name,
            identity = "exact",
            repository = provenance.value.or.null(resolution$repository),
            requested_tag = provenance.value.or.null(resolution$requested_tag),
            resolved_tag = provenance.value.or.null(resolution$resolved_tag),
            asset = provenance.value.or.null(resolution$asset),
            sha256 = provenance.value.or.null(resolution$sha256),
            published_at = provenance.value.or.null(resolution$published_at),
            local_path = provenance.value.or.null(resolution$local_path)
        ))
    }

    creation.date <- read.manager.field(manager, "creation.date")
    modified.date <- read.manager.field(manager, "last.modified.date")
    list(
        name = name,
        identity = "floating",
        reason = "loaded manager has no immutable release resolution",
        manager_name = provenance.value.or.null(read.manager.field(manager, "name")),
        creation_date = provenance.value.or.null(creation.date),
        last_modified_date = provenance.value.or.null(modified.date)
    )
}

default.jheem.repository.paths <- function() {
    global <- globalenv()
    analyses.path <- if (exists("JHEEM.ANALYSES.PATH", envir = global, inherits = FALSE)) {
        get("JHEEM.ANALYSES.PATH", envir = global, inherits = FALSE)
    } else {
        getwd()
    }
    jheem2.path <- if (exists("JHEEM2.PATH", envir = global, inherits = FALSE)) {
        get("JHEEM2.PATH", envir = global, inherits = FALSE)
    } else {
        file.path(dirname(normalizePath(analyses.path, mustWork = FALSE)), "jheem2")
    }
    list(jheem_analyses = analyses.path, jheem2 = jheem2.path)
}

default.jheem.repository.refs <- function() {
    list(
        jheem_analyses = provenance.value.or.null(Sys.getenv("JHEEM_ANALYSES_REF", "")),
        jheem2 = provenance.value.or.null(Sys.getenv("JHEEM2_REF", ""))
    )
}

detect.jheem2.execution.mode <- function() {
    global <- globalenv()
    if (exists("USE.JHEEM2.PACKAGE", envir = global, inherits = FALSE)) {
        if (isTRUE(get("USE.JHEEM2.PACKAGE", envir = global, inherits = FALSE))) {
            return("installed_package")
        }
        return("source_checkout")
    }
    "unknown"
}

#' Collect the execution context visible to a JHEEM run
#'
#' This function is read-only. It does not update repositories, install packages,
#' resolve remote aliases, download managers, mutate caches, or consume randomness.
#'
#' @param application Application name, such as "SHIELD".
#' @param operation Operation name, such as "calibration".
#' @param version Registered model version.
#' @param location Model location code.
#' @param calibration.code Registered calibration code.
#' @param root.dir Resolved JHEEM output root.
#' @param repositories Named repository paths.
#' @param repository.refs Named exact commits declared by an immutable image.
#'   A declaration is used only when a copied source tree has no Git metadata;
#'   when Git metadata exists, a disagreement is reported as a mismatch.
#' @param packages Installed packages whose identities should be reported.
#' @param managers Named list of loaded data-manager objects.
collect.jheem.run.context <- function(application = NULL,
                                      operation = NULL,
                                      version = NULL,
                                      location = NULL,
                                      calibration.code = NULL,
                                      root.dir = NULL,
                                      repositories = default.jheem.repository.paths(),
                                      repository.refs = default.jheem.repository.refs(),
                                      packages = c("jheem2", "bayesian.simulations",
                                                   "distributions", "locations"),
                                      managers = list()) {
    random.seed.before <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
        get(".Random.seed", envir = globalenv(), inherits = FALSE)
    } else {
        NULL
    }

    result <- list(
        schema_version = "0.1.0",
        captured_at = provenance.utc.time(),
        request = list(
            application = provenance.value.or.null(application),
            operation = provenance.value.or.null(operation),
            version = provenance.value.or.null(version),
            location = provenance.value.or.null(location),
            calibration_code = provenance.value.or.null(calibration.code)
        ),
        execution = list(
            jheem2_mode = detect.jheem2.execution.mode(),
            r_version = R.version.string,
            platform = R.version$platform,
            os = paste(Sys.info()[c("sysname", "release", "version")], collapse = " "),
            host = provenance.value.or.null(Sys.info()[["nodename"]]),
            process_id = Sys.getpid(),
            command = commandArgs(FALSE),
            working_directory = normalizePath(getwd(), mustWork = TRUE),
            root_directory = if (is.null(root.dir)) NULL else normalizePath(root.dir, mustWork = FALSE)
        ),
        repositories = lapply(names(repositories), function(name) {
            collect.git.repository.identity(
                name,
                repositories[[name]],
                declared.ref = repository.refs[[name]]
            )
        }),
        packages = lapply(packages, collect.installed.package.identity),
        data_managers = lapply(names(managers), function(name) {
            collect.data.manager.identity(name, managers[[name]])
        })
    )
    names(result$repositories) <- names(repositories)
    names(result$packages) <- packages
    names(result$data_managers) <- names(managers)
    class(result) <- c("jheem.run.context", "list")

    random.seed.after <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
        get(".Random.seed", envir = globalenv(), inherits = FALSE)
    } else {
        NULL
    }
    if (!identical(random.seed.before, random.seed.after)) {
        stop("collect.jheem.run.context() changed the R random-number state")
    }
    result
}

print.jheem.run.context <- function(x, ...) {
    request <- x$request
    requested <- Filter(Negate(is.null), request)
    cat("JHEEM run context\n")
    cat("  Captured: ", x$captured_at, "\n", sep = "")
    if (length(requested) > 0) {
        cat("  Request:  ", paste(names(requested), unlist(requested),
                                  sep = "=", collapse = ", "), "\n", sep = "")
    }
    cat("  jheem2:   ", x$execution$jheem2_mode, "\n", sep = "")
    cat("  Root:     ", if (is.null(x$execution$root_directory)) "unknown"
        else x$execution$root_directory, "\n", sep = "")

    cat("  Repositories:\n")
    for (repository in x$repositories) {
        detail <- if (!is.null(repository$commit)) {
            paste0(substr(repository$commit, 1, 12),
                   if (isTRUE(repository$dirty)) " + local changes" else "")
        } else {
            repository$reason
        }
        cat("    - ", repository$name, ": ", repository$identity,
            " (", detail, ")\n", sep = "")
    }

    cat("  Packages:\n")
    for (package in x$packages) {
        detail <- if (!is.null(package$remote_sha)) substr(package$remote_sha, 1, 12)
        else if (!is.null(package$version)) paste0("version ", package$version)
        else package$reason
        cat("    - ", package$package, ": ", package$identity,
            " (", detail, ")\n", sep = "")
    }

    if (length(x$data_managers) > 0) {
        cat("  Data managers:\n")
        for (manager in x$data_managers) {
            detail <- if (!is.null(manager$resolved_tag)) {
                paste0(manager$resolved_tag, " @ ", substr(manager$sha256, 1, 12))
            } else {
                manager$reason
            }
            cat("    - ", manager$name, ": ", manager$identity,
                " (", detail, ")\n", sep = "")
        }
    }
    invisible(x)
}

provenance.enabled <- function() {
    value <- tolower(trimws(Sys.getenv("JHEEM_PROVENANCE_ENABLED", "true")))
    !(value %in% c("false", "0", "no", "off"))
}

get.jheem.provenance.spool.directory <- function() {
    configured <- trimws(Sys.getenv("JHEEM_PROVENANCE_SPOOL_DIR"))
    if (nzchar(configured)) return(path.expand(configured))
    file.path(tools::R_user_dir("jheem", which = "data"), "provenance")
}

provenance.sha256.file <- function(path) {
    connection <- file(path, open = "rb")
    on.exit(close(connection), add = TRUE)
    as.vector(as.character(openssl::sha256(connection)))
}

provenance.sha256.text <- function(value) {
    as.vector(as.character(openssl::sha256(charToRaw(enc2utf8(value)))))
}

atomic.write.provenance.json <- function(value, path) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    temporary <- paste0(path, ".write.", Sys.getpid())
    on.exit(unlink(temporary), add = TRUE)
    jsonlite::write_json(
        unclass(value), temporary, auto_unbox = TRUE, pretty = TRUE,
        null = "null", digits = NA
    )
    if (!file.rename(temporary, path)) {
        if (file.exists(path)) unlink(path)
        if (!file.rename(temporary, path)) {
            stop("Could not atomically write provenance file: ", path)
        }
    }
    invisible(path)
}

atomic.write.provenance.lines <- function(value, path) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    temporary <- paste0(path, ".write.", Sys.getpid())
    on.exit(unlink(temporary), add = TRUE)
    writeLines(value, temporary, useBytes = TRUE)
    if (!file.rename(temporary, path)) {
        if (file.exists(path)) unlink(path)
        if (!file.rename(temporary, path)) {
            stop("Could not atomically write provenance file: ", path)
        }
    }
    invisible(path)
}

copy.provenance.file <- function(from, to) {
    dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
    temporary <- paste0(to, ".copy.", Sys.getpid())
    on.exit(unlink(temporary), add = TRUE)
    if (!file.copy(from, temporary, overwrite = TRUE)) {
        stop("Could not copy provenance file to local spool: ", to)
    }
    if (!file.rename(temporary, to)) {
        if (file.exists(to)) unlink(to)
        if (!file.rename(temporary, to)) {
            stop("Could not atomically place provenance file in local spool: ", to)
        }
    }
    invisible(to)
}

copy.provenance.to.spool <- function(from, to) {
    tryCatch(
        copy.provenance.file(from, to),
        error = function(error) {
            warning("Could not update the local provenance spool: ",
                    conditionMessage(error), call. = FALSE)
            invisible(NULL)
        }
    )
}

read.single.rdata.object <- function(path) {
    environment <- new.env(parent = emptyenv())
    object.names <- load(path, envir = environment)
    if (length(object.names) != 1) {
        stop("Expected one object in ", path, "; found ", length(object.names))
    }
    environment[[object.names[[1]]]]
}

slot.or.null <- function(object, name) {
    if (!isS4(object) || !(name %in% methods::slotNames(object))) return(NULL)
    methods::slot(object, name)
}

collect.calibration.cache.configuration <- function(calibration.directory) {
    cache.directory <- file.path(calibration.directory, "cache")
    global.file <- file.path(cache.directory, "global_control.Rdata")
    if (!file.exists(global.file)) {
        return(list(identity = "unknown", reason = "calibration cache is not initialized"))
    }

    global.control <- read.single.rdata.object(global.file)
    chain.files <- file.path(cache.directory, slot.or.null(
        global.control, "chain.control.filenames"
    ))
    chains <- lapply(seq_along(chain.files), function(index) {
        if (!file.exists(chain.files[[index]])) {
            return(list(
                chain = index,
                identity = "unknown",
                reason = "chain control file is missing"
            ))
        }
        control <- read.single.rdata.object(chain.files[[index]])
        state <- slot.or.null(control, "chain.state")
        starting.parameters <- slot.or.null(state, "current.parameters")
        list(
            chain = index,
            identity = "exact",
            chain_id = provenance.value.or.null(slot.or.null(control, "chain.id")),
            seeds = as.integer(slot.or.null(control, "seeds")),
            starting_parameters = as.list(starting.parameters)
        )
    })

    control <- slot.or.null(global.control, "control")
    list(
        identity = "exact",
        cache_id = provenance.value.or.null(slot.or.null(global.control, "id")),
        n_chains = as.integer(slot.or.null(global.control, "n.chains")),
        n_chunks = as.integer(slot.or.null(global.control, "n.chunks")),
        chunk_size = as.integer(slot.or.null(global.control, "chunk.size")),
        saved_chunks = as.logical(slot.or.null(global.control, "save.chunk")),
        method = provenance.value.or.null(slot.or.null(control, "method")),
        parameter_names = as.character(slot.or.null(control, "var.names")),
        burn = as.integer(slot.or.null(control, "burn")),
        thin = as.integer(slot.or.null(control, "thin")),
        chains = chains
    )
}

resolve.calibration.directory <- function(version, location, calibration.code,
                                          root.dir) {
    if (exists("get.calibration.dir", mode = "function", inherits = TRUE)) {
        return(get("get.calibration.dir", mode = "function", inherits = TRUE)(
            version = version, location = location,
            calibration.code = calibration.code, root.dir = root.dir
        ))
    }
    if (requireNamespace("jheem2", quietly = TRUE)) {
        function.from.package <- getFromNamespace("get.calibration.dir", "jheem2")
        return(function.from.package(
            version = version, location = location,
            calibration.code = calibration.code, root.dir = root.dir
        ))
    }
    file.path(root.dir, "mcmc_runs", version, location, calibration.code)
}

provenance.run.paths <- function(calibration.directory, run.id = NULL) {
    provenance.directory <- file.path(calibration.directory, "provenance")
    paths <- list(
        provenance_directory = provenance.directory,
        current = file.path(provenance.directory, "current.json")
    )
    if (!is.null(run.id)) {
        if (!grepl("^[A-Za-z0-9._-]+$", run.id)) stop("Invalid provenance run ID")
        paths$run_directory <- file.path(provenance.directory, "runs", run.id)
        paths$context <- file.path(paths$run_directory, "context.json")
        paths$summary <- file.path(paths$run_directory, "RUN_INFO.txt")
        paths$events <- file.path(paths$run_directory, "events")
        paths$receipt <- file.path(paths$run_directory, "receipt.json")
    }
    paths
}

create.provenance.run.id <- function(context) {
    timestamp <- format(Sys.time(), "%Y%m%dT%H%M%OS6Z", tz = "UTC")
    timestamp <- gsub("[^0-9TZ]", "", timestamp)
    fingerprint <- provenance.sha256.text(paste(
        timestamp, Sys.getpid(), context$execution$host,
        unlist(context$request, use.names = TRUE), collapse = "|"
    ))
    paste0("jheem-", timestamp, "-", substr(fingerprint, 1, 12))
}

read.current.provenance.run <- function(calibration.directory) {
    paths <- provenance.run.paths(calibration.directory)
    if (!file.exists(paths$current)) {
        stop("No current provenance run is recorded for ", calibration.directory)
    }
    pointer <- jsonlite::fromJSON(paths$current, simplifyVector = TRUE)
    if (is.null(pointer$run_id) || !grepl("^[A-Za-z0-9._-]+$", pointer$run_id)) {
        stop("The current provenance pointer is malformed: ", paths$current)
    }
    pointer
}

format.jheem.run.summary <- function(context, receipt = NULL) {
    lines <- capture.output(print(context))
    unresolved.repositories <- names(Filter(
        function(item) !identical(item$identity, "exact"), context$repositories
    ))
    unresolved.packages <- names(Filter(
        function(item) !identical(item$identity, "exact"), context$packages
    ))
    unresolved.managers <- names(Filter(
        function(item) !identical(item$identity, "exact"), context$data_managers
    ))
    lines <- c(
        lines,
        "  Identity gaps:",
        paste0("    - repositories: ", if (length(unresolved.repositories)) {
            paste(unresolved.repositories, collapse = ", ")
        } else "none"),
        paste0("    - packages: ", if (length(unresolved.packages)) {
            paste(unresolved.packages, collapse = ", ")
        } else "none"),
        paste0("    - data managers: ", if (length(unresolved.managers)) {
            paste(unresolved.managers, collapse = ", ")
        } else "none")
    )
    if (!is.null(receipt)) {
        lines <- c(
            lines,
            "  Output:",
            paste0("    - status: ", receipt$status),
            paste0("    - artifact: ", receipt$artifact$path),
            paste0("    - bytes: ", receipt$artifact$size_bytes),
            paste0("    - sha256: ", receipt$artifact$sha256)
        )
    }
    lines
}

#' Begin provenance capture for an initialized calibration cache
start.calibration.provenance <- function(version,
                                         location,
                                         calibration.code,
                                         root.dir,
                                         application = NULL,
                                         managers = list(),
                                         repositories = default.jheem.repository.paths()) {
    if (!provenance.enabled()) return(invisible(NULL))
    calibration.directory <- resolve.calibration.directory(
        version, location, calibration.code, root.dir
    )
    current.pointer <- provenance.run.paths(calibration.directory)$current
    if (file.exists(current.pointer) && unlink(current.pointer) != 0) {
        stop("Could not clear the prior provenance pointer: ", current.pointer)
    }
    context <- collect.jheem.run.context(
        application = application,
        operation = "calibration",
        version = version,
        location = location,
        calibration.code = calibration.code,
        root.dir = root.dir,
        repositories = repositories,
        managers = managers
    )
    context$calibration_cache <- collect.calibration.cache.configuration(
        calibration.directory
    )
    context$run_id <- create.provenance.run.id(context)
    context$archive <- list(state = "pending", sink = NULL)

    paths <- provenance.run.paths(calibration.directory, context$run_id)
    atomic.write.provenance.json(context, paths$context)
    context.sha256 <- provenance.sha256.file(paths$context)
    atomic.write.provenance.lines(format.jheem.run.summary(context), paths$summary)

    atomic.write.provenance.json(list(
        schema_version = context$schema_version,
        run_id = context$run_id,
        context_sha256 = context.sha256,
        created_at = context$captured_at
    ), provenance.run.paths(calibration.directory)$current)

    spool.run.directory <- file.path(
        get.jheem.provenance.spool.directory(), "runs", context$run_id
    )
    copy.provenance.to.spool(
        paths$context, file.path(spool.run.directory, "context.json")
    )
    copy.provenance.to.spool(
        paths$summary, file.path(spool.run.directory, "RUN_INFO.txt")
    )
    invisible(context)
}

#' Append a status event to the current calibration run
record.calibration.provenance.event <- function(version,
                                                location,
                                                calibration.code,
                                                root.dir,
                                                status,
                                                chain = NULL,
                                                attempt = NULL,
                                                details = list()) {
    if (!provenance.enabled()) return(invisible(NULL))
    calibration.directory <- resolve.calibration.directory(
        version, location, calibration.code, root.dir
    )
    pointer <- read.current.provenance.run(calibration.directory)
    paths <- provenance.run.paths(calibration.directory, pointer$run_id)
    recorded.at <- provenance.utc.time()
    event <- list(
        schema_version = "0.1.0",
        record_type = "run_event",
        run_id = pointer$run_id,
        recorded_at = recorded.at,
        status = status,
        chain = if (is.null(chain)) NULL else as.integer(chain),
        attempt = if (is.null(attempt)) NULL else as.integer(attempt),
        process_id = Sys.getpid(),
        host = provenance.value.or.null(Sys.info()[["nodename"]]),
        details = details
    )
    event.fingerprint <- provenance.sha256.text(jsonlite::toJSON(
        event, auto_unbox = TRUE, null = "null", digits = NA
    ))
    event.name <- paste0(
        format(Sys.time(), "%Y%m%dT%H%M%OS3Z", tz = "UTC"), "-",
        Sys.getpid(), "-", substr(event.fingerprint, 1, 10), ".json"
    )
    event.name <- gsub(":", "", event.name, fixed = TRUE)
    event.path <- file.path(paths$events, event.name)
    atomic.write.provenance.json(event, event.path)
    copy.provenance.to.spool(
        event.path,
        file.path(get.jheem.provenance.spool.directory(), "runs",
                  pointer$run_id, "events", event.name)
    )
    invisible(event)
}

#' Finalize a calibration run with a saved simulation-set receipt
finalize.calibration.provenance <- function(simset,
                                            root.dir,
                                            artifact.path = NULL) {
    if (!provenance.enabled()) return(invisible(NULL))
    if (!is(simset, "jheem.simulation.set")) {
        stop("Cannot finalize calibration provenance: simset is not a jheem.simulation.set")
    }
    if (is.null(artifact.path)) {
        artifact.path <- get.simset.filename(
            version = simset$version,
            sub.version = simset$sub.version,
            calibration.code = simset$calibration.code,
            n.sim = simset$n.sim,
            location = simset$location,
            intervention.code = simset$intervention.code,
            root.dir = root.dir
        )
    }
    if (!file.exists(artifact.path)) {
        stop("Cannot finalize calibration provenance: artifact does not exist: ",
             artifact.path)
    }

    calibration.directory <- resolve.calibration.directory(
        simset$version, simset$location, simset$calibration.code, root.dir
    )
    pointer <- read.current.provenance.run(calibration.directory)
    paths <- provenance.run.paths(calibration.directory, pointer$run_id)
    context <- jsonlite::fromJSON(paths$context, simplifyVector = FALSE)
    class(context) <- c("jheem.run.context", "list")
    artifact.path <- normalizePath(artifact.path, mustWork = TRUE)
    artifact.sha256 <- provenance.sha256.file(artifact.path)
    receipt <- list(
        schema_version = "0.1.0",
        record_type = "output_receipt",
        run_id = pointer$run_id,
        context_sha256 = pointer$context_sha256,
        completed_at = provenance.utc.time(),
        status = "complete",
        artifact = list(
            role = "calibration_simulation_set",
            path = artifact.path,
            file_name = basename(artifact.path),
            size_bytes = unname(file.info(artifact.path)$size),
            sha256 = artifact.sha256
        )
    )

    tryCatch(
        record.calibration.provenance.event(
            version = simset$version,
            location = simset$location,
            calibration.code = simset$calibration.code,
            root.dir = root.dir,
            status = "artifact_saved",
            details = list(artifact_sha256 = artifact.sha256)
        ),
        error = function(error) {
            warning("Could not append the artifact provenance event: ",
                    conditionMessage(error), call. = FALSE)
        }
    )
    atomic.write.provenance.json(receipt, paths$receipt)
    atomic.write.provenance.lines(
        format.jheem.run.summary(context, receipt), paths$summary
    )

    sidecar <- paste0(artifact.path, ".provenance.json")
    readable.sidecar <- paste0(artifact.path, ".RUN_INFO.txt")
    atomic.write.provenance.json(list(context = context, receipt = receipt), sidecar)
    atomic.write.provenance.lines(
        format.jheem.run.summary(context, receipt), readable.sidecar
    )

    spool <- get.jheem.provenance.spool.directory()
    spool.run.directory <- file.path(spool, "runs", pointer$run_id)
    copy.provenance.to.spool(
        paths$receipt, file.path(spool.run.directory, "receipt.json")
    )
    copy.provenance.to.spool(
        paths$summary, file.path(spool.run.directory, "RUN_INFO.txt")
    )
    copy.provenance.to.spool(
        sidecar, file.path(spool, "by-digest", artifact.sha256,
                           paste0(pointer$run_id, ".json"))
    )
    invisible(receipt)
}

#' Find provenance for a simulation artifact
inspect.jheem.artifact.provenance <- function(artifact.path,
                                              search.directories = character()) {
    if (!file.exists(artifact.path)) stop("Artifact does not exist: ", artifact.path)
    sidecar <- paste0(normalizePath(artifact.path, mustWork = TRUE), ".provenance.json")
    if (file.exists(sidecar)) {
        result <- jsonlite::fromJSON(sidecar, simplifyVector = FALSE)
        result$found_by <- "adjacent_sidecar"
        return(result)
    }

    digest <- provenance.sha256.file(artifact.path)
    roots <- unique(c(get.jheem.provenance.spool.directory(), search.directories))
    legacy.candidates <- file.path(roots, "by-digest", paste0(digest, ".json"))
    digest.directories <- file.path(roots, "by-digest", digest)
    indexed.candidates <- unlist(lapply(digest.directories, function(directory) {
        if (!dir.exists(directory)) return(character())
        list.files(directory, pattern = "[.]json$", full.names = TRUE)
    }), use.names = FALSE)
    matches <- unique(c(
        legacy.candidates[file.exists(legacy.candidates)], indexed.candidates
    ))
    if (length(matches) == 0) {
        return(list(
            found_by = "none",
            artifact_sha256 = digest,
            reason = "no adjacent sidecar or digest-indexed receipt was found"
        ))
    }
    records <- lapply(matches, jsonlite::fromJSON, simplifyVector = FALSE)
    if (length(records) == 1) {
        records[[1]]$found_by <- "artifact_digest"
        return(records[[1]])
    }
    list(
        found_by = "artifact_digest",
        artifact_sha256 = digest,
        match_count = length(records),
        matches = records
    )
}

#' Capture provenance without allowing an observability failure to stop science
capture.jheem.provenance.safely <- function(expression) {
    if (!provenance.enabled()) return(invisible(NULL))
    tryCatch(
        force(expression),
        error = function(error) {
            warning("JHEEM provenance capture failed: ", conditionMessage(error),
                    call. = FALSE)
            invisible(NULL)
        }
    )
}
