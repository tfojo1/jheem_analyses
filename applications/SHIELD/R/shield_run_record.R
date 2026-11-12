# A record of each SHIELD calibration step: the code, data managers, and machine it ran
# with. The modular launcher's setup, run, and assemble steps are separate processes that
# each source the checkouts and load the data managers again, so each step writes its own
# record: a small DCF file under run_records/ in the calibration's directory. Read one with
# read.dcf(file, all = TRUE). Writing a record never stops a run: a failure is a warning.
#
# A step's record is written when the step starts. When the step finishes, the record gains an
# end entry and one output entry per file the step saved, so a step that stopped early (an error,
# or the process killed, for example for memory) is the one whose record has no end entry.

SHIELD.RUN.RECORD.FORMAT <- "shield run record 1"

# step is setup, run, or assemble; chain is for a run step; seed is the seed the step set.
shield.write.run.record <- function(location, calibration.code, step, chain = NA_integer_,
                                    seed = NA_integer_, version = "shield") {
    tryCatch({
        dir <- file.path(get.calibration.dir(version = version, location = location,
                                             calibration.code = calibration.code),
                         "run_records")
        path <- shield.write.run.record.file(
            shield.run.record(location, calibration.code, step, chain, seed), dir)
        cat("Run record written: ", path, "\n", sep = "")
        invisible(path)
    }, error = function(e) {
        warning("Could not write the run record: ", conditionMessage(e), call. = FALSE)
        invisible(NULL)
    })
}

# One entry for the step, one per source checkout or package, one per data manager, one for
# the platform, and one per other package loaded in the session.
shield.run.record <- function(location, calibration.code, step, chain, seed) {
    info <- Sys.info()
    run <- list(record = "run", format = SHIELD.RUN.RECORD.FORMAT, step = step,
                chain = as.integer(chain), location = location,
                calibration.code = calibration.code,
                recorded.at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                host = info[["nodename"]], user = info[["user"]], pid = Sys.getpid(),
                r.version = paste(R.version$major, R.version$minor, sep = "."),
                seed = as.integer(seed),
                run.record = Sys.getenv("JHEEM_RUN_RECORD", unset = NA_character_))
    jheem2 <- if (isTRUE(get0("USE.JHEEM2.PACKAGE", envir = globalenv(), ifnotfound = FALSE))) {
        shield.run.record.package("jheem2")
    } else {
        shield.run.record.checkout("jheem2", get0("JHEEM2.PATH", envir = globalenv(),
                                                  ifnotfound = NA_character_))
    }
    c(list(run,
           shield.run.record.checkout("jheem_analyses", get0("JHEEM.ANALYSES.PATH", envir = globalenv(),
                                                             ifnotfound = NA_character_)),
           jheem2),
      lapply(c("bayesian.simulations", "distributions", "locations"), shield.run.record.package),
      list(shield.run.record.manager("census", "CENSUS.MANAGER"),
           shield.run.record.manager("syphilis", "SURVEILLANCE.MANAGER"),
           shield.run.record.platform()),
      shield.run.record.loaded.packages())
}

# A git checkout: its commit and branch, whether the commit is on a remote (anyone else can
# fetch it) and how far it is ahead of its upstream, and tracked files with uncommitted changes.
shield.run.record.checkout <- function(name, path) {
    entry <- list(record = "source", name = name, kind = "checkout",
                  path = NA_character_, commit = NA_character_, branch = NA_character_,
                  on.remote = NA_character_, ahead = NA_integer_,
                  changed = NA_integer_, changed.files = NA_character_)
    if (is.na(path) || !dir.exists(path) || !nzchar(Sys.which("git"))) return(entry)
    entry$path <- normalizePath(path)
    git <- function(...) {
        out <- suppressWarnings(system2("git", c("-C", shQuote(entry$path), ...),
                                        stdout = TRUE, stderr = FALSE))
        if (is.null(attr(out, "status"))) out else NULL
    }
    commit <- git("rev-parse", "HEAD")
    if (length(commit) != 1L) return(entry)
    entry$commit <- commit
    branch <- git("rev-parse", "--abbrev-ref", "HEAD")
    if (length(branch) == 1L) entry$branch <- branch
    # Remote-tracking branches are as fresh as the checkout's last fetch or pull.
    remote <- git("branch", "-r", "--contains", commit)
    if (!is.null(remote)) entry$on.remote <- if (length(remote) > 0L) "yes" else "no"
    ahead <- git("rev-list", "--count", "@{upstream}..HEAD")
    if (length(ahead) == 1L) entry$ahead <- as.integer(ahead)
    status <- git("status", "--porcelain", "--untracked-files=no")
    if (!is.null(status)) {
        entry$changed <- length(status)
        if (length(status) > 0L) entry$changed.files <- paste(substring(status, 4L), collapse = ", ")
    }
    entry
}

# An installed package: its version and, when installed from GitHub, its commit.
shield.run.record.package <- function(name) {
    description <- suppressWarnings(utils::packageDescription(name))
    if (!is.list(description)) {
        return(list(record = "source", name = name, kind = "package", installed = FALSE))
    }
    list(record = "source", name = name, kind = "package",
         path = find.package(name),
         version = description$Version,
         commit = if (is.null(description$RemoteSha)) NA_character_ else description$RemoteSha)
}

# Every package loaded in the session when the record is written, beyond R's base packages
# (for example deSolve and Rcpp, whose versions differ between servers): its version, the
# library it was loaded from, and its commit when installed from GitHub.
shield.run.record.loaded.packages <- function() {
    base <- c("base", "compiler", "datasets", "graphics", "grDevices", "grid", "methods",
              "parallel", "splines", "stats", "stats4", "tcltk", "tools", "utils")
    lapply(sort(setdiff(loadedNamespaces(), base)), function(name) {
        path <- tryCatch(getNamespaceInfo(name, "path"), error = function(e) NA_character_)
        description <- if (is.na(path)) NULL else
            suppressWarnings(utils::packageDescription(name, lib.loc = dirname(path)))
        list(record = "loaded package", name = name,
             version = as.character(getNamespaceVersion(name)), path = path,
             commit = if (!is.list(description) || is.null(description$RemoteSha)) NA_character_
                      else description$RemoteSha)
    })
}

# The machine's operating system and linear algebra libraries.
shield.run.record.platform <- function() {
    info <- utils::sessionInfo()
    field <- function(x) if (is.null(x) || !nzchar(x)) NA_character_ else as.character(x)
    list(record = "platform", os = field(info$running), blas = field(info$BLAS),
         lapack = field(info$LAPACK), lapack.version = field(La_version()))
}

# A data manager: the release SHIELD declares, and the release and digests it loaded.
# "unrecorded" means it was loaded without a release identity (for example, a manager
# already in memory or a local copy).
shield.run.record.manager <- function(name, variable) {
    declared <- get0("SHIELD.DATA.MANAGERS", envir = globalenv(), ifnotfound = NULL)
    manager <- get0(variable, envir = globalenv(), ifnotfound = NULL)
    resolution <- if (!is.null(manager) && exists("get.data.manager.resolution", mode = "function")) {
        get.data.manager.resolution(manager)
    }
    provenance <- if (!is.null(manager) && exists("get.data.manager.provenance", mode = "function")) {
        get.data.manager.provenance(manager)
    }
    field <- function(x, key) if (is.null(x[[key]])) NA_character_ else as.character(x[[key]])
    list(record = "data manager", name = name, variable = variable,
         declared = if (is.null(declared) || !(name %in% names(declared))) NA_character_
                    else if (is.null(declared[[name]])) "unpinned" else declared[[name]],
         loaded = if (is.null(manager)) "no" else if (is.null(resolution)) "unrecorded" else "release",
         release = field(resolution, "resolved_tag"),
         repository = field(resolution, "repository"),
         sha256 = field(resolution, "sha256"),
         md5 = field(provenance, "md5"),
         modified = if (is.null(provenance$modified)) NA else isTRUE(provenance$modified))
}

shield.write.run.record.file <- function(record, dir) {
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    run <- record[[1L]]
    name <- paste0(run$step, if (!is.na(run$chain)) paste0("-chain", run$chain),
                   "-", gsub("[-:]", "", run$recorded.at), "-", run$host, "-", run$pid, ".dcf")
    path <- file.path(dir, name)
    shield.replace.file(path, shield.run.record.lines(record))
    path
}

# Fields that are NA are left out. (A wide width keeps each value on one line.)
shield.run.record.lines <- function(entries) {
    unlist(lapply(entries, function(entry) {
        c(utils::capture.output(write.dcf(as.data.frame(entry, stringsAsFactors = FALSE),
                                          width = 10000)), "")
    }))
}

# Readers never see a half-written record: write beside it, then rename over it.
shield.replace.file <- function(path, lines) {
    partial <- paste0(path, ".partial")
    writeLines(lines, partial)
    if (!file.rename(partial, path)) stop("could not rename ", partial)
}

# Called at the end of a step with the record's path (what shield.write.run.record returned)
# and the files the step saved. Each output entry gives the file's path, size, time, and
# SHA-256; files over SHIELD_RUN_RECORD_HASH_LIMIT_GB (default 20) get no digest, since hashing
# an 80 GB simulation set takes over ten minutes.
shield.finish.run.record <- function(path, outputs = character()) {
    if (is.null(path) || !file.exists(path)) return(invisible(NULL))
    tryCatch({
        limit <- as.numeric(Sys.getenv("SHIELD_RUN_RECORD_HASH_LIMIT_GB", unset = "20")) * 1e9
        outputs <- unique(outputs[!is.na(outputs) & file.exists(outputs)])
        end <- list(record = "end", ended.at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                    outcome = "completed")
        entries <- c(list(end), lapply(outputs, shield.run.record.output, limit = limit))
        shield.replace.file(path, c(readLines(path), shield.run.record.lines(entries)))
        cat("Run record finished: ", path, "\n", sep = "")
        invisible(path)
    }, error = function(e) {
        warning("Could not finish the run record: ", conditionMessage(e), call. = FALSE)
        invisible(NULL)
    })
}

shield.run.record.output <- function(file, limit) {
    info <- file.info(file)
    list(record = "output", path = normalizePath(file), size = info$size,
         modified = format(info$mtime, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
         sha256 = if (info$size <= limit) shield.sha256(file) else NA_character_)
}

# A file's SHA-256, read in binary as sha256.file() in commoncode/cache_manager.R does, so it
# is the same on every platform; NA when openssl isn't installed or the file can't be read.
shield.sha256 <- function(file) {
    if (!requireNamespace("openssl", quietly = TRUE)) return(NA_character_)
    tryCatch({
        connection <- file(file, open = "rb")
        on.exit(close(connection), add = TRUE)
        as.vector(as.character(openssl::sha256(connection)))
    }, error = function(e) NA_character_)
}

# The files each step of the modular launcher saves. NULL entries (a path jheem2 can't give,
# for example) are dropped; a missing file is skipped when the record is finished.
shield.step.outputs <- function(step, location, calibration.code, chain = NA_integer_,
                                simset = NULL, version = "shield") {
    tryCatch({
        cache <- file.path(get.calibration.dir(version = version, location = location,
                                               calibration.code = calibration.code), "cache")
        switch(step,
               setup = list.files(cache, pattern = "control\\.Rdata$", full.names = TRUE),
               run = file.path(cache, paste0("chain", chain, "_control.Rdata")),
               assemble = c(get.mcmc.summary.file(version, location, calibration.code),
                            if (!is.null(simset)) with(get.simset.filename.and.dir(
                                version = simset$version, sub.version = simset$sub.version,
                                calibration.code = simset$calibration.code, n.sim = simset$n.sim,
                                location = simset$location, intervention.code = simset$intervention.code),
                                file.path(dir, filename))),
               character())
    }, error = function(e) character())
}
