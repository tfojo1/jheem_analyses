# The data managers SHIELD declares in applications/SHIELD/data_managers.csv.
# The container wrapper reads the same file with the same rules
# (jheem-containers workloads/shield/source_snapshot.py, declared_managers()).

SHIELD.DATA.MANAGER.FILES <- c(census = "census.manager.rdata",
                               syphilis = "syphilis.manager.rdata")

# list(census = <release or NULL>, syphilis = <release>), where NULL means
# "unpinned": census then loads the promoted census (census-manager-latest).
shield.declared.data.managers <- function(path) {
    error.prefix <- paste0("Cannot read ", basename(path), ": ")
    if (!file.exists(path)) stop(error.prefix, "the file does not exist", call. = FALSE)
    lines <- readLines(path, warn = FALSE)
    lines <- lines[nzchar(trimws(lines)) & !startsWith(trimws(lines), "#")]
    if (length(lines) == 0L ||
        !identical(trimws(strsplit(lines[[1L]], ",", fixed = TRUE)[[1L]]), c("name", "file", "release"))) {
        stop(error.prefix, "it must have the columns name,file,release", call. = FALSE)
    }
    rows <- utils::read.csv(text = lines, colClasses = "character", strip.white = TRUE)
    if (anyDuplicated(rows$name)) {
        stop(error.prefix, "'", rows$name[anyDuplicated(rows$name)], "' is listed twice", call. = FALSE)
    }
    declared <- list()
    for (i in seq_len(nrow(rows))) {
        name <- rows$name[[i]]
        if (name %in% names(SHIELD.DATA.MANAGER.FILES) &&
            !identical(rows$file[[i]], SHIELD.DATA.MANAGER.FILES[[name]])) {
            stop(error.prefix, "'", name, "' must be ", SHIELD.DATA.MANAGER.FILES[[name]], call. = FALSE)
        }
        if (!nzchar(rows$release[[i]])) stop(error.prefix, "'", name, "' has no release", call. = FALSE)
        declared[name] <- list(if (identical(rows$release[[i]], "unpinned")) NULL else rows$release[[i]])
    }
    missing <- setdiff(names(SHIELD.DATA.MANAGER.FILES), names(declared))
    if (length(missing) > 0L) {
        stop(error.prefix, "it does not list ", paste(missing, collapse = " or "), call. = FALSE)
    }
    if (is.null(declared$syphilis)) {
        stop(error.prefix, "syphilis is unpinned; name a dated release such as ",
             "syphilis-manager-v2026.10.09", call. = FALSE)
    }
    declared[names(SHIELD.DATA.MANAGER.FILES)]
}

# A manager already in memory is reused, as before. Warn when it is not the
# declared release, or its contents changed after loading.
shield.check.loaded.data.manager <- function(manager, variable, declared) {
    if (is.null(declared)) return(invisible(NULL))
    provenance <- if (exists("get.data.manager.provenance", mode = "function")) {
        get.data.manager.provenance(manager)
    }
    if (identical(provenance$release, declared) && identical(provenance$modified, FALSE)) {
        return(invisible(NULL))
    }
    loaded <- if (is.null(provenance$release)) "an unrecorded release" else provenance$release
    changed <- if (isTRUE(provenance$modified)) " (changed since it was loaded)" else ""
    warning(variable, " is already loaded from ", loaded, changed, ", not the declared ",
            declared, "; to load the declared release, rm(", variable, ") and source again",
            call. = FALSE)
    invisible(NULL)
}

# Makes SHIELD's two managers jheem2's session data binding, when jheem2 has
# bindings (CIPHER-Epi/jheem2 #25): kernels, likelihoods, and calibrations then
# record the releases they were built with; CENSUS.MANAGER and
# SURVEILLANCE.MANAGER become aliases that read through the binding, so code that
# uses them is unchanged; and rebuild.data.binding() can reload a recorded
# binding from its releases. With an earlier jheem2, the managers are only
# registered by name, as before.
shield.bind.data.managers <- function(census, syphilis, envir = parent.frame()) {
    if (!exists("create.data.binding", mode = "function")) {
        if (exists("register.data.manager", mode = "function")) {
            register.data.manager("census", census)
            register.data.manager("syphilis", syphilis)
        }
        return(invisible(FALSE))
    }
    # The binding first: a variable reloaded after rm() is then the manager its
    # name is bound to, which is what create.data.manager.alias() requires.
    set.data.binding(create.data.binding(census = census, syphilis = syphilis))
    create.data.manager.alias("CENSUS.MANAGER", "census", envir = envir)
    create.data.manager.alias("SURVEILLANCE.MANAGER", "syphilis", envir = envir)
    set.data.release.resolver(shield.load.data.manager.release)
    invisible(TRUE)
}

# The release resolver for rebuild.data.binding(): loads a recorded release
# through the verified loader. jheem2 then checks it against the recorded digests.
shield.load.data.manager.release <- function(identity) {
    source <- get.github.release.source(identity$file)
    if (is.null(source) || is.na(identity$release) ||
        !identical(identity$repository, source$repo)) {
        stop("Cannot load the data manager '", identity$file, "': only releases of ",
             if (is.null(source)) "a GitHub-backed manager" else source$repo,
             " can be loaded, and the record has ",
             if (is.na(identity$release)) "no release" else
                 paste0(identity$release, " from ", identity$repository), call. = FALSE)
    }
    load.data.manager.from.cache(identity$file, set.as.default = FALSE,
                                 release.tag = identity$release)
}
