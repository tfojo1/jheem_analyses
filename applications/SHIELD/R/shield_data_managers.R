# The data managers SHIELD declares in applications/SHIELD/data_managers.csv.
# The container wrapper reads the same file with the same rules
# (jheem-containers workloads/shield/source_snapshot.py, declared_managers()).

SHIELD.DATA.MANAGER.FILES <- c(census = "census.manager.rdata",
                               syphilis = "syphilis.manager.rdata")

# list(census = <release or NULL>, syphilis = <release>), where NULL means
# "unpinned": census then follows the shared cached-metadata file, as before.
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
