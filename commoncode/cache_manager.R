# A lot of people have done the "first time setup" already, so they need to install this new dependency
if (nchar(system.file(package = "httr2")) == 0) {
    install.packages("httr2")
}

JHEEM.CACHE.DIR <- NULL
if (dir.exists("../../cached")) {
    JHEEM.CACHE.DIR <- "../../cached"
}
if (dir.exists("../jheem_analyses/cached")) {
    JHEEM.CACHE.DIR <- "../jheem_analyses/cached"
}
DATA.MANAGER.CACHE.METADATA.FILE <- "../jheem_analyses/commoncode/data_manager_cache_metadata.Rdata"

if (is.null(JHEEM.CACHE.DIR)) {
    stop("No 'cached' directory exists - you need to get this from Todd's One-Drive")
}

## ----PUBLIC---- ##

load.data.manager.from.cache <- function(file, set.as.default = F) {
    error.prefix <- "Cannot load.data.manager.from.cache(): "
    cache.metadata <- get.data.manager.cache.metadata(pretty.print=F)
    if (!(file %in% names(cache.metadata))) {
        stop(paste0(error.prefix, "'", file, "' is not one of our cached files. Call 'get.data.manager.cache.metadata()' to check what files are cached. File names are capitalization-sensitive."))
    }

    # Download it if it doesn't exist or it exists but is out of date
    if (!file.exists(file.path(JHEEM.CACHE.DIR, file))) {
        download.data.manager.from.onedrive(file.path(JHEEM.CACHE.DIR, file), cache.metadata[[file]]$onedrive.link, error.prefix)
        loaded.data.manager <- load.data.manager(file.path(JHEEM.CACHE.DIR, file), set.as.default = set.as.default)
    } else {
        loaded.data.manager <- load.data.manager(file.path(JHEEM.CACHE.DIR, file), set.as.default = set.as.default)
        if (is.cached.data.manager.out.of.date(file, loaded.data.manager, error.prefix = error.prefix)) {
            download.data.manager.from.onedrive(file.path(JHEEM.CACHE.DIR, file), cache.metadata[[file]]$onedrive.link, error.prefix)
        }
    }    
    loaded.data.manager
}

get.data.manager.cache.metadata <- function(pretty.print=T, error.prefix = "") {
    if (!file.exists(DATA.MANAGER.CACHE.METADATA.FILE)) {
        stop(paste0(error.prefix, "The 'data_manager_cache_metadata.Rdata' file is missing from commoncode - this probably requires a pull"))
    }
    data.manager.cache.metadata <- get(load(DATA.MANAGER.CACHE.METADATA.FILE))

    if (pretty.print) {
        cat("Local copies of each data manager must be last modified by these dates or later: ","\n")
        for (data.manager in names(data.manager.cache.metadata)) {
            cat(data.manager, "-", format(data.manager.cache.metadata[[data.manager]][["last.modified.date"]], usetz = T),"\n")
        }
    }
    invisible(data.manager.cache.metadata)
}

## ---- ZOE ONLY ---- ##

reset.data.manager.cache.metadata <- function(allow.flag = F) {
    if (!allow.flag) {
        stop("Are you sure you want to reset.data.manager.cache.metadata()?  - you need to set the allow.flag to TRUE if you do")
    }
    replacement.cache.metadata <- list()
    save(replacement.cache.metadata, file = DATA.MANAGER.CACHE.METADATA.FILE)
}

sync.cached.data.manager <- function(file, onedrive.link, verbose = F, allow.flag = F) {
    error.prefix <- "Cannot sync.cached.data.manager(): "
    if (!allow.flag) {
        stop("Are you sure you want to sync.cached.data.manager()?  - you need to set the allow.flag to TRUE if you do")
    }

    if (!is.character(onedrive.link) || length(onedrive.link) != 1 || is.na(onedrive.link)) {
        stop(paste0(error.prefix, "'onedrive.link' must be a single character value"))
    }

    # Add "&download=1" to make the link automatically download
    onedrive.link <- paste0(onedrive.link, "&download=1")

    x <- load(file.path(JHEEM.CACHE.DIR, file))
    if (length(x) != 1) {
        stop(paste0(error.prefix, "'file' does not specify a single JHEEM Data Manager object"))
    }
    data.manager <- get(x)
    if (!R6::is.R6(data.manager) && !is(data.manager, "jheem.data.manager")) {
        stop(paste0(error.prefix, "'file' does not specify a single JHEEM Data Manager object"))
    }

    # check link validity
    temporary.filename <- paste0(tempfile(), ".rdata")
    download.data.manager.from.onedrive(temporary.filename, onedrive.link, error.prefix)
    y <- load(temporary.filename)
    unlink(temporary.filename)
    if (length(y) != 1) {
        stop(paste0(error.prefix, "OneDrive link does not download a single JHEEM Data Manager object"))
    }
    downloaded.data.manager <- get(y)
    if (!R6::is.R6(downloaded.data.manager) && !is(downloaded.data.manager, "jheem.data.manager")) {
        stop(paste0(error.prefix, "OneDrive link does not download a single JHEEM Data Manager object"))
    }
    if (data.manager$creation.date != downloaded.data.manager$creation.date || data.manager$last.modified.date != downloaded.data.manager$last.modified.date) {
        stop(paste0(error.prefix, "OneDrive link does not download the same object as that found at 'file'"))
    }

    data.manager.metadata <- list(
        creation.date = data.manager[["creation.date"]],
        last.modified.date = data.manager[["last.modified.date"]],
        onedrive.link = onedrive.link
    )

    if (file.exists(DATA.MANAGER.CACHE.METADATA.FILE)) {
        cache.metadata <- get(load(DATA.MANAGER.CACHE.METADATA.FILE))
        cache.metadata[[file]] <- data.manager.metadata
    } else {
        cache.metadata <- setNames(list(data.manager.metadata), file)
    }

    save(cache.metadata, file = DATA.MANAGER.CACHE.METADATA.FILE)
}

## ---- INTERNAL USE ONLY ---- ##

is.cached.data.manager.out.of.date <- function(file, data.manager, error.prefix = "") {
    # browser()
    if (!R6::is.R6(data.manager) || !is(data.manager, "jheem.data.manager")) {
        stop(paste0(error.prefix, "'load.data.manager.from.cache' can only be called on JHEEM Data Manager objects"))
    }
    
    if (!file.exists(DATA.MANAGER.CACHE.METADATA.FILE)) {
        stop(paste0(error.prefix, "The 'data.manager.cache.metadata.Rdata' file is missing from commoncode - this probably requires a pull"))
    }
    
    data.manager.cache.metadata <- get(load(DATA.MANAGER.CACHE.METADATA.FILE))
    
    if (!(file %in% names(data.manager.cache.metadata))) {
        stop(paste0(error.prefix, "'", file, "' is not one of our cached files. Call 'get.data.manager.cache.metadata()' to check what files are cached. File names are capitalization-sensitive."))
    }
    
    # Check if the creation date and last modified date are both at least as new as the cached dates
    if (!is.null(data.manager.cache.metadata[[file]][["creation.date"]])) {
        if (!is.null(data.manager[["creation.date"]]) && data.manager[["creation.date"]] < data.manager.cache.metadata[[file]][["creation.date"]]) {
            return(TRUE)
        }
    }
    if (!is.null(data.manager.cache.metadata[[file]][["last.modified.date"]])) {
        if (!is.null(data.manager[["last.modified.date"]]) && data.manager[["last.modified.date"]] < data.manager.cache.metadata[[file]][["last.modified.date"]]) {
            return(TRUE)
        }
    }
    
    FALSE
}

download.data.manager.from.onedrive <- function(destination.file, onedrive.link, error.prefix, verbose = F) {
    req <- httr2::request(onedrive.link)
    resp <- req |> httr2::req_perform()
    if (httr2::resp_status(resp) == 200) {
        if (verbose) cat("File downloaded successfully to:", download_path, "\n")
        writeBin(httr2::resp_body_raw(resp), destination.file)
    } else {
        if (verbose) cat("Failed to download the file. HTTP Status:", httr2::resp_status(resp), "\n")
        stop(paste0(error.prefix, "HTTP request failed; cached OneDrive link may be out of date or broken"))
    }
}
