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
PACKAGE.VERSION.CACHE.FILE <- "../jheem_analyses/commoncode/package_version_cache.Rdata"

if (is.null(JHEEM.CACHE.DIR)) {
    stop("No 'cached' directory exists - you need to get this from Todd's One-Drive")
}

## PUBLIC----

#' @title Load Data Manager From Cache
#' @description
#' Loads a data manager for which there is cached metadata, downloading the most recent copy if necessary.
#' 
#' @param file Name of a data manager file, with its extension, that can be appended to the JHEEM.CACHE.DIR path.
#' @param set.as.default Should this data manager be set as the default data manager for this session?
#' @param offline If TRUE, having a missing or out of date data manager will not trigger a download from the internet. Use if offline to avoid errors.
load.data.manager.from.cache <- function(file, set.as.default = F, offline=F) {
    error.prefix <- "Cannot load.data.manager.from.cache(): "
    cache.metadata <- get.data.manager.cache.metadata(pretty.print=F)
    if (!(file %in% names(cache.metadata))) {
        stop(paste0(error.prefix, "'", file, "' is not one of our cached files. Call 'get.data.manager.cache.metadata()' to check what files are cached. File names are capitalization-sensitive."))
    }
    # Download it if it doesn't exist or it exists but is out of date
    if (!file.exists(file.path(JHEEM.CACHE.DIR, file))) {
        if (offline) {
            stop(paste0(error.prefix, "File not found, and cannot download the latest copy from the OneDrive if 'offline' is set to TRUE"))
        } else {
            cat("File not found, so downloading the latest copy from the OneDrive...\n")
            download.data.manager.from.onedrive(file.path(JHEEM.CACHE.DIR, file), cache.metadata[[file]]$onedrive.link, error.prefix)
            loaded.data.manager <- load.data.manager(file.path(JHEEM.CACHE.DIR, file), set.as.default = set.as.default)
        }
    } else {
        loaded.data.manager <- load.data.manager(file.path(JHEEM.CACHE.DIR, file), set.as.default = set.as.default)
        if (is.cached.data.manager.out.of.date(file, loaded.data.manager, error.prefix = error.prefix)) {
            if (offline) {
                warning(paste0("The local copy of '", file, "' is out of date (", loaded.data.manager$last.modified.date, " and needs ", cache.metadata[[file]]$last.modified.date, "), but the latest copy cannot be downloaded from the OneDrive if 'offline' is set to TRUE"))
            } else {
                print(paste0("Local copy is out of date (", loaded.data.manager$last.modified.date, " and needs ", cache.metadata[[file]]$last.modified.date, "), so downloading the latest copy from the OneDrive...\n"))
                download.data.manager.from.onedrive(file.path(JHEEM.CACHE.DIR, file), cache.metadata[[file]]$onedrive.link, error.prefix)
                loaded.data.manager <- load.data.manager(file.path(JHEEM.CACHE.DIR, file), set.as.default = set.as.default)
            }
        }
    }    
    loaded.data.manager
}

#' @title Get Data Manager Cache Metadata
#' @description
#' Get information about a cached data manager, such as its creation/last modified dates and download URL.
#' @param pretty.print Organizes the output
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

# Call this on a case-by-case basis if you want to directly download the latest one without checking what you've already got
#' @inheritParams load.data.manager.from.cache
update.data.manager <- function(file) {
    error.prefix <- "Cannot update.data.manager(): "
    cache.metadata <- get.data.manager.cache.metadata(pretty.print=F)
    if (!(file %in% names(cache.metadata))) {
        stop(paste0(error.prefix, "'", file, "' is not one of our cached files. Call 'get.data.manager.cache.metadata()' to check what files are cached. File names are capitalization-sensitive."))
    }
    download.data.manager.from.onedrive(file.path(JHEEM.CACHE.DIR, file), cache.metadata[[file]]$onedrive.link, error.prefix)
    return(0)
}

#' @title Update JHEEM2 Package
#' @description
#' Install the JHEEM2 package if the version is too old, or if it is not
#' installed at all.
#' @param upgrade.dependencies "default", "ask", "always", or "never", defaulting to "never" try to upgrade dependendencies. Avoids annoyances.
#' 
update.jheem2.package <- function(upgrade.dependencies=c("default", "ask", "always", "never")[4]) {
    if (nchar(system.file(package = "jheem2")) == 0) {
        "Package 'jheem2' not found. Installing from Github..."
        if (Sys.getenv("ON_CLUSTER")=="true")
            tryCatch({devtools::install_github("tfojo1/jheem2", upgrade = upgrade.dependencies, lib=Sys.getenv("R_LIBS_USER"))},
                     error=function(e) {stop("Installing 'jheem2' from Github failed")})
        else
            tryCatch({devtools::install_github("tfojo1/jheem2", upgrade = upgrade.dependencies)},
                     error=function(e) {stop("Installing 'jheem2' from Github failed")})
    }
    if (is.package.out.of.date("jheem2")) {
        "Current installation of package 'jheem2' is out of date. Installing from Github..."
        remove.packages("jheem2")
        if (Sys.getenv("ON_CLUSTER")=="true")
            tryCatch({devtools::install_github("tfojo1/jheem2", upgrade = upgrade.dependencies, lib=Sys.getenv("R_LIBS_USER"))},
                     error=function(e) {stop("Installing 'jheem2' from Github failed. You may need to restart R and try again, making sure to close all R sessions that may be using the package")})
        else
            tryCatch({devtools::install_github("tfojo1/jheem2", upgrade = upgrade.dependencies)},
                 error=function(e) {stop("Installing 'jheem2' from Github failed. You may need to restart R and try again, making sure to close all R sessions that may be using the package")})
    }
    print(paste0("'jheem2' package is up to date with version ", packageVersion("jheem2")))
}

#' @title Check JHEEM2 Version
#' @description
#' Show the required JHEEM2 version and the installed version
check.jheem2.version <- function() {
    is.package.out.of.date("jheem2", verbose=T)
}

## ZOE ONLY ----

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

## ANDREW ONLY ----

sync.package.version <- function(package="jheem2", allow.flag=F) {
    error.prefix <- "Cannot sync.package.version(): "
    if (!is.character(package) || length(package)!=1 || is.na(package))
        stop(paste0(error.prefix, "'package' must be a single character value. Defaults to 'jheem2'"))
    if (!allow.flag) {
        stop("Are you sure you want to sync.package.version()?  - you need to set the allow.flag to TRUE if you do")
    }
    if (nchar(system.file(package = package)) == 0)
        stop(paste0(error.prefix, "package '", package, "' is not installed currently. Rebuild?"))
    current.version = packageVersion(package)
    print(paste0("Setting cached package version to ", current.version))
    if (file.exists(PACKAGE.VERSION.CACHE.FILE))
        cache.file = get(load(PACKAGE.VERSION.CACHE.FILE))
    else cache.file = list()
    cache.file[[package]] = current.version
    save(cache.file, file = PACKAGE.VERSION.CACHE.FILE)
}

## INTERNAL USE ONLY ----

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
        if (is.null(data.manager[["creation.date"]]))
            return(TRUE)
        if (data.manager[["creation.date"]] < data.manager.cache.metadata[[file]][["creation.date"]]) {
            return(TRUE)
        }
    }
    if (!is.null(data.manager.cache.metadata[[file]][["last.modified.date"]])) {
        if (is.null(data.manager[["last.modified.date"]]))
            return(TRUE)
        if (data.manager[["last.modified.date"]] < data.manager.cache.metadata[[file]][["last.modified.date"]]) {
            return(TRUE)
        }
    }
    
    FALSE
}

is.package.out.of.date <- function(package="jheem2", verbose=F) {
    error.prefix <- "Cannot check if package is out of date: "
    if (!is.character(package) || length(package)!=1 || is.na(package))
        stop(paste0(error.prefix, "'package' must be a single character value. Defaults to 'jheem2'"))
    if (!is.logical(verbose) || length(verbose)!=1 || is.na(verbose))
        stop(paste0(error.prefix, "'verbose' must be TRUE or FALSE"))
    if (nchar(system.file(package = package)) == 0)
        stop(paste0(error.prefix, "package '", package, "' is not installed. Install it with 'devtools::install_github('tfojo1/", package, "')'"))
    if (!file.exists(PACKAGE.VERSION.CACHE.FILE))
        stop(paste0(error.prefix, "the file with the cached version could not be found. Make sure your working directory is 'jheem_analyses' or a parallel directory"))
    cache.file = get(load(PACKAGE.VERSION.CACHE.FILE))
    if (!(package %in% names(cache.file)))
        stop(paste0(error.prefix, "The version cache file has no entry for package '", package, "'. Reach out to Andrew if you would like version tracked for this package."))
    if (verbose)
        print(paste0("The version for package '", package, "' must be >= ", cache.file[[package]], "; installed version is ", as.character(packageVersion(package)), "."))
    invisible(packageVersion(package) < cache.file[[package]])
}

download.data.manager.from.onedrive <- function(destination.file, onedrive.link, error.prefix, verbose = F) {
    req <- httr2::request(onedrive.link)
    tryCatch({resp <- req |> httr2::req_perform()},
             error=function(e) {stop(paste0(error.prefix, "Failed to download the file. If you are trying to use 'load.data.manager.from.cache' offline, set argument 'offline' to TRUE and try again"))})
    if (httr2::resp_status(resp) == 200) {
        if (verbose) cat("File downloaded successfully to:", download_path, "\n")
        writeBin(httr2::resp_body_raw(resp), destination.file)
    } else {
        if (verbose) cat("Failed to download the file. HTTP Status:", httr2::resp_status(resp), "\n")
        stop(paste0(error.prefix, "HTTP request failed; cached OneDrive link may be out of date or broken"))
    }
}
