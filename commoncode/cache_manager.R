# A lot of people have done the "first time setup" already, so they need to install this new dependency
if (nchar(system.file(package = "httr2")) == 0) {
    install.packages("httr2")
}
if (nchar(system.file(package = "jsonlite")) == 0) {
    install.packages("jsonlite")
}
if (nchar(system.file(package = "filelock")) == 0) {
    install.packages("filelock")
}
if (nchar(system.file(package = "openssl")) == 0) {
    install.packages("openssl")
}

JHEEM.CACHE.DIR <- NULL
if (dir.exists("../../cached")) {
    JHEEM.CACHE.DIR <- "../../cached"
}
if (dir.exists("../jheem_analyses/cached")) {
    JHEEM.CACHE.DIR <- "../jheem_analyses/cached"
}
DATA.MANAGER.CACHE.METADATA.FILE <- "../jheem_analyses/commoncode/data_manager_cache_metadata.Rdata"
DATA.MANAGER.SOURCES.FILE <- "../jheem_analyses/commoncode/data_manager_sources.json"
PACKAGE.VERSION.CACHE.FILE <- "../jheem_analyses/commoncode/package_version_cache.Rdata"

if (is.null(JHEEM.CACHE.DIR)) {
    stop("No 'cached' directory exists - you need to get this from Todd's One-Drive")
}

## PUBLIC----

#' @title Load Data Manager From Cache
#' @description
#' Loads a data manager, downloading the most recent copy if necessary.
#' Managers listed in data_manager_sources.json are fetched from GitHub Releases.
#' All others use the legacy OneDrive path via data_manager_cache_metadata.Rdata.
#'
#' @param file Name of a data manager file, with its extension, that can be appended to the JHEEM.CACHE.DIR path.
#' @param set.as.default Should this data manager be set as the default data manager for this session?
#' @param offline If TRUE, having a missing or out of date data manager will not trigger a download from the internet. Use if offline to avoid errors.
#' @param release.tag Optional GitHub Release tag. Immutable version tags are
#' recommended; the configured latest alias is resolved to its promoted version.
#' When omitted, the existing latest-manager behavior is unchanged.
load.data.manager.from.cache <- function(file, set.as.default = F, offline=F,
                                         release.tag = NULL) {
    error.prefix <- "Cannot load.data.manager.from.cache(): "

    # Check if this manager has a GitHub Release source
    gh.source <- get.github.release.source(file)

    if (!is.null(release.tag)) {
        if (is.null(gh.source)) {
            stop(paste0(error.prefix, "'release.tag' is only supported for data managers backed by GitHub Releases"))
        }
        return(load.data.manager.from.github.release(
            file, gh.source, release.tag, set.as.default, offline, error.prefix
        ))
    }

    if (!is.null(gh.source)) {
        return(load.data.manager.from.github(file, gh.source, set.as.default, offline, error.prefix))
    }

    # Fall back to legacy OneDrive path
    load.data.manager.from.onedrive.cache(file, set.as.default, offline, error.prefix)
}

#' @title Get the Release Identity of a Loaded Data Manager
#' @description
#' Returns the release tag, repository, asset, digest, and local cache path for a
#' data manager loaded with an explicit `release.tag`. Returns NULL for managers
#' loaded through the legacy or latest-manager paths.
#' @param data.manager A loaded JHEEM data manager.
get.data.manager.resolution <- function(data.manager) {
    attr(data.manager, "jheem.manager.resolution", exact = TRUE)
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

    # Check if this manager has a GitHub Release source
    gh.source <- get.github.release.source(file)
    if (!is.null(gh.source)) {
        download.data.manager.from.github.release(file, gh.source, error.prefix)
        return(0)
    }

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

## GITHUB RELEASE FUNCTIONS ----

get.github.release.source <- function(file) {
    if (!file.exists(DATA.MANAGER.SOURCES.FILE)) return(NULL)
    sources <- jsonlite::fromJSON(DATA.MANAGER.SOURCES.FILE)
    if (!(file %in% names(sources))) return(NULL)
    entry <- sources[[file]]
    if (is.null(entry$source) || entry$source != "github-release") return(NULL)
    entry
}

validate.github.release.component <- function(value, label, error.prefix) {
    if (!is.character(value) || length(value) != 1 || is.na(value) ||
        !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", value)) {
        stop(paste0(error.prefix, "Invalid GitHub Release ", label, ": '", value, "'"))
    }
    value
}

github.release.request <- function(url) {
    req <- httr2::request(url) |>
        httr2::req_headers(
            "Accept" = "application/vnd.github+json",
            "X-GitHub-Api-Version" = "2022-11-28",
            "User-Agent" = "jheem-cache-manager"
        )
    token <- Sys.getenv("GITHUB_TOKEN")
    if (!nzchar(token)) token <- Sys.getenv("GH_TOKEN")
    if (nzchar(token)) req <- httr2::req_auth_bearer_token(req, token)
    req
}

get.github.release.by.tag <- function(repo, tag, error.prefix) {
    tag <- validate.github.release.component(tag, "tag", error.prefix)
    api.url <- paste0(
        "https://api.github.com/repos/", repo, "/releases/tags/",
        utils::URLencode(tag, reserved = TRUE)
    )
    tryCatch({
        resp <- github.release.request(api.url) |> httr2::req_perform()
        jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE)
    }, error = function(e) {
        stop(paste0(error.prefix, "Could not resolve GitHub Release tag '", tag,
                    "' in ", repo, ": ", conditionMessage(e)), call. = FALSE)
    })
}

promoted.github.release.tag <- function(release.info) {
    body <- release.info$body
    if (is.null(body) || length(body) != 1 || is.na(body)) return(NULL)
    match <- regexec("Promoted from:.*?`([^`]+)`", body, perl = TRUE)
    groups <- regmatches(body, match)[[1]]
    if (length(groups) < 2) NULL else groups[[2]]
}

resolve.github.release.asset <- function(file, gh.source, release.tag, error.prefix) {
    if (!is.character(release.tag) || length(release.tag) != 1 ||
        is.na(release.tag) || !nzchar(release.tag)) {
        stop(paste0(error.prefix, "'release.tag' must be one non-empty character value"))
    }

    requested.tag <- validate.github.release.component(release.tag, "tag", error.prefix)
    resolved.tag <- requested.tag
    release.info <- get.github.release.by.tag(gh.source$repo, requested.tag, error.prefix)

    if (identical(requested.tag, gh.source$latest_tag)) {
        resolved.tag <- promoted.github.release.tag(release.info)
        if (is.null(resolved.tag)) {
            stop(paste0(error.prefix, "The mutable alias '", requested.tag,
                        "' does not identify its promoted immutable release"))
        }
        resolved.tag <- validate.github.release.component(resolved.tag, "promoted tag", error.prefix)
        release.info <- get.github.release.by.tag(gh.source$repo, resolved.tag, error.prefix)
    }

    asset.name <- if (!is.null(gh.source$asset)) gh.source$asset else file
    asset.name <- validate.github.release.component(asset.name, "asset name", error.prefix)
    matching.assets <- Filter(
        function(asset) !is.null(asset$name) && identical(asset$name, asset.name),
        release.info$assets
    )
    if (length(matching.assets) != 1) {
        stop(paste0(error.prefix, "Release '", resolved.tag, "' in ", gh.source$repo,
                    " does not contain exactly one asset named '", asset.name, "'"))
    }

    asset <- matching.assets[[1]]
    digest <- asset$digest
    if (is.null(digest) || length(digest) != 1 || is.na(digest) ||
        !grepl("^sha256:[0-9a-fA-F]{64}$", digest)) {
        stop(paste0(error.prefix, "Release asset '", asset.name,
                    "' does not publish a valid SHA-256 digest"))
    }

    list(
        schema_version = 1L,
        manager = file,
        repository = gh.source$repo,
        requested_tag = requested.tag,
        resolved_tag = resolved.tag,
        asset = asset.name,
        sha256 = tolower(sub("^sha256:", "", digest)),
        download_url = asset$browser_download_url,
        published_at = release.info$published_at
    )
}

data.manager.release.paths <- function(resolution, error.prefix) {
    manager <- validate.github.release.component(resolution$manager, "manager name", error.prefix)
    tag <- validate.github.release.component(resolution$resolved_tag, "tag", error.prefix)
    asset <- validate.github.release.component(resolution$asset, "asset name", error.prefix)
    directory <- file.path(JHEEM.CACHE.DIR, "data-managers", manager, tag)
    list(
        directory = directory,
        artifact = file.path(directory, asset),
        metadata = file.path(directory, "resolution.json"),
        lock = paste0(directory, ".lock")
    )
}

sha256.file <- function(path) {
    connection <- file(path, open = "rb")
    on.exit(close(connection), add = TRUE)
    as.vector(as.character(openssl::sha256(connection)))
}

read.data.manager.resolution <- function(path) {
    if (!file.exists(path)) return(NULL)
    tryCatch(
        jsonlite::fromJSON(path, simplifyVector = TRUE),
        error = function(e) NULL
    )
}

cached.release.is.valid <- function(paths, resolution) {
    if (!file.exists(paths$artifact) || !file.exists(paths$metadata)) return(FALSE)
    cached <- read.data.manager.resolution(paths$metadata)
    required <- c("schema_version", "manager", "repository", "requested_tag",
                  "resolved_tag", "asset", "sha256", "published_at")
    if (is.null(cached) || !all(required %in% names(cached))) return(FALSE)
    expected <- unlist(resolution[required], use.names = TRUE)
    actual <- unlist(cached[required], use.names = TRUE)
    if (!identical(as.character(actual), as.character(expected))) return(FALSE)
    identical(tolower(sha256.file(paths$artifact)), tolower(resolution$sha256))
}

write.release.resolution <- function(resolution, path) {
    persisted <- resolution[c(
        "schema_version", "manager", "repository", "requested_tag",
        "resolved_tag", "asset", "sha256", "published_at"
    )]
    jsonlite::write_json(persisted, path, auto_unbox = TRUE, pretty = TRUE)
}

get.cached.github.release.resolution <- function(file, gh.source, release.tag,
                                                 error.prefix) {
    if (identical(release.tag, gh.source$latest_tag)) {
        stop(paste0(error.prefix, "The mutable alias '", release.tag,
                    "' cannot be resolved offline; use its immutable release tag"))
    }
    tag <- validate.github.release.component(release.tag, "tag", error.prefix)
    asset <- if (!is.null(gh.source$asset)) gh.source$asset else file
    candidate <- list(manager = file, resolved_tag = tag, asset = asset)
    paths <- data.manager.release.paths(candidate, error.prefix)
    resolution <- read.data.manager.resolution(paths$metadata)
    if (is.null(resolution) ||
        !identical(as.character(resolution$manager), file) ||
        !identical(as.character(resolution$repository), gh.source$repo) ||
        !identical(as.character(resolution$resolved_tag), tag) ||
        !identical(as.character(resolution$asset), asset) ||
        is.null(resolution$sha256) ||
        !grepl("^[0-9a-fA-F]{64}$", resolution$sha256) ||
        !cached.release.is.valid(paths, resolution)) {
        stop(paste0(error.prefix, "The cached copy of '", file,
                    "' for release '", tag,
                    "' is missing or failed metadata or digest verification, and 'offline' is TRUE"))
    }
    resolution
}

download.github.release.asset <- function(resolution, destination, error.prefix) {
    tryCatch(
        github.release.request(resolution$download_url) |>
            httr2::req_perform(path = destination),
        error = function(e) {
            stop(paste0(error.prefix, "Failed to download '", resolution$asset,
                        "' from release '", resolution$resolved_tag, "': ",
                        conditionMessage(e)), call. = FALSE)
        }
    )
}

materialize.github.release.asset <- function(resolution, offline, error.prefix) {
    paths <- data.manager.release.paths(resolution, error.prefix)
    dir.create(paths$directory, recursive = TRUE, showWarnings = FALSE)
    lock <- filelock::lock(paths$lock, timeout = 300000)
    if (is.null(lock)) {
        stop(paste0(error.prefix, "Could not acquire lock for release '",
                    resolution$resolved_tag, "' (timed out after 5 minutes)"))
    }
    on.exit(filelock::unlock(lock), add = TRUE)

    if (cached.release.is.valid(paths, resolution)) return(paths$artifact)
    if (offline) {
        stop(paste0(error.prefix, "The cached copy of '", resolution$manager,
                    "' for release '", resolution$resolved_tag,
                    "' is missing or failed digest verification, and 'offline' is TRUE"))
    }

    temporary.artifact <- paste0(paths$artifact, ".download.", Sys.getpid())
    temporary.metadata <- paste0(paths$metadata, ".write.", Sys.getpid())
    on.exit(unlink(c(temporary.artifact, temporary.metadata)), add = TRUE)

    download.github.release.asset(resolution, temporary.artifact, error.prefix)
    actual.digest <- tolower(sha256.file(temporary.artifact))
    if (!identical(actual.digest, tolower(resolution$sha256))) {
        stop(paste0(error.prefix, "SHA-256 verification failed for '",
                    resolution$asset, "' from release '", resolution$resolved_tag,
                    "': expected ", resolution$sha256, ", got ", actual.digest))
    }

    write.release.resolution(resolution, temporary.metadata)
    if (file.exists(paths$artifact)) unlink(paths$artifact)
    if (!file.rename(temporary.artifact, paths$artifact)) {
        stop(paste0(error.prefix, "Could not move verified release asset into the cache"))
    }
    if (file.exists(paths$metadata)) unlink(paths$metadata)
    if (!file.rename(temporary.metadata, paths$metadata)) {
        unlink(paths$artifact)
        stop(paste0(error.prefix, "Could not record release metadata in the cache"))
    }
    paths$artifact
}

load.data.manager.from.github.release <- function(file, gh.source, release.tag,
                                                  set.as.default, offline,
                                                  error.prefix) {
    resolution <- if (offline) {
        get.cached.github.release.resolution(file, gh.source, release.tag, error.prefix)
    } else {
        resolve.github.release.asset(file, gh.source, release.tag, error.prefix)
    }
    local.path <- materialize.github.release.asset(resolution, offline, error.prefix)
    data.manager <- load.data.manager(local.path, set.as.default = set.as.default)
    resolution$local_path <- normalizePath(local.path, mustWork = TRUE)
    attr(data.manager, "jheem.manager.resolution") <- resolution
    invisible(data.manager)
}

load.data.manager.from.github <- function(file, gh.source, set.as.default, offline, error.prefix) {
    local.path <- file.path(JHEEM.CACHE.DIR, file)
    version.file <- paste0(local.path, ".version")
    lock.file <- paste0(local.path, ".lock")

    # Offline mode: skip all network checks
    if (file.exists(local.path) && offline) {
        return(load.data.manager(local.path, set.as.default = set.as.default))
    }
    if (!file.exists(local.path) && offline) {
        stop(paste0(error.prefix, "File not found, and cannot download if 'offline' is set to TRUE"))
    }

    # Check remote version before acquiring lock (fast, read-only)
    remote.version <- get.github.release.version(gh.source, error.prefix)
    if (is.null(remote.version)) {
        if (file.exists(local.path)) {
            warning("Could not check GitHub for updates to '", file, "'. Using local copy.")
            return(load.data.manager(local.path, set.as.default = set.as.default))
        }
        stop(paste0(error.prefix, "File not found locally and could not reach GitHub to download it"))
    }

    local.version <- if (file.exists(version.file)) trimws(readLines(version.file, n = 1)) else NULL
    needs.update <- !file.exists(local.path) || is.null(local.version) || local.version != remote.version

    if (needs.update) {
        # Acquire exclusive lock — if another process is downloading, we wait here
        lck <- filelock::lock(lock.file, timeout = 300000)
        if (is.null(lck)) {
            stop(paste0(error.prefix, "Could not acquire lock to download '", file, "' (timed out after 5 minutes)"))
        }
        on.exit(filelock::unlock(lck), add = TRUE)

        # Re-check after acquiring lock — another process may have finished the download
        local.version <- if (file.exists(version.file)) trimws(readLines(version.file, n = 1)) else NULL
        if (file.exists(local.path) && !is.null(local.version) && local.version == remote.version) {
            cat(file, "is up to date (", local.version, ") — updated by another process\n")
        } else {
            if (!is.null(local.version)) {
                cat("Updating ", file, " (", local.version, " -> ", remote.version, ")...\n", sep = "")
            } else if (file.exists(local.path)) {
                cat("Updating ", file, " (unknown local version -> ", remote.version, ")...\n", sep = "")
            } else {
                cat(file, "not found locally. Downloading from GitHub Release...\n")
            }
            download.data.manager.from.github.release(file, gh.source, error.prefix)
        }
    } else {
        cat(file, "is up to date (", local.version, ")\n")
    }

    load.data.manager(local.path, set.as.default = set.as.default)
}

get.github.release.version <- function(gh.source, error.prefix) {
    api.url <- paste0("https://api.github.com/repos/", gh.source$repo,
                       "/releases/tags/", gh.source$latest_tag)
    tryCatch({
        resp <- httr2::request(api.url) |>
            httr2::req_headers("Accept" = "application/vnd.github.v3+json",
                               "User-Agent" = "jheem-cache-manager") |>
            httr2::req_perform()
        release.info <- jsonlite::fromJSON(httr2::resp_body_string(resp))
        # Extract the source version tag from the release body
        # The promotion workflow writes "**Promoted from:** `syphilis-manager-v2026.03.11`"
        body <- release.info$body
        promoted.match <- regmatches(body, regexpr("Promoted from:.*?`([^`]+)`", body, perl = TRUE))
        if (length(promoted.match) == 1) {
            return(gsub(".*`([^`]+)`.*", "\\1", promoted.match, perl = TRUE))
        }
        # Fallback: use the published_at timestamp as version identifier
        release.info$published_at
    }, error = function(e) {
        NULL
    })
}

download.data.manager.from.github.release <- function(file, gh.source, error.prefix) {
    local.path <- file.path(JHEEM.CACHE.DIR, file)
    version.file <- paste0(local.path, ".version")
    asset.name <- if (!is.null(gh.source$asset)) gh.source$asset else file

    download.url <- paste0("https://github.com/", gh.source$repo,
                           "/releases/download/", gh.source$latest_tag,
                           "/", asset.name)

    # Download to a temp file first, then atomically rename into place.
    # This prevents other processes from reading a partially-written file.
    tmp.path <- paste0(local.path, ".download.", Sys.getpid())
    tryCatch({
        resp <- httr2::request(download.url) |>
            httr2::req_headers("User-Agent" = "jheem-cache-manager") |>
            httr2::req_perform()
        if (httr2::resp_status(resp) == 200) {
            writeBin(httr2::resp_body_raw(resp), tmp.path)
            file.rename(tmp.path, local.path)
        } else {
            stop("HTTP ", httr2::resp_status(resp))
        }
    }, error = function(e) {
        unlink(tmp.path)
        stop(paste0(error.prefix, "Failed to download '", file, "' from GitHub Release: ", e$message))
    })

    # Write the version sidecar
    remote.version <- get.github.release.version(gh.source, error.prefix)
    if (!is.null(remote.version)) {
        writeLines(remote.version, version.file)
    }

    cat("Downloaded", file, "from GitHub Release (", gh.source$latest_tag, ")\n")
}

## LEGACY ONEDRIVE FUNCTIONS ----

load.data.manager.from.onedrive.cache <- function(file, set.as.default, offline, error.prefix) {
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
