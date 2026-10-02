## Run from any directory with:
## Rscript commoncode/tests/test_data_manager_release_selection.R
##
## Set RUN_DATA_MANAGER_RELEASE_INTEGRATION=true to also download and load an
## immutable historical syphilis manager from GitHub Releases.

script.argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script.argument) != 1) stop("Could not locate this test script")
script.path <- normalizePath(sub("^--file=", "", script.argument))
repository.root <- normalizePath(file.path(dirname(script.path), "../.."))
bootstrap.root <- tempfile("manager-loader-bootstrap-")
bootstrap.checkout <- file.path(bootstrap.root, "jheem_analyses")
dir.create(file.path(bootstrap.checkout, "cached"), recursive = TRUE)
original.working.directory <- setwd(bootstrap.checkout)
on.exit(setwd(original.working.directory), add = TRUE)

test.environment <- new.env(parent = globalenv())
sys.source(file.path(repository.root, "commoncode/cache_manager.R"), envir = test.environment)
test.environment$DATA.MANAGER.SOURCES.FILE <- file.path(repository.root, "commoncode/data_manager_sources.json")

assert.error <- function(expression, pattern = NULL) {
    error <- tryCatch({
        force(expression)
        NULL
    }, error = identity)
    stopifnot(inherits(error, "error"))
    if (!is.null(pattern)) stopifnot(grepl(pattern, conditionMessage(error)))
    invisible(error)
}

## Public release authentication: use only synthetic tokens and mocked responses.
local({
    token.names <- c("GITHUB_TOKEN", "GH_TOKEN")
    saved.tokens <- Sys.getenv(token.names, unset = NA_character_)
    on.exit({
        Sys.unsetenv(token.names)
        present <- !is.na(saved.tokens)
        if (any(present)) do.call(Sys.setenv, as.list(saved.tokens[present]))
    }, add = TRUE)
    Sys.setenv(GITHUB_TOKEN = "manager-test-token", GH_TOKEN = "shadow-test-token")
    public.url <- "https://api.github.com/repos/tfojo1/jheem_analyses/releases/tags/manager-v1"
    requests <- list()
    messages <- character()
    response <- function(status) httr2::response(
        status, headers = list("content-type" = "application/json"),
        body = charToRaw('{"message":"fixture"}')
    )
    perform <- function(url = public.url, statuses = c(401L, 200L), path = NULL) {
        requests <<- list()
        messages <<- character()
        httr2::with_mocked_responses(function(req) {
            requests[[length(requests) + 1L]] <<- req
            if (length(requests) > length(statuses)) stop("unexpected extra HTTP request")
            response(statuses[[length(requests)]])
        }, withCallingHandlers(
            test.environment$perform.github.release.request(url, path),
            warning = function(w) {
                messages <<- c(messages, conditionMessage(w))
                invokeRestart("muffleWarning")
            }
        ))
    }
    has.auth <- function(req) "authorization" %in% tolower(names(req$headers))
    dummy.authorization <- function(req) {
        # Newer httr2 stores sensitive headers as weak references. Read only
        # this test's synthetic tokens via its public accessor when available;
        # older supported versions keep ordinary strings in req$headers.
        headers <- if ("req_get_headers" %in% getNamespaceExports("httr2")) {
            httr2::req_get_headers(req, redacted = "reveal")
        } else req$headers
        headers[[which(tolower(names(headers)) == "authorization")]]
    }
    result <- perform()
    stopifnot(httr2::resp_status(result) == 200L, length(requests) == 2L,
              identical(requests[[1]]$url, requests[[2]]$url),
              identical(dummy.authorization(requests[[1]]), "Bearer manager-test-token"),
              !has.auth(requests[[2]]), length(messages) == 1L,
              !grepl("manager-test-token|shadow-test-token", messages))

    # Asset requests get the same retry. httr2's mock does not write `path`;
    # materialization and digest enforcement are exercised below separately.
    destination <- tempfile("manager-http-test-")
    on.exit(unlink(destination), add = TRUE)
    perform("https://github.com/tfojo1/jheem_analyses/releases/download/manager-v1/fixture.rdata",
            path = destination)
    stopifnot(length(requests) == 2L,
              !has.auth(requests[[2]]))

    assert.error(perform(statuses = c(401L, 401L)), "401")
    stopifnot(length(requests) == 2L)
    for (status in c(403L, 404L, 429L, 500L)) {
        assert.error(perform(statuses = status), as.character(status))
        stopifnot(length(requests) == 1L, length(messages) == 0L)
    }
    for (url in c("https://api.github.com/repos/example/private/releases/tags/v1",
                  "https://api.github.com/repos/tfojo1/jheem_analyses-other/releases/tags/v1",
                  "https://api.github.com/user")) {
        assert.error(perform(url, statuses = 401L), "401")
        stopifnot(length(requests) == 1L)
    }
    stopifnot(!has.auth(test.environment$github.release.request(
        "https://github.com.example.invalid/asset"
    )))

    Sys.unsetenv("GITHUB_TOKEN")
    perform(statuses = 200L)
    stopifnot(identical(dummy.authorization(requests[[1]]), "Bearer shadow-test-token"))
    Sys.unsetenv("GH_TOKEN")
    perform(statuses = 200L)
    stopifnot(!has.auth(requests[[1]]), length(requests) == 1L)
    assert.error(perform(statuses = 401L), "401")
    stopifnot(length(requests) == 1L, length(messages) == 0L)
})

## The new argument is last so existing positional calls keep their meaning.
stopifnot(identical(
    names(formals(test.environment$load.data.manager.from.cache)),
    c("file", "set.as.default", "offline", "release.tag")
))

## Omitting release.tag retains the existing GitHub/latest dispatch.
original.get.source <- test.environment$get.github.release.source
original.load.latest <- test.environment$load.data.manager.from.github
original.load.release <- test.environment$load.data.manager.from.github.release
test.environment$get.github.release.source <- function(file) list(repo = "example/repo")
test.environment$load.data.manager.from.github <- function(file, gh.source,
                                                           set.as.default,
                                                           offline,
                                                           error.prefix) {
    list(route = "latest", file = file, set.as.default = set.as.default,
         offline = offline)
}
test.environment$load.data.manager.from.github.release <- function(file, gh.source,
                                                                   release.tag,
                                                                   set.as.default,
                                                                   offline,
                                                                   error.prefix) {
    list(route = "release", release.tag = release.tag)
}
latest.result <- test.environment$load.data.manager.from.cache(
    "syphilis.manager.rdata", TRUE, TRUE
)
release.result <- test.environment$load.data.manager.from.cache(
    "syphilis.manager.rdata", TRUE, TRUE, "manager-v1"
)
stopifnot(identical(latest.result$route, "latest"))
stopifnot(isTRUE(latest.result$set.as.default), isTRUE(latest.result$offline))
stopifnot(identical(release.result$route, "release"))
test.environment$get.github.release.source <- original.get.source
test.environment$load.data.manager.from.github <- original.load.latest
test.environment$load.data.manager.from.github.release <- original.load.release

## Resolve an immutable tag and a mutable alias using deterministic fixtures.
fixture.directory <- tempfile("manager-release-test-")
dir.create(fixture.directory)
on.exit(unlink(fixture.directory, recursive = TRUE), add = TRUE)
fixture.file <- file.path(fixture.directory, "fixture.rdata")
writeBin(charToRaw("verified release fixture\n"), fixture.file)
fixture.digest <- test.environment$sha256.file(fixture.file)
fixture.asset <- list(
    name = "fixture.rdata",
    digest = paste0("sha256:", fixture.digest),
    browser_download_url = "https://example.invalid/fixture.rdata"
)
version.release <- list(
    tag_name = "manager-v1",
    body = "",
    published_at = "2026-09-09T00:00:00Z",
    assets = list(fixture.asset)
)
alias.release <- list(
    tag_name = "manager-latest",
    body = "**Promoted from:** `manager-v1`",
    published_at = "2026-09-09T00:01:00Z",
    assets = list(fixture.asset)
)
source.configuration <- list(
    repo = "example/repo",
    latest_tag = "manager-latest",
    asset = "fixture.rdata"
)
original.get.release <- test.environment$get.github.release.by.tag
test.environment$get.github.release.by.tag <- function(repo, tag, error.prefix) {
    if (identical(tag, "manager-latest")) alias.release
    else if (identical(tag, "manager-v1")) version.release
    else stop("fixture tag not found")
}
exact.resolution <- test.environment$resolve.github.release.asset(
    "fixture.rdata", source.configuration, "manager-v1", "test: "
)
alias.resolution <- test.environment$resolve.github.release.asset(
    "fixture.rdata", source.configuration, "manager-latest", "test: "
)
stopifnot(identical(exact.resolution$resolved_tag, "manager-v1"))
stopifnot(identical(alias.resolution$requested_tag, "manager-latest"))
stopifnot(identical(alias.resolution$resolved_tag, "manager-v1"))
stopifnot(identical(alias.resolution$sha256, fixture.digest))
assert.error(
    test.environment$resolve.github.release.asset(
        "fixture.rdata", source.configuration, "../unsafe", "test: "
    ),
    "Invalid GitHub Release tag"
)
test.environment$get.github.release.by.tag <- original.get.release

## Materialization is version-scoped, verified, atomic, and reusable offline.
test.environment$JHEEM.CACHE.DIR <- file.path(fixture.directory, "cache")
original.download <- test.environment$download.github.release.asset
test.environment$download.github.release.asset <- function(resolution, destination,
                                                            error.prefix) {
    stopifnot(file.copy(fixture.file, destination, overwrite = TRUE))
    invisible(destination)
}
cached.path <- test.environment$materialize.github.release.asset(
    exact.resolution, offline = FALSE, error.prefix = "test: "
)
stopifnot(file.exists(cached.path))
stopifnot(grepl("data-managers/fixture.rdata/manager-v1/fixture.rdata$",
                cached.path))
stopifnot(identical(test.environment$sha256.file(cached.path), fixture.digest))
offline.resolution <- test.environment$get.cached.github.release.resolution(
    "fixture.rdata", source.configuration, "manager-v1", "test: "
)
offline.path <- test.environment$materialize.github.release.asset(
    offline.resolution, offline = TRUE, error.prefix = "test: "
)
stopifnot(identical(cached.path, offline.path))
assert.error(
    test.environment$get.cached.github.release.resolution(
        "fixture.rdata", source.configuration, "manager-latest", "test: "
    ),
    "cannot be resolved offline"
)

## Corruption is rejected offline and repaired from the selected release online.
writeBin(charToRaw("corrupt\n"), cached.path)
assert.error(
    test.environment$get.cached.github.release.resolution(
        "fixture.rdata", source.configuration, "manager-v1", "test: "
    ),
    "failed metadata or digest verification"
)
repaired.path <- test.environment$materialize.github.release.asset(
    exact.resolution, offline = FALSE, error.prefix = "test: "
)
stopifnot(identical(test.environment$sha256.file(repaired.path), fixture.digest))

## Concurrent readers converge on one verified cache entry under the file lock.
if (.Platform$OS.type == "unix") {
    concurrent.resolution <- exact.resolution
    concurrent.resolution$resolved_tag <- "manager-concurrent"
    concurrent.resolution$requested_tag <- "manager-concurrent"
    test.environment$download.github.release.asset <- function(resolution, destination,
                                                                error.prefix) {
        Sys.sleep(0.2)
        stopifnot(file.copy(fixture.file, destination, overwrite = TRUE))
        invisible(destination)
    }
    concurrent.paths <- parallel::mclapply(1:2, function(index) {
        test.environment$materialize.github.release.asset(
            concurrent.resolution, offline = FALSE, error.prefix = "test: "
        )
    }, mc.cores = 2)
    stopifnot(identical(concurrent.paths[[1]], concurrent.paths[[2]]))
    stopifnot(identical(
        test.environment$sha256.file(concurrent.paths[[1]]), fixture.digest
    ))
}

## A bad download never becomes the cached release.
bad.resolution <- exact.resolution
bad.resolution$resolved_tag <- "manager-v2"
bad.resolution$requested_tag <- "manager-v2"
test.environment$download.github.release.asset <- function(resolution, destination,
                                                            error.prefix) {
    writeBin(charToRaw("not the release asset\n"), destination)
}
assert.error(
    test.environment$materialize.github.release.asset(
        bad.resolution, offline = FALSE, error.prefix = "test: "
    ),
    "SHA-256 verification failed"
)
bad.paths <- test.environment$data.manager.release.paths(bad.resolution, "test: ")
stopifnot(!file.exists(bad.paths$artifact), !file.exists(bad.paths$metadata))
test.environment$download.github.release.asset <- original.download

## The latest alias uses the same verified, version-scoped cache.
latest.cache <- file.path(fixture.directory, "latest-cache")
dir.create(latest.cache)
test.environment$JHEEM.CACHE.DIR <- latest.cache
fixture2.file <- file.path(fixture.directory, "fixture2.rdata")
writeBin(charToRaw("second verified release\n"), fixture2.file)
fixture2.digest <- test.environment$sha256.file(fixture2.file)
alias.resolution.for <- function(tag, digest) {
    resolution <- alias.resolution
    resolution$resolved_tag <- tag
    resolution$sha256 <- digest
    resolution
}
latest.target <- alias.resolution.for("manager-v1", fixture.digest)
original.resolve <- test.environment$resolve.github.release.asset
test.environment$resolve.github.release.asset <- function(file, gh.source, release.tag,
                                                          error.prefix) {
    if (is.null(latest.target)) stop("network unavailable")
    resolution <- latest.target
    resolution$requested_tag <- release.tag
    resolution
}
downloads <- character()
test.environment$download.github.release.asset <- function(resolution, destination,
                                                            error.prefix) {
    downloads <<- c(downloads, resolution$resolved_tag)
    source <- if (identical(resolution$resolved_tag, "manager-v2")) fixture2.file else fixture.file
    stopifnot(file.copy(source, destination, overwrite = TRUE))
    invisible(destination)
}
test.environment$load.data.manager <- function(file, set.as.default = FALSE) {
    list(path = file)
}
legacy.path <- file.path(latest.cache, "fixture.rdata")
current.path <- file.path(latest.cache, "data-managers", "fixture.rdata", "current.json")
load.latest <- function(offline = FALSE) {
    test.environment$load.data.manager.from.github(
        "fixture.rdata", source.configuration, FALSE, offline, "test: "
    )
}

# First load: resolve, download the immutable version, record it, sync the legacy copy.
loaded <- load.latest()
stopifnot(identical(downloads, "manager-v1"))
stopifnot(grepl("data-managers/fixture.rdata/manager-v1/fixture.rdata$", loaded$path))
stopifnot(identical(test.environment$get.data.manager.resolution(loaded)$resolved_tag, "manager-v1"))
stopifnot(identical(jsonlite::fromJSON(current.path)$resolved_tag, "manager-v1"))
stopifnot(identical(test.environment$sha256.file(legacy.path), fixture.digest))
stopifnot(identical(readLines(paste0(legacy.path, ".version")), "manager-v1"))

# Unchanged latest and an explicit request for the same version reuse the cache.
load.latest()
test.environment$load.data.manager.from.github.release(
    "fixture.rdata", source.configuration, "manager-v1", FALSE, FALSE, "test: "
)
stopifnot(identical(downloads, "manager-v1"))

# A promotion is labeled by the version actually downloaded.
latest.target <- alias.resolution.for("manager-v2", fixture2.digest)
loaded <- load.latest()
stopifnot(identical(downloads, c("manager-v1", "manager-v2")))
stopifnot(identical(jsonlite::fromJSON(current.path)$resolved_tag, "manager-v2"))
stopifnot(identical(test.environment$sha256.file(legacy.path), fixture2.digest))
stopifnot(identical(readLines(paste0(legacy.path, ".version")), "manager-v2"))
stopifnot(file.exists(file.path(latest.cache, "data-managers", "fixture.rdata",
                                "manager-v1", "fixture.rdata")))

# Online failure must not select an older input on the operator's behalf.
latest.target <- NULL
pointer.before <- readBin(current.path, "raw", file.info(current.path)$size)
assert.error(load.latest(), "No cached manager was substituted")
stopifnot(identical(readBin(current.path, "raw", file.info(current.path)$size), pointer.before))
# Explicit offline mode still loads the last verified version without downloading.
stopifnot(grepl("manager-v2/fixture.rdata$", load.latest(offline = TRUE)$path))
stopifnot(identical(length(downloads), 2L))

# Read-only offline loads neither repair the compatibility copy nor lock the release.
v2.path <- loaded$path
release.lock <- paste0(dirname(v2.path), ".lock")
unlink(release.lock)
writeBin(charToRaw("modified legacy copy\n"), legacy.path)
legacy.digest <- test.environment$sha256.file(legacy.path)
load.latest(offline = TRUE)
stopifnot(!file.exists(release.lock),
          identical(test.environment$sha256.file(legacy.path), legacy.digest))

# An online load repairs a damaged compatibility copy, even at the same size.
latest.target <- alias.resolution.for("manager-v2", fixture2.digest)
writeBin(as.raw(rep(120L, file.size(fixture2.file))), legacy.path)
load.latest()
stopifnot(identical(test.environment$sha256.file(legacy.path), fixture2.digest))
latest.target <- NULL

# A known verified cache must not silently downgrade to an unverified copy.
writeBin(charToRaw("corrupt\n"), file.path(latest.cache, "data-managers", "fixture.rdata",
                                            "manager-v2", "fixture.rdata"))
assert.error(load.latest(offline = TRUE), "failed metadata or digest verification")
file.copy(fixture2.file, v2.path, overwrite = TRUE)
writeLines("invalid JSON", current.path)
assert.error(load.latest(offline = TRUE), "current release record is invalid")

# Legacy-only installations retain an explicitly unverified offline fallback.
unlink(current.path)
legacy.before <- test.environment$sha256.file(legacy.path)
assert.error(load.latest(), "No cached manager was substituted")
stopifnot(identical(test.environment$sha256.file(legacy.path), legacy.before),
          !file.exists(current.path))
legacy.warning <- NULL
legacy.load <- withCallingHandlers(load.latest(offline = TRUE), warning = function(w) {
    legacy.warning <<- conditionMessage(w)
    invokeRestart("muffleWarning")
})
stopifnot(identical(legacy.load$path, legacy.path), grepl("unverified", legacy.warning))
unlink(legacy.path)
assert.error(load.latest(offline = TRUE), "has not been downloaded yet")

test.environment$resolve.github.release.asset <- original.resolve
rm("load.data.manager", envir = test.environment)
test.environment$download.github.release.asset <- original.download
test.environment$JHEEM.CACHE.DIR <- file.path(fixture.directory, "cache")

## Optional end-to-end check against a real historical manager.
if (identical(tolower(Sys.getenv("RUN_DATA_MANAGER_RELEASE_INTEGRATION")), "true")) {
    library(jheem2)
    test.environment$JHEEM.CACHE.DIR <- file.path(fixture.directory, "integration-cache")
    manager <- test.environment$load.data.manager.from.cache(
        "syphilis.manager.rdata",
        set.as.default = FALSE,
        offline = FALSE,
        release.tag = "syphilis-manager-v2026.07.27"
    )
    identity <- test.environment$get.data.manager.resolution(manager)
    stopifnot(is(manager, "jheem.data.manager"))
    stopifnot(identical(identity$resolved_tag, "syphilis-manager-v2026.07.27"))
    stopifnot(identical(
        identity$sha256,
        "0d9bf7e02d58554a52844bdce85e0506c99aec27ac578d052f6b4d2eb89339eb"
    ))
    offline.manager <- test.environment$load.data.manager.from.cache(
        "syphilis.manager.rdata",
        set.as.default = FALSE,
        offline = TRUE,
        release.tag = "syphilis-manager-v2026.07.27"
    )
    stopifnot(is(offline.manager, "jheem.data.manager"))
}

cat("data-manager release selection tests passed\n")
