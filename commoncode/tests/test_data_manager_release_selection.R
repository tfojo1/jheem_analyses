## Run from any directory with:
## Rscript commoncode/tests/test_data_manager_release_selection.R
##
## Set RUN_DATA_MANAGER_RELEASE_INTEGRATION=true to also download and load an
## immutable historical syphilis manager from GitHub Releases.

script.argument <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(script.argument) != 1) stop("Could not locate this test script")
script.path <- normalizePath(sub("^--file=", "", script.argument))
repository.root <- normalizePath(file.path(dirname(script.path), "../.."))
original.working.directory <- setwd(repository.root)
on.exit(setwd(original.working.directory), add = TRUE)

test.environment <- new.env(parent = globalenv())
test.environment$JHEEM.ANALYSES.PATH <- repository.root
test.environment$JHEEM.CACHE.DIR <- tempdir()
sys.source("commoncode/cache_manager.R", envir = test.environment)

assert.error <- function(expression, pattern = NULL) {
    error <- tryCatch({
        force(expression)
        NULL
    }, error = identity)
    stopifnot(inherits(error, "error"))
    if (!is.null(pattern)) stopifnot(grepl(pattern, conditionMessage(error)))
    invisible(error)
}

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
