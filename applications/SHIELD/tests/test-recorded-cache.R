# Run from the jheem_analyses repository root:
# Rscript applications/SHIELD/tests/test-recorded-cache.R
test.root <- tempfile("shield-recorded-cache-")
dir.create(test.root)
dir.create(file.path(test.root, "cache"))
old.env <- Sys.getenv(c("SHIELD_RECORDED_RUN", "JHEEM_CACHE_DIR",
                        "JHEEM_ANALYSES_PATH"), unset = NA_character_)
on.exit({
    for (name in names(old.env)) {
        if (is.na(old.env[[name]])) Sys.unsetenv(name)
        else do.call(Sys.setenv, setNames(list(old.env[[name]]), name))
    }
    unlink(test.root, recursive = TRUE)
}, add = TRUE)
Sys.setenv(SHIELD_RECORDED_RUN = "true",
           JHEEM_CACHE_DIR = file.path(test.root, "cache"),
           JHEEM_ANALYSES_PATH = normalizePath("."))
source("commoncode/cache_manager.R")

census.source <- get.github.release.source("census.manager.rdata")
stopifnot(identical(census.source$asset, "census.manager.rdata"))
Sys.setenv(SHIELD_RECORDED_RUN = "false")
stopifnot(is.null(get.github.release.source("census.manager.rdata")))
Sys.setenv(SHIELD_RECORDED_RUN = "true")

resolution <- list(
    schema_version = 1L,
    manager = "census.manager.rdata",
    repository = "tfojo1/jheem_analyses",
    requested_tag = "data-managers-v2026.08.26",
    resolved_tag = "data-managers-v2026.08.26",
    asset = "census.manager.rdata",
    published_at = "2026-08-26T19:50:47Z"
)
paths <- data.manager.release.paths(resolution, "")
dir.create(paths$directory, recursive = TRUE)
writeLines("test artifact", paths$artifact)
resolution$sha256 <- sha256.file(paths$artifact)
write.release.resolution(resolution, paths$metadata)
stopifnot(identical(materialize.github.release.asset(resolution, TRUE, ""),
                    paths$artifact))
stopifnot(!file.exists(paths$lock))

writeLines("damaged", paths$artifact)
error <- tryCatch({
    materialize.github.release.asset(resolution, TRUE, "")
    NULL
}, error = identity)
stopifnot(inherits(error, "error"), grepl("failed digest verification",
                                          conditionMessage(error)),
          !file.exists(paths$lock))
cat("Recorded SHIELD cache tests passed\n")
