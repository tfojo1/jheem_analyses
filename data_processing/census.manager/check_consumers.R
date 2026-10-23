# Consumer check for a census manager: does the SHIELD model still start with it?
#
# Usage (from the jheem_analyses repository root, which must be a directory named
# jheem_analyses):
#   Rscript data_processing/census.manager/check_consumers.R \
#     <census.rdata> <census-tag> <syphilis.rdata> <syphilis-tag> <report.json> [location]
#
# SHIELD runs in recorded mode, as container runs do: offline, with exactly these two
# managers placed in a temporary verified cache, so the result depends only on the
# inputs given. The check sources the SHIELD specification, which is where a census
# whose structure SHIELD can't use fails, and builds one location's engine (jheem2's
# exported create.jheem.engine, which first builds the kernel from the census data).
# It writes a report and exits 0 whether SHIELD passes or not; run it on the current
# census as well, so a failure of the check itself (a missing package, say) is not
# mistaken for a problem with the candidate. It exits non-zero only when it can't
# produce a report.
#
# JHEEM2_PATH names the jheem2 checkout (default ../jheem2); JHEEM2_MODE is package
# (default, uses the installed jheem2) or source. Revisions recorded by recorded mode
# come from JHEEM_ANALYSES_REF, JHEEM2_REF, LOCATIONS_REF, BAYESIAN_SIMULATIONS_REF and
# DISTRIBUTIONS_REF when set; nothing is run or saved, so placeholders are used otherwise.

args <- commandArgs(trailingOnly = TRUE)
if (!(length(args) %in% 5:6)) {
    stop("Usage: Rscript check_consumers.R <census.rdata> <census-tag> <syphilis.rdata> ",
         "<syphilis-tag> <report.json> [location]")
}
census.file <- normalizePath(args[[1]], mustWork = TRUE)
census.tag <- args[[2]]
syphilis.file <- normalizePath(args[[3]], mustWork = TRUE)
syphilis.tag <- args[[4]]
report.file <- args[[5]]
location <- if (length(args) == 6) args[[6]] else "C.12580"
analyses <- normalizePath(".", mustWork = TRUE)
if (!identical(basename(analyses), "jheem_analyses"))
    stop("Run from the jheem_analyses repository root (a directory named jheem_analyses)")

sha256 <- function(path) digest::digest(file = path, algo = "sha256")

# A temporary verified cache with the two releases, laid out as the release loader
# expects (cached/data-managers/<manager>/<tag>/<manager> plus resolution.json).
work <- tempfile("census-consumer-check-")
cache <- file.path(work, "cache")
root <- file.path(work, "root")
dir.create(root, recursive = TRUE)
place <- function(manager, tag, file) {
    directory <- file.path(cache, "data-managers", manager, tag)
    dir.create(directory, recursive = TRUE)
    if (!file.symlink(file, file.path(directory, manager)))
        stopifnot(file.copy(file, file.path(directory, manager)))
    digest <- sha256(file)
    jsonlite::write_json(list(schema_version = 1L, manager = manager,
                              repository = "tfojo1/jheem_analyses", requested_tag = tag,
                              resolved_tag = tag, asset = manager, sha256 = digest,
                              published_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
                         file.path(directory, "resolution.json"), auto_unbox = TRUE, pretty = TRUE)
    list(tag = tag, sha256 = digest)
}
census <- place("census.manager.rdata", census.tag, census.file)
syphilis <- place("syphilis.manager.rdata", syphilis.tag, syphilis.file)

placeholder <- strrep("0", 40)
revision <- function(name) {
    value <- Sys.getenv(name, unset = "")
    if (grepl("^[0-9a-fA-F]{40}$", value)) value else placeholder
}
Sys.setenv(
    SHIELD_RECORDED_RUN = "true",
    JHEEM_ANALYSES_PATH = analyses,
    JHEEM2_PATH = normalizePath(Sys.getenv("JHEEM2_PATH", file.path(dirname(analyses), "jheem2")),
                                mustWork = TRUE),
    JHEEM2_MODE = Sys.getenv("JHEEM2_MODE", "package"),
    JHEEM_CACHE_DIR = cache,
    JHEEM_ROOT_DIR = root,
    JHEEM_ANALYSES_REF = revision("JHEEM_ANALYSES_REF"),
    JHEEM2_REF = revision("JHEEM2_REF"),
    LOCATIONS_REF = revision("LOCATIONS_REF"),
    BAYESIAN_SIMULATIONS_REF = revision("BAYESIAN_SIMULATIONS_REF"),
    DISTRIBUTIONS_REF = revision("DISTRIBUTIONS_REF"),
    JHEEM_CENSUS_MANAGER_TAG = census.tag,
    JHEEM_SYPHILIS_MANAGER_TAG = syphilis.tag,
    SHIELD_RANDOM_SEED = "0"
)

run.check <- function(name, expression) {
    started <- Sys.time()
    message <- tryCatch({ force(expression); NULL }, error = function(e) conditionMessage(e))
    elapsed <- round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1)
    cat(sprintf("%s %s (%.1f s)%s\n", if (is.null(message)) "PASS" else "FAIL", name, elapsed,
                if (is.null(message)) "" else paste0(": ", message)))
    list(name = name, passed = is.null(message), seconds = elapsed,
         message = if (is.null(message)) NULL else message)
}

checks <- list()
setwd(analyses)
checks$specification <- run.check("SHIELD specification compiles",
                                  source("applications/SHIELD/shield_specification.R"))
loaded <- if (exists("CENSUS.MANAGER")) get.data.manager.resolution(CENSUS.MANAGER)$sha256 else NULL
checks$census_used <- run.check("SHIELD loaded the given census", {
    if (!identical(loaded, census$sha256))
        stop("SHIELD loaded census ", if (is.null(loaded)) "none" else loaded,
             ", not the given ", census$sha256)
})
if (checks$specification$passed) {
    checks$engine <- run.check(paste("SHIELD engine builds for", location),
                               create.jheem.engine(version = "shield", location = location,
                                                   end.year = 2030))
}

report <- list(consumer = "SHIELD", location = location, census = census, syphilis = syphilis,
               passed = all(vapply(checks, function(check) check$passed, logical(1))),
               checks = unname(checks))
jsonlite::write_json(report, report.file, auto_unbox = TRUE, pretty = TRUE, null = "null")
cat(if (report$passed) "SHIELD consumer check passed\n" else "SHIELD consumer check FAILED\n")
