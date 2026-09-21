## Loaded automatically by testthat before any test file.
## Brings up the SHIELD environment once per run and defines the skip helpers
## that let each tier degrade gracefully when its inputs are missing.

if (!exists("SHIELD.TEST.ENV")) {
    ## testthat may start in the test directory, in tests/, or at the repo root.
    ## Walk up until the bootstrap turns up. The bootstrap then relocates the
    ## working directory to the repo root and leaves it there.
    local({
        d <- normalizePath(".", mustWork = TRUE)
        repeat {
            candidate <- file.path(d, "applications", "SHIELD", "tests",
                                   "shield_test_bootstrap.R")
            if (file.exists(candidate)) {
                source(candidate, local = FALSE)
                return(invisible(NULL))
            }
            candidate <- file.path(d, "shield_test_bootstrap.R")
            if (file.exists(candidate)) {
                source(candidate, local = FALSE)
                return(invisible(NULL))
            }
            parent <- dirname(d)
            if (identical(parent, d)) stop("Cannot locate shield_test_bootstrap.R")
            d <- parent
        }
    })
}

SHIELD.DIR  <- SHIELD.TEST.ENV$shield.dir
REPO.ROOT   <- SHIELD.TEST.ENV$repo.root

## --- working directory --------------------------------------------------------

## The model only runs from the repo root. commoncode/cache_object_for_version_functions.R
## resolves its cache with the literal path "../jheem_analyses/commoncode/..." ,
## and 40-odd SHIELD files use the same "../jheem_analyses/" prefix, so anything
## that touches the specification must run with the repo root as the working
## directory.
##
## testthat resets the working directory to the test directory before EVERY test
## file, so setting it once in this helper is not enough. Call use_repo_root()
## as the first line of any test that builds a specification, an engine, or a
## likelihood; it is scoped to that test and restored afterwards.
use_repo_root <- function(env = parent.frame()) {
    withr::local_dir(REPO.ROOT, .local_envir = env)
}

## --- skip helpers -------------------------------------------------------------

skip_unless_stage <- function(...) {
    for (stage in c(...)) {
        if (!isTRUE(SHIELD.TEST.ENV[[stage]])) {
            msg <- SHIELD.TEST.ENV[[paste0(stage, ".message")]]
            testthat::skip(paste0("bootstrap stage '", stage, "' unavailable",
                                  if (!is.null(msg) && !is.na(msg)) paste0(": ", msg) else ""))
        }
    }
}

skip_unless_slow <- function() {
    if (identical(tolower(Sys.getenv("SHIELD_TEST_SKIP_SLOW")), "true")) {
        testthat::skip("SHIELD_TEST_SKIP_SLOW=true")
    }
}

## --- file inventory -----------------------------------------------------------

## Directories that hold scratch work rather than model code. They are still
## parsed (a file that cannot be parsed is worth knowing about) but they are
## exempt from the stricter conventions.
SHIELD.SCRATCH.PATTERNS <- c(
    "/untitled folder/",
    "/backups/",
    " copy\\.R$",
    "/CROI/",
    "/Documentation/",
    "/talks/"
)

shield_r_files <- function(include.scratch = TRUE) {
    files <- list.files(SHIELD.DIR, pattern = "\\.[Rr]$", recursive = TRUE,
                        full.names = TRUE)
    ## never lint the test suite itself
    files <- files[!grepl("/tests/", files, fixed = TRUE)]
    if (!include.scratch) {
        for (p in SHIELD.SCRATCH.PATTERNS) files <- files[!grepl(p, files)]
    }
    sort(files)
}

shield_rel <- function(paths) {
    sub(paste0("^", REPO.ROOT, "/"), "", paths)
}

## --- fixtures -----------------------------------------------------------------

## A small, fixed single-year age distribution for the pairing tests, so those
## tests do not depend on the census manager.
fixture_single_year_age_counts <- function(ages = 0:85, per.year = 1000) {
    counts <- rep(per.year, length(ages))
    names(counts) <- as.character(ages)
    counts
}

## The location the integration tier exercises. Baltimore is small enough to be
## quick and is the location shield_engine_test.R has always used.
SHIELD.TEST.LOCATION <- "C.12580"
SHIELD.TEST.VERSION  <- "shield"

## Engine + median-parameter simulation, built once and memoised across files.
shield_test_sim <- function() {
    use_repo_root()
    if (is.null(SHIELD.TEST.ENV$cached.sim)) {
        spec <- shield.test.specification()
        if (is.null(spec)) return(NULL)
        SHIELD.TEST.ENV$cached.sim <- tryCatch({
            engine <- create.jheem.engine(SHIELD.TEST.VERSION, SHIELD.TEST.LOCATION,
                                          end.year = 2030)
            params <- get.medians(SHIELD.FULL.PARAMETERS.PRIOR)
            list(engine = engine, params = params, sim = engine$run(params))
        }, error = function(e) {
            SHIELD.TEST.ENV$cached.sim.error <- conditionMessage(e)
            NA
        })
    }
    if (identical(SHIELD.TEST.ENV$cached.sim, NA)) NULL else SHIELD.TEST.ENV$cached.sim
}

## shield_likelihoods.R needs the specification, the surveillance manager, and a
## jheem2 that accepts every argument it passes. Sourcing it is memoised by the
## bootstrap; this reports why it is unavailable rather than erroring.
skip_unless_likelihoods <- function() {
    skip_unless_stage("has.shield.helpers")
    use_repo_root()
    if (!shield.test.likelihoods()) {
        testthat::skip(paste0(
            "shield_likelihoods.R could not be sourced: ",
            SHIELD.TEST.ENV$has.likelihoods.message,
            " (jheem2 in use: ", SHIELD.TEST.ENV$jheem2.source, " ",
            SHIELD.TEST.ENV$jheem2.version, ")"))
    }
    invisible(TRUE)
}

skip_unless_sim <- function() {
    skip_unless_stage("has.shield.helpers")
    skip_unless_slow()
    s <- shield_test_sim()
    if (is.null(s)) {
        testthat::skip(paste("could not build a simulation:",
                             SHIELD.TEST.ENV$cached.sim.error))
    }
    s
}
