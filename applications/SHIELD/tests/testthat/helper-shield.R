## Loaded automatically by testthat before any test file.
## Brings up the SHIELD environment once per run and defines the skip helpers
## that let each tier degrade gracefully when its inputs are missing.

if (!exists("SHIELD.TEST.ENV")) {
    ## testthat runs with the test directory as wd; the bootstrap relocates to
    ## the repo root and leaves it there.
    source(file.path(dirname(dirname(normalizePath("."))), "tests",
                     "shield_test_bootstrap.R"))
}

SHIELD.DIR  <- SHIELD.TEST.ENV$shield.dir
REPO.ROOT   <- SHIELD.TEST.ENV$repo.root

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
