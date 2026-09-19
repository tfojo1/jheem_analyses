#!/usr/bin/env Rscript
## =============================================================================
## SHIELD test runner
## -----------------------------------------------------------------------------
## Usage, from the repo root:
##
##   Rscript applications/SHIELD/tests/run_tests.R              # everything
##   Rscript applications/SHIELD/tests/run_tests.R static       # static tier only
##   Rscript applications/SHIELD/tests/run_tests.R unit         # static + unit
##   Rscript applications/SHIELD/tests/run_tests.R integration  # everything
##   Rscript applications/SHIELD/tests/run_tests.R --filter=spline
##
## Tiers, cheapest first:
##
##   static       parses every file, checks cross-file wiring and source hygiene.
##                Needs nothing but R. Runs in about a second.
##   unit         numerical contracts of the self-contained functions (spline
##                priors, doxy efficacy, pairing maths, base parameters).
##                Needs jheem2 and the `distributions` package. A few seconds.
##   integration  builds the specification, runs the engine, scores the
##                likelihoods, runs an intervention. Needs the cached data
##                managers. Two to three minutes.
##
## Environment variables:
##
##   JHEEM_SYPHILIS_MANAGER_TAG   manager release tag to test against;
##                                "latest" follows the promoted manager.
##   SHIELD_TEST_SKIP_SLOW=true   skip the engine and intervention runs.
##
## Exit status is 0 when nothing failed, 1 otherwise. Skips are not failures:
## a tier whose inputs are missing reports why and is not counted against you.
## =============================================================================

suppressMessages(library(testthat))

## Show every finding. The default caps the report at 10 failures, which on a
## suite written to document existing problems hides most of them.
options(testthat.progress.max_fails = 500)

args <- commandArgs(trailingOnly = TRUE)
tier <- "all"
filter <- NULL

for (a in args) {
    if (grepl("^--filter=", a)) {
        filter <- sub("^--filter=", "", a)
    } else if (a %in% c("static", "unit", "integration", "all")) {
        tier <- a
    } else if (!grepl("^--", a)) {
        filter <- a
    }
}

## Locate the suite regardless of where the runner was invoked from.
this.file <- local({
    cmd <- commandArgs(trailingOnly = FALSE)
    path <- sub("^--file=", "", cmd[grepl("^--file=", cmd)])
    if (length(path)) normalizePath(path) else NULL
})
tests.dir <- if (!is.null(this.file)) dirname(this.file) else "applications/SHIELD/tests"
testthat.dir <- file.path(tests.dir, "testthat")

if (is.null(filter)) {
    filter <- switch(tier,
                     static      = "^static",
                     unit        = "^(static|unit)",
                     integration = NULL,
                     all         = NULL)
}

cat("SHIELD test suite\n")
cat("  tier:   ", tier, "\n", sep = "")
cat("  filter: ", if (is.null(filter)) "(none)" else filter, "\n", sep = "")
cat("  dir:    ", testthat.dir, "\n\n", sep = "")

started <- Sys.time()
## SummaryReporter caps its report at 10 findings by default; on a suite
## written to document existing problems that hides most of them.
results <- test_dir(testthat.dir,
                    filter = filter,
                    reporter = SummaryReporter$new(max_reports = 500L),
                    stop_on_failure = FALSE)
elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))

df <- as.data.frame(results)
n.failed  <- sum(df$failed)
n.error   <- sum(df$error)
n.skipped <- sum(df$skipped)
n.passed  <- sum(df$passed)

cat("\n", strrep("=", 70), "\n", sep = "")
cat(sprintf("passed %d | failed %d | errors %d | skipped %d | %.0fs\n",
            n.passed, n.failed, n.error, n.skipped, elapsed))

## Report what the bootstrap could not provide, so a wall of skips is
## self-explanatory rather than mysterious.
if (exists("SHIELD.TEST.ENV") && n.skipped > 0) {
    summary.df <- shield.test.tier.summary()
    unavailable <- summary.df$stage[!summary.df$available]
    if (length(unavailable)) {
        cat("\nUnavailable bootstrap stages (these cause the skips):\n")
        for (s in unavailable) {
            cat("  ", s, ": ", SHIELD.TEST.ENV[[paste0(s, ".message")]], "\n", sep = "")
        }
    }
    if (!is.null(SHIELD.TEST.ENV$jheem2.clone.error)) {
        cat("\njheem2: using the ", SHIELD.TEST.ENV$jheem2.source,
            " because the clone did not source (",
            SHIELD.TEST.ENV$jheem2.clone.error, ").\n", sep = "")
    }
}
cat(strrep("=", 70), "\n", sep = "")

quit(status = if (n.failed + n.error > 0) 1L else 0L)
