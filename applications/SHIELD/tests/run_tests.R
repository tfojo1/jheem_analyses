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
## Known findings: the suite documents existing problems, so some tests fail
## on master. known_failures.txt lists each of their findings (failures and
## errors) on one line, from every CI tier. A run fails only when it finds
## something not listed. A listed finding that does not appear is reported, not
## failed: some depend on what the run could load (the unit tier errors without
## the data managers, for example). When a fix removes one, delete its line.
##
##   --update-known   add this run's new findings to known_failures.txt
##   --strict         ignore the list: any failure or error fails the run
##
## Exit status is 0 when every failure and error is listed, 1 otherwise. Skips
## are not failures: a tier whose inputs are missing reports why and is not
## counted against you.
## =============================================================================

suppressMessages(library(testthat))

## Show every finding. The default caps the report at 10 failures, which on a
## suite written to document existing problems hides most of them.
options(testthat.progress.max_fails = 500)

args <- commandArgs(trailingOnly = TRUE)
tier <- "all"
filter <- NULL
update.known <- "--update-known" %in% args
strict <- "--strict" %in% args

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

## ---- Known findings ----
## One line per finding: "test file | test | detail", for failures and errors.
## testthat's comparison lines differ between versions, so only the test's own
## info text is kept, without the checkout's path.
finding.lines <- function(results, repo.root) {
    roots <- unique(c(repo.root, normalizePath(repo.root, mustWork = FALSE)))
    out <- character()
    for (r in results) {
        for (e in r$results) {
            test <- paste(basename(r$file), r$test, sep = " | ")
            if (inherits(e, "expectation_error")) {
                ## The error itself, without testthat's "Error in `call`:" prefix.
                message <- gsub("\\s+", " ", conditionMessage(e))
                message <- sub("^Error( in `.*?`)?:\\s*", "", message, perl = TRUE)
                out <- c(out, paste0(test, " | error: ", message))
                next
            }
            if (!inherits(e, "expectation_failure")) next
            lines <- strsplit(conditionMessage(e), "\n", fixed = TRUE)[[1]]
            for (root in roots) lines <- gsub(paste0(root, "/"), "", lines, fixed = TRUE)
            lines <- trimws(lines)
            ## "file.R: detail" keeps the file: R shortens long paths in parse errors.
            lines <- sub("^(\\S[^:]*\\.[Rr]): .*$", "\\1", lines)
            lines <- lines[nzchar(lines) & !grepl("`", lines, fixed = TRUE) &
                           !grepl("^(Expected |Differences?:|Actual( [a-z]+)*:)", lines) &
                           !grepl(" is not (TRUE|FALSE)$", lines)]
            out <- c(out, if (length(lines)) paste(test, lines, sep = " | ") else test)
        }
    }
    unique(out)
}

known.file <- file.path(tests.dir, "known_failures.txt")
read.known <- function() {
    if (!file.exists(known.file)) return(character())
    lines <- trimws(readLines(known.file, warn = FALSE))
    lines[nzchar(lines) & !startsWith(lines, "#")]
}
test.of <- function(lines) sub("^([^|]*\\|[^|]*) \\|.*$", "\\1", paste0(lines, " |"))

repo.root <- if (exists("SHIELD.TEST.ENV") && !is.null(SHIELD.TEST.ENV$repo.root))
    SHIELD.TEST.ENV$repo.root else normalizePath(file.path(tests.dir, "..", "..", ".."))
found <- finding.lines(results, repo.root)
known <- read.known()
## A listed finding counts as gone only if its test ran and was not skipped.
ran <- paste(df$file, df$test, sep = " | ")[!df$skipped]
new <- setdiff(found, known)
gone <- setdiff(known[test.of(known) %in% ran], found)

if (update.known) {
    writeLines(c(
        "# Findings the SHIELD suite reports on master, one per line: test file | test | detail.",
        "# run_tests.R fails on a finding not listed here. Add new ones with --update-known;",
        "# delete a line by hand when a fix removes the finding.",
        sort(unique(c(known, found)))), known.file)
    cat("Added ", length(new), " findings to ", known.file, "\n", sep = "")
    new <- character()
}

if (!strict) {
    cat(sprintf("\nKnown findings: %d of %d listed seen in tests that ran\n",
                length(intersect(found, known)), length(known[test.of(known) %in% ran])))
    if (length(new)) {
        cat("\nNEW findings (not in known_failures.txt):\n")
        cat(paste0("  ", new, "\n"), sep = "")
    }
    if (length(gone)) {
        cat("\nListed findings not seen in tests that ran (fixed, changed, or not reachable",
            "here); if a fix removed one, delete its line:\n")
        cat(paste0("  ", gone, "\n"), sep = "")
    }
}

failed <- if (strict) n.failed + n.error > 0 else length(new) > 0
quit(status = if (failed) 1L else 0L)
