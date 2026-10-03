#!/usr/bin/env Rscript
# Fast checks of reporting/selection logic; no model, manager, or NAS required.
source("applications/SHIELD/tests/check-stage1-compatibility.R")

expect.error <- function(expr, pattern) {
    error <- tryCatch({ force(expr); NULL }, error = identity)
    stopifnot(inherits(error, "error"), grepl(pattern, conditionMessage(error), fixed = TRUE))
}

parameters <- c(global.transmission.rate.msm = 1.6, global.transmission.rate.het = 1.6,
                other = 0.5)
cases <- shield.stage1.parameter.cases(parameters)
stopifnot(length(cases) == 3L, identical(cases$prior_medians, parameters),
          cases$transmission_minus_10pct[[1L]] == 1.6 * 0.9,
          cases$transmission_plus_10pct[[2L]] == 1.6 * 1.1,
          all(vapply(cases, function(x) x[["other"]] == 0.5, logical(1))))
expect.error(shield.stage1.parameter.cases(unname(parameters)), "uniquely named")
expect.error(shield.stage1.parameter.cases(c(parameters, bad = NA_real_)), "finite")
for (x in list(NULL, numeric(), "1", c(1, NA), c(1, Inf), c(1, -Inf), NaN)) {
    expect.error(shield.stage1.require.finite(x, "test"), "finite")
}

info <- list(likelihood.instructions = "ordinary", special.case.likelihood.instructions =
                 list(C.12580 = "special"))
stopifnot(identical(shield.stage1.instructions(info, "C.12580"), "special"),
          identical(shield.stage1.instructions(info, "C.12060"), "ordinary"))

fake.likelihood <- function(pieces = c(first = -10, second = -5),
                            total = sum(pieces), checked = total) {
    list(compute.piecewise = function(...) pieces,
         compute = function(sim, log, use.optimized.get, check.consistency) {
             if (use.optimized.get) total else checked
         })
}
result <- shield.stage1.score(fake.likelihood(), NULL)
stopifnot(identical(result$total, -15), length(result$components) == 2L,
          result$components[[2L]]$name == "second")
expect.error(shield.stage1.score(fake.likelihood(c(first = -10, second = -Inf)), NULL), "finite")
expect.error(shield.stage1.score(fake.likelihood(total = NaN), NULL), "finite")
expect.error(shield.stage1.score(fake.likelihood(total = -14), NULL), "disagree")
expect.error(shield.stage1.score(fake.likelihood(checked = -14), NULL), "disagree")
expect.error(shield.stage1.score(fake.likelihood(pieces = c(-10, -5)), NULL), "must have names")
expect.error(shield.stage1.score(fake.likelihood(total = c(-15, -15)), NULL), "one total")

# A failed prerequisite must persist a failed report and return an error, not
# skip to a green result. Existing reports must never be overwritten.
temporary <- tempfile("stage1-compatibility-tests-")
dir.create(temporary)
old.flag <- Sys.getenv("SHIELD_RECORDED_RUN", unset = NA_character_)
Sys.setenv(SHIELD_RECORDED_RUN = "false")
report.dir <- file.path(temporary, "failure")
expect.error(shield.stage1.main(report.dir), "requires SHIELD_RECORDED_RUN=true")
report <- jsonlite::fromJSON(file.path(report.dir, "report.json"))
stopifnot(identical(report$status, "failed"), identical(report$stage, "preflight"))
expect.error(shield.stage1.main(report.dir), "already exists")
if (is.na(old.flag)) Sys.unsetenv("SHIELD_RECORDED_RUN") else Sys.setenv(SHIELD_RECORDED_RUN = old.flag)
unlink(temporary, recursive = TRUE)
cat("Stage-1 compatibility helper tests passed.\n")
