## Standard testthat entry point, so the suite can also be driven by tooling
## that expects one. For CI and day-to-day use prefer run_tests.R, which adds
## tier selection and explains any skips.
##
## Run from the repo root:
##   Rscript applications/SHIELD/tests/testthat.R

library(testthat)

test_dir("applications/SHIELD/tests/testthat",
         reporter = SummaryReporter$new(max_reports = 500L),
         stop_on_failure = TRUE)
