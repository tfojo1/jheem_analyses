# Provisional structural comparison for a section-to-final candidate.
# Usage: Rscript validate_candidate.R <baseline.rdata> <candidate.rdata> <report.json>
# This is not an active-model compatibility or data-value validation gate.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop("Usage: Rscript validate_candidate.R <baseline.rdata> <candidate.rdata> <report.json>")
}
if (!all(file.exists(args[1:2]))) stop("Baseline and candidate files must exist")

library(jheem2)
source("data_processing/validation/manager_spec_tools.R")
source("data_processing/hiv.surveillance.manager/manager_value_delta.R")

baseline <- load.data.manager(name = "surveillance.manager", file = args[[1]])
candidate <- load.data.manager(name = "surveillance.manager", file = args[[2]])
spec <- extract_manager_spec(baseline)
result <- validate_manager_structure(candidate, spec)
result$value_delta <- compare.manager.data.values(baseline$data, candidate$data)

jsonlite::write_json(result, args[[3]], pretty = TRUE, auto_unbox = TRUE,
                     null = "null")
cat(sprintf("Structural comparison: %d checks, %d failures, %d warnings, %d notices\n",
            result$n_checks, result$n_failed, length(result$warnings),
            length(result$notices)))
cat(sprintf("Value-delta diagnostic: %d shared arrays, %d differing arrays, %d changed cells at shared coordinates\n",
            result$value_delta$arrays_compared,
            result$value_delta$arrays_with_differences,
            result$value_delta$changed_overlap_cells))
if (length(result$failures)) {
  for (failure in result$failures) cat("FAIL: ", failure$message, "\n", sep = "")
}
if (length(result$warnings) || length(result$notices)) {
  cat("Additions and range changes require review; no baseline is updated.\n")
}
if (!result$passed) quit(save = "no", status = 1L)
