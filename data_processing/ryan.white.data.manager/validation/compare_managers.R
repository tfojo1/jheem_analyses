#!/usr/bin/env Rscript

# Compare a baseline Ryan White web/display manager with a candidate full
# Ryan White manager and emit deterministic JSON and Markdown reports.

suppressPackageStartupMessages(library(jheem2))
source("data_processing/ryan.white.data.manager/validation/manager_compatibility_tools.R")

parse_arguments <- function(arguments) {
  if (length(arguments) %% 2 != 0) {
    stop("Arguments must be supplied as --name value pairs")
  }
  result <- list()
  for (index in seq(1, length(arguments), by = 2)) {
    key <- sub("^--", "", arguments[[index]])
    result[[key]] <- arguments[[index + 1]]
  }
  result
}

required <- c(
  "baseline", "baseline-sha256", "candidate", "candidate-sha256",
  "output-json", "output-markdown"
)
arguments <- parse_arguments(commandArgs(trailingOnly = TRUE))
missing <- required[!required %in% names(arguments)]
if (length(missing)) {
  stop(sprintf(
    paste0(
      "Missing required arguments: %s\n",
      "Usage: Rscript data_processing/ryan.white.data.manager/validation/compare_managers.R ",
      "--baseline FILE --baseline-sha256 SHA256 --candidate FILE ",
      "--candidate-sha256 SHA256 --output-json FILE --output-markdown FILE"
    ),
    paste(missing, collapse = ", ")
  ))
}

baseline <- load_manager_artifact(
  arguments$baseline, arguments[["baseline-sha256"]], "historical_display"
)
candidate <- load_manager_artifact(
  arguments$candidate, arguments[["candidate-sha256"]], "current_full_candidate"
)

comparison <- compare_data_managers(
  baseline$manager, candidate$manager, tolerance = 0
)
required.outcomes <- list(
  "non.adap.clients" = "identical",
  "oahs.clients" = "identical",
  "oahs.suppression" = "identical",
  "adap.proportion" = "identical",
  "adap.suppression" = c("identical", "additive"),
  "diagnosed.prevalence" = c(
    "identical", "additive", "compatible_on_overlap"
  )
)

report <- list(
  schema_version = "jheem-manager-compatibility/v1",
  comparison_id = "ryan-white-web-2025-04-08_to_full-2026-03-16",
  generator = "data_processing/ryan.white.data.manager/validation/compare_managers.R",
  baseline = baseline$metadata,
  candidate = candidate$metadata,
  comparison = comparison,
  required_outcome_checks = validate_required_outcome_statuses(
    comparison, required.outcomes
  ),
  candidate_derived_target_checks = validate_ryan_white_derived_targets(
    candidate$manager, tolerance = 1e-12
  ),
  scope_statement = paste(
    "This is a selective target-compatibility report, not a whole-manager",
    "compatibility or interchangeability claim."
  ),
  provenance_statement = paste(
    "Compatibility on shared target cells does not establish that the",
    "candidate manager was used to fit the historical posterior."
  ),
  candidate_only_statement = paste(
    "Candidate-only outcomes, including adap.clients, have no historical",
    "numeric compatibility evidence in the baseline display manager."
  ),
  derived_check_statement = paste(
    "Derived-target checks establish formula consistency within the candidate",
    "manager; they do not establish historical identity of the inputs or",
    "derived values."
  )
)

dir.create(dirname(arguments[["output-json"]]), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(arguments[["output-markdown"]]), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  report, arguments[["output-json"]], pretty = TRUE, auto_unbox = TRUE,
  null = "null", digits = NA
)
write_manager_compatibility_markdown(report, arguments[["output-markdown"]])

failed.derived <- vapply(
  report$candidate_derived_target_checks,
  function(check) check$status != "passed",
  logical(1)
)
failed.required <- vapply(
  report$required_outcome_checks,
  function(check) check$status != "passed",
  logical(1)
)
if (any(failed.required) || any(failed.derived)) {
  stop("One or more required compatibility checks failed")
}

cat(sprintf("JSON report: %s\n", arguments[["output-json"]]))
cat(sprintf("Markdown report: %s\n", arguments[["output-markdown"]]))
