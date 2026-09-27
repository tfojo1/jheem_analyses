# Render a small review entry point and a machine-readable build status.
# The JSON reports retain detailed evidence.
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) %in% 1:2) stop("Usage: Rscript summarize_candidate.R <output-directory> [baseline-release]")
baseline.label <- if (length(args) == 2L) paste0("`", args[[2]], "`") else "the promoted manager"
read.report <- function(name) jsonlite::read_json(file.path(args[[1]], name))
active <- read.report("active_baseline_report.json")
consumer <- read.report("consumer_report.json")
stopifnot(isTRUE(consumer$passed))
needs.review <- !isTRUE(active$passed)
metadata.matches <- unlist(active$reproduction$metadata_matches)
metadata.differences <- names(metadata.matches)[!metadata.matches]
failure.lines <- vapply(active$failures, function(f) paste0("  - ", f$message), character(1))
text <- c(
  "# HIV surveillance-manager candidate", "",
  if (needs.review) c(
    sprintf("**Needs review:** %d structural check(s) found something present in %s but missing here.",
            active$n_failed, baseline.label),
    "Promoting this build requires confirming these removals are intended.", "",
    failure.lines, "") else NULL,
  "Build candidate only: no active manager or latest alias has changed.",
  paste0("Compared against ", baseline.label, "."), "",
  sprintf("- Structural checks: %d passed, %d failed.",
          active$n_checks - active$n_failed, active$n_failed),
  sprintf("- Additions or range changes: %d.", length(active$warnings) + length(active$notices)),
  sprintf("- Data identical to the promoted manager: **%s**.",
          if (isTRUE(active$reproduction$data_identical)) "yes" else "no"),
  sprintf("- Metadata fields that differ: %s.",
          if (length(metadata.differences)) paste0("`", metadata.differences, "`", collapse = ", ") else "none"),
  sprintf("- Value differences: %d of %d shared arrays; %d changed cells at shared coordinates.",
          active$value_delta$arrays_with_differences, active$value_delta$arrays_compared,
          active$value_delta$changed_overlap_cells),
  sprintf("- Focused consumer checks: %d passed (Ryan White queries, EHE national prevalence, syphilis adult-population import).",
          length(consumer$checks)),
  "", "## Scope", "",
  "This run tests section-to-final assembly, not rebuilding sections from raw files.",
  "An upstream processing edit is tested only after its section inputs are rebuilt.",
  "These checks do not establish scientific correctness or full calibration compatibility.",
  "", "## Evidence", "",
  paste0("Candidate SHA-256: `", active$reproduction$candidate_sha256, "`"),
  paste0("Promoted-manager SHA-256: `", active$reproduction$baseline_sha256, "`"),
  "", "See `input_identity.txt`, `session_info.txt`, `build_log.txt`,",
  "`active_baseline_report.json`, and `consumer_report.json`.",
  "Array counts overlap across stratifications; changed cells are not unique people or records.")
writeLines(text, file.path(args[[1]], "CANDIDATE.md"))
jsonlite::write_json(list(
  schema_version = 1L,
  needs_review = needs.review,
  structural_failures = active$n_failed,
  additions = length(active$warnings) + length(active$notices),
  arrays_with_value_differences = active$value_delta$arrays_with_differences,
  data_identical_to_baseline = isTRUE(active$reproduction$data_identical),
  metadata_differences = I(metadata.differences),
  baseline_release = if (length(args) == 2L) args[[2]] else NULL,
  candidate_sha256 = active$reproduction$candidate_sha256
), file.path(args[[1]], "build_status.json"), pretty = TRUE, auto_unbox = TRUE, null = "null")
