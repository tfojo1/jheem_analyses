# Render a small review entry point; the JSON reports retain detailed evidence.
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) %in% 1:2) stop("Usage: Rscript summarize_candidate.R <output-directory> [baseline-release]")
baseline.label <- if (length(args) == 2L) paste0("`", args[[2]], "`") else "the active baseline"
read.report <- function(name) jsonlite::read_json(file.path(args[[1]], name))
active <- read.report("active_baseline_report.json")
historical <- read.report("historical_baseline_report.json")
consumer <- read.report("consumer_report.json")
stopifnot(isTRUE(active$passed), isTRUE(consumer$passed))
text <- c(
  "# HIV surveillance-manager candidate", "",
  "Build candidate only: no active manager or latest alias has changed.",
  paste0("Compared against ", baseline.label, "."), "",
  sprintf("- Active-baseline structural checks: %d passed, %d failed.",
          active$n_checks - active$n_failed, active$n_failed),
  sprintf("- Stored data and compared metadata reproduce the active baseline: **%s**.",
          if (isTRUE(active$reproduction$equivalent)) "yes" else "no — review required"),
  sprintf("- Active-baseline value differences: %d of %d shared arrays; %d changed cells at shared coordinates.",
          active$value_delta$arrays_with_differences, active$value_delta$arrays_compared,
          active$value_delta$changed_overlap_cells),
  sprintf("- Focused consumer checks: %d passed (Ryan White queries, EHE national prevalence, syphilis adult-population import).",
          length(consumer$checks)),
  sprintf("- Historical August 26 comparison: %d structurally failed checks, %d warnings; %d differing shared arrays; %d changed cells at shared coordinates.",
          historical$n_failed, length(historical$warnings),
          historical$value_delta$arrays_with_differences,
          historical$value_delta$changed_overlap_cells),
  "", "## Scope", "",
  "This run tests section-to-final assembly, not rebuilding sections from raw files.",
  "An upstream processing edit is tested only after its section inputs are rebuilt.",
  "These checks do not establish scientific correctness or full calibration compatibility.",
  "Any candidate-specific data or metadata differences need review before promotion.",
  "", "## Evidence", "",
  paste0("Candidate SHA-256: `", active$reproduction$candidate_sha256, "`"),
  paste0("Active-baseline SHA-256: `", active$reproduction$baseline_sha256, "`"),
  "", "See `input_identity.txt`, `session_info.txt`, `build_log.txt`,",
  "`active_baseline_report.json`, `historical_baseline_report.json`, and `consumer_report.json`.",
  "The historical report preserves changes already present in the active baseline.",
  "Array counts overlap across stratifications; changed cells are not unique people or records.")
writeLines(text, file.path(args[[1]], "CANDIDATE.md"))
