# Render the review summary for a census-manager build (CANDIDATE.md, also used as
# release notes) and a machine-readable build status. The JSON reports keep the evidence.
# Usage: Rscript summarize_candidate.R <output-directory> <baseline-release>
#
# Reads, from the output directory:
#   active_baseline_report.json      structural and value comparison with the baseline
#   consumer_baseline_report.json    the SHIELD check on the baseline census (a control)
#   consumer_candidate_report.json   the SHIELD check on this build
# Stops if the control failed: then the check itself is broken, and the candidate's
# result says nothing. Otherwise a build needs review when it removes structure or
# SHIELD doesn't start with it.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Usage: Rscript summarize_candidate.R <output-directory> <baseline-release>")
baseline.label <- paste0("`", args[[2]], "`")
read.report <- function(name) jsonlite::read_json(file.path(args[[1]], name))
active <- read.report("active_baseline_report.json")
control <- read.report("consumer_baseline_report.json")
consumer <- read.report("consumer_candidate_report.json")
if (!isTRUE(control$passed)) {
  stop("The SHIELD consumer check failed on the baseline census ", args[[2]],
       ", so it can't judge this build; see consumer_baseline_report.json")
}
removes.structure <- !isTRUE(active$passed)
breaks.consumer <- !isTRUE(consumer$passed)
needs.review <- removes.structure || breaks.consumer
metadata.matches <- unlist(active$reproduction$metadata_matches)
metadata.differences <- names(metadata.matches)[!metadata.matches]
MAX.ITEMS <- 50L

item.lines <- function(entries, heading) {
  if (!length(entries)) return(NULL)
  messages <- vapply(entries, function(entry) entry$message, character(1))
  shown <- utils::head(messages, MAX.ITEMS)
  c(paste0("**", heading, " (", length(messages), ")**"), "",
    paste0("- ", shown),
    if (length(messages) > MAX.ITEMS)
      sprintf("- … and %d more (see `active_baseline_report.json`)", length(messages) - MAX.ITEMS),
    "")
}

describe.values <- function(values) {
  if (!length(values)) return("")
  parts <- vapply(names(values), function(dimension) {
    v <- as.character(unlist(values[[dimension]]))
    if (identical(dimension, "year") && all(grepl("^[0-9]{4}$", v)) && length(v) > 2) {
      range <- range(as.integer(v))
      return(sprintf("year %d–%d (%d)", range[[1]], range[[2]], length(v)))
    }
    shown <- if (length(v) > 4) paste0(paste(v[1:3], collapse = ", "), ", … (", length(v), ")")
             else paste(v, collapse = ", ")
    paste(dimension, shown)
  }, character(1))
  paste(parts, collapse = "; ")
}

value.lines <- function(delta) {
  changes <- delta$changes
  if (!length(changes)) return(c("No values differ from the baseline.", ""))
  row <- function(change) {
    if (!isTRUE(change$comparable)) return(sprintf("| `%s` | — | %s | | |", change$path, change$reason))
    sprintf("| `%s` | %s | %s | %s | %s |", change$path,
            format(change$changed_overlap_cells, big.mark = ","),
            describe.values(change$changed_dimension_values),
            describe.values(change$added_dimension_values),
            describe.values(change$removed_dimension_values))
  }
  shown <- utils::head(changes, MAX.ITEMS)
  c("| Array | Changed cells | Where values changed | Added | Removed |",
    "|---|---:|---|---|---|",
    vapply(shown, row, character(1)),
    if (length(changes) > MAX.ITEMS)
      sprintf("\n… and %d more arrays (see `active_baseline_report.json`).", length(changes) - MAX.ITEMS),
    "")
}

consumer.lines <- function(report) {
  vapply(report$checks, function(check) {
    # One line per check; the JSON report keeps the full message.
    message <- if (is.null(check$message)) NULL else gsub("\\s+", " ", gsub("`", "'", check$message))
    if (!is.null(message) && nchar(message) > 400) message <- paste0(substr(message, 1, 400), " …")
    sprintf("- %s %s%s", if (isTRUE(check$passed)) "Passed:" else "**Failed:**", check$name,
            if (is.null(message)) "" else paste0(": `", message, "`"))
  }, character(1))
}

quality.file <- file.path(args[[1]], "quality_report.txt")
quality.lines <- if (file.exists(quality.file)) {
  text <- readLines(quality.file)
  if (sum(nchar(text)) > 60000L) {
    kept <- cumsum(nchar(text) + 1L) <= 60000L
    text <- c(text[kept], "… (truncated; the full report is in validation-evidence.tar.gz)")
  }
  c("<details>", "<summary>Data quality report (click to expand)</summary>", "",
    "Informational only. Accepted issues can be suppressed in",
    "`data_processing/census.manager/known_issues.json`.", "",
    "```", text, "```", "", "</details>", "")
} else {
  c("The data quality report was not produced for this build.", "")
}

text <- c(
  "# Census-manager candidate", "",
  if (removes.structure) c(
    sprintf("**Needs review:** %d structural check(s) found something present in %s but missing here.",
            active$n_failed, baseline.label), "") else NULL,
  if (breaks.consumer) c(
    sprintf("**Needs review:** the SHIELD model does not start with this census (%s); it does with %s.",
            consumer$location, baseline.label), "") else NULL,
  if (needs.review) c("Promoting this build requires confirming these changes are intended.", "") else NULL,
  "Build candidate only: no promoted census or latest alias has changed.",
  paste0("Compared against ", baseline.label, "."), "",
  "## Summary", "",
  sprintf("- Structural checks: %d passed, %d failed.", active$n_checks - active$n_failed, active$n_failed),
  sprintf("- Additions or range changes: %d.", length(active$warnings) + length(active$notices)),
  sprintf("- Data identical to the baseline: **%s**.",
          if (isTRUE(active$reproduction$data_identical)) "yes" else "no"),
  sprintf("- Value differences: %d of %d shared arrays; %s changed cells at shared coordinates.",
          active$value_delta$arrays_with_differences, active$value_delta$arrays_compared,
          format(active$value_delta$changed_overlap_cells, big.mark = ",")),
  sprintf("- Metadata fields that differ: %s.",
          if (length(metadata.differences)) paste0("`", metadata.differences, "`", collapse = ", ") else "none"),
  sprintf("- SHIELD consumer check (%s, with `%s`): %s.", consumer$location, consumer$syphilis$tag,
          if (breaks.consumer) "**failed**" else "passed"),
  "", "## SHIELD consumer check", "",
  "The SHIELD model is started in recorded mode with this census and the syphilis release",
  "SHIELD declares: its specification is compiled and one location's engine is built.",
  "The same check on the baseline census is the control.", "",
  "This build:", "", consumer.lines(consumer), "",
  paste0("Baseline ", baseline.label, ":"), "", consumer.lines(control), "",
  "## Structural changes", "",
  if (!length(active$failures) && !length(active$warnings) && !length(active$notices))
    c("No structural changes from the baseline.", "") else NULL,
  item.lines(active$failures, "Removed"),
  item.lines(active$warnings, "Added"),
  item.lines(active$notices, "Other changes"),
  "## Value changes", "",
  "Arrays whose values, or dimension values, differ from the baseline.",
  "Counts overlap across stratifications; changed cells are not unique people or records.", "",
  value.lines(active$value_delta),
  "## Data quality", "",
  quality.lines,
  "## Scope", "",
  "This run tests section-to-final assembly, not rebuilding sections from raw files.",
  "An upstream processing edit is tested only after its section inputs are rebuilt.",
  "The SHIELD check shows the model starts; it does not establish scientific correctness",
  "or calibration compatibility, and other applications that use census are not checked here.",
  "", "## Evidence", "",
  paste0("Candidate SHA-256: `", active$reproduction$candidate_sha256, "`"),
  paste0("Baseline SHA-256: `", active$reproduction$baseline_sha256, "`"),
  "", "See `input_identity.txt`, `session_info.txt`, `build_log.txt`, `active_baseline_report.json`,",
  "`consumer_candidate_report.json`, `consumer_baseline_report.json`, and `quality_report.txt`.")
writeLines(text, file.path(args[[1]], "CANDIDATE.md"))
jsonlite::write_json(list(
  schema_version = 1L,
  needs_review = needs.review,
  structural_failures = active$n_failed,
  consumer_check_passed = !breaks.consumer,
  additions = length(active$warnings) + length(active$notices),
  arrays_with_value_differences = active$value_delta$arrays_with_differences,
  data_identical_to_baseline = isTRUE(active$reproduction$data_identical),
  metadata_differences = I(metadata.differences),
  baseline_release = args[[2]],
  candidate_sha256 = active$reproduction$candidate_sha256
), file.path(args[[1]], "build_status.json"), pretty = TRUE, auto_unbox = TRUE, null = "null")
cat("Summary written; needs review:", needs.review, "\n")
