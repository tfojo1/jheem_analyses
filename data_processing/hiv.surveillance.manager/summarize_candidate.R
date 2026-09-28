# Render the review summary (CANDIDATE.md, also used as release notes) and a
# machine-readable build status. The JSON reports retain the full evidence.
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
MAX.ITEMS <- 50L

# Itemize messages, keeping the notes readable when a build changes a lot.
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

# Summarize dimension values compactly, e.g. "year 2010–2019 (10); location TGA.OAKLAND".
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
  if (!length(changes)) return(c("No values differ from the promoted manager.", ""))
  row <- function(change) {
    if (!isTRUE(change$comparable)) {
      return(sprintf("| `%s` | — | %s | | |", change$path, change$reason))
    }
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

quality.file <- file.path(args[[1]], "quality_report.txt")
quality.lines <- if (file.exists(quality.file)) {
  text <- readLines(quality.file)
  if (sum(nchar(text)) > 60000L) {
    kept <- cumsum(nchar(text) + 1L) <= 60000L
    text <- c(text[kept], "… (truncated; the full report is in validation-evidence.tar.gz)")
  }
  c("<details>", "<summary>Data quality report (click to expand)</summary>", "",
    "Informational only. Accepted issues can be suppressed in",
    "`data_processing/hiv.surveillance.manager/known_issues.json`.", "",
    "```", text, "```", "", "</details>", "")
} else {
  c("The data quality report was not produced for this build.", "")
}

text <- c(
  "# HIV surveillance-manager candidate", "",
  if (needs.review) c(
    sprintf("**Needs review:** %d structural check(s) found something present in %s but missing here.",
            active$n_failed, baseline.label),
    "Promoting this build requires confirming these removals are intended.", "") else NULL,
  "Build candidate only: no active manager or latest alias has changed.",
  paste0("Compared against ", baseline.label, "."), "",
  "## Summary", "",
  sprintf("- Structural checks: %d passed, %d failed.",
          active$n_checks - active$n_failed, active$n_failed),
  sprintf("- Additions or range changes: %d.", length(active$warnings) + length(active$notices)),
  sprintf("- Data identical to the promoted manager: **%s**.",
          if (isTRUE(active$reproduction$data_identical)) "yes" else "no"),
  sprintf("- Value differences: %d of %d shared arrays; %s changed cells at shared coordinates.",
          active$value_delta$arrays_with_differences, active$value_delta$arrays_compared,
          format(active$value_delta$changed_overlap_cells, big.mark = ",")),
  sprintf("- Metadata fields that differ: %s.",
          if (length(metadata.differences)) paste0("`", metadata.differences, "`", collapse = ", ") else "none"),
  sprintf("- Focused consumer checks: %d passed (Ryan White queries, EHE national prevalence, syphilis adult-population import).",
          length(consumer$checks)),
  "", "## Structural changes", "",
  if (!length(active$failures) && !length(active$warnings) && !length(active$notices))
    c("No structural changes from the promoted manager.", "") else NULL,
  item.lines(active$failures, "Removed"),
  item.lines(active$warnings, "Added"),
  item.lines(active$notices, "Other changes"),
  "## Value changes", "",
  "Arrays whose values, or dimension values, differ from the promoted manager.",
  "Counts overlap across stratifications; changed cells are not unique people or records.", "",
  value.lines(active$value_delta),
  "## Data quality", "",
  quality.lines,
  "## Scope", "",
  "This run tests section-to-final assembly, not rebuilding sections from raw files.",
  "An upstream processing edit is tested only after its section inputs are rebuilt.",
  "These checks do not establish scientific correctness or full calibration compatibility.",
  "", "## Evidence", "",
  paste0("Candidate SHA-256: `", active$reproduction$candidate_sha256, "`"),
  paste0("Promoted-manager SHA-256: `", active$reproduction$baseline_sha256, "`"),
  "", "See `input_identity.txt`, `session_info.txt`, `build_log.txt`,",
  "`active_baseline_report.json`, `consumer_report.json`, and `quality_report.txt`.")
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
