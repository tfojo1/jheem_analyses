# Synthetic checks for the review summary and build status.
# Run from the repository root: Rscript data_processing/hiv.surveillance.manager/test_summarize_candidate.R

script <- "data_processing/hiv.surveillance.manager/summarize_candidate.R"
consumer <- list(passed = TRUE, checks = list(a = list(passed = TRUE)))
report <- function(failures = list(), warnings = list(), changes = list()) list(
  passed = length(failures) == 0L, n_checks = 10L, n_failed = length(failures),
  failures = failures, warnings = warnings, notices = list(),
  value_delta = list(arrays_compared = 5L, arrays_with_differences = length(changes),
                     changed_overlap_cells = 12L, changes = changes),
  reproduction = list(data_identical = length(changes) == 0L,
                      metadata_matches = list(name = FALSE, description = TRUE),
                      candidate_sha256 = "c", baseline_sha256 = "b"))
summarize <- function(active, quality = NULL) {
  dir <- tempfile("summary-")
  dir.create(dir)
  jsonlite::write_json(active, file.path(dir, "active_baseline_report.json"), auto_unbox = TRUE)
  jsonlite::write_json(consumer, file.path(dir, "consumer_report.json"), auto_unbox = TRUE)
  if (!is.null(quality)) writeLines(quality, file.path(dir, "quality_report.txt"))
  stopifnot(system2("Rscript", c(script, dir, "baseline-v1")) == 0L)
  list(text = paste(readLines(file.path(dir, "CANDIDATE.md")), collapse = "\n"),
       status = jsonlite::read_json(file.path(dir, "build_status.json")))
}

clean <- summarize(report())
stopifnot(!clean$status$needs_review,
          grepl("No structural changes", clean$text, fixed = TRUE),
          grepl("No values differ", clean$text, fixed = TRUE),
          grepl("was not produced", clean$text, fixed = TRUE),
          grepl("Metadata fields that differ: `name`", clean$text, fixed = TRUE))

change <- list(path = "x > y", comparable = TRUE, changed_overlap_cells = 12L,
               changed_dimension_values = list(year = as.character(2010:2019), location = "TGA.OAKLAND"),
               added_dimension_values = list(), removed_dimension_values = list(location = "C.1"))
changed <- summarize(report(failures = list(list(message = "outcome 'z' removed")),
                            warnings = list(list(message = "new location Q")),
                            changes = list(change)),
                     quality = c("=== DATA QUALITY REPORT ===", "--- NA Analysis ---"))
stopifnot(changed$status$needs_review,
          identical(changed$status$structural_failures, 1L),
          grepl("**Needs review:**", changed$text, fixed = TRUE),
          grepl("**Removed (1)**\n\n- outcome 'z' removed", changed$text, fixed = TRUE),
          grepl("**Added (1)**", changed$text, fixed = TRUE),
          grepl("| `x > y` | 12 | year 2010–2019 (10); location TGA.OAKLAND |  | location C.1 |",
                changed$text, fixed = TRUE),
          grepl("--- NA Analysis ---", changed$text, fixed = TRUE))
cat("summarize_candidate tests passed\n")
