# Synthetic checks for check_marginal_consistency() and its printed summary.
# Run from the repository root: Rscript data_processing/validation/test_marginal_consistency.R

source("data_processing/validation/data_quality_report.R")

rows <- function(pct, super = 100, sum = 50, n = 1L) data.frame(
  year = 2020L + seq_len(n) - 1L, location = rep("C.12580", n),
  super_value = rep(super, n), marginal_sum = rep(sum, n),
  magnitude_diff = rep(super - sum, n), percent_diff = pct)
nested <- function(outcome, frame, sub = "year__location__sex") {
  structure(list(list(src = list(ont = list(year__location = setNames(list(frame), sub))))),
            names = outcome)
}
fake.manager <- function(results) list(
  outcome.info = lapply(results, function(x) list(metadata = list(scale = "non.negative.number"))),
  data = lapply(results, function(x) list()),
  inspect_marginals = function(outcome) {
    result <- results[[outcome]]
    if (is.character(result)) stop(result)
    result
  })

manager <- fake.manager(list(
  clean = NULL,
  flagged = nested("flagged", rows(c(50, -80), n = 2L)),
  zero = nested("zero", rows(Inf, super = 0, sum = 60)),
  broken = "cannot inspect"))
results <- check_marginal_consistency(manager)
status <- setNames(vapply(results, function(r) r$status, ""), vapply(results, function(r) r$outcome, ""))
stopifnot(identical(status[["clean"]], "ok"),
          identical(status[["flagged"]], "discrepancies_found"),
          identical(status[["zero"]], "discrepancies_found"),
          identical(status[["broken"]], "error"))
flagged <- results[[which(names(status) == "flagged")]]
stopifnot(identical(flagged$n_rows, 2), identical(flagged$max_discrepancy_pct, 80),
          identical(flagged$details[[1]]$source, "src"),
          identical(flagged$details[[1]]$sub_stratification, "year__location__sex"),
          grepl("C.12580 2021 total 100 vs sum 50", flagged$details[[1]]$worst, fixed = TRUE))

printed <- capture.output(print_quality_report(list(
  manager_name = "fake", generated_at = "now",
  sections = list(marginal_consistency = results))))
stopifnot(any(grepl("Outcomes checked: 4 (ok: 1, with discrepancies: 2, errors: 1)", printed, fixed = TRUE)),
          any(grepl("flagged: 2 row(s) over threshold, max discrepancy 80.0%", printed, fixed = TRUE)),
          any(grepl("n/a (total is 0)", printed, fixed = TRUE)),
          any(grepl("broken: ERROR - cannot inspect", printed, fixed = TRUE)))

# Output stays bounded when many comparisons are flagged.
many <- setNames(lapply(sprintf("o%02d", 1:30), function(o) nested(o, rows(50))), sprintf("o%02d", 1:30))
printed <- capture.output(print_quality_report(list(
  manager_name = "fake", generated_at = "now",
  sections = list(marginal_consistency = check_marginal_consistency(fake.manager(many))))))
stopifnot(sum(grepl("^    src / ont:", printed)) == 25L,
          any(grepl("... and 5 more comparison(s)", printed, fixed = TRUE)))
cat("marginal consistency tests passed\n")
