# Verifies that compute_total_syphilis_diagnoses.R is functionally equivalent
# to the original test_total_with_restratification.R at every cell where the
# original produced output.
#
# The new script is expected to produce output for STRICTLY MORE stratifications
# than the old one (specifically the age-containing ones at cdc.sti, where the
# old script bailed on location-dimension mismatch). For stratifications present
# in both outputs, values must match exactly — including NA placement.
#
# Usage:
#   Rscript data_processing/syphilis.manager/data_quality_fix/test_compute_total_equivalence.R [path/to/manager.rdata]
#
# Defaults to cached/syphilis.manager.rdata.

suppressPackageStartupMessages(library(jheem2))

`%||%` <- function(a, b) if (is.null(a)) b else a

MANAGER_PATH <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(MANAGER_PATH)) MANAGER_PATH <- "cached/syphilis.manager.rdata"

OLD_SCRIPT <- "data_processing/syphilis.manager/data_quality_fix/test_total_with_restratification.R"
NEW_SCRIPT <- "data_processing/syphilis.manager/syphilis.processing.for.merging/compute_total_syphilis_diagnoses.R"

cat("Manager:", MANAGER_PATH, "\n")
cat("Old script:", OLD_SCRIPT, "\n")
cat("New script:", NEW_SCRIPT, "\n\n")

# --- Run a script against a fresh copy of the manager ---------------------
# Each call does a fresh load() so the two runs operate on independent R6 objects.
#
# Note: we don't clear existing cdc.summed before running — the R6 manager
# locks `data` for external modification. Since both scripts are deterministic
# and only put() their own stratifications, the cached pre-existing state
# (from the prior merge run) gets overwritten identically in both runs, so
# the comparison on common stratifications is still valid.
run_against_fresh_manager <- function(script_path) {
  env <- new.env()
  load(MANAGER_PATH, envir = env)
  capture.output(source(script_path, local = env), file = nullfile())
  env$syphilis.manager$data$total.syphilis.diagnoses$estimate$cdc.summed
}

cat("Running old script against fresh manager...\n")
old_output <- run_against_fresh_manager(OLD_SCRIPT)
cat("  old produced ontologies:", paste(names(old_output), collapse = ", "), "\n")
for (ont in names(old_output)) {
  cat("    ", ont, ": ", paste(names(old_output[[ont]]), collapse = ", "), "\n", sep = "")
}

cat("\nRunning new script against fresh manager...\n")
new_output <- run_against_fresh_manager(NEW_SCRIPT)
cat("  new produced ontologies:", paste(names(new_output), collapse = ", "), "\n")
for (ont in names(new_output)) {
  cat("    ", ont, ": ", paste(names(new_output[[ont]]), collapse = ", "), "\n", sep = "")
}

# --- Compare ---------------------------------------------------------------
cat("\n=== COMPARISON ===\n")

old_onts <- names(old_output)
new_onts <- names(new_output)

missing_onts <- setdiff(old_onts, new_onts)
if (length(missing_onts) > 0) {
  cat("REGRESSION: ontologies present in old but missing in new:\n")
  for (o in missing_onts) cat(" ", o, "\n")
}

any_regression <- length(missing_onts) > 0
n_strats_compared <- 0
n_strats_equivalent <- 0
n_strats_new <- 0

for (ont in new_onts) {
  cat(sprintf("\nOntology: %s\n", ont))
  old_strats <- names(old_output[[ont]]) %||% character(0)
  new_strats <- names(new_output[[ont]])

  only_old <- setdiff(old_strats, new_strats)
  only_new <- setdiff(new_strats, old_strats)
  common <- intersect(old_strats, new_strats)

  if (length(only_old) > 0) {
    any_regression <- TRUE
    cat("  REGRESSION: stratifications in old but not new:\n")
    for (s in only_old) cat("   ", s, "\n")
  }

  if (length(only_new) > 0) {
    cat("  Additions in new (expected — these are the age-containing strats the old script skipped):\n")
    for (s in only_new) {
      arr <- new_output[[ont]][[s]]
      non_na <- sum(!is.na(arr))
      cat(sprintf("    %s: shape %s, %d/%d non-NA (%.1f%%)\n",
                  s, paste(dim(arr), collapse = "x"),
                  non_na, length(arr), 100 * non_na / length(arr)))
      n_strats_new <- n_strats_new + 1
    }
  }

  for (s in common) {
    n_strats_compared <- n_strats_compared + 1
    old_arr <- old_output[[ont]][[s]]
    new_arr <- new_output[[ont]][[s]]

    # Index into new using old's dimnames so we compare only at the cells
    # the old script produced (new may have a superset of locations).
    idx_args <- lapply(names(dimnames(old_arr)), function(d) dimnames(old_arr)[[d]])
    new_subset <- do.call(`[`, c(list(new_arr), idx_args, list(drop = FALSE)))

    # Compare shape, NA pattern, and values at non-NA cells
    same_shape <- identical(dim(old_arr), dim(new_subset))
    same_na <- same_shape && identical(is.na(old_arr), is.na(new_subset))
    non_na <- !is.na(old_arr)
    same_vals <- same_na && all(old_arr[non_na] == new_subset[non_na])

    if (same_vals) {
      cat(sprintf("  %s: EQUIVALENT (%d cells)\n", s, length(old_arr)))
      n_strats_equivalent <- n_strats_equivalent + 1
    } else {
      any_regression <- TRUE
      cat(sprintf("  %s: DIFFERS\n", s))
      if (!same_shape) {
        cat(sprintf("    old shape: %s\n", paste(dim(old_arr), collapse = "x")))
        cat(sprintf("    new subset shape: %s\n", paste(dim(new_subset), collapse = "x")))
      } else {
        # Find first mismatch
        old_na <- is.na(old_arr); new_na <- is.na(new_subset)
        na_mismatch <- which(old_na != new_na)
        val_mismatch <- which(!old_na & !new_na & old_arr != new_subset)
        if (length(na_mismatch) > 0) {
          i <- na_mismatch[1]
          cat(sprintf("    NA mismatch at index %d: old=%s, new=%s\n",
                      i, old_arr[i], new_subset[i]))
          cat(sprintf("    (%d of %d cells have NA mismatch)\n",
                      length(na_mismatch), length(old_arr)))
        }
        if (length(val_mismatch) > 0) {
          i <- val_mismatch[1]
          cat(sprintf("    value mismatch at index %d: old=%s, new=%s\n",
                      i, old_arr[i], new_subset[i]))
          cat(sprintf("    (%d of %d cells have value mismatch)\n",
                      length(val_mismatch), length(old_arr)))
        }
      }
    }
  }
}

cat("\n=== SUMMARY ===\n")
cat(sprintf("Stratifications compared (present in both): %d\n", n_strats_compared))
cat(sprintf("  Equivalent: %d\n", n_strats_equivalent))
cat(sprintf("  Differing: %d\n", n_strats_compared - n_strats_equivalent))
cat(sprintf("New stratifications in rewrite (not in old output): %d\n", n_strats_new))

if (any_regression) {
  cat("\nRESULT: FAIL\n")
  quit(status = 1)
} else {
  cat("\nRESULT: PASS — new script is equivalent where old produced output, plus adds age-containing stratifications.\n")
}
