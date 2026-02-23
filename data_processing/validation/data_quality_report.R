# Generic Data Quality Report
# Produces an informational summary of data quality for human review.
# Does NOT gate the build — that's validate_manager_structure()'s job.

# -- Main Entry Point ---------------------------------------------------------

#' Generate a data quality report for a data manager
#'
#' Runs computed checks (no spec needed) and produces a readable summary.
#' This is informational — it surfaces issues for review, not a pass/fail gate.
#'
#' @param manager A jheem data manager object
#' @param component.checks A list defining component consistency checks.
#'   Each element should have $total (outcome name) and $components (character
#'   vector of outcome names that should sum to the total). NULL to skip.
#' @param marginals.outcomes Character vector of outcome names to run
#'   inspect.marginals on, or NULL to auto-detect (all non.negative.number
#'   outcomes). Set to character(0) to skip.
#' @return A report list with sections for each check type
report_data_quality <- function(manager,
                                component.checks = NULL,
                                marginals.outcomes = NULL) {

  report <- list(
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    manager_name = manager$name,
    sections = list()
  )

  # Section 1: NA analysis
  report$sections$na_analysis <- analyze_na_patterns(manager)

  # Section 2: Dimension value sanity
  report$sections$dimension_sanity <- check_dimension_sanity(manager)

  # Section 3: Component consistency
  if (!is.null(component.checks)) {
    report$sections$component_consistency <- check_component_consistency(
      manager, component.checks)
  }

  # Section 4: Marginal consistency
  report$sections$marginal_consistency <- check_marginal_consistency(
    manager, marginals.outcomes)

  report
}


# -- NA Analysis --------------------------------------------------------------

#' Analyze NA patterns across all stratifications
#' @keywords internal
analyze_na_patterns <- function(manager) {

  results <- list()

  for (outcome in names(manager$data)) {
    estimate_data <- manager$data[[outcome]][["estimate"]]
    if (is.null(estimate_data)) next

    for (source in names(estimate_data)) {
      for (ontology in names(estimate_data[[source]])) {
        for (strat in names(estimate_data[[source]][[ontology]])) {
          arr <- estimate_data[[source]][[ontology]][[strat]]
          total_cells <- length(arr)
          na_cells <- sum(is.na(arr))
          nan_cells <- sum(is.nan(arr))
          na_pct <- round(na_cells / total_cells * 100, 1)

          if (na_pct > 0) {
            results[[length(results) + 1]] <- list(
              path = paste(outcome, source, ontology, strat, sep = " > "),
              total_cells = total_cells,
              na_cells = na_cells,
              nan_cells = nan_cells,
              na_pct = na_pct
            )
          }
        }
      }
    }
  }

  # Sort by NA percentage descending
  if (length(results) > 0) {
    na_pcts <- sapply(results, function(x) x$na_pct)
    results <- results[order(na_pcts, decreasing = TRUE)]
  }

  results
}


# -- Dimension Value Sanity ---------------------------------------------------

#' Check that dimension values look reasonable
#'
#' Flags cases where dimension values don't match expected patterns — e.g.,
#' year dimensions containing non-year values, which indicates dimension
#' labels may have been swapped during data processing.
#' @keywords internal
check_dimension_sanity <- function(manager) {

  results <- list()
  year_pattern <- "^\\d{4}$"

  for (outcome in names(manager$data)) {
    estimate_data <- manager$data[[outcome]][["estimate"]]
    if (is.null(estimate_data)) next

    for (source in names(estimate_data)) {
      for (ontology in names(estimate_data[[source]])) {
        for (strat in names(estimate_data[[source]][[ontology]])) {
          arr <- estimate_data[[source]][[ontology]][[strat]]
          dims <- dimnames(arr)

          # Check year dimension: all values should be 4-digit years
          if ("year" %in% names(dims)) {
            year_vals <- dims[["year"]]
            bad_years <- year_vals[!grepl(year_pattern, year_vals)]
            if (length(bad_years) > 0) {
              results[[length(results) + 1]] <- list(
                path = paste(outcome, source, ontology, strat, sep = " > "),
                dimension = "year",
                issue = "non_year_values",
                bad_values = bad_years,
                message = sprintf(
                  "Year dimension contains %d non-year value(s): %s",
                  length(bad_years),
                  paste(head(bad_years, 5), collapse = ", "))
              )
            }
          }

          # Check location dimension: values should be state codes, FIPS, or CBSA codes
          if ("location" %in% names(dims)) {
            loc_vals <- dims[["location"]]
            loc_pattern <- "^([A-Z]{2}|\\d{5}|C\\.\\d{5}|US)$"
            bad_locs <- loc_vals[!grepl(loc_pattern, loc_vals)]
            if (length(bad_locs) > 0) {
              results[[length(results) + 1]] <- list(
                path = paste(outcome, source, ontology, strat, sep = " > "),
                dimension = "location",
                issue = "unrecognized_location_codes",
                bad_values = bad_locs,
                message = sprintf(
                  "Location dimension contains %d unrecognized code(s): %s",
                  length(bad_locs),
                  paste(head(bad_locs, 5), collapse = ", "))
              )
            }
          }
        }
      }
    }
  }

  results
}


# -- Component Consistency ----------------------------------------------------

#' Check that component outcomes sum correctly to totals
#'
#' @param manager A jheem data manager object
#' @param checks A list of check definitions. Each has $total and $components.
#' @keywords internal
check_component_consistency <- function(manager, checks) {

  results <- list()

  for (check in checks) {
    total_outcome <- check$total
    component_outcomes <- check$components

    # Find common sources/ontologies/stratifications
    total_data <- manager$data[[total_outcome]][["estimate"]]
    if (is.null(total_data)) {
      results[[length(results) + 1]] <- list(
        check = sprintf("%s >= components", total_outcome),
        status = "skipped",
        reason = sprintf("Outcome '%s' has no estimate data", total_outcome)
      )
      next
    }

    for (source in names(total_data)) {
      for (ontology in names(total_data[[source]])) {
        # Only check stratifications without demographic dims (year__location)
        # to avoid ontology/restratification complications
        if (!("year__location" %in% names(total_data[[source]][[ontology]]))) next

        total_arr <- total_data[[source]][[ontology]][["year__location"]]

        # Sum components
        component_sum <- array(0, dim = dim(total_arr), dimnames = dimnames(total_arr))
        all_components_found <- TRUE

        for (comp in component_outcomes) {
          comp_arr <- tryCatch(
            manager$data[[comp]][["estimate"]][[source]][[ontology]][["year__location"]],
            error = function(e) NULL
          )

          if (is.null(comp_arr)) {
            all_components_found <- FALSE
            break
          }

          # Align to common years/locations
          common_years <- intersect(dimnames(total_arr)$year, dimnames(comp_arr)$year)
          common_locs <- intersect(dimnames(total_arr)$location, dimnames(comp_arr)$location)

          if (length(common_years) == 0 || length(common_locs) == 0) {
            all_components_found <- FALSE
            break
          }

          component_sum[common_years, common_locs] <-
            component_sum[common_years, common_locs] + comp_arr[common_years, common_locs]
        }

        if (!all_components_found) {
          results[[length(results) + 1]] <- list(
            check = sprintf("%s >= components [%s / %s]", total_outcome, source, ontology),
            status = "skipped",
            reason = "Not all component outcomes found under this source/ontology"
          )
          next
        }

        # Compare: total should be >= sum of components (within floating point tolerance)
        mask <- !is.na(total_arr) & !is.na(component_sum)
        n_comparable <- sum(mask)
        violations <- total_arr[mask] < (component_sum[mask] - 0.01)
        n_violations <- sum(violations)

        result <- list(
          check = sprintf("%s >= components [%s / %s]", total_outcome, source, ontology),
          n_compared = n_comparable,
          n_violations = n_violations,
          status = ifelse(n_violations == 0, "ok", "issues_found")
        )

        if (n_violations > 0) {
          # Report worst violations
          diff_arr <- total_arr - component_sum
          diff_arr[!mask] <- NA
          worst_idx <- which(diff_arr == min(diff_arr, na.rm = TRUE), arr.ind = TRUE)[1, ]
          result$worst_violation <- list(
            year = dimnames(total_arr)$year[worst_idx[1]],
            location = dimnames(total_arr)$location[worst_idx[2]],
            total_value = total_arr[worst_idx[1], worst_idx[2]],
            component_sum = component_sum[worst_idx[1], worst_idx[2]]
          )
        }

        results[[length(results) + 1]] <- result
      }
    }
  }

  results
}


# -- Marginal Consistency -----------------------------------------------------

# TODO: inspect_marginals() currently fails on managers loaded from .rdata
# because R6 method environment bindings are lost during serialization — the
# method can't find internal jheem2 helpers (intersect.shared.dim.names,
# array.access). This is a known R6 limitation, not a bug in our code or in
# inspect_marginals itself.
#
# Options to fix properly (pick one when we get to it):
#   1. Have jheem2 export the helpers inspect_marginals depends on
#   2. Use devtools::load_all() instead of library(jheem2) in the validation
#      script, which rebuilds R6 class bindings from source
#   3. Add a restore/rebind method to the data manager class
#
# For now, we detect the failure and skip gracefully.

#' Run inspect.marginals and summarize results
#' @keywords internal
check_marginal_consistency <- function(manager, outcomes = NULL) {

  # Auto-detect non.negative.number outcomes if not specified
  if (is.null(outcomes)) {
    outcomes <- names(manager$outcome.info)[
      sapply(manager$outcome.info, function(x) {
        !is.null(x$metadata$scale) && x$metadata$scale == "non.negative.number"
      })
    ]
    # Only check outcomes that actually have data
    outcomes <- intersect(outcomes, names(manager$data))
  }

  if (length(outcomes) == 0) return(list())

  # Quick check: does inspect_marginals work on this manager?
  # Fails on deserialized R6 objects due to lost environment bindings.
  test_error <- tryCatch({
    manager$inspect_marginals(outcome = outcomes[1])
    NULL
  }, error = function(e) e$message)

  if (!is.null(test_error) && grepl("non-function", test_error)) {
    return(list(list(
      outcome = "(all)",
      status = "skipped",
      message = paste0(
        "inspect_marginals() unavailable on this manager (R6 environment ",
        "bindings lost during save/load). See TODO in data_quality_report.R ",
        "for fix options.")
    )))
  }

  results <- list()

  for (outcome in outcomes) {
    tryCatch({
      marginals <- manager$inspect_marginals(outcome = outcome)

      if (identical(marginals, NA) || length(marginals) == 0) {
        results[[length(results) + 1]] <- list(
          outcome = outcome,
          status = "ok",
          message = "No discrepancies above threshold"
        )
      } else {
        # Summarize: find max discrepancy across all source/ontology combos
        max_discrepancy <- 0
        details <- list()

        for (source_name in names(marginals)) {
          source_results <- marginals[[source_name]]
          if (identical(source_results, NA)) next

          for (ontology_name in names(source_results)) {
            ont_result <- source_results[[ontology_name]]
            if (identical(ont_result, NA) || !is.matrix(ont_result)) next

            non_na_vals <- ont_result[!is.na(ont_result)]
            if (length(non_na_vals) > 0) {
              this_max <- max(abs(non_na_vals))
              if (this_max > max_discrepancy) max_discrepancy <- this_max

              details[[length(details) + 1]] <- list(
                source = source_name,
                ontology = ontology_name,
                max_discrepancy_pct = round(this_max * 100, 1),
                n_pairs_with_issues = sum(!is.na(ont_result))
              )
            }
          }
        }

        results[[length(results) + 1]] <- list(
          outcome = outcome,
          status = ifelse(max_discrepancy > 0, "discrepancies_found", "ok"),
          max_discrepancy_pct = round(max_discrepancy * 100, 1),
          details = details
        )
      }
    }, error = function(e) {
      results[[length(results) + 1]] <<- list(
        outcome = outcome,
        status = "error",
        message = e$message
      )
    })
  }

  results
}


# -- Output Formatting --------------------------------------------------------

#' Print data quality report to console
#'
#' @param report Output from report_data_quality()
print_quality_report <- function(report) {

  cat("=== DATA QUALITY REPORT ===\n")
  cat("(informational — for review, does not affect pass/fail)\n\n")
  cat(sprintf("Manager: %s\n", report$manager_name))
  cat(sprintf("Generated: %s\n\n", report$generated_at))

  # -- NA Analysis --
  na_results <- report$sections$na_analysis
  if (length(na_results) > 0) {
    cat("--- NA Analysis ---\n")

    high_na <- Filter(function(x) x$na_pct >= 50, na_results)
    moderate_na <- Filter(function(x) x$na_pct >= 10 && x$na_pct < 50, na_results)
    low_na <- Filter(function(x) x$na_pct < 10, na_results)

    cat(sprintf("  %d high (>=50%%), %d moderate (10-50%%), %d low (<10%%) — %d total\n",
                length(high_na), length(moderate_na), length(low_na), length(na_results)))

    # Show top 10 worst
    n_show <- min(10, length(na_results))
    if (n_show > 0) {
      cat(sprintf("  Top %d:\n", n_show))
      for (item in na_results[1:n_show]) {
        cat(sprintf("    %.1f%% NA: %s\n", item$na_pct, item$path))
      }
      if (length(na_results) > n_show) {
        cat(sprintf("    ... and %d more\n", length(na_results) - n_show))
      }
    }
    cat("\n")
  } else {
    cat("--- NA Analysis ---\n  No NAs found\n\n")
  }

  # -- Dimension Value Sanity --
  dim_results <- report$sections$dimension_sanity
  if (length(dim_results) > 0) {
    cat("--- Dimension Value Sanity ---\n")
    cat(sprintf("  %d issue(s) found:\n", length(dim_results)))
    for (item in dim_results) {
      cat(sprintf("  WARNING: %s\n", item$message))
      cat(sprintf("    at: %s\n", item$path))
    }
    cat("\n")
  } else {
    cat("--- Dimension Value Sanity ---\n  All dimension values look reasonable\n\n")
  }

  # -- Component Consistency --
  comp_results <- report$sections$component_consistency
  if (!is.null(comp_results) && length(comp_results) > 0) {
    cat("--- Component Consistency ---\n")
    for (result in comp_results) {
      if (result$status == "ok") {
        cat(sprintf("  %s: OK (%d comparisons)\n", result$check, result$n_compared))
      } else if (result$status == "skipped") {
        cat(sprintf("  %s: SKIPPED (%s)\n", result$check, result$reason))
      } else {
        cat(sprintf("  %s: %d/%d violations\n", result$check,
                    result$n_violations, result$n_compared))
        if (!is.null(result$worst_violation)) {
          wv <- result$worst_violation
          cat(sprintf("    Worst: %s %s (total=%s, components=%s)\n",
                      wv$location, wv$year, wv$total_value, wv$component_sum))
        }
      }
    }
    cat("\n")
  }

  # -- Marginal Consistency --
  marg_results <- report$sections$marginal_consistency
  if (length(marg_results) > 0) {
    cat("--- Marginal Consistency (via inspect.marginals) ---\n")

    # Check for the skipped-entirely case (R6 deserialization issue)
    if (length(marg_results) == 1 && marg_results[[1]]$status == "skipped") {
      cat(sprintf("  SKIPPED: %s\n", marg_results[[1]]$message))
    } else {
      ok_count <- sum(sapply(marg_results, function(x) x$status == "ok"))
      issue_count <- sum(sapply(marg_results, function(x) x$status == "discrepancies_found"))
      error_count <- sum(sapply(marg_results, function(x) x$status == "error"))

      cat(sprintf("  Outcomes checked: %d (ok: %d, with discrepancies: %d, errors: %d)\n",
                  length(marg_results), ok_count, issue_count, error_count))

      # Show details for outcomes with discrepancies
      for (result in marg_results) {
        if (result$status == "discrepancies_found") {
          cat(sprintf("  %s: max discrepancy %.1f%%\n",
                      result$outcome, result$max_discrepancy_pct))
          for (detail in result$details) {
            cat(sprintf("    %s / %s: %.1f%% (%d pair%s)\n",
                        detail$source, detail$ontology, detail$max_discrepancy_pct,
                        detail$n_pairs_with_issues,
                        ifelse(detail$n_pairs_with_issues == 1, "", "s")))
          }
        }
        if (result$status == "error") {
          cat(sprintf("  %s: ERROR - %s\n", result$outcome, result$message))
        }
      }
    }
    cat("\n")
  }
}
