# Generic Data Manager Spec Tools
# Extract structural specs from data managers and validate against them.
# These tools are manager-agnostic — they work with any jheem data manager.

library(jsonlite)

# -- Spec Extraction ----------------------------------------------------------

#' Extract a structural spec from a data manager
#'
#' Walks the manager's nested data structure and captures a fingerprint of
#' what exists: outcomes, sources, ontologies, stratifications, dimension
#' values (for fixed dims like race/sex/age), and year/location counts.
#'
#' @param manager A jheem data manager object
#' @param variable.dimensions Character vector of dimension names that are
#'   expected to grow over time (captured as counts, not values). Defaults to
#'   c("year", "location").
#' @return A nested list suitable for JSON serialization
extract_manager_spec <- function(manager,
                                 variable.dimensions = c("year", "location")) {

  spec <- list(
    metadata = list(
      generated_from = manager$name,
      generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ),
    outcomes = list()
  )

  for (outcome_name in names(manager$data)) {

    outcome_info <- manager$outcome.info[[outcome_name]]

    outcome_spec <- list(
      scale = if (!is.null(outcome_info)) outcome_info$metadata$scale else NA,
      denominator_outcome = if (!is.null(outcome_info)) outcome_info$denominator.outcome else NA,
      sources = list()
    )

    # Only walk the 'estimate' metric — other metrics mirror the structure
    estimate_data <- manager$data[[outcome_name]][["estimate"]]
    if (is.null(estimate_data)) next

    for (source_name in names(estimate_data)) {
      source_spec <- list(ontologies = list())

      for (ontology_name in names(estimate_data[[source_name]])) {
        ontology_spec <- list(stratifications = list())

        for (strat_name in names(estimate_data[[source_name]][[ontology_name]])) {
          arr <- estimate_data[[source_name]][[ontology_name]][[strat_name]]
          strat_spec <- extract_stratification_spec(arr, variable.dimensions)
          ontology_spec$stratifications[[strat_name]] <- strat_spec
        }

        source_spec$ontologies[[ontology_name]] <- ontology_spec
      }

      outcome_spec$sources[[source_name]] <- source_spec
    }

    spec$outcomes[[outcome_name]] <- outcome_spec
  }

  spec
}


#' Extract spec for a single stratification array
#' @keywords internal
extract_stratification_spec <- function(arr, variable.dimensions) {

  dn <- dimnames(arr)
  dim_names <- names(dn)

  spec <- list(dimensions = dim_names)

  for (d in dim_names) {
    vals <- sort(dn[[d]])
    if (d %in% variable.dimensions) {
      # Variable dimensions: capture values, count, and range.
      # New values are OK (warning), lost values are a failure.
      spec[[d]] <- list(
        n = length(vals),
        min = min(vals),
        max = max(vals),
        values = vals
      )
    } else {
      # Fixed dimensions: capture actual values (sorted for stable comparison)
      spec[[d]] <- vals
    }
  }

  spec
}


# -- Structural Validation ----------------------------------------------------

#' Validate a data manager's structure against a spec
#'
#' Walks the spec and checks that everything it describes exists in the manager
#' with matching structure. Reports failures (missing/changed items) and
#' warnings (expanded ranges). Does NOT check data values or quality.
#'
#' @param manager A jheem data manager object
#' @param spec A spec list (from extract_manager_spec or loaded from JSON)
#' @param variable.dimensions Character vector matching what was used to
#'   generate the spec. Defaults to c("year", "location").
#' @return A results list with $passed, $failures, $warnings, $notices
validate_manager_structure <- function(manager, spec,
                                       variable.dimensions = c("year", "location")) {

  failures <- list()
  warnings <- list()
  notices <- list()
  n_checks <- 0
  n_outcomes <- 0
  n_sources <- 0
  n_ontologies <- 0
  n_stratifications <- 0

  add_failure <- function(level, path, expected, actual, message) {
    failures[[length(failures) + 1]] <<- list(
      level = level, path = path,
      expected = expected, actual = actual,
      message = message
    )
  }

  add_warning <- function(path, message) {
    warnings[[length(warnings) + 1]] <<- list(path = path, message = message)
  }

  add_notice <- function(path, message) {
    notices[[length(notices) + 1]] <<- list(path = path, message = message)
  }

  # -- Check each outcome in spec --
  for (outcome_name in names(spec$outcomes)) {
    n_checks <- n_checks + 1
    n_outcomes <- n_outcomes + 1

    if (!(outcome_name %in% names(manager$data))) {
      add_failure("outcome", outcome_name, "present", "missing",
                  sprintf("Outcome '%s' missing from manager", outcome_name))
      next
    }

    outcome_spec <- spec$outcomes[[outcome_name]]
    estimate_data <- manager$data[[outcome_name]][["estimate"]]

    # -- Check each source --
    for (source_name in names(outcome_spec$sources)) {
      path <- paste(outcome_name, source_name, sep = " > ")
      n_checks <- n_checks + 1
      n_sources <- n_sources + 1

      if (!(source_name %in% names(estimate_data))) {
        add_failure("source", path, "present", "missing",
                    sprintf("Source '%s' missing from outcome '%s'",
                            source_name, outcome_name))
        next
      }

      source_spec <- outcome_spec$sources[[source_name]]

      # -- Check each ontology --
      for (ontology_name in names(source_spec$ontologies)) {
        path <- paste(outcome_name, source_name, ontology_name, sep = " > ")
        n_checks <- n_checks + 1
        n_ontologies <- n_ontologies + 1

        if (!(ontology_name %in% names(estimate_data[[source_name]]))) {
          add_failure("ontology", path, "present", "missing",
                      sprintf("Ontology '%s' missing from %s > %s",
                              ontology_name, outcome_name, source_name))
          next
        }

        ontology_spec <- source_spec$ontologies[[ontology_name]]

        # -- Check each stratification --
        for (strat_name in names(ontology_spec$stratifications)) {
          path <- paste(outcome_name, source_name, ontology_name, strat_name, sep = " > ")
          n_checks <- n_checks + 1
          n_stratifications <- n_stratifications + 1

          if (!(strat_name %in% names(estimate_data[[source_name]][[ontology_name]]))) {
            add_failure("stratification", path, "present", "missing",
                        sprintf("Stratification '%s' missing from %s > %s > %s",
                                strat_name, outcome_name, source_name, ontology_name))
            next
          }

          # Check dimensions of the array
          arr <- estimate_data[[source_name]][[ontology_name]][[strat_name]]
          strat_spec <- ontology_spec$stratifications[[strat_name]]

          check_stratification_details(arr, strat_spec, path,
                                       variable.dimensions,
                                       add_failure, add_warning)
        }
      }
    }
  }

  # -- Check for unexpected items in manager (not in spec) --
  for (outcome_name in names(manager$data)) {
    if (!(outcome_name %in% names(spec$outcomes))) {
      add_notice(outcome_name,
                 sprintf("Outcome '%s' present in manager but not in spec", outcome_name))
    }
  }

  list(
    passed = length(failures) == 0,
    n_checks = n_checks,
    n_passed = n_checks - length(failures),
    n_failed = length(failures),
    check_breakdown = list(
      outcomes = n_outcomes,
      sources = n_sources,
      ontologies = n_ontologies,
      stratifications = n_stratifications
    ),
    failures = failures,
    warnings = warnings,
    notices = notices
  )
}


#' Check stratification array details against spec
#' @keywords internal
check_stratification_details <- function(arr, strat_spec, path,
                                         variable.dimensions,
                                         add_failure, add_warning) {
  dn <- dimnames(arr)

  for (d in strat_spec$dimensions) {
    if (!(d %in% names(dn))) {
      add_failure("dimension", path, d, "missing",
                  sprintf("Dimension '%s' missing from %s", d, path))
      next
    }

    if (d %in% variable.dimensions) {
      # Variable dimension: new values are OK (warning), lost values are a failure
      spec_info <- strat_spec[[d]]
      actual_vals <- sort(dn[[d]])

      if (!is.null(spec_info$values)) {
        # Check for lost values (failure) and new values (warning)
        expected_vals <- spec_info$values
        lost_vals <- setdiff(expected_vals, actual_vals)
        new_vals <- setdiff(actual_vals, expected_vals)

        if (length(lost_vals) > 0) {
          # Summarize lost values (could be many locations)
          lost_summary <- if (length(lost_vals) <= 5)
            paste(lost_vals, collapse = ", ")
          else
            sprintf("%s, ... (%d total)", paste(head(lost_vals, 5), collapse = ", "), length(lost_vals))

          add_failure("lost_values", path,
                      sprintf("%s: %d values", d, length(expected_vals)),
                      sprintf("%s: %d values (lost %d)", d, length(actual_vals), length(lost_vals)),
                      sprintf("%s '%s' lost values in %s: %s",
                              d, d, path, lost_summary))
        }

        if (length(new_vals) > 0) {
          new_summary <- if (length(new_vals) <= 5)
            paste(new_vals, collapse = ", ")
          else
            sprintf("%s, ... (%d total)", paste(head(new_vals, 5), collapse = ", "), length(new_vals))

          add_warning(path,
                      sprintf("%s '%s' has new values in %s: %s",
                              d, d, path, new_summary))
        }
      } else {
        # Backwards compat: old specs without values, fall back to count/range
        actual_max <- max(actual_vals)
        if (actual_max < spec_info$max) {
          add_failure("year_range", path,
                      sprintf("%s max: %s", d, spec_info$max),
                      sprintf("%s max: %s", d, actual_max),
                      sprintf("Range regressed in %s: expected max %s, got %s",
                              path, spec_info$max, actual_max))
        } else if (actual_max > spec_info$max) {
          add_warning(path,
                      sprintf("Range expanded in %s: spec max %s, manager max %s",
                              path, spec_info$max, actual_max))
        }

        if (length(actual_vals) < spec_info$n) {
          add_warning(path,
                      sprintf("%s count decreased in %s: spec %d, manager %d",
                              d, path, spec_info$n, length(actual_vals)))
        }
      }
    } else {
      # Fixed dimension: check values match exactly
      expected_vals <- sort(strat_spec[[d]])
      actual_vals <- sort(dn[[d]])

      if (!identical(expected_vals, actual_vals)) {
        missing_vals <- setdiff(expected_vals, actual_vals)
        extra_vals <- setdiff(actual_vals, expected_vals)

        msg_parts <- c()
        if (length(missing_vals) > 0)
          msg_parts <- c(msg_parts, sprintf("missing: %s", paste(missing_vals, collapse = ", ")))
        if (length(extra_vals) > 0)
          msg_parts <- c(msg_parts, sprintf("unexpected: %s", paste(extra_vals, collapse = ", ")))

        add_failure("dimension_values", path,
                    paste(expected_vals, collapse = ", "),
                    paste(actual_vals, collapse = ", "),
                    sprintf("Dimension '%s' values changed in %s (%s)",
                            d, path, paste(msg_parts, collapse = "; ")))
      }
    }
  }
}


# -- Output Formatting --------------------------------------------------------

#' Print validation results to console
#'
#' @param results Output from validate_manager_structure()
print_validation_results <- function(results) {

  cat("=== STRUCTURAL VALIDATION ===\n\n")

  b <- results$check_breakdown
  cat(sprintf("Checked %d outcomes, %d sources, %d ontologies, %d stratifications (%d total)\n",
              b$outcomes, b$sources, b$ontologies, b$stratifications, results$n_checks))
  cat(sprintf("Result: %d passed, %d failed\n\n",
              results$n_passed, results$n_failed))

  if (length(results$failures) > 0) {
    cat("FAILURES:\n")
    for (f in results$failures) {
      cat(sprintf("  [%s] %s\n", toupper(f$level), f$message))
    }
    cat("\n")
  }

  if (length(results$warnings) > 0) {
    cat("WARNINGS:\n")
    for (w in results$warnings) {
      cat(sprintf("  %s\n", w$message))
    }
    cat("\n")
  }

  if (length(results$notices) > 0) {
    cat("NOTICES:\n")
    for (n in results$notices) {
      cat(sprintf("  %s\n", n$message))
    }
    cat("\n")
  }

  if (results$passed) {
    cat("RESULT: PASS\n")
  } else {
    cat(sprintf("RESULT: FAIL (%d failure%s, %d warning%s)\n",
                results$n_failed, ifelse(results$n_failed == 1, "", "s"),
                length(results$warnings), ifelse(length(results$warnings) == 1, "", "s")))
  }
}
