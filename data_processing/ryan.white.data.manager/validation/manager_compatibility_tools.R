# Data Manager Compatibility Tools
#
# Compare two serialized JHEEM data managers without modifying either input.
# Compatibility is evaluated at named outcome/source/ontology/stratification
# paths and over aligned dimension labels. Exact value equality is intentional:
# a non-zero tolerance can be supplied explicitly when a scientific contract
# permits it.

sha256_file <- function(path) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("The 'digest' package is required to compute SHA-256 hashes")
  }

  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

load_manager_artifact <- function(path, expected.sha256, role) {
  if (!file.exists(path)) {
    stop(sprintf("%s manager does not exist: %s", role, path))
  }

  actual.sha256 <- sha256_file(path)
  if (!identical(tolower(actual.sha256), tolower(expected.sha256))) {
    stop(sprintf(
      "%s manager SHA-256 mismatch: expected %s, got %s",
      role, expected.sha256, actual.sha256
    ))
  }

  artifact.env <- new.env(parent = emptyenv())
  loaded.objects <- load(path, envir = artifact.env)
  manager.objects <- loaded.objects[vapply(
    loaded.objects,
    function(name) {
      object <- artifact.env[[name]]
      is.environment(object) && !is.null(object$data) && !is.null(object$name)
    },
    logical(1)
  )]

  if (length(manager.objects) != 1) {
    stop(sprintf(
      "%s artifact must contain exactly one JHEEM data manager; found: %s",
      role,
      if (length(manager.objects)) paste(manager.objects, collapse = ", ") else "none"
    ))
  }

  manager <- artifact.env[[manager.objects[[1]]]]
  list(
    manager = manager,
    metadata = list(
      role = role,
      file_name = basename(path),
      sha256 = actual.sha256,
      size_bytes = unname(file.info(path)$size),
      serialized_object = manager.objects[[1]],
      manager_name = manager$name,
      manager_description = manager$description,
      manager_creation_date = format_manager_date(manager$creation.date),
      manager_last_modified_date = format_manager_date(manager$last.modified.date),
      manager_code_iteration = manager$code.iteration,
      outcome_count = length(manager$outcomes),
      source_count = length(manager$source.info)
    )
  )
}

format_manager_date <- function(value) {
  if (is.null(value) || !length(value)) return(NA_character_)
  format(value, "%Y-%m-%d %H:%M:%S %Z")
}

collect_estimate_arrays <- function(manager) {
  arrays <- list()

  for (outcome in sort(names(manager$data))) {
    estimates <- manager$data[[outcome]][["estimate"]]
    if (is.null(estimates)) next

    for (source in sort(names(estimates))) {
      for (ontology in sort(names(estimates[[source]]))) {
        stratifications <- estimates[[source]][[ontology]]
        for (stratification in sort(names(stratifications))) {
          value <- stratifications[[stratification]]
          if (is.atomic(value) && !is.null(dim(value))) {
            path <- paste(
              outcome, source, ontology, stratification,
              sep = "/"
            )
            arrays[[path]] <- value
          }
        }
      }
    }
  }

  arrays
}

summarize_values <- function(values, limit = 20L) {
  values <- as.character(values)
  list(
    count = length(values),
    examples = unname(head(values, limit))
  )
}

array_dimension_summary <- function(array) {
  dimension.names <- names(dimnames(array))
  if (is.null(dimension.names)) {
    dimension.names <- paste0("dimension_", seq_along(dim(array)))
  }

  dimensions <- lapply(seq_along(dim(array)), function(index) {
    labels <- dimnames(array)[[index]]
    list(
      name = dimension.names[[index]],
      size = dim(array)[[index]],
      minimum = if (length(labels)) min(labels) else NA_character_,
      maximum = if (length(labels)) max(labels) else NA_character_
    )
  })

  names(dimensions) <- dimension.names
  dimensions
}

normalize_dimension_order <- function(baseline, candidate) {
  baseline.names <- names(dimnames(baseline))
  candidate.names <- names(dimnames(candidate))

  if (is.null(baseline.names) || is.null(candidate.names)) {
    if (!identical(length(dim(baseline)), length(dim(candidate)))) return(NULL)
    return(list(baseline = baseline, candidate = candidate))
  }

  if (!setequal(baseline.names, candidate.names)) return(NULL)
  permutation <- match(baseline.names, candidate.names)
  list(
    baseline = baseline,
    candidate = if (identical(permutation, seq_along(permutation))) {
      candidate
    } else {
      aperm(candidate, permutation)
    }
  )
}

compare_manager_arrays <- function(baseline, candidate, tolerance = 0) {
  ordered <- normalize_dimension_order(baseline, candidate)
  if (is.null(ordered)) {
    return(list(
      status = "incompatible",
      reason = "dimension_names_differ",
      baseline_dimensions = array_dimension_summary(baseline),
      candidate_dimensions = array_dimension_summary(candidate)
    ))
  }

  baseline <- ordered$baseline
  candidate <- ordered$candidate
  baseline.names <- names(dimnames(baseline))
  if (is.null(baseline.names)) {
    baseline.names <- paste0("dimension_", seq_along(dim(baseline)))
  }

  baseline.indices <- vector("list", length(dim(baseline)))
  candidate.indices <- vector("list", length(dim(candidate)))
  dimension.comparison <- list()

  for (index in seq_along(baseline.indices)) {
    baseline.labels <- dimnames(baseline)[[index]]
    candidate.labels <- dimnames(candidate)[[index]]

    if (is.null(baseline.labels) || is.null(candidate.labels)) {
      common.size <- min(dim(baseline)[[index]], dim(candidate)[[index]])
      baseline.indices[[index]] <- seq_len(common.size)
      candidate.indices[[index]] <- seq_len(common.size)
      baseline.missing <- if (dim(baseline)[[index]] > common.size) {
        seq.int(common.size + 1L, dim(baseline)[[index]])
      } else integer()
      candidate.added <- if (dim(candidate)[[index]] > common.size) {
        seq.int(common.size + 1L, dim(candidate)[[index]])
      } else integer()
    } else {
      common.labels <- intersect(baseline.labels, candidate.labels)
      baseline.indices[[index]] <- match(common.labels, baseline.labels)
      candidate.indices[[index]] <- match(common.labels, candidate.labels)
      baseline.missing <- setdiff(baseline.labels, candidate.labels)
      candidate.added <- setdiff(candidate.labels, baseline.labels)
    }

    dimension.comparison[[baseline.names[[index]]]] <- list(
      baseline_size = dim(baseline)[[index]],
      candidate_size = dim(candidate)[[index]],
      baseline_values_missing_from_candidate = summarize_values(baseline.missing),
      candidate_values_added = summarize_values(candidate.added)
    )
  }

  if (any(vapply(baseline.indices, length, integer(1)) == 0)) {
    return(list(
      status = "incompatible",
      reason = "no_common_dimension_values",
      dimensions = dimension.comparison
    ))
  }

  baseline.shared <- do.call(
    `[`, c(list(baseline), baseline.indices, list(drop = FALSE))
  )
  candidate.shared <- do.call(
    `[`, c(list(candidate), candidate.indices, list(drop = FALSE))
  )

  baseline.na <- is.na(baseline.shared)
  candidate.na <- is.na(candidate.shared)
  both.present <- !baseline.na & !candidate.na

  if (is.numeric(baseline.shared) && is.numeric(candidate.shared)) {
    exactly.equal <- baseline.shared == candidate.shared
    exactly.equal[is.na(exactly.equal)] <- FALSE
    within.tolerance <-
      is.finite(baseline.shared) & is.finite(candidate.shared) &
      abs(baseline.shared - candidate.shared) <= tolerance
    changed <- both.present & !(exactly.equal | within.tolerance)
    maximum.difference <- if (any(changed)) {
      max(abs(baseline.shared[changed] - candidate.shared[changed]))
    } else 0
  } else {
    changed <- both.present & baseline.shared != candidate.shared
    maximum.difference <- NA_real_
  }

  candidate.fills <- baseline.na & !candidate.na
  candidate.gaps <- !baseline.na & candidate.na
  missing.dimension.values <- sum(vapply(
    dimension.comparison,
    function(value) value$baseline_values_missing_from_candidate$count,
    integer(1)
  ))
  added.dimension.values <- sum(vapply(
    dimension.comparison,
    function(value) value$candidate_values_added$count,
    integer(1)
  ))

  if (sum(changed) > 0 || sum(candidate.gaps) > 0) {
    status <- "incompatible"
  } else if (missing.dimension.values > 0) {
    status <- "compatible_on_overlap"
  } else if (added.dimension.values > 0 || sum(candidate.fills) > 0) {
    status <- "additive"
  } else {
    status <- "identical"
  }

  list(
    status = status,
    shared_cells = length(baseline.shared),
    shared_nonmissing_cells = sum(both.present),
    changed_shared_values = sum(changed),
    candidate_fills = sum(candidate.fills),
    candidate_gaps = sum(candidate.gaps),
    maximum_absolute_difference = maximum.difference,
    dimensions = dimension.comparison
  )
}

compare_manager_outcome <- function(outcome, baseline.arrays, candidate.arrays,
                                    tolerance = 0) {
  prefix <- paste0(outcome, "/")
  baseline.paths <- names(baseline.arrays)[startsWith(names(baseline.arrays), prefix)]
  candidate.paths <- names(candidate.arrays)[startsWith(names(candidate.arrays), prefix)]
  common.paths <- intersect(baseline.paths, candidate.paths)

  if (!length(baseline.paths)) {
    return(list(
      status = "candidate_only",
      baseline_path_count = 0,
      candidate_path_count = length(candidate.paths),
      common_path_count = 0,
      baseline_only_paths = character(),
      candidate_only_paths = unname(candidate.paths),
      paths = list()
    ))
  }
  if (!length(candidate.paths)) {
    return(list(
      status = "baseline_only",
      baseline_path_count = length(baseline.paths),
      candidate_path_count = 0,
      common_path_count = 0,
      baseline_only_paths = unname(baseline.paths),
      candidate_only_paths = character(),
      paths = list()
    ))
  }

  path.results <- lapply(common.paths, function(path) {
    compare_manager_arrays(
      baseline.arrays[[path]], candidate.arrays[[path]], tolerance = tolerance
    )
  })
  names(path.results) <- common.paths
  path.statuses <- vapply(path.results, `[[`, character(1), "status")
  baseline.only <- setdiff(baseline.paths, candidate.paths)
  candidate.only <- setdiff(candidate.paths, baseline.paths)

  if (any(path.statuses == "incompatible")) {
    status <- "incompatible"
  } else if (
    !length(baseline.only) && !length(candidate.only) &&
      all(path.statuses == "identical")
  ) {
    status <- "identical"
  } else if (
    !length(baseline.only) &&
      all(path.statuses %in% c("identical", "additive"))
  ) {
    status <- "additive"
  } else {
    status <- "compatible_on_overlap"
  }

  list(
    status = status,
    baseline_path_count = length(baseline.paths),
    candidate_path_count = length(candidate.paths),
    common_path_count = length(common.paths),
    baseline_only_paths = unname(baseline.only),
    candidate_only_paths = unname(candidate.only),
    paths = path.results
  )
}

sum_outcome_metric <- function(outcome.result, metric) {
  if (!length(outcome.result$paths)) return(0)
  sum(vapply(
    outcome.result$paths,
    function(path.result) {
      value <- path.result[[metric]]
      if (is.null(value)) 0 else value
    },
    numeric(1)
  ))
}

compare_data_managers <- function(baseline.manager, candidate.manager,
                                  tolerance = 0) {
  baseline.arrays <- collect_estimate_arrays(baseline.manager)
  candidate.arrays <- collect_estimate_arrays(candidate.manager)
  outcomes <- sort(unique(c(
    sub("/.*$", "", names(baseline.arrays)),
    sub("/.*$", "", names(candidate.arrays))
  )))

  outcome.results <- lapply(outcomes, function(outcome) {
    compare_manager_outcome(
      outcome, baseline.arrays, candidate.arrays, tolerance = tolerance
    )
  })
  names(outcome.results) <- outcomes

  summary <- lapply(outcomes, function(outcome) {
    result <- outcome.results[[outcome]]
    list(
      outcome = outcome,
      status = result$status,
      baseline_path_count = result$baseline_path_count,
      candidate_path_count = result$candidate_path_count,
      common_path_count = result$common_path_count,
      shared_cells = sum_outcome_metric(result, "shared_cells"),
      changed_shared_values = sum_outcome_metric(result, "changed_shared_values"),
      candidate_fills = sum_outcome_metric(result, "candidate_fills"),
      candidate_gaps = sum_outcome_metric(result, "candidate_gaps")
    )
  })

  list(
    tolerance = tolerance,
    summary = summary,
    outcomes = outcome.results
  )
}

get_manager_array <- function(manager, outcome, source, ontology, stratification) {
  value <- manager$data[[outcome]][["estimate"]][[source]][[ontology]][[stratification]]
  if (is.null(value)) {
    stop(sprintf(
      "Required manager path is missing: %s/%s/%s/%s",
      outcome, source, ontology, stratification
    ))
  }
  value
}

align_named_arrays <- function(arrays) {
  reference.names <- names(dimnames(arrays[[1]]))
  if (is.null(reference.names)) stop("Derived checks require named dimensions")

  arrays <- lapply(arrays, function(array) {
    dimension.names <- names(dimnames(array))
    if (!setequal(reference.names, dimension.names)) {
      stop("Derived-check arrays do not have the same named dimensions")
    }
    permutation <- match(reference.names, dimension.names)
    if (identical(permutation, seq_along(permutation))) array else aperm(array, permutation)
  })

  common.labels <- lapply(seq_along(reference.names), function(index) {
    Reduce(intersect, lapply(arrays, function(array) dimnames(array)[[index]]))
  })
  names(common.labels) <- reference.names

  aligned <- lapply(arrays, function(array) {
    indices <- lapply(seq_along(common.labels), function(index) {
      match(common.labels[[index]], dimnames(array)[[index]])
    })
    do.call(`[`, c(list(array), indices, list(drop = FALSE)))
  })

  list(arrays = aligned, labels = common.labels)
}

validate_derived_array <- function(id, output, inputs, formula, tolerance = 1e-12) {
  aligned <- align_named_arrays(c(list(output), inputs))
  actual <- aligned$arrays[[1]]
  expected <- do.call(formula, aligned$arrays[-1])
  comparable <- !is.na(actual) & !is.na(expected)
  numeric.mismatches <- comparable & abs(actual - expected) > tolerance
  output.missing <- is.na(actual) & !is.na(expected)
  output.unexpected <- !is.na(actual) & is.na(expected)
  mismatch.count <- sum(numeric.mismatches) + sum(output.missing) + sum(output.unexpected)

  list(
    id = id,
    status = if (mismatch.count == 0) "passed" else "failed",
    tolerance = tolerance,
    aligned_cells = length(actual),
    comparable_cells = sum(comparable),
    mismatches = mismatch.count,
    numeric_mismatches = sum(numeric.mismatches),
    output_missing_when_expected_present = sum(output.missing),
    output_present_when_expected_missing = sum(output.unexpected),
    maximum_absolute_difference = if (any(numeric.mismatches)) {
      max(abs(actual[numeric.mismatches] - expected[numeric.mismatches]))
    } else 0,
    dimensions = lapply(aligned$labels, summarize_values)
  )
}

validate_required_outcome_statuses <- function(comparison, requirements) {
  summary.by.outcome <- setNames(
    comparison$summary,
    vapply(comparison$summary, `[[`, character(1), "outcome")
  )

  lapply(names(requirements), function(outcome) {
    actual <- summary.by.outcome[[outcome]]$status
    if (is.null(actual)) actual <- "missing"
    allowed <- requirements[[outcome]]
    list(
      outcome = outcome,
      status = if (actual %in% allowed) "passed" else "failed",
      actual = actual,
      allowed = allowed
    )
  })
}

rename_array_dimension_values <- function(array, dimension, mapping) {
  dimension.index <- match(dimension, names(dimnames(array)))
  if (is.na(dimension.index)) {
    stop(sprintf("Array does not have a '%s' dimension", dimension))
  }

  old.values <- dimnames(array)[[dimension.index]]
  new.values <- unname(mapping[old.values])
  if (anyNA(new.values) || anyDuplicated(new.values)) {
    stop(sprintf(
      "Mapping for '%s' must cover every value and remain one-to-one",
      dimension
    ))
  }
  dimnames(array)[[dimension.index]] <- new.values
  array
}

aggregate_array_dimension <- function(array, dimension, mapping) {
  dimension.index <- match(dimension, names(dimnames(array)))
  if (is.na(dimension.index)) {
    stop(sprintf("Array does not have a '%s' dimension", dimension))
  }

  old.values <- dimnames(array)[[dimension.index]]
  mapped.values <- unname(mapping[old.values])
  if (anyNA(mapped.values)) {
    stop(sprintf("Mapping for '%s' does not cover every value", dimension))
  }
  target.values <- unique(mapped.values)

  permutation <- c(
    setdiff(seq_along(dim(array)), dimension.index), dimension.index
  )
  permuted <- aperm(array, permutation)
  permuted.dimensions <- dim(permuted)
  matrix.view <- matrix(
    permuted,
    nrow = prod(permuted.dimensions[-length(permuted.dimensions)]),
    ncol = permuted.dimensions[[length(permuted.dimensions)]]
  )
  aggregated <- vapply(target.values, function(target) {
    rowSums(matrix.view[, mapped.values == target, drop = FALSE])
  }, numeric(nrow(matrix.view)))
  if (length(target.values) == 1) {
    aggregated <- matrix(aggregated, ncol = 1)
  }

  aggregated.array <- array(
    aggregated,
    dim = c(
      permuted.dimensions[-length(permuted.dimensions)],
      length(target.values)
    ),
    dimnames = c(
      dimnames(permuted)[-length(permuted.dimensions)],
      list(target.values)
    )
  )
  names(dimnames(aggregated.array))[[length(dim(aggregated.array))]] <- dimension
  aperm(aggregated.array, order(permutation))
}

validate_ryan_white_derived_targets <- function(manager, tolerance = 1e-12) {
  clients <- get_manager_array(
    manager, "adap.clients", "ryan.white.program", "ryan.white.pdfs",
    "year__location"
  )
  diagnosed <- get_manager_array(
    manager, "diagnosed.prevalence", "cdc.hiv", "cdc", "year__location"
  )
  suppression <- get_manager_array(
    manager, "adap.suppression", "nastad.adap", "ryan.white.pdfs",
    "year__location"
  )

  clients.by.sex <- get_manager_array(
    manager, "adap.clients", "ryan.white.program", "ryan.white.pdfs",
    "year__location__sex"
  )
  diagnosed.by.sex <- get_manager_array(
    manager, "diagnosed.prevalence", "cdc.hiv", "cdc",
    "year__location__sex"
  )

  age.mapping <- c(
    "13-24 years" = "13-24 years",
    "25-34 years" = "25-34 years",
    "35-44 years" = "35-44 years",
    "45-54 years" = "45-54 years",
    "55-64 years" = "55+ years",
    "65+ years" = "55+ years"
  )
  clients.by.age <- aggregate_array_dimension(
    get_manager_array(
      manager, "adap.clients", "ryan.white.program", "ryan.white.pdfs",
      "year__location__age"
    ),
    "age", age.mapping
  )
  diagnosed.by.age <- get_manager_array(
    manager, "diagnosed.prevalence", "cdc.hiv", "cdc",
    "year__location__age"
  )

  race.mapping <- c(
    "american indian/alaska native" = "american indian alaska native",
    "asian" = "asian",
    "black/african american" = "black",
    "hispanic/latino" = "hispanic",
    "native hawaiian/other pacific islander" =
      "native hawaiian pacific islander",
    "white" = "white"
  )
  clients.by.race <- get_manager_array(
    manager, "adap.clients", "ryan.white.program", "ryan.white.pdfs",
    "year__location__race"
  )
  diagnosed.by.race <- rename_array_dimension_values(
    get_manager_array(
      manager, "diagnosed.prevalence", "cdc.hiv", "cdc",
      "year__location__race"
    ),
    "race", race.mapping
  )

  list(
    validate_derived_array(
      "adap_proportion_of_diagnosed_total",
      get_manager_array(
        manager, "adap.proportion.of.diagnosed", "ryan.white.program",
        "ryan.white.pdfs", "year__location"
      ),
      list(clients, diagnosed),
      function(adap.clients, diagnosed.prevalence) {
        adap.clients / diagnosed.prevalence
      },
      tolerance = tolerance
    ),
    validate_derived_array(
      "adap_proportion_of_diagnosed_by_sex",
      get_manager_array(
        manager, "adap.proportion.of.diagnosed", "ryan.white.program",
        "ryan.white.pdfs", "year__location__sex"
      ),
      list(clients.by.sex, diagnosed.by.sex),
      function(adap.clients, diagnosed.prevalence) {
        adap.clients / diagnosed.prevalence
      },
      tolerance = tolerance
    ),
    validate_derived_array(
      "adap_proportion_of_diagnosed_by_age",
      get_manager_array(
        manager, "adap.proportion.of.diagnosed", "ryan.white.program",
        "cdc", "year__location__age"
      ),
      list(clients.by.age, diagnosed.by.age),
      function(adap.clients, diagnosed.prevalence) {
        adap.clients / diagnosed.prevalence
      },
      tolerance = tolerance
    ),
    validate_derived_array(
      "adap_proportion_of_diagnosed_by_race",
      get_manager_array(
        manager, "adap.proportion.of.diagnosed", "ryan.white.program",
        "ryan.white.pdfs", "year__location__race"
      ),
      list(clients.by.race, diagnosed.by.race),
      function(adap.clients, diagnosed.prevalence) {
        result <- adap.clients / diagnosed.prevalence
        result[!is.finite(result) | result > 1] <- NA_real_
        result
      },
      tolerance = tolerance
    ),
    validate_derived_array(
      "adap_suppressed_proportion_of_diagnosed_total",
      get_manager_array(
        manager, "adap.suppressed.proportion.of.diagnosed",
        "ryan.white.program", "ryan.white.pdfs", "year__location"
      ),
      list(clients, suppression, diagnosed),
      function(adap.clients, adap.suppression, diagnosed.prevalence) {
        adap.clients * adap.suppression / diagnosed.prevalence
      },
      tolerance = tolerance
    )
  )
}

write_manager_compatibility_markdown <- function(report, path) {
  lines <- c(
    "# Ryan White Data Manager Compatibility Report",
    "",
    sprintf("**Schema:** `%s`", report$schema_version),
    "",
    "## Inputs",
    "",
    "| Role | File | SHA-256 | Embedded creation | Embedded modification |",
    "|---|---|---|---|---|",
    sprintf(
      "| Baseline | `%s` | `%s` | %s | %s |",
      report$baseline$file_name, report$baseline$sha256,
      report$baseline$manager_creation_date,
      report$baseline$manager_last_modified_date
    ),
    sprintf(
      "| Candidate | `%s` | `%s` | %s | %s |",
      report$candidate$file_name, report$candidate$sha256,
      report$candidate$manager_creation_date,
      report$candidate$manager_last_modified_date
    ),
    "",
    "## Outcome comparison",
    "",
    "| Outcome | Status | Shared cells | Changed values | Candidate fills | Candidate gaps |",
    "|---|---:|---:|---:|---:|---:|"
  )

  for (summary in report$comparison$summary) {
    lines <- c(lines, sprintf(
      "| `%s` | `%s` | %s | %s | %s | %s |",
      summary$outcome, summary$status, summary$shared_cells,
      summary$changed_shared_values, summary$candidate_fills,
      summary$candidate_gaps
    ))
  }

  lines <- c(
    lines,
    "",
    "## Required outcome contract",
    "",
    "| Outcome | Check | Actual status | Allowed statuses |",
    "|---|---:|---:|---|"
  )
  for (check in report$required_outcome_checks) {
    lines <- c(lines, sprintf(
      "| `%s` | `%s` | `%s` | %s |",
      check$outcome, check$status, check$actual,
      paste(sprintf("`%s`", check$allowed), collapse = ", ")
    ))
  }

  lines <- c(
    lines,
    "",
    "## Candidate derived-target checks",
    "",
    "| Check | Status | Comparable cells | Mismatches | Maximum absolute difference |",
    "|---|---:|---:|---:|---:|"
  )
  for (check in report$candidate_derived_target_checks) {
    lines <- c(lines, sprintf(
      "| `%s` | `%s` | %s | %s | %s |",
      check$id, check$status, check$comparable_cells, check$mismatches,
      format(check$maximum_absolute_difference, scientific = FALSE)
    ))
  }

  lines <- c(
    lines,
    "",
    "## Scope and provenance boundary",
    "",
    sprintf("- %s", report$scope_statement),
    sprintf("- %s", report$provenance_statement),
    sprintf("- %s", report$candidate_only_statement),
    sprintf("- %s", report$derived_check_statement),
    "",
    "## Interpretation contract",
    "",
    "- `identical`: paths, dimensions, missingness, and values are unchanged.",
    "- `additive`: every baseline path/value is retained; the candidate only adds dimension values or fills.",
    "- `compatible_on_overlap`: shared values are unchanged, but one manager has paths or dimension values absent from the other.",
    "- `incompatible`: a shared value changed, a baseline value became missing, or dimensions cannot be aligned."
  )

  writeLines(lines, path, useBytes = TRUE)
}
