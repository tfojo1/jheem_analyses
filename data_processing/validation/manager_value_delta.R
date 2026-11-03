# Report data-value differences alongside the structural manager comparison.
# This is a diagnostic, not a scientific acceptance rule.

compare.manager.data.values <- function(baseline.data, candidate.data) {
  arrays.compared <- 0L
  changes <- list()

  compare.array <- function(baseline, candidate, path) {
    arrays.compared <<- arrays.compared + 1L
    old.dims <- dimnames(baseline)
    new.dims <- dimnames(candidate)
    old.names <- names(old.dims)
    new.names <- names(new.dims)

    if (is.null(old.dims) || is.null(new.dims) ||
        is.null(old.names) || is.null(new.names) ||
        !identical(old.names, new.names)) {
      changes[[length(changes) + 1L]] <<- list(
        path = path,
        comparable = FALSE,
        reason = "Dimension names differ or are unavailable"
      )
      return(invisible(NULL))
    }

    shared <- Map(intersect, old.dims, new.dims)
    added <- Map(setdiff, new.dims, old.dims)
    removed <- Map(setdiff, old.dims, new.dims)
    names(shared) <- names(added) <- names(removed) <- old.names
    added <- Filter(length, added)
    removed <- Filter(length, removed)

    if (any(lengths(shared) == 0L)) {
      n.changed <- 0L
      changed.values <- list()
    } else {
      old.index <- Map(match, shared, old.dims)
      new.index <- Map(match, shared, new.dims)
      old.overlap <- do.call(`[`, c(list(baseline), unname(old.index),
                                   list(drop = FALSE)))
      new.overlap <- do.call(`[`, c(list(candidate), unname(new.index),
                                   list(drop = FALSE)))

      old.missing <- is.na(old.overlap)
      new.missing <- is.na(new.overlap)
      same <- (old.missing & new.missing &
               (is.nan(old.overlap) == is.nan(new.overlap))) |
              (!old.missing & !new.missing & old.overlap == new.overlap)
      same[is.na(same)] <- FALSE
      changed.index <- which(!same, arr.ind = TRUE)
      n.changed <- nrow(changed.index)
      changed.values <- list()
      if (n.changed > 0L) {
        for (i in seq_along(shared)) {
          changed.values[[old.names[[i]]]] <-
            unique(shared[[i]][changed.index[, i]])
        }
      }
    }

    if (n.changed > 0L || length(added) > 0L || length(removed) > 0L) {
      changes[[length(changes) + 1L]] <<- list(
        path = path,
        comparable = TRUE,
        changed_overlap_cells = n.changed,
        changed_dimension_values = changed.values,
        added_dimension_values = added,
        removed_dimension_values = removed
      )
    }
    invisible(NULL)
  }

  walk <- function(baseline, candidate, path = character()) {
    if (is.list(baseline) && !is.array(baseline) &&
        is.list(candidate) && !is.array(candidate)) {
      for (name in intersect(names(baseline), names(candidate))) {
        walk(baseline[[name]], candidate[[name]], c(path, name))
      }
    } else if (is.array(baseline) && is.array(candidate)) {
      compare.array(baseline, candidate, paste(path, collapse = " > "))
    }
    invisible(NULL)
  }

  walk(baseline.data, candidate.data)
  n.changed.values <- sum(vapply(changes, function(change) {
    if (isTRUE(change$comparable)) change$changed_overlap_cells else 0L
  }, integer(1)))
  list(
    status = "diagnostic_only",
    arrays_compared = arrays.compared,
    arrays_with_differences = length(changes),
    changed_overlap_cells = n.changed.values,
    changes = changes
  )
}
