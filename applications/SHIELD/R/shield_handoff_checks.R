# Exact parameter copying is an operational assertion, not a numerical
# equivalence tolerance. The pinned engine copies predecessor values directly.
shield.handoff.require.transfer <- function(previous, initial) {
    valid <- function(values) is.numeric(values) && length(values) > 0L &&
        !is.null(names(values)) && !anyNA(names(values)) &&
        all(nzchar(names(values))) && !anyDuplicated(names(values)) &&
        all(is.finite(values))
    if (!valid(previous) || !valid(initial) || !setequal(names(previous), names(initial))) {
        stop("Predecessor and starting model parameters must be complete, named, and finite")
    }
    if (!identical(unname(previous), unname(initial[names(previous)]))) {
        stop("Stage-1 starting parameters do not match the saved stage-0 summary")
    }
    invisible(length(previous))
}
