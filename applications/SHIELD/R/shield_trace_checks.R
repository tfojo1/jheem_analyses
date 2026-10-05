# Numerical exports for a completed, trusted calibration cache. Decimal strings
# retain all double precision through JSON; timestamps and runtime durations are
# deliberately absent from the values compared for replay.
shield.trace.numeric <- function(values) {
    if (!is.numeric(values) || !length(values) || any(!is.finite(values))) {
        stop("Trace values must be nonempty, finite, and numeric", call. = FALSE)
    }
    shape <- dim(values)
    labels <- dimnames(values)
    if (is.null(shape)) {
        shape <- length(values)
        labels <- list(if (is.null(names(values))) as.character(seq_along(values)) else names(values))
        names(labels) <- "element"
    } else {
        if (is.null(labels)) labels <- vector("list", length(shape))
        axis.names <- names(labels)
        if (is.null(axis.names)) axis.names <- paste0("axis", seq_along(shape))
        for (i in seq_along(shape)) {
            if (is.null(labels[[i]])) labels[[i]] <- as.character(seq_len(shape[[i]]))
        }
        names(labels) <- axis.names
    }
    if (any(vapply(seq_along(shape), function(i) {
        length(labels[[i]]) != shape[[i]] || anyNA(labels[[i]]) ||
            any(!nzchar(labels[[i]])) || anyDuplicated(labels[[i]]) > 0L
    }, logical(1)))) stop("Trace axis labels must be complete and unique", call. = FALSE)
    list(dimensions = lapply(labels, as.list),
         values = as.list(sprintf("%.17g", as.vector(values))))
}

shield.trace.state <- function(state) {
    convert <- function(value) {
        if (is.list(value)) {
            if (!length(value)) stop("Empty adaptive state", call. = FALSE)
            return(lapply(value, convert))
        }
        shield.trace.numeric(value)
    }
    fields <- setdiff(methods::slotNames(state), "run.time")
    result <- lapply(fields, function(field) {
        value <- methods::slot(state, field)
        # The sampler resets this marker after saving an iteration. It is an
        # unset sentinel, not a missing scientific value.
        if (field == "first.step.for.iter" && length(value) == 1L && is.na(value)) {
            return(list(unset = TRUE))
        }
        convert(value)
    })
    names(result) <- fields
    result
}

shield.trace.load.one <- function(path, expected.class) {
    objects <- new.env(parent = emptyenv())
    loaded <- load(path, envir = objects)
    if (length(loaded) != 1L || !methods::is(objects[[loaded]], expected.class)) {
        stop("Unexpected object in calibration cache: ", path, call. = FALSE)
    }
    objects[[loaded]]
}
