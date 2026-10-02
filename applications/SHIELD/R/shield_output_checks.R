# Inspect completed simsets without sourcing the model or running a calibration.
# These checks establish readable, finite results, not scientific equivalence.
shield.output.report <- function(simset, location, calibration.code,
                                  years = seq(2010L, 2030L, 5L)) {
    fail <- function(message) stop(message, call. = FALSE)
    if (!inherits(simset, "jheem.simulation.set")) fail("Not a JHEEM simulation set")
    expected <- list(version = "shield", location = location, calibration.code = calibration.code)
    for (field in names(expected)) {
        actual <- simset[[field]]
        # locations::sanitize() preserves the input as the scalar's name;
        # compare the identifier value, not that harmless name attribute.
        if (!is.character(actual) || length(actual) != 1L || is.na(actual) ||
            !identical(unname(actual), unname(expected[[field]]))) {
            fail(paste("Simulation set", field, "does not match the requested identity"))
        }
    }
    n <- simset$n.sim
    if (!is.numeric(n) || length(n) != 1L || !is.finite(n) ||
        n < 1L || n != as.integer(n)) fail("Invalid simulation count")
    years <- as.character(years)
    if (!length(years) || anyNA(years) || anyDuplicated(years)) {
        fail("Expected years must be nonempty and unique")
    }
    # get.params() otherwise defaults to only the last simulation.
    params <- simset$get.params(simulation.indices = seq_len(n), drop = FALSE)
    if (!is.matrix(params) || !is.numeric(params) || nrow(params) == 0L ||
        ncol(params) != n || is.null(rownames(params)) ||
        anyNA(rownames(params)) || any(!nzchar(rownames(params))) ||
        anyDuplicated(rownames(params)) || any(!is.finite(params))) {
        fail("Missing, non-finite, or inconsistent simulation parameters")
    }
    sample.indices <- seq_len(min(n, 5L))
    summarize <- function(values) list(
        minimum = min(values), median = stats::median(values), maximum = max(values),
        negative_values = sum(values < 0),
        sample_values = unname(as.list(values[sample.indices]))
    )
    outcomes <- lapply(c("population", "incidence", "diagnosis.total", "diagnosis.ps"),
        function(outcome) {
            # Do not let the usual display defaults replace Inf with zero or
            # remove missing values during aggregation.
            values <- simset$get(
                outcomes = outcome, keep.dimensions = "year",
                dimension.values = list(year = years),
                summary.type = "individual.simulation",
                drop.single.sim.dimension = FALSE,
                replace.inf.values.with.zero = FALSE, na.rm = FALSE
            )
            if (!is.numeric(values) || length(dim(values)) != 2L ||
                !setequal(names(dimnames(values)), c("year", "sim"))) {
                fail(paste("Missing or unexpected dimensions for", outcome))
            }
            values <- aperm(values, match(c("year", "sim"), names(dimnames(values))))
            if (!identical(rownames(values), years) || ncol(values) != n ||
                !identical(colnames(values), as.character(seq_len(n))) ||
                any(!is.finite(values))) {
                fail(paste("Missing, non-finite, or inconsistent values for", outcome))
            }
            if (outcome == "population" && any(values <= 0)) {
                fail("Total population must be positive in every selected year and simulation")
            }
            list(outcome = outcome, years = lapply(seq_along(years), function(i) {
                c(list(year = years[[i]]), summarize(values[i, ]))
            }))
        })
    list(
        schema_version = 1L, location = location, calibration_code = calibration.code,
        n_sim = n, parameter_count = nrow(params),
        sample_simulation_indices = as.list(sample.indices),
        interpretation = paste("Descriptive output check, not a convergence or scientific",
                               "equivalence test. Negative counts are reported, not hidden."),
        parameters = lapply(seq_len(nrow(params)), function(i) {
            c(list(parameter = rownames(params)[[i]]), summarize(params[i, ]))
        }),
        outcomes = outcomes
    )
}

# Keep structured output separate from R/renv startup messages on stdout.
shield.output.write.report <- function(report, path) {
    if (file.exists(path)) stop("Refusing to replace an existing output report: ", path, call. = FALSE)
    if (!dir.exists(dirname(path))) stop("Output report directory does not exist", call. = FALSE)
    jsonlite::write_json(report, path, auto_unbox = TRUE, pretty = TRUE, digits = NA)
    invisible(path)
}
