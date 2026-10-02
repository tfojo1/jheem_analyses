# Pure output-check tests: no model, managers, or calibration required.
source("applications/SHIELD/R/shield_output_checks.R")
expect.error <- function(expr, pattern) {
    error <- tryCatch({ force(expr); NULL }, error = identity)
    stopifnot(inherits(error, "error"), grepl(pattern, conditionMessage(error)))
}
fixture <- function(n = 2L, bad = NULL) {
    years <- as.character(seq(2010L, 2030L, 5L))
    structure(list(
        version = "shield", location = "C.12580", calibration.code = "stage0", n.sim = n,
        get.params = function(simulation.indices, drop) {
            stopifnot(identical(simulation.indices, seq_len(n)), identical(drop, FALSE))
            params <- matrix(seq_len(2L * n), nrow = 2L,
                             dimnames = list(c("beta.f", "beta.m"), NULL))
            if (identical(bad, "parameters")) params[1, 1] <- NA_real_
            params
        },
        get = function(outcomes, keep.dimensions, dimension.values, summary.type,
                       drop.single.sim.dimension, replace.inf.values.with.zero, na.rm) {
            stopifnot(identical(keep.dimensions, "year"),
                      identical(dimension.values, list(year = years)),
                      identical(summary.type, "individual.simulation"),
                      !drop.single.sim.dimension, !replace.inf.values.with.zero, !na.rm)
            values <- matrix(seq_len(length(years) * n), nrow = length(years),
                             dimnames = list(year = years, sim = as.character(seq_len(n))))
            if (identical(bad, "nonfinite")) values[1, 1] <- Inf
            if (identical(bad, "year")) rownames(values)[1] <- "2009"
            if (identical(bad, "count")) values <- values[, 1, drop = FALSE]
            if (identical(bad, "population") && outcomes == "population") values[1, 1] <- 0
            if (identical(bad, "negative") && outcomes == "incidence") values[1, 1] <- -0.001
            if (identical(bad, "missing") && outcomes == "diagnosis.ps") stop("missing outcome")
            if (identical(bad, "transposed")) values <- t(values)
            values
        }
    ), class = "jheem.simulation.set")
}
report <- shield.output.report(fixture(), "C.12580", "stage0")
named.location <- fixture()
named.location$location <- c(C.12580 = "C.12580")
stopifnot(identical(report, shield.output.report(named.location, "C.12580", "stage0")))
named.location$location <- c(C.12580 = "C.99999")
expect.error(shield.output.report(named.location, "C.12580", "stage0"), "location.*identity")
stopifnot(report$n_sim == 2L, report$parameter_count == 2L,
          length(report$outcomes) == 4L,
          identical(report$parameters[[1]]$sample_values, list(1L, 3L)),
          identical(report$outcomes[[1]]$years[[1]]$sample_values, list(1L, 6L)))
stopifnot(identical(report, shield.output.report(fixture(bad = "transposed"), "C.12580", "stage0")))
stopifnot(shield.output.report(fixture(1L), "C.12580", "stage0")$n_sim == 1L)
stopifnot(length(shield.output.report(fixture(8L), "C.12580", "stage0")$
                   parameters[[1]]$sample_values) == 5L)
for (bad in c("parameters", "nonfinite", "year", "count", "population", "missing")) {
    expect.error(shield.output.report(fixture(bad = bad), "C.12580", "stage0"),
                 "Missing|non-finite|positive|missing")
}
expect.error(shield.output.report(fixture(), "C.99999", "stage0"), "identity")
expect.error(shield.output.report(fixture(), "C.12580", "wrong"), "identity")
expect.error(shield.output.report(fixture(0L), "C.12580", "stage0"), "simulation count")
expect.error(shield.output.report(fixture(), "C.12580", "stage0", years = integer()), "years")
negative <- shield.output.report(fixture(bad = "negative"), "C.12580", "stage0")
stopifnot(negative$outcomes[[2]]$years[[1]]$negative_values == 1L,
          negative$outcomes[[2]]$years[[1]]$minimum == -0.001)
local({
    path <- tempfile("shield-output-report-", fileext = ".json")
    on.exit(unlink(path), add = TRUE)
    shield.output.write.report(report, path)
    decoded <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    stopifnot(decoded$n_sim == 2L, length(decoded$parameters) == 2L,
              identical(decoded$outcomes[[1]]$years[[1]]$sample_values, list(1L, 6L)))
    original <- readLines(path)
    expect.error(shield.output.write.report(negative, path), "Refusing to replace")
    stopifnot(identical(readLines(path), original))
})
cat("SHIELD output checks passed\n")
