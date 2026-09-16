# SHIELD engine integration test
#
# This deliberately runs one real simulation against locally available,
# versioned input managers. It is independent of the container image so that
# source-readiness failures are distinguishable from image-build failures.

analyses.path <- Sys.getenv("JHEEM_ANALYSES_PATH", unset = "")
if (!nzchar(analyses.path)) {
  stop("JHEEM_ANALYSES_PATH must identify the jheem_analyses checkout", call. = FALSE)
}

source(file.path(analyses.path, "applications", "SHIELD", "shield_specification.R"))

version <- "shield"
location <- Sys.getenv("SHIELD_TEST_LOCATION", unset = "C.12580")
end.year.text <- Sys.getenv("SHIELD_TEST_END_YEAR", unset = "2030")
end.year <- suppressWarnings(as.integer(end.year.text))

if (is.na(end.year) || as.character(end.year) != end.year.text) {
  stop("SHIELD_TEST_END_YEAR must be an integer", call. = FALSE)
}

engine <- create.jheem.engine(version, location, end.year = end.year)
params <- get.medians(SHIELD.FULL.PARAMETERS.PRIOR)
simulation <- engine$run(params)

if (!inherits(simulation, "jheem.simulation.set")) {
  stop("Engine did not return a jheem.simulation.set", call. = FALSE)
}
if (!simulation$n.sim %in% 1) {
  stop("Engine test expected exactly one simulation", call. = FALSE)
}
if (length(simulation$outcomes) == 0L) {
  stop("Engine test returned no outcomes", call. = FALSE)
}

preferred.outcomes <- c("population", "diagnosis.total")
outcome <- intersect(preferred.outcomes, simulation$outcomes)[1L]
if (is.na(outcome)) {
  outcome <- simulation$outcomes[1L]
}

values <- simulation$get(
  outcomes = outcome,
  drop.single.outcome.dimension = TRUE,
  drop.single.sim.dimension = TRUE
)
nonmissing.values <- values[!is.na(values)]

if (length(nonmissing.values) == 0L) {
  stop(sprintf("Engine outcome '%s' contained no non-missing values", outcome), call. = FALSE)
}
if (any(!is.finite(nonmissing.values))) {
  stop(sprintf("Engine outcome '%s' contained non-finite values", outcome), call. = FALSE)
}

cat(sprintf(
  "SHIELD engine integration test passed: location=%s end_year=%d outcome=%s values=%d\n",
  location,
  end.year,
  outcome,
  length(nonmissing.values)
))
