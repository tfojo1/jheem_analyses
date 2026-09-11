#!/usr/bin/env Rscript

source("data_processing/ryan.white.data.manager/validation/manager_compatibility_tools.R")

fixture.manager <- new.env(parent = emptyenv())
fixture.manager$data <- list()
fixture.manager$name <- "fixture.manager"
fixture.manager$description <- "fixture"
fixture.manager$creation.date <- as.POSIXct("2026-01-01", tz = "UTC")
fixture.manager$last.modified.date <- as.POSIXct("2026-01-02", tz = "UTC")
fixture.manager$code.iteration <- 1
fixture.manager$outcomes <- character()
fixture.manager$source.info <- list()
fixture.path <- tempfile(fileext = ".rdata")
save(fixture.manager, file = fixture.path)
fixture.hash <- sha256_file(fixture.path)
loaded.fixture <- load_manager_artifact(fixture.path, fixture.hash, "fixture")
stopifnot(loaded.fixture$metadata$sha256 == fixture.hash)
wrong.hash.error <- tryCatch(
  {
    load_manager_artifact(fixture.path, paste(rep("0", 64), collapse = ""), "fixture")
    NULL
  },
  error = identity
)
stopifnot(inherits(wrong.hash.error, "error"))
unlink(fixture.path)

make_array <- function(years, locations, values) {
  array(
    values,
    dim = c(length(years), length(locations)),
    dimnames = list(year = years, location = locations)
  )
}

baseline <- make_array(
  c("2020", "2021"), c("A", "B"),
  c(1, 2, 3, NA)
)
candidate.additive <- make_array(
  c("2020", "2021", "2022"), c("A", "B"),
  c(1, 2, 9, 3, 4, 10)
)
candidate.changed <- candidate.additive
candidate.changed["2020", "A"] <- 100

additive.result <- compare_manager_arrays(baseline, candidate.additive)
stopifnot(
  additive.result$status == "additive",
  additive.result$changed_shared_values == 0,
  additive.result$candidate_fills == 1,
  additive.result$candidate_gaps == 0
)

changed.result <- compare_manager_arrays(baseline, candidate.changed)
stopifnot(
  changed.result$status == "incompatible",
  changed.result$changed_shared_values == 1
)

candidate.with.gap <- candidate.additive
candidate.with.gap["2020", "A"] <- NA
gap.result <- compare_manager_arrays(baseline, candidate.with.gap)
stopifnot(
  gap.result$status == "incompatible",
  gap.result$candidate_gaps == 1
)

infinite.baseline <- make_array(c("2020"), c("A"), Inf)
infinite.candidate <- make_array(c("2020"), c("A"), Inf)
infinite.result <- compare_manager_arrays(infinite.baseline, infinite.candidate)
stopifnot(
  infinite.result$status == "identical",
  infinite.result$changed_shared_values == 0
)

candidate.missing.location <- candidate.additive[, "A", drop = FALSE]
overlap.result <- compare_manager_arrays(baseline, candidate.missing.location)
stopifnot(
  overlap.result$status == "compatible_on_overlap",
  overlap.result$changed_shared_values == 0
)

required.check <- validate_required_outcome_statuses(
  list(summary = list(list(outcome = "fixture", status = "incompatible"))),
  list(fixture = c("identical", "additive"))
)
stopifnot(required.check[[1]]$status == "failed")

clients <- make_array(c("2020", "2021"), c("A"), c(20, 30))
diagnosed <- make_array(c("2020", "2021"), c("A"), c(100, 120))
derived <- clients / diagnosed
derived.result <- validate_derived_array(
  "fixture", derived, list(clients, diagnosed),
  function(adap.clients, diagnosed.prevalence) {
    adap.clients / diagnosed.prevalence
  }
)
stopifnot(
  derived.result$status == "passed",
  derived.result$comparable_cells == 2,
  derived.result$mismatches == 0
)

derived.with.gap <- derived
derived.with.gap["2020", "A"] <- NA
derived.gap.result <- validate_derived_array(
  "fixture_gap", derived.with.gap, list(clients, diagnosed),
  function(adap.clients, diagnosed.prevalence) {
    adap.clients / diagnosed.prevalence
  }
)
stopifnot(
  derived.gap.result$status == "failed",
  derived.gap.result$output_missing_when_expected_present == 1
)

age.array <- array(
  c(1, 2, 3, 4, 5, 6),
  dim = c(1, 1, 6),
  dimnames = list(
    year = "2020", location = "A",
    age = c(
      "13-24 years", "25-34 years", "35-44 years", "45-54 years",
      "55-64 years", "65+ years"
    )
  )
)
age.mapping <- c(
  "13-24 years" = "13-24 years",
  "25-34 years" = "25-34 years",
  "35-44 years" = "35-44 years",
  "45-54 years" = "45-54 years",
  "55-64 years" = "55+ years",
  "65+ years" = "55+ years"
)
aggregated.age <- aggregate_array_dimension(age.array, "age", age.mapping)
stopifnot(
  identical(dimnames(aggregated.age)$age, c(
    "13-24 years", "25-34 years", "35-44 years", "45-54 years",
    "55+ years"
  )),
  aggregated.age["2020", "A", "55+ years"] == 11
)

race.array <- make_array(c("2020"), c("black/african american"), 1)
names(dimnames(race.array))[[2]] <- "race"
renamed.race <- rename_array_dimension_values(
  race.array, "race", c("black/african american" = "black")
)
stopifnot(identical(dimnames(renamed.race)$race, "black"))

cat("manager compatibility tool tests passed\n")
