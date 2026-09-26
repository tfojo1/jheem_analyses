# Focused consumers of the HIV surveillance manager. No calibration is launched.
# Usage: Rscript check_consumers.R <candidate> <active-baseline> <syphilis> <report>
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4L || !all(file.exists(args[1:3]))) {
  stop("Usage: Rscript check_consumers.R <candidate> <active-baseline> <syphilis> <report>")
}
library(jheem2)
# source_code.R registers these mappings before application code runs. Load the
# same definitions without the full bootstrap's cache downloads and model runs.
source("applications/EHE/ehe_ontology_mappings.R")
candidate <- load.data.manager(args[[1]])
baseline <- load.data.manager(args[[2]])
checks <- list()
record <- function(name, action) {
  checks[[name]] <<- tryCatch({
    details <- action()
    c(list(passed = TRUE), details)
  }, error = function(e) list(passed = FALSE, error = conditionMessage(e)))
}

# Queries used by applications/ryan_white/for_manuscript/new_vs_total_prev_exploration.R.
# Exercise the same pull + rowMeans boundary without executing its plotting/run code.
for (location in c("TX", "CA")) {
  for (outcome in c("diagnoses", "diagnosed.prevalence", "awareness", "suppression")) {
    record(paste("ryan_white", location, outcome, sep = "/"), function() {
      current <- rowMeans(candidate$pull(outcome, location = location), na.rm = TRUE)
      previous <- rowMeans(baseline$pull(outcome, location = location), na.rm = TRUE)
      stopifnot(length(current) > 0L, any(is.finite(current)))
      required.years <- names(previous)[is.finite(previous)]
      stopifnot(length(required.years) > 0L,
                all(required.years %in% names(current)),
                all(is.finite(current[required.years])))
      list(years_with_values = names(current)[is.finite(current)],
           identical_to_baseline = identical(current, previous))
    })
  }
}

# Query used by do.get.empiric.hiv.aging.rates in EHE/ehe_specification_helpers.R.
record("EHE/US/stratified_prevalence", function() {
  pull.prevalence <- function(manager) manager$pull(
    outcome = "diagnosed.prevalence",
    dimension.values = list(location = "US"),
    keep.dimensions = c("year", "age", "race", "sex", "risk"),
    from.ontology.names = "cdc.national")
  current <- pull.prevalence(candidate)
  previous <- pull.prevalence(baseline)
  stopifnot(!is.null(current), any(is.finite(current)),
            !is.null(previous), any(is.finite(previous)),
            identical(names(dimnames(current)), names(dimnames(previous))))
  # New years or source columns are allowed; previously available coordinates
  # and finite values must remain usable.
  indices <- Map(match, dimnames(previous), dimnames(current))
  stopifnot(!anyNA(unlist(indices)))
  overlap <- do.call(`[`, c(list(current), unname(indices), list(drop = FALSE)))
  stopifnot(all(is.finite(overlap[is.finite(previous)])))
  list(dimensions = as.list(dim(current)),
       identical_to_baseline = identical(current, previous))
})

# Execute the actual syphilis merge's adult-population transfer in isolation.
# Start with an empty destination and the released syphilis source registry, so
# existing values cannot mask an omitted import. This is the transfer boundary,
# not a rebuild of the entire syphilis manager. On-disk inputs remain untouched.
record("syphilis/adult_population_transfer", function() {
  workspace <- tempfile("hiv-consumer-")
  dir.create(file.path(workspace, "data_managers"), recursive = TRUE)
  destination <- file.path(workspace, "data_managers", "surveillance.manager.rdata")
  stopifnot(file.symlink(normalizePath(args[[1]]), destination))
  run <- new.env(parent = globalenv())
  run$Q_ROOT <- workspace
  released.syphilis <- load.data.manager(args[[3]])
  run$syphilis.manager <- create.data.manager("consumer.probe", "Population import probe")
  for (info in released.syphilis$parent.source.info) {
    do.call(run$syphilis.manager$register.parent.source, info)
  }
  for (info in released.syphilis$source.info) {
    do.call(run$syphilis.manager$register.source, info)
  }
  script <- "data_processing/transfer_adult_population_to_syphilis_manager.R"
  sys.source(script, envir = run)
  imported <- run$syphilis.manager$data$adult.population
  expected <- candidate$data$adult.population
  arrays <- 0L
  for (metric in names(expected)) {
    for (source in names(expected[[metric]])) {
      for (ontology in names(expected[[metric]][[source]])) {
        for (stratification in names(expected[[metric]][[source]][[ontology]])) {
          x <- expected[[metric]][[source]][[ontology]][[stratification]]
          y <- imported[[metric]][[source]][[ontology]][[stratification]]
          stopifnot(!is.null(y), identical(dimnames(x), dimnames(y)),
                    identical(as.vector(x[!is.na(x)]), as.vector(y[!is.na(x)])),
                    identical(is.na(x), is.na(y)))
          arrays <- arrays + 1L
        }
      }
    }
  }
  stopifnot(arrays > 0L)
  list(arrays_imported = arrays,
       consumer_source_sha256 = digest::digest(file = script, algo = "sha256"))
})

report <- list(
  passed = all(vapply(checks, function(check) check$passed, logical(1))),
  scope = "HIV consumer queries and syphilis population import; no full model calibration or complete syphilis build",
  inputs = list(
    candidate_sha256 = digest::digest(file = args[[1]], algo = "sha256"),
    baseline_sha256 = digest::digest(file = args[[2]], algo = "sha256"),
    syphilis_sha256 = digest::digest(file = args[[3]], algo = "sha256")),
  jheem2_version = as.character(packageVersion("jheem2")),
  ontology_mappings_sha256 = digest::digest(
    file = "applications/EHE/ehe_ontology_mappings.R", algo = "sha256"),
  checks = checks)
jsonlite::write_json(report, args[[4]], pretty = TRUE, auto_unbox = TRUE)
for (name in names(checks)) {
  cat(if (checks[[name]]$passed) "PASS" else "FAIL", name, "\n")
  if (!checks[[name]]$passed) cat(checks[[name]]$error, "\n")
}
if (!report$passed) quit(save = "no", status = 1L)
