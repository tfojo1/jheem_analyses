# Fast SHIELD/data-manager compatibility contract.
#
# Usage:
#   Rscript applications/SHIELD/tests/test_manager_contract.R \
#     path/to/syphilis.manager.rdata path/to/census.manager.rdata

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2)
  stop("Usage: test_manager_contract.R <syphilis.manager.rdata> <census.manager.rdata>")

script.argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script.argument) != 1)
  stop("Cannot determine the test script path")
script.path <- normalizePath(sub("^--file=", "", script.argument), mustWork = TRUE)
analyses.path <- dirname(dirname(dirname(dirname(script.path))))

syphilis.manager.path <- normalizePath(args[[1]], mustWork = TRUE)
census.manager.path <- normalizePath(args[[2]], mustWork = TRUE)
test.root <- tempfile("shield-manager-contract-")
dir.create(test.root, recursive = TRUE)
on.exit(unlink(test.root, recursive = TRUE, force = TRUE), add = TRUE)

suppressPackageStartupMessages(library(jheem2))
source(file.path(analyses.path,
                 "applications/SHIELD/R/initialize_shield_runtime.R"))
initialize.shield.runtime(
  census.manager = census.manager.path,
  surveillance.manager = syphilis.manager.path,
  root.dir = test.root,
  analyses.path = analyses.path,
  envir = .GlobalEnv
)

expected.races <- c("black", "hispanic", "other")
age.levels <- c(
  "0-14 years", "15-19 years", "20-24 years", "25-29 years",
  "30-34 years", "35-39 years", "40-44 years", "45-49 years",
  "50-54 years", "55-64 years", "65+ years"
)

failures <- character()
for (location.name in names(SHIELD.TEN.MSAS)) {
  location <- unname(SHIELD.TEN.MSAS[[location.name]])
  specification.metadata <- list(dim.names = list(
    location = location,
    age = age.levels,
    race = expected.races
  ))

  result <- tryCatch(
    get.best.guess.msm.proportions(
      location = location,
      specification.metadata = specification.metadata,
      years = DEFAULT.POPULATION.YEARS,
      keep.age = FALSE,
      keep.race = TRUE
    ),
    error = function(error) error
  )

  failure <- NULL
  if (inherits(result, "error")) {
    failure <- conditionMessage(result)
  } else if (!identical(names(dim(result)), "race")) {
    failure <- paste0("unexpected dimensions: ", paste(names(dim(result)), collapse = ", "))
  } else if (!setequal(names(result), expected.races)) {
    failure <- paste0("unexpected races: ", paste(names(result), collapse = ", "))
  } else if (any(!is.finite(result))) {
    failure <- "non-finite MSM proportion"
  } else if (any(result < 0 | result > 1)) {
    failure <- "MSM proportion outside [0, 1]"
  }

  if (is.null(failure)) {
    cat(sprintf("PASS\t%s\t%s\n", location.name, location))
  } else {
    failures <- c(failures, sprintf("%s (%s): %s", location.name, location, failure))
    cat(sprintf("FAIL\t%s\t%s\t%s\n", location.name, location, failure))
  }
}

if (length(failures) > 0)
  stop(paste(c("SHIELD manager contract failed:", failures), collapse = "\n- "))

cat(sprintf("SHIELD manager contract passed for %d locations\n",
            length(SHIELD.TEN.MSAS)))
