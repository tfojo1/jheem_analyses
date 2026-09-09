# End-to-end SHIELD/data-manager compatibility smoke for Baltimore.
#
# This is intentionally not an MCMC run. It initializes SHIELD from explicit
# artifacts, compiles the model, runs one deterministic simulation, instantiates
# the current stage-0 and stage-1 likelihood sets, and evaluates both.
#
# Usage:
#   Rscript applications/SHIELD/tests/smoke_manager_compatibility.R \
#     path/to/syphilis.manager.rdata path/to/census.manager.rdata

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2)
  stop("Usage: smoke_manager_compatibility.R <syphilis.manager.rdata> <census.manager.rdata>")

script.argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script.argument) != 1)
  stop("Cannot determine the smoke script path")
script.path <- normalizePath(sub("^--file=", "", script.argument), mustWork = TRUE)
analyses.path <- dirname(dirname(dirname(dirname(script.path))))

syphilis.manager.path <- normalizePath(args[[1]], mustWork = TRUE)
census.manager.path <- normalizePath(args[[2]], mustWork = TRUE)
test.root <- tempfile("shield-manager-smoke-")
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

old.working.directory <- getwd()
on.exit(setwd(old.working.directory), add = TRUE)
setwd(analyses.path)

source(file.path(analyses.path, "applications/SHIELD/shield_specification.R"))
source(file.path(analyses.path, "applications/SHIELD/shield_likelihoods.R"))

version <- "shield"
location <- "C.12580"
parameters <- get.medians(SHIELD.FULL.PARAMETERS.PRIOR)

cat("Building Baltimore SHIELD engine\n")
engine <- create.jheem.engine(version, location, end.year = 2030)
cat("Running deterministic median-parameter simulation\n")
simulation <- engine$run(parameters)

likelihood.instructions <- list(
  stage0 = lik.inst.stage0.2021,
  stage1 = lik.inst.stage1.2021.V1
)

for (likelihood.name in names(likelihood.instructions)) {
  cat(sprintf("Instantiating %s likelihood\n", likelihood.name))
  likelihood <- likelihood.instructions[[likelihood.name]]$instantiate.likelihood(
    version,
    location,
    verbose = FALSE
  )
  result <- likelihood$compute.piecewise(simulation)
  numeric.result <- unlist(result, recursive = TRUE, use.names = FALSE)
  if (length(numeric.result) == 0 || !is.numeric(numeric.result) ||
      any(!is.finite(numeric.result)))
    stop(sprintf("%s likelihood did not produce finite numeric values", likelihood.name))
  cat(sprintf("PASS\t%s\t%d likelihood values\n",
              likelihood.name, length(numeric.result)))
}

cat("SHIELD Baltimore manager compatibility smoke passed\n")
