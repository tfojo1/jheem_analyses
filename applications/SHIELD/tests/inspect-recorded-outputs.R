# Read a completed, trusted run with its matching installed jheem2 package:
# Rscript inspect-recorded-outputs.R ROOT LOCATION CALIBRATION > report.json
# Never use this on files being written by an active run.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) stop("Usage: inspect-recorded-outputs.R ROOT LOCATION CALIBRATION")
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1L]])
helpers <- file.path(dirname(normalizePath(script, mustWork = TRUE)), "..", "R")
source(file.path(helpers, "shield_recorded_runtime.R"))
source(file.path(helpers, "shield_output_checks.R"))
config <- list(root_dir = normalizePath(args[[1L]], mustWork = TRUE))
record <- shield.recorded.validate.outputs(config, args[[2L]], args[[3L]])
output <- Filter(function(x) identical(x$role, "simulation_set"), record$outputs)[[1L]]
suppressPackageStartupMessages(library(jheem2))
simset <- jheem2::load.simulation.set(file.path(config$root_dir, output$path))
report <- shield.output.report(simset, args[[2L]], args[[3L]])
report$inputs <- record$inputs
report$simulation_set <- output
cat(jsonlite::toJSON(report, auto_unbox = TRUE, pretty = TRUE, digits = NA), "\n")
