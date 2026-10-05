#!/usr/bin/env Rscript
# Read only completed, disposable test state in its matching package environment.
# Rscript inspect-calibration-trace.R ROOT LOCATION CALIBRATION REPORT_JSON
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4L) stop("Usage: inspect-calibration-trace.R ROOT LOCATION CALIBRATION REPORT_JSON")
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1L]])
helpers <- file.path(dirname(normalizePath(script, mustWork = TRUE)), "..", "R")
source(file.path(helpers, "shield_recorded_runtime.R"))
source(file.path(helpers, "shield_output_checks.R"))
source(file.path(helpers, "shield_trace_checks.R"))
if (file.exists(args[[4L]])) stop("Refusing to replace an existing trace report")
config <- list(root_dir = normalizePath(args[[1L]], mustWork = TRUE))
record <- shield.recorded.validate.outputs(config, args[[2L]], args[[3L]])
suppressPackageStartupMessages(library(jheem2))
suppressPackageStartupMessages(library(bayesian.simulations))
directory <- shield.recorded.calibration.dir(config, args[[2L]], args[[3L]])
global <- shield.trace.load.one(file.path(directory, "cache", "global_control.Rdata"),
                                "mcmcsim_cache_global_control")
if (global@n.chains != 1L || !all(global@save.chunk) || !is.null(global@prior.mcmc)) {
    stop("Trace diagnostic requires one chain, every chunk saved, and no prior MCMC")
}
chain <- shield.trace.load.one(file.path(directory, "cache", global@chain.control.filenames[[1L]]),
                               "mcmcsim_cache_chain_control")
if (!identical(chain@global.id, global@id) || !all(chain@chunk.done) ||
    length(chain@chunk.filenames) != global@n.chunks || length(chain@seeds) != global@n.chunks) {
    stop("Cache is incomplete or has inconsistent controls")
}
# At the pinned engine revision these initial values are retained in the saved
# simulation closure. Read them without evaluating the function or model.
initial.env <- environment(global@control@simulation.function)
initial <- lapply(c("all.initial.model.parameter.values", "starting.mcmc.parameter.values"),
                  function(name) {
    if (!exists(name, envir = initial.env, inherits = FALSE)) {
        stop("Saved simulation closure is missing initial values: ", name)
    }
    shield.trace.numeric(get(name, envir = initial.env, inherits = FALSE))
})
names(initial) <- c("model_parameters", "sampled_parameters")
offset <- 0L
chunks <- lapply(seq_len(global@n.chunks), function(i) {
    mcmc <- shield.trace.load.one(file.path(directory, "cache", chain@chain.dir,
                                           chain@chunk.filenames[[i]]), "mcmcsim")
    if (mcmc@n.chains != 1L || mcmc@n.iter != global@chunk.size[[i]] ||
        mcmc@thin != 1L || mcmc@burn != 0L ||
        !identical(mcmc@var.names, global@control@var.names) || length(mcmc@chain.states) != 1L) {
        stop("Unexpected trace length, variables, or thinning in chunk ", i)
    }
    fields <- c("samples", "log.likelihoods", "log.priors", "n.accepted", "first.step.for.iter")
    values <- lapply(fields, function(field) shield.trace.numeric(methods::slot(mcmc, field)))
    names(values) <- fields
    first <- offset + 1L
    offset <<- offset + global@chunk.size[[i]]
    list(chunk = i, first_iteration = first, last_iteration = offset,
         seed = as.character(chain@seeds[[i]]), values = values,
         ending_state = shield.trace.state(mcmc@chain.states[[1L]]))
})
attempt.dir <- file.path(config$root_dir, "run_records", "shield", args[[2L]], args[[3L]], "attempts")
attempts <- lapply(sort(list.files(attempt.dir, pattern = "[.]json$", full.names = TRUE)), function(path) {
    value <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    list(run_mode = value$run_mode, status = value$status, image = value$image,
         sources = value$sources, settings = value$settings)
})
if (!length(attempts) || !identical(tail(attempts, 1L)[[1L]]$status, "succeeded")) {
    stop("No successful final attempt")
}
report <- list(schema_version = 1L, status = "completed",
               location = args[[2L]], calibration_code = args[[3L]], inputs = record$inputs,
               inspector_sha256 = shield.recorded.sha256(normalizePath(script)),
               environment = list(r_version = as.character(getRversion()),
                   platform = R.version$platform, rng_kind = as.list(RNGkind()),
                   blas = extSoftVersion()[["BLAS"]],
                   jheem2 = as.character(utils::packageVersion("jheem2")),
                   bayesian_simulations = as.character(utils::packageVersion("bayesian.simulations"))),
               setup = list(n_chains = global@n.chains, n_chunks = global@n.chunks,
                   chunk_sizes = as.list(global@chunk.size), n_iterations = sum(global@chunk.size),
                   thin = global@control@thin, burn = global@control@burn,
                   method = global@control@method, variables = as.list(global@control@var.names),
                   sampling_steps = as.list(global@control@sample.steps)),
               attempts = attempts, initial = initial, chunks = chunks,
               final_state = shield.trace.state(chain@chain.state),
               outputs = record$outputs,
               interpretation = "Same-environment trace diagnostic; no convergence or multi-chain claim.")
shield.output.write.report(report, args[[4L]])
message("Wrote full-precision trace report: ", args[[4L]])
