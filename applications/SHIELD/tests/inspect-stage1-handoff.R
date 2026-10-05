# Read completed, trusted, disposable handoff state; do not execute the model.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) stop("Usage: inspect-stage1-handoff.R ROOT LOCATION REPORT_JSON")
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1L]])
helpers <- file.path(dirname(normalizePath(script)), "..", "R")
for (name in c("shield_recorded_runtime.R", "shield_trace_checks.R", "shield_handoff_checks.R")) {
    source(file.path(helpers, name))
}
suppressPackageStartupMessages(library(jheem2))
suppressPackageStartupMessages(library(bayesian.simulations))
config <- list(root_dir = normalizePath(args[[1L]], mustWork = TRUE))
location <- args[[2L]]
codes <- c("container.actual.stage0", "container.actual.stage1")
records <- lapply(codes, function(code) shield.recorded.validate.outputs(config, location, code))
controls <- lapply(codes, function(code) {
    directory <- shield.recorded.calibration.dir(config, location, code)
    global <- shield.trace.load.one(file.path(directory, "cache", "global_control.Rdata"),
                                   "mcmcsim_cache_global_control")
    if (global@n.chains != 1L || sum(global@chunk.size) != 2L ||
        global@control@thin != 1L || global@control@burn != 0L) {
        stop("Unexpected handoff canary configuration")
    }
    chain <- shield.trace.load.one(file.path(directory, "cache", global@chain.control.filenames[[1L]]),
                                  "mcmcsim_cache_chain_control")
    if (!identical(chain@global.id, global@id) || !all(chain@chunk.done)) {
        stop("Handoff cache is incomplete or inconsistent")
    }
    traces <- lapply(seq_len(global@n.chunks), function(i) {
        chunk <- shield.trace.load.one(file.path(directory, "cache", chain@chain.dir,
                                                chain@chunk.filenames[[i]]), "mcmcsim")
        list(samples = shield.trace.numeric(chunk@samples),
             log_likelihoods = shield.trace.numeric(chunk@log.likelihoods),
             log_priors = shield.trace.numeric(chunk@log.priors))
    })
    saved <- environment(global@control@simulation.function)
    info <- get("calibration.info", envir = saved, inherits = FALSE)
    likelihood <- get("likelihood", envir = environment(global@control@log.likelihood),
                      inherits = FALSE)
    list(info = info, variables = global@control@var.names, traces = traces,
         likelihood_names = names(likelihood$sub.likelihoods),
         initial = get("all.initial.model.parameter.values", envir = saved, inherits = FALSE))
})
if (!identical(controls[[1L]]$info$preceding.calibration.codes, character()) ||
    !identical(controls[[2L]]$info$preceding.calibration.codes, codes[[1L]]) ||
    !any(grepl("prop.male.ps.diag.among.msm", controls[[2L]]$likelihood_names, fixed = TRUE))) {
    stop("Saved stage-1 setup does not use the expected predecessor and actual MSM likelihood term")
}
parent.file <- file.path(config$root_dir, "run_records", "shield", location, codes[[1L]], "outputs.json")
expected <- list(list(calibration_code = codes[[1L]],
                      outputs_sha256 = shield.recorded.sha256(parent.file)))
if (!identical(records[[2L]]$inputs$preceding, expected)) stop("Wrong recorded predecessor identity")
summary.record <- Filter(function(x) identical(x$role, "mcmc_summary"), records[[1L]]$outputs)[[1L]]
objects <- new.env(parent = emptyenv())
loaded <- load(file.path(config$root_dir, summary.record$path), envir = objects)
if (length(loaded) != 1L || !is.list(objects[[loaded]])) stop("Unexpected predecessor summary")
previous <- objects[[loaded]]$last.sim.parameters
count <- shield.handoff.require.transfer(previous, controls[[2L]]$initial)
report <- list(schema_version = 1L, status = "passed", location = location,
    inputs = records[[2L]]$inputs, predecessor_parameters_copied = count,
    predecessor_parameters = shield.trace.numeric(previous),
    stage1_initial_parameters = shield.trace.numeric(controls[[2L]]$initial),
    stages = lapply(seq_along(codes), function(i) list(
        calibration_code = codes[[i]], iterations = 2L,
        sampled_parameter_names = as.list(controls[[i]]$variables),
        likelihood_names = as.list(controls[[i]]$likelihood_names),
        traces = controls[[i]]$traces, outputs = records[[i]]$outputs)),
    interpretation = "Actual stage-1 likelihood and predecessor transfer; two iterations, not convergence or scientific acceptance.")
if (file.exists(args[[3L]]) || !dir.exists(dirname(args[[3L]]))) stop("Choose a new report file in an existing directory")
jsonlite::write_json(report, args[[3L]], auto_unbox = TRUE, pretty = TRUE, digits = NA)
message("Actual stage-1 handoff passed; copied ", count, " predecessor parameters")
