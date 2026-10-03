#!/usr/bin/env Rscript
# Called while the disposable test container is paused. Exit 0 only when the
# requested checkpoint's control and saved chunks deserialize consistently.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 5L) stop("Usage: check-calibration-checkpoint.R ROOT LOCATION CALIBRATION CHUNKS_DONE REPORT_JSON")
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1L]])
helpers <- file.path(dirname(normalizePath(script, mustWork = TRUE)), "..", "R")
source(file.path(helpers, "shield_recorded_runtime.R"))
source(file.path(helpers, "shield_output_checks.R"))
source(file.path(helpers, "shield_trace_checks.R"))
suppressPackageStartupMessages(library(jheem2))
suppressPackageStartupMessages(library(bayesian.simulations))
config <- list(root_dir = normalizePath(args[[1L]], mustWork = TRUE))
directory <- shield.recorded.calibration.dir(config, args[[2L]], args[[3L]])
global.path <- file.path(directory, "cache", "global_control.Rdata")
global <- shield.trace.load.one(global.path, "mcmcsim_cache_global_control")
chain.path <- file.path(directory, "cache", global@chain.control.filenames[[1L]])
chain <- shield.trace.load.one(chain.path, "mcmcsim_cache_chain_control")
target <- as.integer(args[[4L]])
if (is.na(target) || target < 1L || target >= global@n.chunks || global@n.chains != 1L ||
    !identical(chain@global.id, global@id) ||
    !identical(chain@chunk.done, seq_len(global@n.chunks) <= target)) {
    stop("Requested checkpoint is not the current complete checkpoint")
}
paths <- file.path(directory, "cache", chain@chain.dir, chain@chunk.filenames)
if (any(file.exists(paths[-seq_len(target)]))) stop("A later chunk has already started writing")
chunks <- lapply(seq_len(target), function(i) {
    value <- shield.trace.load.one(paths[[i]], "mcmcsim")
    if (value@n.chains != 1L || value@n.iter != global@chunk.size[[i]]) {
        stop("Completed checkpoint has an unexpected trace size")
    }
    list(chunk = i, sha256 = shield.recorded.sha256(paths[[i]]))
})
report <- list(schema_version = 1L, chunks_done = target,
               control_sha256 = shield.recorded.sha256(chain.path),
               chunk_seeds = as.list(as.character(chain@seeds)), completed_chunks = chunks)
shield.output.write.report(report, args[[5L]])
message("Complete checkpoint verified: ", target)
