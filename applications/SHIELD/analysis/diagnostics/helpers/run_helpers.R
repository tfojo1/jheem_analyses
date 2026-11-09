# ****************************************************************************************************
# RUN HELPERS ----
# ****************************************************************************************************
# Functions only, used by the runners (run_mcmc_diagnostics.R, run_likelihood_diagnostics.R).
# Sourced by diagnostics_setup.R.
#
# Contents:
#  1. run.diagnostic.scripts() - sources each script in turn; a script that fails is reported and skipped
#  2. newest.file()            - newest time-stamped output file with a given prefix
# ****************************************************************************************************

## run.diagnostic.scripts ----
# scripts: paths inside diagnostics/, e.g. "mcmc/mixing_check.R"
# 1. prints what each script prints when run by hand
# 2. a script that fails is reported and the rest still run
# 3. returns one row per script: status, minutes and the error (if any)
run.diagnostic.scripts <- function(scripts) {
    one <- function(script) {
        start <- Sys.time()
        cat("\n\n####################################################################\n")
        cat("## ", script, "  (", format(start, "%F %T"), ")\n", sep = "")
        cat("####################################################################\n")
        err <- tryCatch({ source(file.path(DIAGNOSTICS.DIR, script), print.eval = TRUE); "" },
                        error = function(e) { message(script, " FAILED: ", conditionMessage(e)); conditionMessage(e) })
        data.frame(script  = script,
                   status  = if (err == "") "done" else "FAILED",
                   minutes = round(as.numeric(difftime(Sys.time(), start, units = "mins")), 1),
                   error   = err)
    }
    do.call(rbind, lapply(scripts, one))
}

## newest.file ----
# outputs end in a time stamp (_YYYYMMDD_HHMM), so the last name in sorted order is the newest
newest.file <- function(folder, prefix) {
    f <- sort(list.files(folder, pattern = paste0("^", prefix), full.names = TRUE))
    if (length(f) == 0) NULL else tail(f, 1)
}
