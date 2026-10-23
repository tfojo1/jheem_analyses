# ****************************************************************************************************
# RUN DIAGNOSTICS ----
# ****************************************************************************************************
# Runs the diagnostic scripts you switch on below, one after another, for the calibration set in
# diagnostics_setup.R.
# 1. TRUE = run, FALSE = skip
# 2. a script that fails is reported and skipped; the rest still run
# 3. at the end: one row per script with its status, minutes and error (if any)
#
# USAGE
#   1. set CALIBRATION.CODE (and LOCATIONS) in diagnostics_setup.R
#   2. switch scripts on or off in RUN below
#   3. from jheem_analyses:  source("applications/SHIELD/analysis/diagnostics/run_diagnostics.R")
#   Each script's own settings (periods, outcomes, plots on/off) stay in that script.
# ****************************************************************************************************

## SOURCES ----
source("../jheem_analyses/applications/SHIELD/analysis/diagnostics/diagnostics_setup.R")

## RUN ----
# script (path inside diagnostics/) = TRUE to run
RUN <- c(
    # data only: quick, no simsets
    "prior/check_spline_prior_against_data.R"   = TRUE,
    "prior/check_diagnosis_shifts_by_sex.R"     = TRUE,
    # fit to data: loads one simset per city (slow the first time; reused after)
    "fit/check_fit_by_sex_over_time.R"          = TRUE,
    "fit/check_fit_by_age.R"                    = TRUE,
    "fit/check_ps_el_split.R"                   = TRUE,
    "fit/check_msm_proportion.R"                = TRUE,
    "prior/check_prior_position.R"              = TRUE,
    # MCMC mixing: loads the raw MCMC; slow. mixing_check.R runs every step, including the
    # HTML report and the likelihood breakdown (step 8, the slowest)
    "mcmc/mixing_check.R"                       = FALSE,
    "mcmc/mixing_correlated_parameter_pairs.R"  = FALSE,
    # reports: knitted to HTML next to the .Rmd (section 6 of msm_share loads simsets)
    "reports/msm_share_emory_vs_brfss.Rmd"      = FALSE
)

## run.one ----
# source an .R script (printing what it prints when run by hand), or knit an .Rmd
# returns status, minutes and the error message
run.one <- function(script) {
    path  <- file.path(DIAGNOSTICS.DIR, script)
    start <- Sys.time()
    cat("\n\n####################################################################\n")
    cat("## ", script, "\n", sep = "")
    cat("####################################################################\n")
    err <- tryCatch({
        if (grepl("\\.Rmd$", script)) {
            out <- rmarkdown::render(path, envir = globalenv(), quiet = TRUE)
            cat("Knitted:", out, "\n")
        } else {
            source(path, print.eval = TRUE)
        }
        ""
    }, error = function(e) { message(script, " FAILED: ", conditionMessage(e)); conditionMessage(e) })
    data.frame(script  = script,
               status  = if (err == "") "done" else "FAILED",
               minutes = round(as.numeric(difftime(Sys.time(), start, units = "mins")), 1),
               error   = err)
}

## RUN SELECTED ----
selected <- names(RUN)[RUN]
cat("Running", length(selected), "scripts for", CALIBRATION.CODE, "\n")
run.log <- do.call(rbind, lapply(selected, run.one))

## SUMMARY ----
# setup is sourced again by each script, so CALIBRATION.CODE here is the one that was used
cat("\n\n== Diagnostics run for ", CALIBRATION.CODE, " ==\n", sep = "")
print(run.log, row.names = FALSE)
cat("Outputs in", OUT.DIR, "\n")
