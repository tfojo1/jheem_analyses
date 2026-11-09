# ****************************************************************************************************
# RUN MCMC DIAGNOSTICS ----
# ****************************************************************************************************
# Step 1 of 2: do the chains mix? Run this first; run_likelihood_diagnostics.R comes after.
#
# Reads the raw MCMC only (parameter draws, log-likelihood, log-prior, acceptance), no simsets.
# Uses the last MIXING.BURN.KEEP of each chain (diagnostics_setup.R; 0.5 = second half, the same stretch
# as N.SIM). mcmc/mixing_check.R explains which draws each check uses, and why not the whole chain.
#
# What it does:
# 1. checks that every city has an MCMC cache (mcmc/build_mcmc_cache.R); stops if not, unless
#    READ.CHUNKS.IF.NO.CACHE = TRUE (stage 4: about 84 GB of chunks per city, hours each)
# 2. runs the scripts switched on in RUN: mixing and acceptance (with the HTML report), correlated pairs
# 3. prints one row per script (status, minutes, error) and one row per city (PASS / CHECK / FAIL)
#
# USAGE (from jheem_analyses)
#   1. set CALIBRATION.CODE, LOCATIONS and MIXING.BURN.KEEP in diagnostics_setup.R
#   2. build the cache first if needed (see mcmc/build_mcmc_cache.R)
#   3. in R:          source("applications/SHIELD/analysis/diagnostics/run_mcmc_diagnostics.R")
#      or unattended: nohup Rscript applications/SHIELD/analysis/diagnostics/run_mcmc_diagnostics.R > run_mcmc_diagnostics.out 2>&1 &
# Outputs: <jheem root>/shield/calibration_diagnostics/<code>/mixing/ (report, CSVs, plots/)
# ****************************************************************************************************

## SOURCES ----
source("../jheem_analyses/applications/SHIELD/analysis/diagnostics/diagnostics_setup.R")

## SETTINGS ----
READ.CHUNKS.IF.NO.CACHE <- FALSE   # TRUE = a city with no cache is read from its chunks (slow; not saved by the pairs script)
RUN <- c(
    "mcmc/mixing_check.R"                      = TRUE,   # R-hat, ESS, acceptance, ...; HTML report
    "mcmc/mixing_correlated_parameter_pairs.R" = TRUE    # parameter pairs that trade off; same sampling block?
)

## 1. CHECK THE MCMC CACHE ----
cache <- data.frame(city     = .city.name(unname(LOCATIONS)),
                    location = unname(LOCATIONS),
                    cached   = file.exists(file.path(.cache.dir(), paste0(CALIBRATION.CODE, "_", unname(LOCATIONS), ".rds"))))
cat("\n== MCMC cache for", CALIBRATION.CODE, "==\n")
print(cache, row.names = FALSE)
if (!all(cache$cached) && !READ.CHUNKS.IF.NO.CACHE)
    stop("No MCMC cache for: ", paste(cache$city[!cache$cached], collapse = ", "), ".\n",
         "Build it first: Rscript applications/SHIELD/analysis/diagnostics/mcmc/build_mcmc_cache.R\n",
         "or set READ.CHUNKS.IF.NO.CACHE <- TRUE (slow), or leave those cities out of LOCATIONS in diagnostics_setup.R")

## 2. RUN ----
# when run with Rscript, on-screen plots go nowhere instead of to Rplots.pdf (the report has its own plots)
cat("\nMCMC diagnostics:", CALIBRATION.CODE, "|", length(LOCATIONS), "cities | share of each chain used:", MIXING.BURN.KEEP, "\n")
if (!interactive()) pdf(NULL)
mcmc.run.log <- run.diagnostic.scripts(names(RUN)[RUN])
if (!interactive()) invisible(dev.off())

## 3. SUMMARY ----
cat("\n\n== MCMC diagnostics for ", CALIBRATION.CODE, " ==\n", sep = "")
print(mcmc.run.log, row.names = FALSE)
if (exists("overview") && is.data.frame(overview) && identical(overview$calibration.code[1], CALIBRATION.CODE)) {
    cat("\n== Mixing verdict by city (share of each chain used: ", MIXING.BURN.KEEP, ") ==\n", sep = "")
    print(overview[, c("city", "verdict", "reasons")], row.names = FALSE)
}
cat("\nOutputs in", .mixing.dir(CALIBRATION.CODE), "\n")
report <- newest.file(.mixing.dir(CALIBRATION.CODE), paste0("mixing_report_", CALIBRATION.CODE))
if (!is.null(report)) {
    cat("Report:", report, "\n")
    if (interactive()) browseURL(report)
}
cat("Next, once the chains mix: run_likelihood_diagnostics.R\n")
