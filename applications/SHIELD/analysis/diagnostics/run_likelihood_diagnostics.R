# ****************************************************************************************************
# RUN LIKELIHOOD DIAGNOSTICS ----
# ****************************************************************************************************
# Step 2 of 2: what does each likelihood contribute? Run this after run_mcmc_diagnostics.R.
#
# Reads the N.SIM.NO.BURN simset (diagnostics_setup.R; stage 4: 2,000, no burn) and the likelihoods the
# calibration used, one city at a time. likelihood/likelihood_breakdown.R explains which sims each part uses.
#
# What it does:
# 1. shows the newest mixing verdict for each city (from run_mcmc_diagnostics.R), and warns about cities
#    that FAIL: their likelihood breakdown mixes chains that do not agree, so read it with care.
#    Stops if there is no mixing result yet, unless REQUIRE.MIXING.FIRST = FALSE.
# 2. checks that every city has the N.SIM.NO.BURN simset; a city without it is skipped by the script
# 3. runs the scripts switched on in RUN
# 4. prints one row per script (status, minutes, error) and the likelihood x city table
#
# USAGE (from jheem_analyses)
#   1. set CALIBRATION.CODE, LOCATIONS and N.SIM.NO.BURN in diagnostics_setup.R
#   2. in R:          source("applications/SHIELD/analysis/diagnostics/run_likelihood_diagnostics.R")
#      or unattended: nohup Rscript applications/SHIELD/analysis/diagnostics/run_likelihood_diagnostics.R > run_likelihood_diagnostics.out 2>&1 &
# Outputs: <jheem root>/shield/calibration_diagnostics/<code>/likelihood/ (CSVs)
# ****************************************************************************************************

## SOURCES ----
source("../jheem_analyses/applications/SHIELD/analysis/diagnostics/diagnostics_setup.R")

## SETTINGS ----
REQUIRE.MIXING.FIRST <- TRUE   # FALSE = run even if run_mcmc_diagnostics.R has not been run for this calibration
RUN <- c(
    "likelihood/likelihood_breakdown.R" = TRUE   # change during the run, spread across kept sims, conflicts
)

## 1. MIXING FIRST ----
# newest mixing overview for this calibration, written by run_mcmc_diagnostics.R (mcmc/mixing_check.R)
mixing.file <- newest.file(.mixing.dir(CALIBRATION.CODE), paste0("mixing_overview_", CALIBRATION.CODE))
if (is.null(mixing.file)) {
    msg <- paste0("No mixing result for ", CALIBRATION.CODE, ". Run run_mcmc_diagnostics.R first ",
                  "(or set REQUIRE.MIXING.FIRST <- FALSE).")
    if (REQUIRE.MIXING.FIRST) stop(msg) else warning(msg)
} else {
    mixing <- read.csv(mixing.file, stringsAsFactors = FALSE)
    mixing <- mixing[mixing$location %in% unname(LOCATIONS), ]
    cat("\n== Mixing verdict by city (", basename(mixing.file), ") ==\n", sep = "")
    print(mixing[, intersect(c("city", "verdict", "burn.keep", "reasons"), names(mixing))], row.names = FALSE)
    failed <- mixing$city[mixing$verdict %in% c("FAIL", "NOT LOADED")]
    if (length(failed) > 0)
        warning("Chains do not mix (or were not loaded) for: ", paste(failed, collapse = ", "),
                ". Their likelihood breakdown mixes chains that do not agree; read it with care.")
}

## 2. CHECK THE SIMSETS ----
simsets <- data.frame(city     = .lik.city.name(unname(LOCATIONS)),
                      location = unname(LOCATIONS),
                      found    = sapply(unname(LOCATIONS), function(loc)
                          file.exists(get.simset.filename(version = "shield", location = loc,
                                                          calibration.code = CALIBRATION.CODE, n.sim = N.SIM.NO.BURN))))
cat("\n== ", N.SIM.NO.BURN, "-sim simset (no burn) for ", CALIBRATION.CODE, " ==\n", sep = "")
print(simsets, row.names = FALSE)
if (!any(simsets$found)) stop("No city has the ", N.SIM.NO.BURN, "-sim simset. Check N.SIM.NO.BURN in diagnostics_setup.R.")
if (!all(simsets$found))
    warning("These cities will be skipped (no ", N.SIM.NO.BURN, "-sim simset): ",
            paste(simsets$city[!simsets$found], collapse = ", "))

## 3. RUN ----
cat("\nLikelihood diagnostics:", CALIBRATION.CODE, "|", sum(simsets$found), "cities with a simset\n")
lik.run.log <- run.diagnostic.scripts(names(RUN)[RUN])

## 4. SUMMARY ----
cat("\n\n== Likelihood diagnostics for ", CALIBRATION.CODE, " ==\n", sep = "")
print(lik.run.log, row.names = FALSE)
if (exists("lik.by.city") && !is.null(lik.by.city)) {
    cat("\n== posterior.sd: likelihood x city (large = drives which sims are kept) ==\n")
    print(lik.by.city)
}
cat("\nOutputs in", .likelihood.dir(CALIBRATION.CODE), "\n")
