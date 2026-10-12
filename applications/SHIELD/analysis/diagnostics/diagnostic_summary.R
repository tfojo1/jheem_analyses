# ****************************************************************************************************
# SHIELD CALIBRATION — MCMC MIXING & ACCEPTANCE DIAGNOSTICS
# ****************************************************************************************************
# Works on multi-chain calibrations (e.g. stage 3, 4 chains).
# Reads the raw MCMC (not the simsets), because acceptance rates only live there.
#
# This file only sets things up and runs the diagnostics.
# The functions live in diagnostic_helpers.R - source that file alone to use them in other code.
#
# How to use:
#  1. Set LOCATIONS and CALIBRATION.CODE in the SETUP section
#     (start with one city to test, then switch to SHIELD.TEN.MSAS)
#  2. Source the file up to "RUN", then run the RUN section line by line
#
# What each statistic tells you:
#  1. Acceptance rate       - how often proposals are accepted. Target 0.238; ~0.15-0.35 is fine.
#                             Very low = stuck. Very high = tiny steps (slow exploration).
#  2. R-hat                 - do the chains agree? < 1.05 good, < 1.1 ok, > 1.1 not mixed.
#  3. Rank-normalized R-hat - same idea, but robust to skewed / heavy-tailed parameters.
#  4. ESS                   - effective sample size: how many independent draws the samples are worth.
#                             < 100 is too few to trust summaries of that parameter.
#  5. Lag-1 autocorrelation - how similar each draw is to the previous one. Close to 1 = slow mixing.
#  6. Geweke z              - does the first 10% of a chain agree with the last 50%?
#                             |z| > 2 means the chain was still drifting (not converged).
#  7. Stuck fraction        - share of saved draws where the value did not change at all.
#                             (with thin = 50, "no change" means nothing accepted in 50 iterations)
#  8. Log-likelihood by chain - if one chain sits much lower, it is stuck in a worse spot.
# ****************************************************************************************************

source('../jheem_analyses/commoncode/locations_of_interest.R')
source("../jheem_analyses/applications/SHIELD/shield_specification.R")   # also sets the jheem root dir
source("../jheem_analyses/applications/SHIELD/shield_likelihoods.R")      # likelihood instructions (for 3i)
source("../jheem_analyses/applications/SHIELD/shield_calib_register.R")   # so get.calibration.info() finds the run settings
source("../jheem_analyses/applications/SHIELD/analysis/diagnostics/diagnostic_helpers.R")   # all the functions

# ---- SETUP ----
CALIBRATION.CODE <- "calib.9.23.stage3.pk"
LOCATIONS        <- SHIELD.TEN.MSAS      # test with one city first; then use SHIELD.TEN.MSAS
# Outputs are saved on the server (shared with the team): <jheem root>/shield/mcmc_diagnostics/
# To change the PASS / CHECK / FAIL cut-offs, edit MIXING.THRESHOLDS here after sourcing the helpers,
# e.g. MIXING.THRESHOLDS$ess.min <- 200


# ****************************************************************************************************
# RUN
# ****************************************************************************************************

# 1. Overview (one row per city). Slow the first time; fast after (memory / disk cache)
overview <- run.mixing.diagnostics(LOCATIONS, CALIBRATION.CODE)
overview[, c("city", "verdict", "reasons")]
overview

# 2. Look closer at one city
loc <- unname(LOCATIONS[1])
m   <- mcmc.list[[.mcmc.key(loc, CALIBRATION.CODE)]]
d   <- diag.list[[.mcmc.key(loc, CALIBRATION.CODE)]]

d$by.chain                  # acceptance + log-likelihood per chain
head(d$by.parameter, 15)    # worst-mixing parameters first
d$accept.by.block           # acceptance per parameter block, lowest first
d$accept.block.by.chain     # block x chain

# 3. Is one chain the problem? Would more burn-in help?
drop.one.chain.check(m)
burn.in.check(m)

# 4. Plots (on screen for one city; to PDF for all)
plot.mcmc.diagnostics(m, d, title = .city.name(loc))
# save.mixing.plots(LOCATIONS, CALIBRATION.CODE)

# 5. Once LOCATIONS = SHIELD.TEN.MSAS: parameters that mix badly in several cities
# params.failing.across.cities(LOCATIONS, CALIBRATION.CODE)

# 6. Compare two calibrations
# compare.calibration.mixing(LOCATIONS, c("calib.9.23.stage3", "calib.9.23.stage3.pk"))

# 7. Shareable report: one HTML file (+ 2 CSVs) in <jheem root>/shield/mcmc_diagnostics/reports, to send to the team
write.mixing.report(LOCATIONS, CALIBRATION.CODE)

# 8. Likelihood breakdown (slow: loads each city's simset and likelihood, one city at a time)
lik.by.city <- run.likelihood.breakdown(LOCATIONS, CALIBRATION.CODE)
lik.by.city                                                         # posterior.sd: likelihood x city
lik.breakdown.list[[.mcmc.key(loc, CALIBRATION.CODE)]]$by.component  # one city: change + spread per likelihood
lik.breakdown.list[[.mcmc.key(loc, CALIBRATION.CODE)]]$conflicts     # one city: likelihoods that pull against each other

# 9. Report with the likelihood breakdown included (reuses what step 8 computed)
# write.mixing.report(LOCATIONS, CALIBRATION.CODE, include.likelihood = TRUE)
