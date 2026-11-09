# ****************************************************************************************************
# SHIELD CALIBRATION — LIKELIHOOD DIAGNOSTICS
# ****************************************************************************************************
# What does each likelihood contribute to the fit? Run this after the chains mix (mcmc/mixing_check.R).
#
# This file only sets things up and runs the diagnostics.
# The functions live in likelihood/likelihood_helpers.R - source that file alone to use them in other code.
#
# How to use:
#  1. Set CALIBRATION.CODE, LOCATIONS and N.SIM.NO.BURN in diagnostics_setup.R
#  2. Source the file up to "RUN", then run the RUN section line by line
#  Slow: loads each city's simset and builds its likelihood, one city at a time.
#
# WHICH SIMULATIONS ARE USED
#  1. Not the raw MCMC: the likelihood is computed from model outputs, so this needs a saved simset.
#  2. The simset with no burn, N.SIM.NO.BURN (stage 4: 2,000 = one sim every 500 iterations, 500 per chain).
#     It still has the start of each chain, which change.during.run needs.
#  3. Which sims each part uses:
#       change.during.run   first and last sim of each chain (the likelihood at the start vs the end of the run)
#       posterior.sd        50 sims spread over the last LIK.BURN.KEEP of each chain (default: second half,
#       conflicts           the same stretch as N.SIM and the mixing statistics)
#  4. A city without the N.SIM.NO.BURN simset is reported and skipped.
#
# What each output tells you:
#  1. change.during.run - how much each likelihood improved from the start of each chain to its end
#                         (averaged over chains). Shows which targets the chains worked on.
#  2. posterior.sd      - how much each likelihood varies across the kept sims.
#                         Large = it drives which sims are kept. Near zero = it barely constrains anything.
#  3. conflicts         - pairs of likelihoods that pull against each other (correlation < -0.5)
#  Raw values are not comparable between likelihoods (constants, number of data points), so look at changes.
# ****************************************************************************************************

# specification, calibration register (+ likelihoods) and likelihood/likelihood_helpers.R
source("../jheem_analyses/applications/SHIELD/analysis/diagnostics/diagnostics_setup.R")

# ---- SETTINGS ----
LIK.BURN.KEEP <- 0.5     # share of each chain (last part) for posterior.sd and conflicts
LIK.N.SIMS    <- 50      # sims to score in that stretch (each one computes every likelihood)
# Outputs are saved on the server (shared with the team): <jheem root>/shield/calibration_diagnostics/<code>/likelihood/


# ****************************************************************************************************
# RUN
# ****************************************************************************************************

# 1. All cities: posterior.sd as likelihood x city
lik.by.city <- run.likelihood.breakdown(LOCATIONS, CALIBRATION.CODE, n.sim.saved = N.SIM.NO.BURN,
                                        burn.keep = LIK.BURN.KEEP, n.sims = LIK.N.SIMS)
lik.by.city

# 2. Look closer at one city
loc <- unname(LOCATIONS[1])
lik.breakdown.list[[.lik.key(loc, CALIBRATION.CODE)]]$by.component  # change + spread per likelihood
lik.breakdown.list[[.lik.key(loc, CALIBRATION.CODE)]]$conflicts     # likelihoods that pull against each other

# 3. Save: CSVs in calibration_diagnostics/<code>/likelihood/
save.likelihood.breakdown(LOCATIONS, CALIBRATION.CODE, lik.by.city)
