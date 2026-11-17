# ****************************************************************************************************
# SHIELD CALIBRATION — MCMC MIXING & ACCEPTANCE DIAGNOSTICS
# ****************************************************************************************************
# Do the chains mix? Run this first, before the likelihood diagnostics (likelihood/likelihood_breakdown.R).
# Works on multi-chain calibrations (e.g. stage 3 and 4, 4 chains).
#
# This file only sets things up and runs the diagnostics.
# The functions live in mcmc/mcmc_helpers.R - source that file alone to use them in other code.
#
# How to use:
#  1. Set CALIBRATION.CODE and LOCATIONS in diagnostics_setup.R
#     (start with one city to test, then switch to all 10)
#  2. For a long calibration (stage 4: about 84 GB of chunks per city), build the MCMC cache first
#     with mcmc/build_mcmc_cache.R in the background; this script then reads the cache
#  3. Source the file up to "RUN", then run the RUN section line by line
#
# WHICH DRAWS ARE USED
#  1. Not the simsets. This script reads the raw MCMC: what the sampler saved for each draw
#     (parameter values, log-likelihood, log-prior, number of accepted proposals). It never looks at
#     model outputs, and needs none of the simsets (N.SIM, N.SIM.NO.BURN or the full one).
#  2. What the sampler saved, per city (stage 4): 4 chains x 250,000 iterations, one draw saved every
#     50 iterations = 5,000 draws per chain, 20,000 per city.
#  3. Which of those each check uses (MIXING.BURN.KEEP = 0.5, set in diagnostics_setup.R):
#       R-hat, ESS, lag-1 autocorrelation,
#       Geweke                                   last 2,500 draws per chain (iterations 125,001-250,000)
#       log-likelihood / log-posterior,
#       acceptance (by chain and block)          same last 2,500 per chain
#       leave one chain out, trace and
#       acceptance plots, correlated pairs       same last 2,500 per chain
#       burn-in check                            whole chain (5,000 per chain), then without the first 25%, 50% and 75%
#  4. How this relates to the simsets:
#       N.SIM (1000) used by the fit scripts:    the same second half of each chain, every 10th saved draw
#                                                (250 per chain). Mixing uses all 2,500 draws in that stretch,
#                                                so good mixing here means the 1000 sims come from a well-mixed stretch.
#       N.SIM.NO.BURN (2000):                    only used by likelihood/likelihood_breakdown.R
#       full simset (20,000):                    not used by any diagnostic
#
# WHY NOT THE WHOLE CHAIN (MIXING.BURN.KEEP = 1)
#  1. Each chain starts where the previous stage ended, and the first part of the chain is still
#     moving away from that start toward where the chain settles.
#  2. Mixing statistics compare chains with each other, and the start of a chain with its end. With the
#     start included they measure that early movement, not whether the chains mix where they settled:
#       R-hat goes up (the chains start in different places)
#       Geweke compares the first 10% with the last 50%, so it flags almost every parameter
#       ESS and the log-likelihood by chain mix early and late draws
#  3. The fit scripts use only the second half, so the mixing should be judged on the same draws.
#  4. Use 1 only to compare, e.g. to see how much the first half changes the verdict.
#     burn.in.check() already shows this for R-hat (whole chain vs without the first 25%, 50% and 75%).
#
# What each statistic tells you (* = used in the PASS / CHECK / FAIL verdict; cut-offs in MIXING.THRESHOLDS):
#  1. Acceptance rate *       - how often proposals are accepted, by chain. Target 0.238; ~0.15-0.35 is fine.
#                               < 0.10 FAIL (stuck); > 0.50 CHECK (tiny steps, slow exploration).
#  2. Block acceptance *      - the same for each sampling block in each chain.
#                               < 0.05 in a chain CHECK: that block's parameters barely move in that chain.
#  3. R-hat *                 - do the chains agree? Rank-normalized split R-hat (Vehtari et al. 2021):
#                               a. split: each chain is cut in half (4 chains -> 8 halves), so a drift
#                                  inside the kept half shows up, even one all chains share
#                               b. rank-normalized: robust to skewed / heavy-tailed parameters
#                               c. also checks the tails (distance from the median), not only the centre
#                               < 1.05 good; > 1.05 CHECK; > 1.1 FAIL.
#  4. R-hat of the log-posterior * - the same for the whole model in one number. > 1.05 CHECK; > 1.1 FAIL.
#  5. ESS *                   - effective sample size: how many independent draws the samples are worth.
#                               Bulk ESS across all chains: it counts disagreement between chains, so chains
#                               in different places give a low ESS (adding up each chain's ESS would not).
#                               < 400 (about 100 per chain) CHECK.
#  6. R-hat classic           - get.rhats() from bayesian.simulations, not split. Only to compare with older reports.
#  7. Lag-1 autocorrelation   - how similar each draw is to the previous one. Close to 1 = slow mixing.
#  8. Geweke z                - does the first 10% of a chain agree with the last 50%? |z| > 2 = drifting.
#                               Not in the verdict: one test per parameter per chain (692 for stage 4), so
#                               about 5% pass |z| > 2 by chance. Split R-hat (3) covers drift.
#  9. Log-likelihood by chain - if one chain sits much lower, it is stuck in a worse spot.
#                               The gap is shown but not in the verdict (it depends on the likelihood scale);
#                               R-hat of the log-posterior (4) answers the same question.
#
# Not checked: "stuck fraction" (share of saved draws with no change). The sampler proposes one block per
# iteration and there are about 70 blocks, so each block is proposed less than once per saved draw
# (thin = 50). No change between saved draws is the normal case (stage 3: every parameter "stuck"
# in every chain). Block acceptance (2) is the check for a stuck block.
# ****************************************************************************************************

# specification, calibration register and mcmc/mcmc_helpers.R
source("../jheem_analyses/applications/SHIELD/analysis/diagnostics/diagnostics_setup.R")

# ---- SETTINGS ----
# MIXING.BURN.KEEP (share of each chain to use) is set in diagnostics_setup.R, so this script,
# mixing_correlated_parameter_pairs.R and run_mcmc_diagnostics.R all use the same value.
# 0.5 = second half (recommended); 1 = whole chain (see WHY NOT THE WHOLE CHAIN above).
# To change the PASS / CHECK / FAIL cut-offs, edit MIXING.THRESHOLDS here, e.g. MIXING.THRESHOLDS$ess.min <- 200
# Outputs are saved on the server (shared with the team): <jheem root>/shield/calibration_diagnostics/<code>/mixing/


# ****************************************************************************************************
# RUN
# ****************************************************************************************************

# 1. Overview (one row per city). Slow the first time; fast after (memory / disk cache)
#    Recomputed for a city when MIXING.BURN.KEEP has changed since it was last computed
overview <- run.mixing.diagnostics(LOCATIONS, CALIBRATION.CODE)
overview[, c("city", "verdict", "reasons")]
overview

# 2. Look closer at one city
loc <- unname(LOCATIONS[1])
m   <- mcmc.list[[.mcmc.key(loc, CALIBRATION.CODE)]]    # whole chain; the functions below burn it
d   <- diag.list[[.mcmc.key(loc, CALIBRATION.CODE)]]

d$by.chain                  # acceptance + log-likelihood per chain
d$log.density               # R-hat and ESS of the log-likelihood and log-posterior
head(d$by.parameter, 15)    # worst-mixing parameters first
d$blocks.low                # blocks with acceptance < MIXING.THRESHOLDS$block.accept.low in a chain
d$accept.by.block           # acceptance per parameter block, lowest first
d$accept.block.by.chain     # chain x block

# 3. Is one chain the problem? (last MIXING.BURN.KEEP of each chain) Would more burn-in help? (whole chain)
drop.one.chain.check(m)
burn.in.check(m)

# 4. Plots (on screen for one city; to PDF for all)
plot.mcmc.diagnostics(m, d, title = .city.name(loc))
# save.mixing.plots(LOCATIONS, CALIBRATION.CODE)

# 5. Once LOCATIONS = SHIELD.TEN.MSAS: parameters that mix badly in several cities
# params.failing.across.cities(LOCATIONS, CALIBRATION.CODE)

# 6. Compare two calibrations
# compare.calibration.mixing(LOCATIONS, c("calib.10.15.stage3", CALIBRATION.CODE))

# 7. Shareable report: one HTML file (+ 2 CSVs) in calibration_diagnostics/<code>/mixing/, to send to the team
write.mixing.report(LOCATIONS, CALIBRATION.CODE)
