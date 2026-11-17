# ****************************************************************************************************
# DIAGNOSTICS2: MCMC MIXING REPORT ----
# ****************************************************************************************************
# Do the chains mix, and if not, why? One self-contained HTML report for the team, plus CSVs.
# Methods, assumptions and limitations: METHODS.md (Part A). Settings: 0-setup.R.
#
# What it does, one city at a time (only one MCMC in memory at a time):
#   1. loads the MCMC from the calibration's cache (or builds the cache; METHODS 1.5)
#   2. mixing statistics on the last MIXING.BURN.KEEP of each chain (METHODS 2), verdict (METHODS 3)
#   3. leave one chain out, burn-in check (METHODS 4)
#   4. identifiability: combinations of parameters (METHODS 5), if INCLUDE.IDENTIFIABILITY
#   5. the city's section of the report: A Fit, B Parameters, C Sampler, D Causes, E Identifiability
# Then the overview, parameters failing across cities, identifiability across cities, and the files.
#
# No model is run. Time: about the same as the old mixing report, plus the pair search in identifiability
# (all pairs of parameters with R-hat > 1.5; more failing parameters = more pairs).
#
# OUTPUTS: <jheem root>/shield/diagnostics/<calibration code>/mixing/
#   mixing_report_<code>_<date>.html                 the report
#   mixing_overview_<code>_<date>.csv                one row per city
#   mixing_parameters_<code>_<date>.csv              every parameter in every city
#   identifiability_combinations_<code>_<date>.csv   every designed combination in every city
#   identifiability_best_partner_<code>_<date>.csv   best partner of every parameter with R-hat > 1.5
#   Each CSV starts with two rows describing the columns; read back with read.csv(file, skip = 2)
#
# USAGE (working directory: jheem_analyses)
#   1. set CALIBRATION.CODE, LOCATIONS and MIXING.BURN.KEEP in 0-setup.R (test with one city first)
#   2. source this file
# ****************************************************************************************************

source("../jheem_analyses/applications/SHIELD/analysis/diagnostics2/0-setup.R")

## SETTINGS ----
INCLUDE.IDENTIFIABILITY <- TRUE

## RUN ----
report <- write.mixing.report(LOCATIONS, CALIBRATION.CODE, include.identifiability = INCLUDE.IDENTIFIABILITY)
report$overview[, c("city", "verdict", "reasons")]
