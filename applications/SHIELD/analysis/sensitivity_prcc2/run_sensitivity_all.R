# Sensitivity analysis (PRCC, v3): run steps 1-4 in order
# Run from the jheem_analyses folder. Set TEST.MSA in 00_sensitivity_settings.R first
# (one MSA = test run, NULL = all 10). Packages and SHIELD base files load once
# (see 00_sensitivity_setup.R); each step can also be run on its own.

SENS.DIR <- '../jheem_analyses/applications/SHIELD/analysis/sensitivity_prcc2/'

source(paste0(SENS.DIR, "sensitivity_step1_sort_sims.R"))
source(paste0(SENS.DIR, "sensitivity_step2_top_bottom_ranking.R"))
source(paste0(SENS.DIR, "sensitivity_step3_find_prccs.R"))
source(paste0(SENS.DIR, "sensitivity_step4_tornado_plots.R"))
