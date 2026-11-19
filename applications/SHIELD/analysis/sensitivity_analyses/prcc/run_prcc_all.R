# PRCC sensitivity analysis (v3): run steps 1-4 in order
# Run from the jheem_analyses folder. Set TEST.MSA in 00_prcc_settings.R first
# (one MSA = test run, NULL = all 10). Packages and SHIELD base files load once
# (see ../shared/sensitivity_setup.R); each step can also be run on its own.

cat("\n==== PRCC: running steps 1-4 (started ", format(Sys.time(), "%H:%M:%S"), ") ====\n", sep = "")

SENS.DIR <- '../jheem_analyses/applications/SHIELD/analysis/sensitivity_analyses/prcc/'

source(paste0(SENS.DIR, "sensitivity_step1_sort_sims.R"))
source(paste0(SENS.DIR, "sensitivity_step2_top_bottom_ranking.R"))
source(paste0(SENS.DIR, "sensitivity_step3_find_prccs.R"))
source(paste0(SENS.DIR, "sensitivity_step4_tornado_plots.R"))
