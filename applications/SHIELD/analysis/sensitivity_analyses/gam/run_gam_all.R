# GAM sensitivity analysis: run steps 1-3 in order
# Run from the jheem_analyses folder. Set TEST.MSA in 00_gam_settings.R first
# (one MSA = test run, NULL = all 10). Packages and SHIELD base files load once
# (see ../shared/sensitivity_setup.R); each step can also be run on its own.

cat("\n==== GAM: running steps 1-3 (started ", format(Sys.time(), "%H:%M:%S"), ") ====\n", sep = "")

SENS.DIR <- '../jheem_analyses/applications/SHIELD/analysis/sensitivity_analyses/gam/'

source(paste0(SENS.DIR, "sensitivity_step1_param_correlations.R"))
source(paste0(SENS.DIR, "sensitivity_step2_variance_shares.R"))
source(paste0(SENS.DIR, "sensitivity_step3_heatmap.R"))
