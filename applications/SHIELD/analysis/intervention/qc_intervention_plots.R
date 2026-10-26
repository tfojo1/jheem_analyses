# ****************************************************************************************************
# QC: INTERVENTION SIMSET PLOTS (OPTIONAL) ----
# ****************************************************************************************************
# Trajectories read straight from the intervention simsets, not from the result arrays, so it
# can run any time after the simsets exist. The calibration and N.SIM come from 00_settings.R.
#
# The comparison examples that used to follow (calibration.codes, calib.simsets, ...) used
# objects that were never defined here. They were removed; they are in git history.
# ****************************************************************************************************
source('../jheem_analyses/applications/SHIELD/analysis/intervention/00_settings.R')
source("../jheem_analyses/applications/SHIELD/shield_calib_register.R")
source('../jheem_analyses/applications/SHIELD/analysis/intervention/int_simset_helper_functions.R')

# ---- SETUP ----
# LOCATIONS and N.SIM come from 00_settings.R
CALIBRATION.CODE <- CALIB.NAME

INTERVENTION.LABELS <- c(
    noint        = "No Doxy-PEP Intervention",
    doxy.cov.20 ="20% coverage",
    doxy.cov.50 ="50% coverage"     # was 100%, which the current runs do not include
    
)
QC.INTERVENTION.CODES <- names(INTERVENTION.LABELS)

# ---- READ SIMULATIONS ----

int.simsets <- load.int.simsets(
    locations           = LOCATIONS[1],
    intervention.codes  = QC.INTERVENTION.CODES,
    calibration.code    = CALIBRATION.CODE[1],
    n.sim               = N.SIM,
    intervention.labels = INTERVENTION.LABELS,
    append=T
)




# Outcome sets used across examples below
# outcomes.all <- c("diagnosis.total", "diagnosis.ps",
#                   "diagnosis.el.misclassified", "diagnosis.late.misclassified","hiv.testing")
# outcomes.all <- c("diagnosis.ps","doxy.uptake")

# Note: "plot.int.location" can only take one location at a time. Consider adding argument validation.

for (loc in names(LOCATIONS)[1]){
    plot.int.location(int.simsets = int.simsets,
                      location = loc,
                      calib.code =CALIBRATION.CODE,
                      interventions =QC.INTERVENTION.CODES,
                      outcomes = c("diagnosis.total", "diagnosis.ps", "diagnosis.el.misclassified", "diagnosis.late.misclassified",
                                   "hiv.testing","prop.male.ps.diag.among.msm","doxy.coverage"),
                      # outcomes=c( "doxy.uptake"),#"doxy.coverage"), #because of repeated names
                      years = c(2018:2040),
                      save = T,
                      create.dirs = T,debug = F)
    
    
    plot.int.location(int.simsets = int.simsets,
                      location = loc,
                      calib.code =CALIBRATION.CODE,
                      interventions =QC.INTERVENTION.CODES,
                      outcomes = c("diagnosis.total", "diagnosis.ps"),
                      facet.by = "sex",
                      plot.which = "sim.only",
                      years = c(2018:2040),
                      save = T,create.dirs = T,debug = F)
}


plot.int.comparison(
    int.simsets = int.simsets,
    calibration.codes = CALIBRATION.CODE,
    interventions = QC.INTERVENTION.CODES, 
    locations = names(LOCATIONS),
    outcomes          = c("diagnosis.total"),
    
    separate.by       = "outcome",
    years = 2018:2030,
    # nrow=2,
    folder.name       = "7.16.accross.locations", 
    # facet.by = "sex",
    save              = TRUE,
    create.dirs       = TRUE,
    style.manager     = int.style.manager(
        intervention.labels = QC.INTERVENTION.CODES,
        calibration.codes   = CALIBRATION.CODE
    ),
    debug = F
)
