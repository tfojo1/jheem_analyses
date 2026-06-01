# ****************************************************************************************************
# SHIELD CALIBRATION COMPARISON — SAMPLE CODE
# ****************************************************************************************************
# Demonstrates how to compare calib.5.11.stage2.az vs calib.5.19.stage2.pk
# across all 10 cities using plot.calib.comparison() and plot.calib.location()
# ****************************************************************************************************
source('../jheem_analyses/commoncode/locations_of_interest.R')
source("../jheem_analyses/applications/SHIELD/shield_specification.R")
source("../jheem_analyses/applications/SHIELD/shield_calib_register.R")
source('../jheem_analyses/applications/SHIELD/analysis/analysis_helper_functions.R')


# ---- SETUP ----
calibration.codes <- c(
    "calib.5.11.stage2.az"   # master branch
    # "calib.5.19.stage2.pk"    # sa with higher msm-female partnership (oe.female)
    # "calib.5.12.stage2.pk"
)
 
# read simulations into the simset
calib.simsets <- load.calib.simsets(
    locations         = SHIELD.TEN.MSAS, #seattle
    calibration.codes = calibration.codes
)


# Outcome sets used across examples below
outcomes.all <- c("diagnosis.total", "diagnosis.ps",
                  "diagnosis.el.misclassified", "diagnosis.late.misclassified","hiv.testing")

# ****************************************************************************************************
# 1. COMPARE BOTH CALIBRATIONS ACROSS ALL CITIES — SEPARATE BY OUTCOME ----
# ****************************************************************************************************

# Save all outcome plots to disk
p1=plot.calib.comparison(
    calib.simsets = calib.simsets,
    calibration.codes = calibration.codes,
    outcomes          = outcomes.all,
    
    separate.by       = "outcome",
    years = 2018:2030,
    style.manager     =create.style.manager(color.sim.by = "simset",alpha.line = .1 
    )
)

p1

calib.simsets$`Atlanta – calib.5.11.stage2.az`$full_simset$traceplot("transmission.rate.multiplier.het")
