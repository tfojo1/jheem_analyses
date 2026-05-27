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
    "calib.5.11.stage2.az",   # master branch
    "calib.5.19.stage2.pk"    # sa with higher msm-female partnership
)

# read simulations into the simset
calib.simsets <- load.calib.simsets(
    locations         = SHIELD.TEN.MSAS,
    calibration.codes = calibration.codes
)

# Outcome sets used across examples below
outcomes.all <- c("diagnosis.total", "diagnosis.ps",
                        "diagnosis.el.misclassified", "diagnosis.late.misclassified","hiv.testing")
                        

my.style.manager     = create.style.manager(
    color.sim.by    = "stratum",
    linetype.sim.by = "simset",
    alpha.ribbon    = 0.05   ,    # ← adjust between 0 (invisible) and 0.2 (default)
    linewidth.slope = 0    
)
# ****************************************************************************************************
# 1. COMPARE BOTH CALIBRATIONS ACROSS ALL CITIES — SEPARATE BY OUTCOME ----
# ****************************************************************************************************
# One plot per outcome. Each plot is a grid of 10 city panels.
# Both calibration codes appear as separate colored lines within each city panel.

# View in RStudio (returns named list of plots)
# comparison.by.outcome <- plot.calib.comparison(
#     calib.simsets     = calib.simsets,
#     calibration.codes = calibration.codes,
#     outcomes          = outcomes.all[1],
#     separate.by       = "outcome"
# )
# 
# # Access individual outcome plots
# comparison.by.outcome[["diagnosis.total"]]
# comparison.by.outcome[["diagnosis.ps"]]

# Save all outcome plots to disk
plot.calib.comparison(
    calib.simsets     = calib.simsets,
    calibration.codes = calibration.codes,
    outcomes          = outcomes.all,
    separate.by       = "outcome",
    folder.name       = "5.11.vs.5.19.stage2", 
    save              = TRUE, create.dirs       = TRUE,  nrow=2
)

# by sex
plot.calib.comparison(
    calib.simsets     = calib.simsets,
    calibration.codes = calibration.codes,
    outcomes          = c("diagnosis.ps"),
    split.by          = "sex",
    # plot.which        = "sim.only",
        
    separate.by       = "outcome",
    folder.name       = "5.11.vs.5.19.stage2",
    nrow              = 2,
    legend.nrow       = 3,
    legend.height     = 0.15,    # fraction of total height — increase if still cut off
    save              = TRUE,
    create.dirs       = TRUE,
    style.manager     = create.style.manager(
        color.sim.by    = "stratum",
        linetype.sim.by = "simset",
        alpha.ribbon    = 0.05,
        linewidth.slope = 0
    )
)
 
# ****************************************************************************************************
# 2. COMPARE BOTH CALIBRATIONS ACROSS ALL CITIES — SEPARATE BY LOCATION
# ****************************************************************************************************
# One plot per city. Each plot shows all outcomes as separate panels.
# Both calibration codes appear as separate colored lines within each outcome panel.

# Save all cities
plot.calib.comparison(
    calib.simsets     = calib.simsets,
    # locations = "Baltimore",
    calibration.codes = calibration.codes,
    outcomes          = outcomes.all,
    separate.by       = "location",
    folder.name       = "5.11.vs.5.19.stage2",
    save              = TRUE,
    create.dirs       = TRUE 
)


# ****************************************************************************************************
# 3. VIEW EACH CALIBRATION INDEPENDENTLY — SEPARATE BY CALIBRATION
# ****************************************************************************************************
# One plot per calibration code. Each plot is a grid of 10 city panels.
# Useful for reviewing each calibration on its own before comparing.

comparison.by.calib <- plot.calib.comparison(
    calib.simsets     = calib.simsets,
    calibration.codes = calibration.codes,
    outcomes          = outcomes.diagnosis,
    separate.by       = "calibration"
)

comparison.by.calib[["calib.5.11.stage2.az"]]
comparison.by.calib[["calib.5.19.stage2.pk"]]

