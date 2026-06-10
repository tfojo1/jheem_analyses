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
    "calib.6.9.stage2.az"
)

intervention.codes <-c(
    "noint",
    # "doxy.u.60.p.50",
    # "doxy.u.60.p.100",
    # "doxy.u.100.p.50",
    "doxy.u.100.p.100",
    "doxy.rapid.uptake"
)
# read simulations into the simset
int.simsets <- load.int.simsets(
    locations         = SHIELD.TEN.MSAS, #seattle
    n.sim = 300,
    intervention.codes = intervention.codes,
    calibration.codes = calibration.codes
)


# Outcome sets used across examples below
# outcomes.all <- c("diagnosis.total", "diagnosis.ps",
#                   "diagnosis.el.misclassified", "diagnosis.late.misclassified","hiv.testing")
outcomes.all <- c("diagnosis.ps","doxy.uptake")
                  

plot.int.location(int.simsets = int.simsets,
                  location = "Seattle",
                  calib.code ="calib.6.9.stage2.az",
                  interventions =intervention.codes,
                  outcomes = "diagnosis.total",
                  facet.by = "sex",
                  # plot.which = "sim.only",
                  years = c(2018:2025),save = T,create.dirs = T )



# ****************************************************************************************************
# 1. COMPARE BOTH CALIBRATIONS ACROSS ALL CITIES — SEPARATE BY OUTCOME ----
# ****************************************************************************************************

# Save all outcome plots to disk
plot.int.comparison(
    int.simsets = int.simsets,
    calibration.codes = calibration.codes,
    interventions = intervention.codes, 
    outcomes          = c("diagnosis.total","diagnosis.ps"),
    
    separate.by       = "outcome",
    years = 2018:2030,
    nrow=2,
    folder.name       = "5.11.vs.5.19.stage2", 
    save              = TRUE,
    create.dirs       = TRUE,
    style.manager     = int.style.manager(
        intervention.labels = intervention.codes,
        calibration.codes   = calibration.codes
    )
)

# by sex 
plot.int.comparison(
    int.simsets       = int.simsets,
    calibration.codes = calibration.codes,
    interventions     = intervention.codes,
    outcomes          = c("diagnosis.total","diagnosis.ps"),
    
    separate.by       = "outcome",
    facet.by = "sex",
    # plot.which="sim.only", 
    years = 2020:2030,
    ncol = 1, #each simplot creates multiple facets for sex
    folder.name       = "5.11.vs.5.19.stage2",
    
    save              = TRUE,
    create.dirs       = TRUE,
    style.manager     = int.style.manager(
        intervention.labels = intervention.codes,
        calibration.codes   = calibration.codes
    )
)

#
int.simsets$`Seattle – calib.5.11.stage2.az – noint`$full_simset$traceplot("oe.") 
# int.simsets$`Seattle – calib.5.12.stage2.pk – noint`$full_simset$traceplot("oe.")
int.simsets$`Seattle – calib.5.19.stage2.pk – noint`$full_simset$traceplot("oe.")

int.simsets$`Seattle – calib.5.11.stage2.az – noint`$full_simset$traceplot("fraction.msm") 
# int.simsets$`Seattle – calib.5.12.stage2.pk – noint`$full_simset$traceplot("fraction.msm")
int.simsets$`Seattle – calib.5.19.stage2.pk – noint`$full_simset$traceplot("fraction.msm")


# ****************************************************************************************************
# 2.SEATTLE
# ****************************************************************************************************

plot.int.location(
    int.simsets = int.simsets,
    # calib.code = calibration.codes[1],
    calib.code = calibration.codes[2],
    interventions = intervention.codes, 
    location = "Seattle",
    outcomes          = "diagnosis.total",
    facet.by="sex",
    years = 2020:2025,
    save              = TRUE,
    create.dirs       = TRUE,
    style.manager     = create.style.manager(color.sim.by = "simset",
                                             alpha.line = 1    )
)

sim= calib.simsets$`Seattle – calib.5.11.stage2.az`$last_sim

get.best.guess.msm.proportions("C.42660",get.specification.metadata("shield","C.42660"),keep.age = F,keep.race = F)
apply(sim$population,c("year","sex"),sum)/rowSums(sim$population)

apply(sim$diagnosis.ps,c("year","sex"),sum)/rowSums(sim$diagnosis.ps)

