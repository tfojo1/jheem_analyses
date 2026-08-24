

# ============================================================================
# DoxyPEP Intervention Analysis
# ============================================================================
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
source("../jheem_analyses/applications/SHIELD/intervention/intervention_definitions.R")
# source("../jheem_analyses/applications/SHIELD/intervention/intervention_helper_functions.R")

# =============================================================================
# SECTION 1: Configuration
# =============================================================================
BASE.PATH <- paste0(ROOT.DIR,"/simulations/shield")

LOCATION         <- "C.12580"
CALIBRATION.CODE <- "calib.8.14.stage3.az"
N.SIM <- 400
FIRST.YEAR <- 2000
LAST.YEAR <- 2040

INTERVENTION.LABELS <- c(
    noint        = "No Doxy-PEP Intervention",
    doxy.cov.100   = "coverage 100%",
    doxy.cov.50   = "coverage 50%"
    
)
INTERVENTION.CODES <- names(INTERVENTION.LABELS)

# =============================================================================
# SECTION 2: Run Interventions
# =============================================================================
# --- Create and Run Simulation Collection ---
if (1==1){
    sim.collection <- create.simset.collection(
        version = "shield",
        calibration.code = CALIBRATION.CODE,
        locations = LOCATION,
        interventions = INTERVENTION.CODES,
        n.sim = N.SIM
    )
    #
    FORCE.OVERWRITE<- FALSE
    #
    sim.collection$run(
        FIRST.YEAR,
        LAST.YEAR,
        verbose = T,
        stop.for.errors = FALSE,
        overwrite.prior = FORCE.OVERWRITE,
        keep.from.year = FIRST.YEAR
    )
}
