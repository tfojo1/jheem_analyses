# ============================================================================
# DoxyPEP Intervention Analysis
# ============================================================================
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
source("../jheem_analyses/applications/SHIELD/intervention/intervention_definitions.R")
source("../jheem_analyses/applications/SHIELD/intervention/intervention_helper_functions.R")

# =============================================================================
# SECTION 1: Configuration
# =============================================================================
LOCATIONS        <- SHIELD.TEN.MSAS    # Named vector: names = city, values = codes


CALIBRATION.CODE <- "calib.6.9.stage2.az" # 
# CALIBRATION.CODE <- "calib.5.19.stage2.pk" # older version

N.SIM <- 300
FIRST.YEAR <- 2000
LAST.YEAR <- 2040

BASE.PATH <- paste0(ROOT.DIR,"/simulations/shield")


INTERVENTION.LABELS <- c(
    noint        = "No Doxy-PEP Intervention",
    doxy.u.100.p.100   = "Uptake 100% Persistence 100%",
    doxy.rapid.uptake="Uptake 15%, 40% and then 100%"
    
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
        locations = LOCATIONS,
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
