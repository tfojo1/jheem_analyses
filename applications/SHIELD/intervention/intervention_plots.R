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
LOCATIONS        <- SHIELD.TEN.MSAS     # Named vector: names = city, values = codes

# CALIBRATION.CODE <- "calib.4.24.stage2.az"

CALIBRATION.CODE <- "calib.5.4.stage1.az"

N.SIM <- 300
FIRST.YEAR <- 2000
LAST.YEAR <- 2040

BASE.PATH <- paste0(ROOT.DIR,"/simulations/shield")


INTERVENTION.LABELS <- c(
    noint        = "No Doxy-PEP Intervention",
    doxy.25.2020   = "25% Doxy-PEP Coverage 2020-2025",
    doxy.50.2020   = "50% Doxy-PEP Coverage 2020-2025",
    doxy.75.2020   = "75% Doxy-PEP Coverage 2020-2025",
    doxy.25.2022   = "25% Doxy-PEP Coverage 2022-2025",
    doxy.50.2022   = "50% Doxy-PEP Coverage 2022-2025",
    doxy.75.2022   = "75% Doxy-PEP Coverage 2022-2025"
)
INTERVENTION.CODES <- names(INTERVENTION.LABELS)

# =============================================================================
# SECTION 2: Run Interventions
# =============================================================================
# --- Create and Run Simulation Collection ---
# if (1==2){
#     sim.collection <- create.simset.collection(
#         version = "shield",
#         calibration.code = CALIBRATION.CODE,
#         locations = LOCATIONS,
#         interventions = INTERVENTION.CODES,
#         n.sim = N.SIM
#     )
#     #
#     FORCE.OVERWRITE<- FALSE
#     #
#     sim.collection$run(
#         FIRST.YEAR,
#         LAST.YEAR,
#         verbose = T,
#         stop.for.errors = FALSE,
#         overwrite.prior = FORCE.OVERWRITE,
#         keep.from.year = FIRST.YEAR
#     )
# }
# =============================================================================
# SECTION 3: Load All Simsets
# The result is a flat named list. Each entry is one simset, keyed by
# "{City} – {Intervention Label}" for unambiguous lookup and plotting.
# =============================================================================
LOCATIONS=SHIELD.TEN.MSAS[1] 
all.simsets <- load.all.simsets(
    locations           = LOCATIONS,
    intervention.codes  = INTERVENTION.CODES,
    calibration.code    = CALIBRATION.CODE,
    n.sim               = N.SIM,
    base.path           = BASE.PATH,
    intervention.labels = INTERVENTION.LABELS,
    # cache               = all.simsets,   # explicit — takes priority Anything already in `all.simsets` is reused; only new keys are loaded from file
    append=T
    # force.reload        = FALSE # set TRUE to ignore cache and reload everything
)
LOCATIONS=SHIELD.TEN.MSAS
all.simsets <- load.all.simsets(
    locations           = LOCATIONS,
    intervention.codes  = INTERVENTION.CODES,
    calibration.code    = CALIBRATION.CODE,
    n.sim               = N.SIM,
    base.path           = BASE.PATH,
    intervention.labels = INTERVENTION.LABELS,
    # cache               = all.simsets,   # explicit — takes priority Anything already in `all.simsets` is reused; only new keys are loaded from file
    append=T
    # force.reload        = FALSE # set TRUE to ignore cache and reload everything
)
# Quick inventory of what was loaded
cat(paste0(names(all.simsets), "\n"))

# =============================================================================
# SECTION 4: Plotting
# simplot() takes simsets as named ... arguments, so use do.call() to
# pass a named list. Swap in any subset from the helpers above.
# =============================================================================
intervention.style.manager <- create.style.manager(color.sim.by = "simset",alpha.line=1)

# Plot output directories
ROOT.DIR <- get.jheem.root.directory()
PLOT.BASE.DIR <- file.path(ROOT.DIR, "shield", "interventionPlots", CALIBRATION.CODE)
PLOT.COMPARISON.DIR <- file.path(PLOT.BASE.DIR, "comparisons")

# --- Single city, display only ---
plot_int_single_city(
    all.simsets,
    city     = "Seattle",
    outcomes = c("diagnosis.ps", "doxy.uptake")
    # save = T,create.dirs = T
)

# --- Single city, save to: ROOT.DIR/shield/interventionPlots/CALIBRATION.CODE/Chicago/ ---
plot_int_single_city(
    all.simsets,
    city          = "Chicago",
    interventions = c("No Doxy-PEP Intervention", "50% Doxy-PEP Coverage"),
    outcomes      = c("diagnosis.ps", "doxy.uptake"),
    years         = 2018:2030,
    save          = TRUE,
    create.dirs   = TRUE
)

# --- Compare cities, save to: ROOT.DIR/shield/interventionPlots/CALIBRATION.CODE/comparisons/ ---
plot_int_mult_cities(
    all.simsets,
    cities        = c("Seattle", "Chicago", "Atlanta"),
    interventions = c("No Doxy-PEP Intervention", "50% Doxy-PEP Coverage"),
    outcomes      = "diagnosis.ps",
    ncol          = 3,
    save          = TRUE,
    create.dirs   = TRUE
)

# --- All cities, saves each to: ROOT.DIR/shield/interventionPlots/CALIBRATION.CODE/{city}/ ---
plot_int_all_cities(
    all.simsets,
    interventions = c("No Doxy-PEP Intervention", "50% Doxy-PEP Coverage"),
    outcomes      = c("diagnosis.ps", "doxy.uptake"),
    years         = 2020:2035,
    create.dirs   = TRUE
)