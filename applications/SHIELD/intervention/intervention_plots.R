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

CALIBRATION.CODE <- "calib.4.24.stage2.az"
N.SIM <- 300
FIRST.YEAR <- 2000
LAST.YEAR <- 2040

BASE.PATH <- "~/../../Volumes/jheem$/simulations/shield"

INTERVENTION.LABELS <- c(
    noint        = "No Doxy-PEP Intervention",
    doxypep.10   = "10% Doxy-PEP Coverage",
    doxypep.25   = "25% Doxy-PEP Coverage",
    doxypep.50   = "50% Doxy-PEP Coverage"
)
INTERVENTION.CODES <- names(INTERVENTION.LABELS)

# =============================================================================
# SECTION 2: Run Interventions
# =============================================================================
# --- Create and Run Simulation Collection ---
if (1==2){
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
# =============================================================================
# SECTION 3: Load All Simsets
# The result is a flat named list. Each entry is one simset, keyed by
# "{City} – {Intervention Label}" for unambiguous lookup and plotting.
# =============================================================================
LOCATIONS=SHIELD.TEN.MSAS[10]
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
intervention.style.manager <- create.style.manager(color.sim.by = "simset")

# --- Example A: all interventions for one city ---
Seattle.simsets <- get.simsets.for.city(all.simsets, "Seattle")

do.call(simplot, c(
    Seattle.simsets[c(1,4)],
    list(
        # outcomes    = c("diagnosis.ps"),
        outcomes  = c( "doxy.uptake"),
        dimension.values = list(year = 2018:2025),
        style.manager    = intervention.style.manager,
        summary.type     = "median.and.interval"
    )
))

# --- Example B: one intervention across all cities ---
no.int.simsets <- get.simsets.for.intervention(all.simsets, "No Doxy-PEP Intervention")

do.call(simplot, c(
    no.int.simsets,
    list(
        outcomes         = c("diagnosis.ps", "doxy.uptake"),
        dimension.values = list(year = 2020:2030),
        style.manager    = intervention.style.manager,
        summary.type     = "median.and.interval"
    )
))

# --- Example C: two specific simsets side by side ---
comparison.simsets <- list(
    get.simset(all.simsets, "Chicago", "No Doxy-PEP Intervention"),
    get.simset(all.simsets, "Chicago", "50% Doxy-PEP Coverage")
)
names(comparison.simsets) <- c("Chicago – No Doxy-PEP Intervention",
                               "Chicago – 50% Doxy-PEP Coverage")

do.call(simplot, c(
    comparison.simsets,
    list(
        outcomes         = c("diagnosis.ps", "doxy.uptake"),
        dimension.values = list(year = 2020:2030),
        style.manager    = intervention.style.manager,
        summary.type     = "median.and.interval"
    )
))


create_intervention_plots(int_sims, create.dirs = T)
