# ============================================================================
# DoxyPEP Intervention Analysis
# ============================================================================
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
source("../jheem_analyses/applications/SHIELD/intervention/intervention_definitions.R")
source("../jheem_analyses/applications/SHIELD/intervention/intervention_helper_functions.R")
# source("../jheem_analyses/applications/SHIELD/intervention/intervention_plots.R")
# source("../jheem_analyses/applications/SHIELD/calibration/shield_calibration_plots.R")

# =============================================================================
# SECTION 1: Configuration
# =============================================================================
# LOCATIONS        <- SHIELD.TEN.MSAS       # Named vector: names = city, values = codes

LOCATIONS        <- SHIELD.TEN.MSAS     # Named vector: names = city, values = codes

CALIBRATION.CODE <- "calib.4.24.stage2.az"
N.SIM            <- 300
FIRST.YEAR <- 2000
LAST.YEAR <- 2040

BASE.PATH <- "~/../../Volumes/jheem$/simulations/shield"

INTERVENTION.CODES <- c("noint" 
                        # "doxypep.10",
                        # "doxypep.25",
                        # "doxypep.50"
                        )

INTERVENTION.LABELS <- c(
    "noint"      = "No Doxy-PEP Intervention"
    # "doxypep.10" = "10% Doxy-PEP Coverage",
    # "doxypep.25" = "25% Doxy-PEP Coverage",
    # "doxypep.50" = "50% Doxy-PEP Coverage"
)


# =============================================================================
# SECTION 2: Run Interventions
# =============================================================================
# --- Create and Run Simulation Collection ---
# sim.collection <- create.simset.collection(
#     version = "shield", 
#     calibration.code = CALIBRATION.CODE, 
#     locations = LOCATIONS, 
#     interventions = INTERVENTION.CODES, 
#     n.sim = N.SIM
# )
# VERBOSE<-T
# FORCE.OVERWRITE<- F
# sim.collection$run(
#     FIRST.YEAR, 
#     LAST.YEAR, 
#     verbose = VERBOSE, 
#     stop.for.errors = FALSE, 
#     overwrite.prior = FORCE.OVERWRITE,
#     keep.from.year = FIRST.YEAR
# )
# 
# =============================================================================
# SECTION 3: Load All Simsets
# The result is a flat named list. Each entry is one simset, keyed by
# "{City} – {Intervention Label}" for unambiguous lookup and plotting.
# =============================================================================
LOCATIONS=SHIELD.TEN.MSAS[6]
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

# #  
# # Get simsets using the shield_calibration_plots code ----
# simset_data <- prepare_simsets_for_plots(CALIBRATION.CODE, LOCATIONS)
# 
# int_sims <- get_intervention_simsets(simset_data, INTERVENTION.CODES)
simset0<-load.simulation.set(paste0("~/../../Volumes/jheem$/simulations/shield/calib.4.24.stage2.az-300/",LOCATIONS,"/shield_calib.4.24.stage2.az-300_",LOCATIONS,"_noint.Rdata"))
simset10<-load.simulation.set(paste0("~/../../Volumes/jheem$/simulations/shield/calib.4.24.stage2.az-300/",LOCATIONS,"/shield_calib.4.24.stage2.az-300_",LOCATIONS,"_doxypep.10.Rdata"))
simset25<-load.simulation.set(paste0("~/../../Volumes/jheem$/simulations/shield/calib.4.24.stage2.az-300/",LOCATIONS,"/shield_calib.4.24.stage2.az-300_",LOCATIONS,"_doxypep.25.Rdata"))
simset50<-load.simulation.set(paste0("~/../../Volumes/jheem$/simulations/shield/calib.4.24.stage2.az-300/",LOCATIONS,"/shield_calib.4.24.stage2.az-300_",LOCATIONS,"_doxypep.50.Rdata"))


`No Doxy-PEP Intervention` = simset0
`10% Doxy-PEP Coverage` = simset10
`25% Doxy-PEP Coverage`= simset25
`50% Doxy-PEP Coverage`= simset50
intervention.style.manager = create.style.manager(color.sim.by = "simset")
# library(ggplot2)
simplot(
    `No Doxy-PEP Intervention`, 
    # `10% Doxy-PEP Coverage`,
    # `25% Doxy-PEP Coverage`,
    `50% Doxy-PEP Coverage`,
    outcomes = c("diagnosis.ps","doxy.uptake"), 
    # split.by="sex",
    dimension.values = list(year = 2020:2030),
    style.manager = intervention.style.manager,
    summary.type = "median.and.interval"
) #+ theme_minimal() 

simplot(
    `No Doxy-PEP Intervention`, 
    # `10% Doxy-PEP Coverage`,
    # `25% Doxy-PEP Coverage`,
    `50% Doxy-PEP Coverage`,
    # outcomes = c("diagnosis.ps"),
    outcomes = c("doxy.uptake"),
    facet.by="sex",plot.which = "sim.only",
    dimension.values = list(year = 2020:2030),
    style.manager = intervention.style.manager,
    summary.type = "median.and.interval"
) #+ theme_minimal() 


# --- Define Primary Outcomes ---
PRIMARY.OUTCOMES <- c(
    "incidence",                    # Syphilis incidence
    "diagnosis.total",              # Total diagnoses
    "diagnosis.ps",                 # Primary & secondary diagnoses
    "diagnosis.el.misclassified",   # Early latent (misclassified)
    "diagnosis.late.misclassified", # Late latent (misclassified)
    "hiv.testing",                  # HIV testing rates
    "population",                   # Population size
    "doxy.uptake"                   # DoxyPEP uptake
)




# --- Simplot ----
# uses individual simsets 

# need a function to load the sims
# and then some function to generate plots


# --- Pull Total Results ---
print("PULLING TOTALS...")
total.results <- sim.collection$get(
    outcomes = PRIMARY.OUTCOMES,
    output = 'numerator',
    dimension.values = list(year = 2010:2030),
    keep.dimensions = c('year'),
    verbose = VERBOSE
)

# --- Pull Parameters ---
print("PULLING PARAMETERS...")
all.parameters <- sim.collection$get.parameters(verbose = VERBOSE)

# --- Pull Incidence by Age ---
print("PULLING INCIDENCE BY AGE...")
incidence.by.age <- sim.collection$get(
    outcomes = 'incidence',
    output = 'numerator',
    dimension.values = list(year = 2020:2040),
    keep.dimensions = c('year', 'age'),
    verbose = VERBOSE
)

# --- Pull Incidence by Race ---
print("PULLING INCIDENCE BY RACE...")
incidence.by.race <- sim.collection$get(
    outcomes = 'incidence',
    output = 'numerator',
    dimension.values = list(year = 2020:2040),
    keep.dimensions = c('year', 'race'),
    verbose = VERBOSE
)

# --- Pull Incidence by Sex/Risk ---
print("PULLING INCIDENCE BY SEX/RISK...")
incidence.by.sex.risk <- sim.collection$get(
    outcomes = 'incidence',
    output = 'numerator',
    dimension.values = list(year = 2020:2040),
    keep.dimensions = c('year', 'sex', 'race'),
    verbose = VERBOSE
)

# --- Pull Diagnoses by Stage ---
print("PULLING DIAGNOSES BY STAGE...")
diagnoses.by.stage <- sim.collection$get(
    outcomes = c('diagnosis.total', 'diagnosis.ps', 
                 'diagnosis.el.misclassified', 'diagnosis.late.misclassified'),
    output = 'numerator',
    dimension.values = list(year = 2020:2040),
    keep.dimensions = c('year'),
    verbose = VERBOSE
)

# --- Pull DoxyPEP Uptake by Risk Group ---
print("PULLING DOXYPEP UPTAKE BY RISK GROUP...")
doxy.uptake.by.risk <- sim.collection$get(
    outcomes = 'doxy.uptake',
    output = 'numerator',
    dimension.values = list(year = 2020:2040),
    keep.dimensions = c('year', 'risk'),
    verbose = VERBOSE
)

# --- Extract Individual Arrays from Total Results ---
print("EXTRACTING INDIVIDUAL OUTCOMES...")
total.incidence <- array.access(total.results, outcome = 'incidence', drop = TRUE)
total.diagnosis <- array.access(total.results, outcome = 'diagnosis.total', drop = TRUE)
total.diagnosis.ps <- array.access(total.results, outcome = 'diagnosis.ps', drop = TRUE)
total.diagnosis.el <- array.access(total.results, outcome = 'diagnosis.el.misclassified', drop = TRUE)
total.diagnosis.late <- array.access(total.results, outcome = 'diagnosis.late.misclassified', drop = TRUE)
total.hiv.testing <- array.access(total.results, outcome = 'hiv.testing', drop = TRUE)
total.pop <- array.access(total.results, outcome = 'population', drop = TRUE)
total.doxy.uptake <- array.access(total.results, outcome = 'doxy.uptake', drop = TRUE)

# --- Save Results ---
filename <- paste0(
    'Q:/results/doxypep/doxypep_results_', 
    paste0(LOCATIONS, collapse = "_"), 
    '_', DOXY.ANCHOR.YEAR, 
    "_", Sys.Date(), 
    ".Rdata"
)

print(paste0("SAVING RESULTS TO: ", filename))

save(
    # Full results
    total.results,
    diagnoses.by.stage,
    
    # Incidence
    total.incidence,
    incidence.by.race,
    incidence.by.age,
    incidence.by.sex.risk,
    
    # Diagnoses by stage
    total.diagnosis,
    total.diagnosis.ps,
    total.diagnosis.el,
    total.diagnosis.late,
    
    # Other outcomes
    total.hiv.testing,
    total.pop,
    total.doxy.uptake,
    doxy.uptake.by.risk,
    
    # Parameters
    all.parameters,
    
    file = filename
)

print("DONE!")