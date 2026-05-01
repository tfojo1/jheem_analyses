# ============================================================================
# DoxyPEP Intervention Analysis
# ============================================================================
# source('../jheem_analyses/applications/SHIELD/shield_specification.R')
# source('../jheem_analyses/commoncode/locations_of_interest.R')
source("../jheem_analyses/applications/SHIELD/intervention/intervention_definitions.R")
source("../jheem_analyses/applications/SHIELD/intervention/intervention_plots.R")

source("../jheem_analyses/applications/SHIELD/calibration/shield_calibration_plots.R")


# --- Settings ---
# LOCATIONS <- SHIELD.TEN.MSAS  # or your DOXY.LOCATIONS vector
LOCATIONS <- SHIELD.TEN.MSAS
CALIBRATION.CODE <- "calib.4.24.stage2.az"  # Replace with your calibration code
N.SIM <- 300  # or your desired number of simulations
FORCE.OVERWRITE <- TRUE
VERBOSE <- TRUE
DOXY.ANCHOR.YEAR <- DOXY.START  # Year DoxyPEP implementation begins

# Intervention codes for DoxyPEP scenarios
INTERVENTION.CODES <- c('noint', 'doxypep.10', 'doxypep.25','doxypep.50') 

print(paste0("Doing locations: ", paste0(LOCATIONS, collapse = ', ')))

# --- Create and Run Simulation Collection ---
sim.collection <- create.simset.collection(
    version = "shield", 
    calibration.code = CALIBRATION.CODE, 
    locations = LOCATIONS, 
    interventions = INTERVENTION.CODES, 
    n.sim = N.SIM
)

sim.collection$run(
    2020, 
    2035, 
    verbose = TRUE, 
    stop.for.errors = FALSE, 
    overwrite.prior = FORCE.OVERWRITE,
    keep.from.year = 2020
)

#  
# # Get simsets using the shield_calibration_plots code ----
# simset_data <- prepare_simsets_for_plots(CALIBRATION.CODE, LOCATIONS)
# 
# int_sims <- get_intervention_simsets(simset_data, INTERVENTION.CODES)
simset0<-load.simulation.set("~/../../Volumes/jheem$/simulations/shield/calib.4.24.stage2.az-300/C.12580/shield_calib.4.24.stage2.az-300_C.12580_noint.Rdata")
simset10<-load.simulation.set("~/../../Volumes/jheem$/simulations/shield/calib.4.24.stage2.az-300/C.12580/shield_calib.4.24.stage2.az-300_C.12580_doxypep.10.Rdata")
simset20<-load.simulation.set("~/../../Volumes/jheem$/simulations/shield/calib.4.24.stage2.az-300/C.12580/shield_calib.4.24.stage2.az-300_C.12580_doxypep.20.Rdata")


`No Doxy-PEP Intervention` = simset0
`10% Doxy-PEP Coverage` = simset10
`20% Doxy-PEP Coverage`= simset20
intervention.style.manager = create.style.manager(color.sim.by = "simset")
library(ggplot2)
simplot(
    `No Doxy-PEP Intervention`,
    `10% Doxy-PEP Coverage`,
    outcomes = c("diagnosis.ps","doxy.uptake"), split.by="sex",
    dimension.values = list(year = 2020:2035),
    style.manager = intervention.style.manager,
    summary.type = "median.and.interval"
) + theme_minimal() 




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