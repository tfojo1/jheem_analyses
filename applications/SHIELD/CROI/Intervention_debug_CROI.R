################################################################################
################################################################################
################################################################################
## predefined code
################################################################################
################################################################################
################################################################################
library(ggplot2)
library(tidyverse)

source('applications/SHIELD/shield_specification.R')
source('applications/SHIELD/shield_likelihoods.R')

set.jheem.root.directory('/Volumes/jheem$')

run_calibration <- function(upload_method,
                            date,
                            calibration_code,
                            location,
                            version          = "shield",
                            allow_incomplete = TRUE) {
    
    # Helper to assemble from calibration
    assemble_now <- function() {
        get.calibration.progress(version, location, calibration_code)
        assemble.simulations.from.calibration(
            version          = version,
            location         = location,
            calibration.code = calibration_code,
            allow.incomplete = allow_incomplete
        )
    }
    
    # If user wants to skip loading, just assemble
    if (upload_method == "assemble") {
        return(assemble_now())
    }
    
    # Otherwise build the expected file path
    if (upload_method == "root") {
        rdata_path <- file.path(
            get.jheem.root.directory(),
            version,
            paste0(calibration_code, "_simset_", date, "_", location, ".Rdata")
        )
    } else if (upload_method == "prelim") {
        rdata_path <- file.path(
            "../jheem_analyses/prelim_results",
            paste0(calibration_code, "_simset_", date, "_", location, ".Rdata")
        )
    } else {
        stop("`upload_method` must be one of 'root', 'prelim', or 'assemble'")
    }
    
    # Try to load if it exists
    if (file.exists(rdata_path)) {
        load(rdata_path)   # expects object `simset`
        return(simset)
    }
    
    # Fallback to assembling fresh
    assemble_now()
}


################################################################################
################################################################################
################################################################################
## updated models (improved stage 1 fits)
################################################################################
################################################################################
################################################################################

simset.MIA.updated      <-  run_calibration(
    upload_method  = "root",
    date           = "2026-01-09",
    calibration_code = "calib.01.09.stage1.1",
    location       = "C.33100"
)
simset.NYC.updated      <-  run_calibration(
    upload_method  = "root",
    date           = "2026-01-09",
    calibration_code = "calib.01.09.stage1.1",
    location       = "C.35620"
)
simset.ATL.updated      <-  run_calibration(
    upload_method  = "root",
    date           = "2026-01-09",
    calibration_code = "calib.01.09.stage1.1",
    location       = "C.12060"
)
simset.BLT.updated      <-  run_calibration(
    upload_method  = "root",
    date           = "2026-01-09",
    calibration_code = "calib.01.09.stage1.1",
    location       = "C.12580"
)

simset.NYC.burned.updated = simset.NYC.updated$burn(keep = 100)
simset.MIA.burned.updated = simset.MIA.updated$burn(keep = 100)
simset.ATL.burned.updated = simset.ATL.updated$burn(keep = 100)
simset.BLT.burned.updated = simset.BLT.updated$burn(keep = 100)

rm(simset.NYC.updated)
rm(simset.MIA.updated)
rm(simset.ATL.updated)
rm(simset.BLT.updated)

gc()


################################################################################
################################################################################
################################################################################
## Doxy-PEP interventions
################################################################################
################################################################################
################################################################################

# Linear ramp: Oct 2022 -> Oct 2025 or Oct 2027
# Effect on susceptibility: (1 - coverage) + (coverage * doxy.pep.relative.risk)


# Single target population for all MSM
MSM.POPULATION = create.target.population(sex = "msm", name = 'MSM')

# Timing
DOXY.START <- 2022 + 9/12       # Oct 1, 2022
DOXY.END   <- 2025 + 9/12      #  Oct 1, 2025
DOXY.END.2   <- 2027 + 9/12      #  Oct 1, 2027

# Doxy-pep Efficacy Distribution

fit_rr_lognorm_from_mean_ci <- function(rr_mean, rr_lo, rr_hi, p_lo=0.025, p_hi=0.975){
    if (any(c(rr_mean, rr_lo, rr_hi) <= 0)) stop("RR mean/CI must be > 0 for a lognormal.")
    zspan  <- qnorm(p_hi) - qnorm(p_lo)             
    sdlog  <- (log(rr_hi) - log(rr_lo)) / zspan
    meanlog <- log(rr_mean) - 0.5 * sdlog^2         # E[lognormal] = exp(mu + 0.5*sigma^2)
    list(meanlog = meanlog, sdlog = sdlog)
}

draw_rr_lognorm <- function(n, rr_mean, rr_lo, rr_hi, cap_at_one=TRUE){
    p <- fit_rr_lognorm_from_mean_ci(rr_mean, rr_lo, rr_hi)
    rr <- rlnorm(n, meanlog = p$meanlog, sdlog = p$sdlog)
    if (cap_at_one) rr <- pmin(rr, 1)               # optional: keep RR ≤ 1
    rr
}

doxy_rr_draws <- draw_rr_lognorm(1000, 0.20, 0.08, 0.48)


DOXY.PARAMS <- matrix(doxy_rr_draws, nrow = 1)
rownames(DOXY.PARAMS) <- "doxy.rr"


# coverage scenario: 10%
coverage.effect.10 =  create.intervention.effect(
    quantity.name    = "doxy.coverage",
    start.time       = DOXY.START,
    effect.values    = 0.10,
    times            = DOXY.END,
    scale            = "ratio",
    apply.effects.as = "value",
    allow.values.less.than.otherwise  = FALSE,
    allow.values.greater.than.otherwise = TRUE
)


# coverage scenario: 10% 5 yr
coverage.effect.10.slow =  create.intervention.effect(
    quantity.name    = "doxy.coverage",
    start.time       = DOXY.START,
    effect.values    = 0.10,
    times            =  DOXY.END.2,
    scale            = "ratio",
    apply.effects.as = "value",
    allow.values.less.than.otherwise  = FALSE,
    allow.values.greater.than.otherwise = TRUE
)


# coverage scenario: 25%
coverage.effect.25.slow =  create.intervention.effect(
    quantity.name    = "doxy.coverage",
    start.time       = DOXY.START,
    effect.values    = 0.25,
    times            =  DOXY.END.2,
    scale            = "ratio",
    apply.effects.as = "value",
    allow.values.less.than.otherwise  = FALSE,
    allow.values.greater.than.otherwise = TRUE
)


# coverage scenario: 25% 5yr 
coverage.effect.25 =  create.intervention.effect(
    quantity.name    = "doxy.coverage",
    start.time       = DOXY.START,
    effect.values    = 0.25,
    times            =  DOXY.END,
    scale            = "ratio",
    apply.effects.as = "value",
    allow.values.less.than.otherwise  = FALSE,
    allow.values.greater.than.otherwise = TRUE
)


# coverage scenario: 10%
doxyPEP_10 <- create.intervention(
    coverage.effect.10,
    parameters = DOXY.PARAMS,
    MSM.POPULATION,
    code = "doxyPEP.10.3.msm"
)

# coverage scenario: 10% 5yr
doxyPEP_10_slow <- create.intervention(
    coverage.effect.10.slow,
    parameters = DOXY.PARAMS,
    MSM.POPULATION,
    code = "doxyPEP.10.5.msm"
)

# coverage scenario: 25%
doxyPEP_25 <- create.intervention(
    coverage.effect.25,
    parameters = DOXY.PARAMS,
    MSM.POPULATION,
    code = "doxyPEP.25.3.msm"
)

# coverage scenario: 25% 5 yr
doxyPEP_25_slow <- create.intervention(
    coverage.effect.25.slow, 
    parameters = DOXY.PARAMS,
    MSM.POPULATION,
    code = "doxyPEP.25.5.msm"
)

# No Doxy-PEP
no.intervention = get.null.intervention()


{sim_doxy10.NYC  <- doxyPEP_10$run(simset.NYC.burned.updated,  start.year = 2022, end.year = 2030)
    sim_doxy10.NYC.slow  <- doxyPEP_10_slow$run(simset.NYC.burned.updated,  start.year = 2022, end.year = 2030)
    sim_doxy25.NYC <- doxyPEP_25$run(simset.NYC.burned.updated,  start.year = 2022, end.year = 2030)
    sim_doxy25.NYC.slow <- doxyPEP_25_slow$run(simset.NYC.burned.updated, start.year = 2022, end.year = 2030)
    no.intervention.NYC = no.intervention$run(simset.NYC.burned.updated, start.year=2022, end.year=2030)
    
    
    sim_doxy10.MIA  <- doxyPEP_10$run(simset.MIA.burned.updated,  start.year = 2022, end.year = 2030)
    sim_doxy10.MIA.slow  <- doxyPEP_10_slow$run(simset.MIA.burned.updated,  start.year = 2022, end.year = 2030)
    sim_doxy25.MIA  <- doxyPEP_25$run(simset.MIA.burned.updated,  start.year = 2022, end.year = 2030)
    sim_doxy25.MIA.slow <- doxyPEP_25_slow$run(simset.MIA.burned.updated, start.year = 2022, end.year = 2030)
    no.intervention.MIA = no.intervention$run(simset.MIA.burned.updated, start.year=2022, end.year=2030)
    
    sim_doxy10.ATL  <- doxyPEP_10$run(simset.ATL.burned.updated,  start.year = 2022, end.year = 2030)
    sim_doxy10.ATL.slow  <- doxyPEP_10_slow$run(simset.ATL.burned.updated,  start.year = 2022, end.year = 2030)
    sim_doxy25.ATL  <- doxyPEP_25$run(simset.ATL.burned.updated,  start.year = 2022, end.year = 2030)
    sim_doxy25.ATL.slow <- doxyPEP_25_slow$run(simset.ATL.burned.updated, start.year = 2022, end.year = 2030)
    no.intervention.ATL = no.intervention$run(simset.ATL.burned.updated, start.year=2022, end.year=2030)
    
    
    sim_doxy10.BLT  <- doxyPEP_10$run(simset.BLT.burned.updated,  start.year = 2022, end.year = 2030)
    sim_doxy10.BLT.slow  <- doxyPEP_10_slow$run(simset.BLT.burned.updated,  start.year = 2022, end.year = 2030)
    sim_doxy25.BLT  <- doxyPEP_25$run(simset.BLT.burned.updated,  start.year = 2022, end.year = 2030)
    sim_doxy25.BLT.slow <- doxyPEP_25_slow$run(simset.BLT.burned.updated, start.year = 2022, end.year = 2030)
    no.intervention.BLT = no.intervention$run(simset.BLT.burned.updated, start.year=2022, end.year=2030)
    
    
}

