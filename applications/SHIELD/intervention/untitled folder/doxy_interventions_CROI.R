## ---------------------------------------------
## Doxy-PEP interventions (All MSM)
## Linear ramp: Oct 2022 -> Oct 2025 
## Effect on susceptibility: (1 - coverage) + (coverage * doxy.pep.relative.risk)
## ---------------------------------------------

source("applications/SHIELD/shield_specification.R")

# Single target population for all MSM
WHOLE.POPULATION = create.target.population(name = 'Whole Population')

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
        scale            = "proportion",
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
     scale            = "proportion",
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
     scale            = "proportion",
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
     scale            = "proportion",
     apply.effects.as = "value",
     allow.values.less.than.otherwise  = FALSE,
     allow.values.greater.than.otherwise = TRUE
 )


 # coverage scenario: 10%
 doxyPEP_10 <- create.intervention(
     coverage.effect.10,
     parameters = DOXY.PARAMS,
     WHOLE.POPULATION,
     code = "doxyPEP.10"
 )
 
 # coverage scenario: 10% 5yr
 doxyPEP_10_slow <- create.intervention(
     coverage.effect.10.slow,
     parameters = DOXY.PARAMS,
     WHOLE.POPULATION,
     code = "doxyPEP.15"
 )
 
 
 # coverage scenario: 25%
 doxyPEP_25 <- create.intervention(
     coverage.effect.25,
     parameters = DOXY.PARAMS,
     WHOLE.POPULATION,
     code = "doxyPEP.20"
 )

 # coverage scenario: 25% 5 yr
 doxyPEP_25_slow <- create.intervention(
     coverage.effect.25.slow, 
     parameters = DOXY.PARAMS,
     WHOLE.POPULATION,
     code = "doxyPEP.25"
 )
 


################################################################################

 {
 simset.NYC = load.simulation.set(file= "prelim_results/NYC.09.19.2025.rdata")
 simset.MIA = load.simulation.set(file= "prelim_results/MIA.09.19.2025.rdata")
 simset.ATL = load.simulation.set(file= "prelim_results/ATL.09.19.2025.rdata")  
 simset.BLT = load.simulation.set(file= "prelim_results/BLT.09.19.2025.rdata")     
    
 no.intervention = get.null.intervention()

 
 sim_doxy10.NYC  <- doxyPEP_10$run(simset.NYC,  start.year = 2022, end.year = 2030)
 sim_doxy10.NYC.slow  <- doxyPEP_10_slow$run(simset.NYC,  start.year = 2022, end.year = 2030)
 sim_doxy25.NYC <- doxyPEP_25$run(simset.NYC,  start.year = 2022, end.year = 2030)
 sim_doxy25.NYC.slow <- doxyPEP_25_slow$run(simset.NYC, start.year = 2022, end.year = 2030)
 no.intervention.NYC = no.intervention$run(simset.NYC, start.year=2022, end.year=2030, verbose=T)
 
 
 sim_doxy10.MIA  <- doxyPEP_10$run(simset.MIA,  start.year = 2022, end.year = 2030)
 sim_doxy10.MIA.slow  <- doxyPEP_10_slow$run(simset.MIA,  start.year = 2022, end.year = 2030)
 sim_doxy25.MIA  <- doxyPEP_25$run(simset.MIA,  start.year = 2022, end.year = 2030)
 sim_doxy25.MIA.slow <- doxyPEP_25_slow$run(simset.MIA, start.year = 2022, end.year = 2030)
 no.intervention.MIA = no.intervention$run(simset.MIA, start.year=2022, end.year=2030, verbose=T)
 
 sim_doxy10.ATL  <- doxyPEP_10$run(simset.ATL,  start.year = 2022, end.year = 2030)
 sim_doxy10.ATL.slow  <- doxyPEP_10_slow$run(simset.ATL,  start.year = 2022, end.year = 2030)
 sim_doxy25.ATL  <- doxyPEP_25$run(simset.ATL,  start.year = 2022, end.year = 2030)
 sim_doxy25.ATL.slow <- doxyPEP_25_slow$run(simset.ATL, start.year = 2022, end.year = 2030)
 no.intervention.ATL = no.intervention$run(simset.ATL, start.year=2022, end.year=2030, verbose=T)
 
 
 sim_doxy10.BLT  <- doxyPEP_10$run(simset.BLT,  start.year = 2022, end.year = 2030)
 sim_doxy10.BLT.slow  <- doxyPEP_10_slow$run(simset.BLT,  start.year = 2022, end.year = 2030)
 sim_doxy25.BLT  <- doxyPEP_25$run(simset.BLT,  start.year = 2022, end.year = 2030)
 sim_doxy25.BLT.slow <- doxyPEP_25_slow$run(simset.BLT, start.year = 2022, end.year = 2030)
 no.intervention.BLT = no.intervention$run(simset.BLT, start.year=2022, end.year=2030, verbose=T)
 
 
}
 
`No Doxy-PEP Intervention` = no.intervention.NYC
`10% Doxy-PEP Coverage` = sim_doxy10.NYC.slow
`25% Doxy-PEP Coverage`= sim_doxy25.NYC.slow
intervention.style.manager = create.style.manager(color.sim.by = "simset")
library(ggplot2)
simplot(
    `No Doxy-PEP Intervention`,
    `10% Doxy-PEP Coverage`,
    outcomes = c("diagnosis.ps"), 
    dimension.values = list(year = 1993:2030),
    style.manager = intervention.style.manager,
    summary.type = "median.and.interval"
) + theme_minimal() + coord_cartesian(ylim = c(0, 15000))

simplot(
    `No Doxy-PEP Intervention`,
    `25% Doxy-PEP Coverage`,
    outcomes = c("diagnosis.ps"),
    dimension.values = list(year = 1993:2030),
    style.manager = intervention.style.manager,
    summary.type = "median.and.interval"
) + theme_minimal() + coord_cartesian(ylim = c(0, 15000))

dimnames(no.intervention.NYC$diagnosis.ps)


