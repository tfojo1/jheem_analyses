# New Doxy Intervention code
# 4-14-2026

source("applications/SHIELD/shield_specification.R")

# (COPIED FROM ORIGINAL) ----

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

# Will want a distribution of 1000 values of DOxy effectiveness

DOXY.PARAMS <- matrix(1-doxy_rr_draws, nrow = 1)
rownames(DOXY.PARAMS) <- "doxy.effectiveness"
# coverage scenario: 10%
coverage.effect.10 =  create.intervention.effect(
    quantity.name    = "doxy.uptake",
    start.time       = DOXY.START,
    effect.values    = 0.10,
    # times            = DOXY.END, # actually means when the rollout finishes
    times = DOXY.END,
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
clear.interventions()

# NEW ----
if (1==2)
{simset <- retrieve.simulation.set(version = "shield",location = "C.12580", calibration.code = "calib.4.8.stage2.az", 300)}

no.intervention = get.null.intervention()


sim_doxy10  <- doxyPEP_10$run(sim,  start.year = 2022, end.year = 2030)
simplot(sim, sim_doxy10, "diagnosis.ps")
# Make an intervention by supplying a target population and an intervention effect.
# 
