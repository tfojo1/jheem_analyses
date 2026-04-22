# New Doxy Intervention code
# 4-14-2026
# inputs and sources are: ~/jheem/code/jheem_analyses/applications/SHIELD/inputs/input_doxy_pep_parameters.R

# source("applications/SHIELD/shield_specification.R")

# Single target population for all MSM
WHOLE.POPULATION = create.target.population(name = 'Whole Population') #MSM?

# Timing
DOXY.START <- 2022 + 9/12       # Oct 1, 2022
DOXY.END   <- 2030      #  Jan 1, 2030



# DOXY-PEP EFFICATY (Studies report: RR: Rate Ratio of incident syphilis cases in doxy vs no-doxy arms per person-time)
# we have pooled estiamtes from clinical trials to estimate the meanlog and sdlog
# Draw RR samples from the final lognormal distribution to use in the model
# log(HR) ~ Normal(mean,sd) >>> HR=lognormal(meanlog,sdlog)
draw_rr_lnorm <- function(n, 
                          rr_meanlog,rr_sdlog, # mean and sd log for a lognormal dist
                                    cap_at_one = TRUE) {
    rr_samples <- rlnorm(n, meanlog = rr_meanlog, sdlog = rr_sdlog) 
    #
    if (cap_at_one) rr_samples <- pmin(rr_samples, 1)
    rr_samples
}

# generate 1000 values of Doxy effectiveness
rr_samples <- draw_rr_lnorm(n = 1000, rr_meanlog = -1.540424,rr_sdlog =0.2510223,cap_at_one = T )
effectiveness_samples=1-rr_samples


# PERSISTANCE and DISCONTINUATION RATE #----
# P: proportion of population, taking Doxy at the end of the year
# Discontinuation rate= -log(Peristance)

draw_persistance_uniform<-function(n){
    persistence_samples = runif(n,0,1)
    persistence_samples
}
persistence_samples<-draw_persistance_uniform(10000)
discont_rate_samples = -log(persistence_samples)

# Putting them together ----
DOXY.PARAMS <- rbind(effectiveness_samples,discont_rate_samples)
rownames(DOXY.PARAMS) <- c("doxy.effectiveness","doxy.discontinuationRate")




# INTERVENTION ----
# intervnetion controls the coverage 0 >>> 10% from 2022-2030
# we know C=U/(1+r) > U=C*(1+r) 
# where r is discontinuation rate
# where U is the proportion of population filling a prescription for doxy (regardless app having been on Doxy before)
# so in each year the uptake can be calculated as target Coverage * (1+r)

# Intervention control coverage levels
coverage.effect.10 =  create.intervention.effect(
    quantity.name    = "doxy.coverage",
    start.time       = DOXY.START,
    effect.values    = 0.10,
    times            = DOXY.END, # when scale up ends
    scale            = "proportion",
    apply.effects.as = "value",
    allow.values.less.than.otherwise  = FALSE,
    allow.values.greater.than.otherwise = TRUE
)
 
clear.interventions()
# coverage scenario: 10%
doxyPEP_10 <- create.intervention(
        coverage.effect.10,
    parameters = DOXY.PARAMS,
    WHOLE.POPULATION, 
    code = "doxyPEP.10"
)

no_intervention = get.null.intervention()


# NEW ----
if (1==2)
{simset <- retrieve.simulation.set(version = "shield",location = "C.12580", calibration.code = "calib.4.8.stage2.az", 300)



sim_doxy10  <- doxyPEP_10$run(sim,  start.year = 2022, end.year = 2030)
simplot(sim, sim_doxy10, "diagnosis.ps")}
# Make an intervention by supplying a target population and an intervention effect.
# 
