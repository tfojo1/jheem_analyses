# inputs and sources are: ~/jheem/code/jheem_analyses/applications/SHIELD/inputs/input_doxy_pep_parameters.R

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

# draw_effectiveness_uniform<-function(n){
#     persistence_samples = runif(n,1,1)
#     persistence_samples
# }
# rr_samples<-draw_effectiveness_uniform(1000)
# effectiveness_samples=rr_samples


# PERSISTANCE and DISCONTINUATION RATE #----
# P: proportion of population, taking Doxy at the end of the year
# Discontinuation rate= -log(Peristance)

draw_persistance_uniform<-function(n){
    persistence_samples = runif(n,1,1)
    persistence_samples
}
persistence_samples<-draw_persistance_uniform(10000)
discont_rate_samples = -log(persistence_samples)

# Putting them together ----
DOXY.PARAMS <- rbind(effectiveness_samples,discont_rate_samples)
rownames(DOXY.PARAMS) <- c("doxy.effectiveness","doxy.discontinuationRate")




# INTERVENTION ----
# intervnetion controls the uptake among eligible population >>> 10% from 2022-2030
# we know C=U/(1+r) > in the model, we will calculate the coverage
# r is discontinuation rate
# U is the proportion of eligible population filling a prescription for doxy (regardless app having been on Doxy before)
# C is the proportion of eligible population receiving doxyPep by the end of the year?

# Intervention control uptake levels
clear.interventions() 
for (uptake in c(10,20)){
    uptake.effect =  create.intervention.effect(
        quantity.name    = "doxy.uptake",
        start.time       = DOXY.START,
        effect.values    = uptake/100,
        times            = DOXY.END, # when scale up ends
        scale            = "proportion",
        apply.effects.as = "value",
        allow.values.less.than.otherwise  = FALSE,
        allow.values.greater.than.otherwise = TRUE
    )
    
    uptake_intervention <- create.intervention(
        uptake.effect,
        parameters = DOXY.PARAMS,
        WHOLE.POPULATION, 
        code = paste0("doxypep.",uptake)
    )
}

noint = get.null.intervention()


# # NEW ----
# if (1==2)
# {
#     simset <- retrieve.simulation.set(version = "shield",location = "C.12580", calibration.code = "calib.4.8.stage2.az", 300)
#     lastB=simset$last.sim()
#     engine=create.jheem.engine('shield', "C.12580", end.year = 2030)
#     simB=engine$run(lastB$get.params())
#     
#     sim_int  <- uptake_intervention$run(simB,  start.year = 2022, end.year = 2030)
#     sim_noInt  <- no_intervention$run(simB,  start.year = 2022, end.year = 2030)
#     
#     simplot(simB,sim_noInt,sim_int, 
#             outcomes = c("diagnosis.ps","doxy.uptake"),
#             dimension.values = list(year=2020:2030),split.by = "sex",plot.which = "sim.only")
#     
# }
# # Make an intervention by supplying a target population and an intervention effect.
# # 
