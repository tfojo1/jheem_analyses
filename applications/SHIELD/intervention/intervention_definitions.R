# inputs and sources are: ~/jheem/code/jheem_analyses/applications/SHIELD/inputs/input_doxy_pep_parameters.R

# Single target population for all MSM
WHOLE.POPULATION = create.target.population(name = 'Whole Population') #MSM?

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

DOXY.PARAMS <- matrix(effectiveness_samples,
                      nrow = 1,
                      dimnames = list("doxy.effectiveness", NULL))


# INTERVENTION ----
# intervnetion controls the uptake among eligible population >>> 10% from 2022-2030
# we know C=U/(1+r) > in the model, we will calculate the coverage
# r is discontinuation rate
# U is the proportion of eligible population filling a prescription for doxy (regardless app having been on Doxy before)
# C is the proportion of eligible population receiving doxyPep by the end of the year?

clear.interventions() 

# scenarios: changing target coverage in 2030 -----
for (coverage in seq(10,100,10)){  
        coverage.effect =  create.intervention.effect(
            quantity.name    = "doxy.coverage",
            effect.values    = coverage/100,
            start.time       = 2023,# when scale up begins
            times            = 2030, # when scale up ends
            scale            = "proportion",
            apply.effects.as = "value",
            allow.values.less.than.otherwise  = FALSE,
            allow.values.greater.than.otherwise = TRUE
        )
        name=paste0("doxy.cov.",coverage)
        doxy_int <- create.intervention(
            coverage.effect,
            parameters = DOXY.PARAMS,
            WHOLE.POPULATION, 
            code = paste0("doxy.cov.",coverage)
        )
        print(name)
    }

#no int ---
noint = get.null.intervention()

print(paste0("created: ", "noint"))
