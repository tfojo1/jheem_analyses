# inputs and sources are: ~/jheem/code/jheem_analyses/applications/SHIELD/inputs/input_doxy_pep_parameters.R
source("../jheem_analyses/applications/SHIELD/intervention/doxy_effectiveness.R")

# the population is defined as the whole population here, but in the code it only applies to MSM
WHOLE.POPULATION = create.target.population(name = 'Whole Population') 

# Generate 1,000 simulated relative risk values
doxy_rr_draws <- draw_rr_lognorm(
    n          = 1000,
    rr_mean    = 0.20,
    rr_lo      = 0.08,
    rr_hi      = 0.48,
    cap_at_one = TRUE
)
# Convert RR to doxy-PEP efficacy
doxy_effectiveness_1000 <- 1 - doxy_rr_draws

DOXY.PARAMS <- matrix(doxy_effectiveness_1000,
                      nrow = 1,
                      dimnames = list("doxy.effectiveness", NULL))

# INTERVENTION ----
# intervnetion controls the coverage among eligible population directly
# this doesn't require us to know what proportion of population is truly eligible; and
# at the same time doesn't allow us to calculate how many people are receiving Doxy

clear.interventions() 

# scenarios: changing target coverage in 2030 -----
for (coverage in seq(5,50,5)){  
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
 
