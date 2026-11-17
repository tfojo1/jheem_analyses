# inputs and sources are: ~/jheem/code/jheem_analyses/applications/SHIELD/inputs/input_doxy_pep_parameters.R
source("../jheem_analyses/applications/SHIELD/intervention/doxy_effectiveness.R")

# the population is defined as the whole population here, but in the code it only applies to MSM
WHOLE.POPULATION = create.target.population(name = 'Whole Population') 

# # Generate 1,000 plausible RR values using the reported estimate from DoxyPEP trial
# #     RR = 0.20 (relative risk)
# #     95% CI = 0.08–0.48
doxy_rr_draws <- draw_rr_lognorm(
    n          = 1000,
    rr_mean    = 0.20,
    rr_lo      = 0.08,
    rr_hi      = 0.48,
    cap_at_one = TRUE
)
# Convert RR to doxy-PEP efficacy
doxy_effectiveness_1000 <- 1 - doxy_rr_draws
# hist(doxy_effectiveness_1000)

DOXY.PARAMS <- matrix(doxy_effectiveness_1000,
                      nrow = 1,
                      dimnames = list("doxy.effectiveness", NULL))

# INTERVENTION ----
# intervnetion controls the coverage among eligible population directly
# this doesn't require us to know what proportion of population is truly eligible; and
# at the same time doesn't allow us to calculate how many people are receiving Doxy

clear.interventions() 

# KING COUNTY-ALIGNED SCENARIOS (Seattle only) -----
# Anchors: Menza et al. 2026, Clin Infect Dis (PMID 41885610), prior-month doxy-PEP use among MSM:
#   ~12-15% mid-2023; 31% (Pride surveys) to 45% (Sexual Health Clinic) by mid-2024
# Start Jan 2023 (clinical prescriptions predate the Mar 2023 SHC rollout), coverage 0 at 2023.0,
# piecewise-linear between anchors, held at the last value afterwards:
#   doxy.kc.30.flat : 0 (2023.0) -> 15% (2023.5) -> 30% (May 2024)               -> flat 30%
#   doxy.kc.30.to50 : 0 (2023.0) -> 15% (2023.5) -> 30% (May 2024) -> 50% (2026.0) -> flat 50%
#   doxy.kc.45.flat : 0 (2023.0) -> 15% (2023.5) -> 45% (May 2024)               -> flat 45%
#   doxy.kc.45.to50 : 0 (2023.0) -> 15% (2023.5) -> 45% (May 2024) -> 50% (2026.0) -> flat 50%
# CHECK: create.intervention.effect() accepts vector times / effect.values (piecewise ramp)
KC.SCENARIOS <- list(
    doxy.kc.30.flat = list(times = c(2023.5, 2024 + 4/12),       values = c(0.15, 0.30)),
    doxy.kc.30.to50 = list(times = c(2023.5, 2024 + 4/12, 2026), values = c(0.15, 0.30, 0.50)),
    doxy.kc.45.flat = list(times = c(2023.5, 2024 + 4/12),       values = c(0.15, 0.45)),
    doxy.kc.45.to50 = list(times = c(2023.5, 2024 + 4/12, 2026), values = c(0.15, 0.45, 0.50))
)

for (code in names(KC.SCENARIOS)) {
    sc <- KC.SCENARIOS[[code]]
    kc.effect <- create.intervention.effect(
        quantity.name    = "doxy.coverage",
        effect.values    = sc$values,
        start.time       = 2023,       # scale-up begins: 0 coverage at Jan 2023
        times            = sc$times,   # anchor times for the values above
        scale            = "proportion",
        apply.effects.as = "value",
        allow.values.less.than.otherwise    = FALSE,
        allow.values.greater.than.otherwise = TRUE
    )
    doxy_int <- create.intervention(
        kc.effect,
        parameters = DOXY.PARAMS,
        WHOLE.POPULATION,
        code = code
    )
    print(code)
}

#no int ---
noint = get.null.intervention()
print(paste0("created: ", "noint"))
 
