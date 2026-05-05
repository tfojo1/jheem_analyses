# Modeling a rapid scale up in Seattle according to DOXYPEP Trial 
# DoxyPEP Intervention Analysis
# ============================================================================
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
source('../jheem_analyses/applications/SHIELD/intervention/intervention_helper_functions.R')

# enrolled in 2020–2022 across San Francisco (UCSF) and Seattle (UW).  
# Enrollment occurred largely in 2020–2021 with 12 months of follow‑up per participant.
# Enrollment start: August 19, 2020
# Enrollment stopped: May 13, 2022 (early stop for efficacy)
# The first main results were presented in 2022 (notably at the International AIDS Conference, AIDS 2022, in July 2022
# Published paper: The pivotal results were published in the New England Journal of Medicine on April 6, 2023 (Molina et al. for the French trial; Luetkemeyer et al. for the UCSF/UW DoxyPEP trial also in NEJM 2023

# When trial enrollment was halted in May 2022, we offered all current trial participants doxy-PEP as part of the open-label extension phase, and later that year, we offered all past trial participants doxy-PEP prescriptions through the SHC. By March 2023, we began offering doxy-PEP prescriptions to patients who did not participate in the trial and tracking doxy-PEP use.
#uptake: 
# https://academic.oup.com/cid/advance-article/doi/10.1093/cid/ciag209/8541760
# From May 2023 to June 2024:  the proportion of MSM who reported doxy-PEP use in the prior month increased from 12% to 45%
# self-reported doxy-PEP use in the prior month increased from 15% to 31% among MSM

# Single target population for all MSM
# SECTION 1: Configuration ----

# Just for SEATTLE
LOCATIONS        <- SHIELD.TEN.MSAS[10]     # Named vector: names = city, values = codes

CALIBRATION.CODE <- "calib.4.24.stage2.az"
N.SIM <- 300
FIRST.YEAR <- 2000
LAST.YEAR <- 2040
BASE.PATH <- paste0(ROOT.DIR,"/simulations/shield")

WHOLE.POPULATION = create.target.population(name = 'Whole Population') #MSM?

# Timeline:
# August 2020. DoxyPEP trial begins Uptake0
# May 2022. DoxyPEP trial ends 
# May 2023 guideline dropped Uptake 12-15%
# June 2024 last interview  Uptake 31-45%

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


# PERSISTANCE and DISCONTINUATION RATE  
# P: proportion of population, taking Doxy at the end of the year
# Discontinuation rate= -log(Peristance)

draw_persistance_uniform<-function(n){
    persistence_samples = runif(n,1,1)
    persistence_samples
}
persistence_samples<-draw_persistance_uniform(10000)
discont_rate_samples = -log(persistence_samples)

# Putting them together  
DOXY.PARAMS <- rbind(effectiveness_samples,discont_rate_samples)
rownames(DOXY.PARAMS) <- c("doxy.effectiveness","doxy.discontinuationRate")

####
# intervnetion controls the uptake among eligible population >>> 10% from 2022-2030
# we know C=U/(1+r) > in the model, we will calculate the coverage
# r is discontinuation rate
# U is the proportion of eligible population filling a prescription for doxy (regardless app having been on Doxy before)
# C is the proportion of eligible population receiving doxyPep by the end of the year?

if (1==1){
    clear.interventions() 
    for (uptake in c(30,45)){
        # LINEAR scaleup
        uptake.effect =  create.intervention.effect(
            quantity.name    = "doxy.uptake",
            effect.values    = uptake/100,
            start.time       = 2020 + 8/12 ,# when scale up begins in Aug 2020
            times            = 2024 + 6/12, # when scale up ends - stays up at this level
            scale            = "proportion",
            apply.effects.as = "value",
            allow.values.less.than.otherwise  = FALSE,
            allow.values.greater.than.otherwise = TRUE
        );
        uptake_intervention <- create.intervention(
            uptake.effect,
            parameters = DOXY.PARAMS,
            WHOLE.POPULATION,
            code = paste0("doxypep.",uptake,".linear.2024")
        )
        # STEPWISE Scaleup
        uptake.effect =  create.intervention.effect(
            quantity.name    = "doxy.uptake",
            effect.values    = c(15,15,uptake)/100,
            
            start.time       = 2020 + 8/12 ,# when scale up begins
            times            = c(2022 + 5/12 ,
                                 2023 + 5/12 ,
                                 2024 + 6/12),       
            scale            = "proportion",
            apply.effects.as = "value",
            allow.values.less.than.otherwise  = FALSE,
            allow.values.greater.than.otherwise = TRUE
        );
        uptake_intervention <- create.intervention(
            uptake.effect,
            parameters = DOXY.PARAMS,
            WHOLE.POPULATION,
            code = paste0("doxypep.",uptake,".stepwise.2024")
        )
        noint = get.null.intervention()
    }
}

INTERVENTION.LABELS <- c(
    noint        = "No Doxy-PEP Intervention",
    doxypep.30.linear.2024   = "30% Doxy-PEP linear Coverage 2020-2024",
    doxypep.45.linear.2024   = "45% Doxy-PEP linear Coverage 2020-2024",
    doxypep.30.stepwise.2024 = "30% Doxy-PEP stepwise Coverage 2020-2024",
    doxypep.45.stepwise.2024 = "45% Doxy-PEP stepwise Coverage 2020-2024"
)
INTERVENTION.CODES <- names(INTERVENTION.LABELS)


# SECTION 2: Run ----
if (1==1){
    sim.collection <- create.simset.collection(
        version = "shield",
        calibration.code = CALIBRATION.CODE,
        locations = LOCATIONS,
        interventions = INTERVENTION.CODES,
        n.sim = N.SIM
    )
    #
    FORCE.OVERWRITE<- FALSE
    #
    sim.collection$run(
        FIRST.YEAR,
        LAST.YEAR,
        verbose = T,
        stop.for.errors = FALSE,
        overwrite.prior = FORCE.OVERWRITE,
        keep.from.year = FIRST.YEAR
    )
}

# SECTION 3: Load All Simsets ----
# rm(all.simsets)
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

# SECTION 4: Plotting ----

intervention.style.manager <- create.style.manager(color.sim.by = "simset",alpha.line=1)

# Plot output directories
ROOT.DIR <- get.jheem.root.directory()
PLOT.BASE.DIR <- file.path(ROOT.DIR, "shield", "interventionPlots", CALIBRATION.CODE)
PLOT.COMPARISON.DIR <- file.path(PLOT.BASE.DIR, "comparisons")

# --- Single city, display only ---
plot_int_single_city(
    all.simsets,
    city     = "Seattle",
    outcomes = c("diagnosis.ps", "doxy.uptake")
    # save = T,create.dirs = T
)
plot_int_single_city(
    all.simsets,
    city     = "Seattle",
    outcomes = c("diagnosis.ps"),
    facet.by = "sex",
    # plot.which="sim.only",
    years=c(2000:2025),
    # years=c(2000,2025),
    style.manager = intervention.style.manager
    # save = T,create.dirs = T
)
plot_int_single_city(
    all.simsets,
    city     = "Seattle",
    outcomes = c("doxy.uptake"),
    facet.by = "sex",
    years=c(2020,2025),
    style.manager = intervention.style.manager
    # save = T,create.dirs = T
)

# Across the study period, MSM comprised 69% of all syphilis cases among men 
# (84% in 2017–2020 and 56% in 2021–2025). 
