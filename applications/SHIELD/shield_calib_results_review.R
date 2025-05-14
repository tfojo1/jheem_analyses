# Load Required Libraries and Commoncode----
library(plotly)
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
# Set Plotting Styles ----
location.style.manager = create.style.manager(color.data.by = "location.type")
source.style.manager   = create.style.manager(color.data.by = "source")
stratum.style.manager  = create.style.manager(color.data.by = "stratum")

# Configuration ----
VERSION <- 'shield'
LOCATION <- 'C.12580'  # Baltimore MSA
CALIBRATION.CODE.TO.RUN <- 'syphilis.diagnoses.5.pk'
DATE <- "2025-05-07"

# Load or Assemble Simulation Set ----
if (FALSE) {
    load(paste0("/Volumes/jheem$/results/Shield/", CALIBRATION.CODE.TO.RUN, "_simset_", DATE, "_", LOCATION, ".Rdata"))
}
if (FALSE) {
    load(paste0("../jheem_analyses/prelim_results/", CALIBRATION.CODE.TO.RUN, "_simset_", DATE, "_", LOCATION, ".Rdata"))
}
if (TRUE) {
    get.calibration.progress('shield', LOCATION, CALIBRATION.CODE.TO.RUN)
    simset <- assemble.simulations.from.calibration(
        version = VERSION,
        location = LOCATION,
        calibration.code = CALIBRATION.CODE.TO.RUN,
        allow.incomplete = TRUE
    )
}

# Quick checkpoint ----
simset$n.sim
# Extract first and last simulations and their parameters 
sim.first    <- simset$first.sim()
sim.last     <- simset$last.sim()
params.first <- sim.first$params
params.last  <- sim.last$params


# Run Manual Simulation ----
# engine <- create.jheem.engine(VERSION, LOCATION, end.year = 2030)
# #
# {
#     params.manual <- params.last
#     # params.manual["transmission.rate.multiplier.msm0"] <- 1 #1990
#     params.manual["transmission.rate.multiplier.msm1"] <- 1.2 #1995
#     params.manual["transmission.rate.multiplier.msm2"] <- 0.95 #2000
#     params.manual["transmission.rate.multiplier.msm3"] <- 1.07 #2010
#     params.manual["transmission.rate.multiplier.msm4"] <- 1.07 #2020
#     
#     # params.manual["transmission.rate.multiplier.heterosexual0"] <- 1 #1990
#     params.manual["transmission.rate.multiplier.heterosexual1"] <- 1.2 #1995
#     params.manual["transmission.rate.multiplier.heterosexual2"] <- 0.92 #2000
#     params.manual["transmission.rate.multiplier.heterosexual3"] <- 1.06 #2010
#     params.manual["transmission.rate.multiplier.heterosexual4"] <- 1.05 #2020
#     
#     sim.manual <- engine$run(params.manual)
# }


# Plot syphilis total diagnosis  
simplot(
    sim.first,
    sim.last,
    # sim.manual,
    # split.by = "sex",
    # split.by = "race", facet.by = "sex", #we are matching the totals only for now
    # split.by = "race", facet.by = "age",
    # outcomes = c("diagnosis.total"),
    outcomes = c("diagnosis.ps"),
    # outcomes = c("diagnosis.el.misclassified"),
    # outcomes = c("diagnosis.late.misclassified"),
    dimension.values = list(year = 1990:2025) 
)
 

# Plot hiv.testing 
simplot(
    sim.first,
    sim.last,
    #sim.manual,
    # facet.by = "sex",
    # facet.by = "age",
    outcomes = c("hiv.testing"),
    dimension.values = list(year = 2000:2030)
)
# Plot Population
simplot(
    sim.first,
    sim.last,
    #sim.manual,
    split.by = "race", facet.by = "age",
    outcomes = c("population"),
    dimension.values = list(year = 2000:2030)
)
# Deaths
simplot(
    sim.first,
    sim.last,
    outcomes = c("deaths"),
    dimension.values = list(year = 2000:2030)
)

# Plot Fertility 
simplot(
    sim.first,
    sim.last,
    #sim.manual,
    split.by = "race", facet.by = "age",
    outcomes = c("fertility.rate"),
    dimension.values = list(year = 2000:2030)
)

# Plot Immigration / Emigration 
simplot(sim.last, outcomes = "immigration", dimension.values = list(year = 2000:2030))
simplot(sim.last, outcomes = "emigration",  dimension.values = list(year = 2000:2030))

# MCMC Diagnostics ----
simset$get.mcmc.mixing.statistic()
simset$traceplot("trans")
cbind(simset$get.params("trans"))
cbind(sim.manual$get.params("trans"))

simset$traceplot("black.aging")
simset$traceplot("other.aging")
simset$traceplot("hispanic.aging")
simset$traceplot("mortality")
simset$traceplot("fertility")

# Likelihood Comparison ----
if (1==2){
    source("applications/SHIELD/debug_likelihoods.R")
    
    lik         <- likelihood.instructions.syphilis.diagnoses$instantiate.likelihood(VERSION, LOCATION)
    lik.sex.race <- population.likelihood.instructions.2way.sex.race$instantiate.likelihood(VERSION, LOCATION)
    lik.sex.age  <- population.likelihood.instructions.2way.sex.age$instantiate.likelihood(VERSION, LOCATION)
    lik.age.race <- population.likelihood.instructions.2way.age.race$instantiate.likelihood(VERSION, LOCATION)
    lik.age      <- population.likelihood.instructions.1way.age$instantiate.likelihood(VERSION, LOCATION)
    lik.fert     <- fertility.likelihood.instructions$instantiate.likelihood(VERSION, LOCATION)
    
    lik$compare.sims(sim.first, sim.last, piecewise = TRUE, log = TRUE)
    lik.age.race$compare.sims(sim.first, sim.last, piecewise = TRUE)
    lik.sex.age$compare.sims(sim.first, sim.last, piecewise = TRUE)
    lik.sex.race$compare.sims(sim.first, sim.last, piecewise = TRUE)
    lik.age$compare.sims(sim.first, sim.last, piecewise = TRUE, log = TRUE)
    lik.fert$compare.sims(sim.first, sim.last, piecewise = TRUE, log = TRUE)
    lik.fert$compute(sim.last, debug = TRUE)
}


source("applications/SHIELD/shield_likelihoods.R")
# Likelihood Comparison ----
lik<- likelihood.instructions.syphilis.diagnoses.totals$instantiate.likelihood(VERSION, LOCATION)
lik$compare.sims( sim.last, sim.manual, piecewise = F)
lik$compute(sim.last, debug = T)

lik.ps=ps.diagnosis.total.likelihood.instructions$instantiate.likelihood(VERSION,LOCATION)
lik.ps$compute(sim.last, debug = T)
lik.ps$compute(sim.manual, debug = T)
lik.ps$compare.sims(sim.last, sim.manual, piecewise = T)


# Save simset ----
# save(simset, file = paste0("prelim_results/", CALIBRATION.CODE.TO.RUN, "_simset_", Sys.Date(), "_", LOCATION, ".Rdata"))
# Save sim.manual
# save(sim.manual, file = paste0("prelim_results/", CALIBRATION.CODE.TO.RUN, "_sim.manual_", Sys.Date(), "_", LOCATION, ".Rdata"))



# Gaussian Reference Proportions ----
dnorm(0, mean = 0, sd = 1)
dnorm(1, 0, 1) / dnorm(0, 0, 1)  # ~60% of peak
dnorm(2, 0, 1) / dnorm(0, 0, 1)  # ~13% of peak
