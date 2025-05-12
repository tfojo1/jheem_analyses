# Load Required Libraries ----
library(plotly)

# Load SHIELD and Common Code ----
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
CALIBRATION.CODE.TO.RUN <- 'syphilis.diagnoses.3.pk'
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

# Extract first and last simulations and their parameters ----
sim.first    <- simset$first.sim()
sim.last     <- simset$last.sim()
params.first <- sim.first$params
params.last  <- sim.last$params

# Run Manual Simulation ----
 engine <- create.jheem.engine(VERSION, LOCATION, end.year = 2030)
# params.manual <- params.last
# params.manual["age40.44.hispanic.fertility.rate.multiplier"] <- 10 # last: 0.06205035, first: 1.0000000

# sim.manual <- engine$run(params.manual)

sim.first <- engine$run(params.first)
sim.last <- engine$run(params.last)

# q=engine$extract.quantity.values() #returns the input values to the model
# input.fertility = q$fertility.rate[["2020"]]
# dimnames(sim.manual$immigration)

# Save simset (optional)
# save(simset, file = paste0("prelim_results/", CALIBRATION.CODE.TO.RUN, "_simset_", Sys.Date(), "_", LOCATION, ".Rdata"))

# Plot syphilis total diagnosis ----
simplot(
    sim.first,
    sim.last,
    #sim.manual,
    # split.by = "race", facet.by = "age",
    outcomes = c("diagnosis.total"),
    dimension.values = list(year = 1990:2025)
)

# Plot syphilis total diagnosis ----
simplot(
    sim.first,
    sim.last,
    #sim.manual,
    split.by = "race", facet.by = "sex", #we are matching the totals only for now
    #split.by = "sex",
    # outcomes = c("diagnosis.ps"),
    # outcomes = c("diagnosis.el.misclassified"),
    outcomes = c("diagnosis.late.misclassified"),
    dimension.values = list(year = 1990:2025),
    style.manager = stratum.style.manager
    
)

# Plot Population ----
simplot(
    sim.first,
    sim.last,
    #sim.manual,
    # split.by = "race", facet.by = "age",
    outcomes = c("hiv.testing"),
    dimension.values = list(year = 2000:2030)
)

# Deaths
simplot(
    sim.first,
    sim.last,
    outcomes = c("deaths"),
    dimension.values = list(year = 2000:2030)
)

# Plot Fertility ----
simplot(
    sim.first,
    sim.last,
    #sim.manual,
    split.by = "race", facet.by = "age",
    outcomes = c("fertility.rate"),
    dimension.values = list(year = 2000:2030)
)

# Plot Immigration / Emigration ----
simplot(sim.last, outcomes = "immigration", dimension.values = list(year = 2000:2030))
simplot(sim.last, outcomes = "emigration",  dimension.values = list(year = 2000:2030))

# MCMC Diagnostics ----
simset$get.mcmc.mixing.statistic()
simset$traceplot("black.aging")
simset$traceplot("other.aging")
simset$traceplot("hispanic.aging")
simset$traceplot("mortality")
simset$traceplot("fertility")

# Likelihood Comparison ----
if (1==2){
    source("applications/SHIELD/debug_likelihoods.R")
    
    lik         <- likelihood.instructions.demographics$instantiate.likelihood(VERSION, LOCATION)
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

# Gaussian Reference Proportions ----
dnorm(0, mean = 0, sd = 1)
dnorm(1, 0, 1) / dnorm(0, 0, 1)  # ~60% of peak
dnorm(2, 0, 1) / dnorm(0, 0, 1)  # ~13% of peak
