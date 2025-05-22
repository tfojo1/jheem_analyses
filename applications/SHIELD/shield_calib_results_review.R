# Load Required Libraries and Commoncode----
library(plotly)
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
# Set Plotting Styles ----
location.style.manager = create.style.manager(color.data.by = "location.type")
source.style.manager   = create.style.manager( shape.data.by = "source",
                                               color.data.by = "stratum")
stratum.style.manager  = create.style.manager(color.data.by = "stratum")

# Configuration ----
VERSION <- 'shield'
LOCATION <- 'C.12580'  # Baltimore MSA

# get.jheem.root.directory() #"/Volumes/jheem$"
ROOT.DIR="../../files/"
set.jheem.root.directory(ROOT.DIR)

# DATE <- "2025-05-07"
CALIBRATION.CODE.TO.RUN <- 'syphilis.9.pk.psTotal'
# CALIBRATION.CODE.TO.RUN <- 'syphilis.diagnoses.5.pk'



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
# sim=extract.last.simulation.from.calibration(version,LOCATION,CALIBRATION.CODE.TO.RUN,allow.incomplete = T)

# simset9=simset
# simset10=simset
# # simset11=simset
# simset=simset9
# Quick checkpoint ----
simset$n.sim
# Extract first and last simulations and their parameters 
sim.first    <- simset$first.sim()
sim.last     <- simset$last.sim()
params.first <- sim.first$params
params.last  <- sim.last$params

# 
#Run Manual Simulation ----
# engine <- create.jheem.engine(VERSION, LOCATION, end.year = 2030)
#
{
    # params.manual <- params.last
    # params.manual["transmission.rate.multiplier.msm0"] <- 1.16 #1990
    # params.manual["transmission.rate.multiplier.msm1"] <- 1.18 #1995
    # params.manual["transmission.rate.multiplier.msm2"] <- 0.93 #2000
    # params.manual["transmission.rate.multiplier.msm3"] <- 1.07 #2010
    # params.manual["transmission.rate.multiplier.msm4"] <- 1.05 #2020
    # 
    # params.manual["transmission.rate.multiplier.heterosexual0"] <- .988 #1990
    # params.manual["transmission.rate.multiplier.heterosexual1"] <- 1.215 #1995
    # params.manual["transmission.rate.multiplier.heterosexual2"] <- 0.905 #2000
    # params.manual["transmission.rate.multiplier.heterosexual3"] <- 1.06 #2010
    # params.manual["transmission.rate.multiplier.heterosexual4"] <- 1.055 #2020
    # 
    # sim.manual <- engine$run(params.manual)

# Plot syphilis total diagnosis  
simplot(
    # sim.first,
    sim.last,
    # sim,
    # sim2,
    # sim.manual,
    # split.by = "sex",
    split.by = "age",
    # split.by = "race",
    # split.by = "race", facet.by = "sex", #we are matching the totals only for now
    # split.by = "race", facet.by = "age",
    # outcomes = c("diagnosis.total"),
    # outcomes = c("diagnosis.ps"),
    # outcomes = c("diagnosis.el.misclassified"),
    outcomes = c("diagnosis.late.misclassified"),
    dimension.values = list(year = 1990:2025),
    style.manager = source.style.manager
)
}

engine= create.jheem.engine(VERSION, LOCATION, end.year = 2030)
enable.jheem.solver.tracking() #will slow down but track additional data
sim=engine$run(sim.last$params)
x=get.jheem.solver.tracked.info()
qplot(x$diffeq.computed.times)
length(x$diffeq.computed.times)
# the more states are changing the smaller steps we need to take

#to test the solver for a good simulation 
solver = create.solver.metadata(rtol =0.01, atol=0.1 ) #default solver
engine2=create.jheem.engine(VERSION, LOCATION, end.year = 2030, solver.metadata = solver)
sim2=engine2$run(sim.last$params)
x2=get.jheem.solver.tracked.info()
qplot(x2$diffeq.computed.times)
sim2$run.metadata$n.diffeq.evaluations

range(sim$infected-sim2$infected)


lik.ps=ps.diagnosis.total.likelihood.instructions$instantiate.likelihood(VERSION,LOCATION)
lik.total=likelihood.instructions.syphilis.diagnoses.psTotal$instantiate.likelihood(VERSION,LOCATION)
# lik.ps$compute(sim.last, debug = T)
# lik.ps$compute(sim.manual, debug = T)
# lik.ps$compare.sims(sim.first, sim.last, piecewise = T)
lik.ps$compare.sims(sim.last, sim.manual, piecewise = T)
lik.total$compare.sims(sim.last, sim.manual, piecewise = F)
# 

calculate.density(SHIELD.FULL.PARAMETERS.PRIOR, sim.manual$params) / calculate.density(SHIELD.FULL.PARAMETERS.PRIOR, sim.last$params) 

# # # Plot hiv.testing 
# simplot(
#     sim.first,
#     sim.last,
#     #sim.manual,
#     # facet.by = "sex",
#     # facet.by = "age",
#     outcomes = c("hiv.testing"),
#     dimension.values = list(year = 2000:2030)
# )
# Plot Population
simplot(
    # sim.first,
    sim.last,
    # sim.manual,
    # split.by = "race", facet.by = "age",
    split.by = "sex", facet.by = "age",
    
    outcomes = c("population"),
    dimension.values = list(year = 2000:2030)
)
# Deaths
simplot(
    # sim.first,
    sim.last,
    outcomes = c("deaths"),
    dimension.values = list(year = 2000:2030)
)

# Plot Fertility
simplot(
    # sim.first,
    sim.last,
    # sim.manual,
    split.by = "race", facet.by = "age",
    outcomes = c("fertility.rate"),
    dimension.values = list(year = 2000:2030)
)
#
# Plot Immigration / Emigration
simplot(sim.last, outcomes = "immigration", dimension.values = list(year = 2000:2030))
simplot(sim.last, outcomes = "emigration",  dimension.values = list(year = 2000:2030))

# MCMC Diagnostics ----
head(simset$get.mcmc.mixing.statistic())
simset$traceplot("trans")
cbind(simset$get.params("trans"))
cbind(sim.last$get.params("trans"))

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
lik<- likelihood.instructions.syphilis.diagnoses.psTotal$instantiate.likelihood(VERSION, LOCATION)
lik$compare.sims( sim.last, sim.manual, piecewise = F)
lik$compute(sim.last, debug = T)

lik.ps=ps.diagnosis.total.likelihood.instructions$instantiate.likelihood(VERSION,LOCATION)
lik.ps$compute(sim.last, debug = T)
lik.ps$compute(sim.manual, debug = T)
lik.ps$compare.sims(sim.last, sim.manual, piecewise = T)

# lik$compute(sim,debug=T)
# mask = lik.summary$stratum== "15-19 years __hispanic"
# lik.summary[mask]
# Save simset ----
# save(simset, file = paste0("prelim_results/", CALIBRATION.CODE.TO.RUN, "_simset_", Sys.Date(), "_", LOCATION, ".Rdata"))
# Save sim.manual
# save(sim.manual, file = paste0("prelim_results/", CALIBRATION.CODE.TO.RUN, "_sim.manual_", Sys.Date(), "_", LOCATION, ".Rdata"))

# Looking inside the engine -----
q=engine$extract.quantity.values() #returns the input values to the model
input.fertility = q$fertility.rate[["2020"]]
dimnames(sim.manual$immigration)

# Gaussian Reference Proportions ----
dnorm(0, mean = 0, sd = 1)
dnorm(1, 0, 1) / dnorm(0, 0, 1)  # ~60% of peak
dnorm(2, 0, 1) / dnorm(0, 0, 1)  # ~13% of peak
