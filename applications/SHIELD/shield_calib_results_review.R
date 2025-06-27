# Load Required Libraries and Commoncode----
library(plotly)
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
# Set Plotting Styles ----
location.style.manager = create.style.manager(color.data.by = "location.type")
source.style.manager   = create.style.manager( shape.data.by = "source",color.data.by = "stratum")
stratum.style.manager  = create.style.manager(color.data.by = "stratum")

# Configuration ----
VERSION <- 'shield'
LOCATION <- 'C.12580'  # Baltimore MSA

get.jheem.root.directory() #"/Volumes/jheem$"
# ROOT.DIR="../../files/"
# set.jheem.root.directory(ROOT.DIR)
set.jheem.root.directory("/Volumes/jheem$")

# CALIBRATION.CODE.TO.RUN <- 'calib.demog.06.09.pk'; DATE <- "2025-06-09"
CALIBRATION.CODE.TO.RUN <- 'calib.diag.06.17.pk1'; DATE <- "2025-06-17"



# Load or Assemble Simulation Set ----
if (FALSE) {
    load(paste0("/Volumes/jheem$/results/Shield/", CALIBRATION.CODE.TO.RUN, "_simset_", DATE, "_", LOCATION, ".Rdata"))
}
if (TRUE) {
    load(paste0("../jheem_analyses/prelim_results/", CALIBRATION.CODE.TO.RUN, "_simset_", DATE, "_", LOCATION, ".Rdata"))
}
if (FALSE) {
    get.calibration.progress('shield', LOCATION, CALIBRATION.CODE.TO.RUN)
    simset <- assemble.simulations.from.calibration(
        version = VERSION,
        location = LOCATION,
        calibration.code = CALIBRATION.CODE.TO.RUN,
        allow.incomplete = TRUE
    )
}
# sim=extract.last.simulation.from.calibration(version,LOCATION,CALIBRATION.CODE.TO.RUN,allow.incomplete = T)
simset2=simset


# save(simset2, file = paste0(get.jheem.root.directory(),"/shield/","calib.diag.06.17.pk1",".Rdata"))

simset=simset1
# Quick checkpoint ----
simset$n.sim
# Extract first and last simulations and their parameters 
sim.first    <- simset$first.sim()
sim.last     <- simset$last.sim()
params.first <- simset$first.sim()$params
params.last  <- simset$last.sim()$params


simset=simset2
simset$n.sim
# Extract first and last simulations and their parameters 
sim.first2    <- simset$first.sim()
sim.last2     <- simset$last.sim()
params.first2 <- simset$first.sim()$params
params.last2  <- simset$last.sim()$params

# REVIEW
simplot(
    # sim.first,
    # sim.last,
    sim.first2,
    sim.last2,
    # split.by = "sex",
    # split.by = "age",
    # split.by = "race",
    # split.by = "race", facet.by = "sex",
    # split.by = "race", facet.by = "age",
    # outcomes = c("population"),
    outcomes = c("diagnosis.ps","diagnosis.el.misclassified","hiv.testing"),
     dimension.values = list(year = 1990:2030),
    style.manager = source.style.manager
)

# MCMC Diagnostics ----
{
    head(simset$get.mcmc.mixing.statistic())
    simset$traceplot("trans")
    cbind(simset$get.params("trans"))
    cbind(sim.last$get.params("trans"))
    
    simset$traceplot("screen")
    simset$traceplot("initial")
    simset$traceplot("test")
    
    simset$traceplot("black.aging")
    simset$traceplot("hispanic.aging")
    simset$traceplot("mortality")
    simset$traceplot("fertility")
}
#Run Manual Simulation ----
engine <- create.jheem.engine(VERSION, LOCATION, end.year = 2030)
#
{
    params.manual <- params.last
    # params.manual["hiv.testing.or"] <- 1.004 
    # params.manual["hiv.testing.slope.or"] <- 0.9995
    # params.manual["rate.screening.ps.multiplier"] <- 0.5
    # params.manual["rate.screening.el.multiplier"] <- 1.
    # 
    sim.manual <- engine$run(params.manual)
    
    simplot(
        # sim.first,
        sim.last,
        sim.last2,
        sim.manual,
        # split.by = "sex",
        # split.by = "age",
        # split.by = "race",
        # split.by = "race", facet.by = "sex", #we are matching the totals only for now
        # split.by = "race", facet.by = "age",
        # outcomes = c("population"),
        # outcomes = c("diagnosis.total"),
        outcomes = c("diagnosis.ps","diagnosis.el.misclassified","hiv.testing"),
        # dimension.values = list(year = 1970:2030),
        style.manager = source.style.manager
    )
}



{
    lik=likelihood.instructions.syphilis.diag.total.no.demog$instantiate.likelihood(VERSION,LOCATION)
    # lik.ps$compute(sim.last, debug = T)
    # lik.ps$compute(sim.manual, debug = T)
    # lik.ps$compare.sims(sim.first, sim.last, piecewise = T)
    lik$compare.sims(sim.manual, sim.last2, piecewise = T, log = F) #values greater than 1 mean than sim2 is better than sim1, while values less than 1 mean that sim2 is worse than sim1. 
    lik.total$compare.sims(sim.last, sim.manual, piecewise = F)
    # 
    # Yes, if you add debug=T to either the compute.likelihood or compare.sims functions, you will enter a debug mode in the likelihood. From there, you can view the “lik.summary" (it's just a data frame object that should already be computed), which will have the actual values for each stratum (called "obs"), the sim values ("mean") and the standard deviation I think. There might also be a Z score column that standardizes how far off the sim is from the observed value, though I can't remember off the top of my head. (Note that because compare.sims takes two different sims as the arguments, you will enter the debug mode for whichever sim you have listed first).  

}

# Reviewing the engine computation time
{
    # engine= create.jheem.engine(VERSION, LOCATION, end.year = 2030)
    # enable.jheem.solver.tracking() #will slow down but track additional data
    # sim=engine$run(sim.last$params)
    # x=get.jheem.solver.tracked.info()
    # qplot(x$diffeq.computed.times)
    # length(x$diffeq.computed.times)
    # # the more states are changing the smaller steps we need to take
    # 
    # #to test the solver for a good simulation
    # solver = create.solver.metadata(rtol =0.01, atol=0.1 ) #default solver
    # engine2=create.jheem.engine(VERSION, LOCATION, end.year = 2030, solver.metadata = solver)
    # sim2=engine2$run(sim.last$params)
    # x2=get.jheem.solver.tracked.info()
    # qplot(x2$diffeq.computed.times)
    # sim2$run.metadata$n.diffeq.evaluations
}

range(sim$infected-sim2$infected)


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
