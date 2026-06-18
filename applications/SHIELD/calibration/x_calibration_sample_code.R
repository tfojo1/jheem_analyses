# Steps to manage a bad fit:
# 1- check the fit using simplot()
# 2- check mixing 
# >>Mixing issue? downweighting the likelihoods 
# >>are we using correct variance correlation:  "autoregressive.1" vs "compound symmetry"
# 3- look at the likelihood.compute(debug=T):
# >>  are we looking at the correct values in the plot? data points and simulated points align? 
# 4- look for a manual sim that looks better
# >> does the likelihood also look better? likelihood.compare()
# >> how are the parameter values generating this fit compared to prior?
# >> do we need to revise the prior?
# 5- check the variable values inside the engine q=engine$extract.quantity.values()        
# 
# 6- Simultanous fit is not achieved:can we simplify the model more? 
#     
# >>>run a demographic calibration first model    
# >>>use it as a starting point for the next calibration focusing on total diagnosis (relax the demographic parameters)
# >>>strip away unrelated model dynamics to simplify the model (e.g., relapse=0)
# 
# Load Required Libraries and Commoncode----
library(plotly)
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
# Set Plotting Styles ----
location.style.manager = create.style.manager(color.data.by = "location.type")
source.style.manager   = create.style.manager( color.data.by = "source",shade.data.by =  "stratum")
stratum.style.manager  = create.style.manager(color.data.by = "stratum")

# Configuration ----
# VERSION <- 'shield'
# LOCATION <- 'C.12060'  # Baltimore MSA
# MSAS.OF.INTEREST #list of MSAs
# CALIBRATION.CODE.TO.RUN <- 'calib.demog.06.09.pk'; DATE <- "2025-06-09"

get.jheem.root.directory() #"/Volumes/jheem$"
# ROOT.DIR="../../files/"
# ROOT.DIR="/Volumes/jheem$"
# set.jheem.root.directory(ROOT.DIR)

# get calibration progress ----
get.calibration.progress('shield', LOCATION, CALIBRATION.CODE.TO.RUN)

# assemble simulations from MCMC ----    
simset <- assemble.simulations.from.calibration(
    version = VERSION,
    location = LOCATION,
    calibration.code = CALIBRATION.CODE.TO.RUN,
    allow.incomplete = TRUE
)
save(simset,file=paste0(get.jheem.root.directory(),"/shield/",CALIBRATION.CODE.TO.RUN, "_simset_", DATE, "_", LOCATION, ".Rdata"))

# extract the last simulation from MCMC ---- # (this has an option to also include the first sim)
sim<-extract.last.simulation.from.calibration(VERSION, LOCAITON,CALIBRATION.CODE.TO.RUN,allow.incomplete = T )

# retrieve the simulations that are complete ----
# n.sim=400
simset<-retrieve.simulation.set(version=VERSION,location = LOCATION,calibration.code = calib.name,n.sim = n.sim)


# Review Results ----
# Extract first and last simulations and their parameters 
sim.first    <- simset$first.sim()
sim.last     <- simset$last.sim()
params.first <- simset$first.sim()$params
params.last  <- simset$last.sim()$params

simplot(
    sim.last,
    outcomes = c("diagnosis.ps","diagnosis.el.misclassified","diagnosis.late.misclassified","hiv.testing"),
    dimension.values = list(year = 1970:2023),
    style.manager = source.style.manager
)


# MCMC Diagnostics ----
params=cbind(simset$get.params())

# 1-check MCMC mixing statistics: # RV is ratio of variance in the first half to the second half of chain (if search is stuck, RV goes up)
head(simset$get.mcmc.mixing.statistic())
# 2-trace plot
simset$traceplot("transmission")
# 3-review parameters
cbind(simset$get.params("transmission"))

#Run Manual Simulation ----
VERSION='shield'
LOCATION="C.12580"
engine <- create.jheem.engine(VERSION, LOCATION, end.year = 2030)
param.manual=get.means(SHIELD.FULL.PARAMETERS.PRIOR)
sim.manual <- engine$run(param.manual)
simplot(sim.manual,"diagnosis.ps")

params.manual <- params.last
params.manual["hiv.testing.or"] <- 1.004
sim.manual <- engine$run(params.manual)
#
simplot(
    sim.last,
    sim.manual,
    outcomes = c("diagnosis.ps","diagnosis.el.misclassified","hiv.testing")
)

# Likelihood Review ----
# instantiate a likelihood compute and compare values for different Sims
# COMPARE: sim2/sim1: values greater than 1 mean than sim2 is better than sim1
{
    lik=lik.inst.stage0$instantiate.likelihood(VERSION,LOCATION)
    # compare: # Yes, if you add debug=T to either the compute.likelihood or compare.sims functions, you will enter a debug mode in the likelihood. From there, you can view the “lik.summary" (it's just a data frame object that should already be computed), which will have the actual values for each stratum (called "obs"), the sim values ("mean") and the standard deviation I think. There might also be a Z score column that standardizes how far off the sim is from the observed value, though I can't remember off the top of my head. (Note that because compare.sims takes two different sims as the arguments, you will enter the debug mode for whichever sim you have listed first).
    lik$compare.sims(sim.last0,sim.last1, piecewise = T)
    
    lik$compare.sims(sim1 = simManual,sim2 = simCalib,piecewise = F,log=F)
    exp(lik$compare.sims(sim1 = simManual,sim2 = simCalib,piecewise = F,log=T))
    
    # or compute seperately
    lik.hiv$compute(sim.last1,debug = T)
    lik.hiv$compute(sim.last2,debug = T)
    #     
    # Can MCMC move in the correct direction? 
    # prior.density* lik at point A/B determines if MCMC can choose A over B
    calculate.density(SHIELD.FULL.PARAMETERS.PRIOR,paramManual)*lik$compute(simManual,log = F)/
        calculate.density(SHIELD.FULL.PARAMETERS.PRIOR,paramCalib)*lik$compute(simCalib,log=F)
    
    # Compute likelihood for a subset of data----
    mask = lik.summary$stratum== "15-19 years __hispanic"
    lik.summary[mask]
}

# Reviewing the engine computation time -----
{
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
}



# compare priors ----
# calculate.density(SHIELD.FULL.PARAMETERS.PRIOR, sim.manual$params) / calculate.density(SHIELD.FULL.PARAMETERS.PRIOR, sim.last$params) 




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

# Looking inside the engine -----
{
    q=engine$extract.quantity.values() #returns the input values to the model
    q$rate.sti.screening
    apply(q$rate.sti.screening[[1]],c("stage","age"),mean)
    
    sapply(q$rate.sti.screening[[1]],mean)
    apply(q$rate.sti.screening[[1]],c("stage","age"),mean)
}
# Gaussian Reference Proportions ----
dnorm(0, mean = 0, sd = 1)
dnorm(1, 0, 1) / dnorm(0, 0, 1)  # ~60% of peak
dnorm(2, 0, 1) / dnorm(0, 0, 1)  # ~13% of peak


# CHECK over 10X change ----
# Function to take in simset and check the number of the last X sims that
# have a ratio of 2030 PS diagnoses to 2022 that is over 10.

# What you get from prepare_simsets_for_plots of the calibration plot code
# prepared_simset_list
find_prp_over_10 <- function(simset, last_n) {
    sim_subset <- simset$subset((1 + simset$n.sim - last_n) : simset$n.sim)
    vals <- sim_subset$get("diagnosis.ps", dimension.values = list(year=c(2020, 2030)), keep.dimensions = "year")
    ratio <- apply(vals, "sim", function(x) {x["2030"]/x["2020"]})
}

lapply(prepared_simset_list, function(x) {
    if (is.null(x)) return(NULL)
    y <- find_prp_over_10(x$full_simset, 10)
    mean(y > 10)
})
