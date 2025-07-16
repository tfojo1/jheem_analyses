# rm(list = ls())  # Clear workspace
# detach(name = "jheem", unload = TRUE)  # Detach conflicting package if loaded
# Load deSolve
# library(deSolve)

# Load Required Libraries and Commoncode----
library(plotly)
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
# Set Plotting Styles ----
location.style.manager = create.style.manager(color.data.by = "location.type")
source.style.manager   = create.style.manager( color.data.by = "source",shade.data.by =  "stratum")
stratum.style.manager  = create.style.manager(color.data.by = "stratum")
sim.style.manager  = create.style.manager(color.data.by = "simulation")

# Configuration ----
VERSION <- 'shield'
LOCATION <- 'C.12580'  # Baltimore MSA


get.jheem.root.directory() #"/Volumes/jheem$"
# set.jheem.root.directory("../../files/") #local Mac
# set.jheem.root.directory("/Volumes/jheem$") #Q drive

# # CALIBRATION.CODE.TO.RUN <- 'calib.demog.06.09.pk'; DATE <- "2025-06-09"
# # Load or Assemble Simulation Set ----
# if (FALSE) {
#     load(paste0("/Volumes/jheem$/results/Shield/", CALIBRATION.CODE.TO.RUN, "_simset_", DATE, "_", LOCATION, ".Rdata"))
# }
# if (FALSE) {
#     load(paste0("../jheem_analyses/prelim_results/", CALIBRATION.CODE.TO.RUN, "_simset_", DATE, "_", LOCATION, ".Rdata"))
# }
# if (TRUE) {
#     get.calibration.progress('shield', LOCATION, CALIBRATION.CODE.TO.RUN)
#     simset <- assemble.simulations.from.calibration(
#         version = VERSION,
#         location = LOCATION,
#         calibration.code = CALIBRATION.CODE.TO.RUN,
#         allow.incomplete = TRUE
#     )
#     save(simset,file=paste0(get.jheem.root.directory(),"/shield/",CALIBRATION.CODE.TO.RUN, "_simset_", DATE, "_", LOCATION, ".Rdata"))
# }

# Load simset from the Q drive
#07.10: These models have 5 additional knots for EL screening in 1990-95-2000-2010-2020'
# also relapse and infectiousness 
# sti.screening.multiplier.el       3.6171130
# sti.screening.multiplier.1990.el  1.1867342
# sti.screening.multiplier.1995.el  1.2618675
# sti.screening.multiplier.2000.el  1.1342260
# sti.screening.multiplier.2010.el  1.2642888
# sti.screening.multiplier.2020.el  0.8733673
# >> this one worked reakkt well for recent trends
for (i in c(0)){
    CALIBRATION.CODE.TO.RUN <- paste0('calib.07.10.pk',i); DATE <- "2025-07-10" 
    filename=  paste0(get.jheem.root.directory(),"/shield/", CALIBRATION.CODE.TO.RUN, "_simset_", DATE, "_", LOCATION, ".Rdata");print(filename) 
    load(filename)
    assign(paste0("simset",i),simset) #dynamic assignment
}
 

for (i in c(0:3)){
    simset=get(paste0("simset",i))
    print(simset$n.sim)
    # Extract first and last simulations and their parameters
    assign(paste0("sim.first",i)   ,simset$first.sim())
    assign(paste0("sim.last",i)   ,simset$last.sim())
    assign(paste0("params.first",i) ,simset$first.sim()$params)
    assign(paste0("params.last",i) ,simset$last.sim()$params)
}
# 
# engine <- create.jheem.engine(VERSION, LOCATION, end.year = 2030)
# params.manual <- simset1$last.sim()$params
# # params.manual["or.symptomatic.1990"] <- params.manual["or.symptomatic.1990"]*1# 0.970793 
# # params.manual["or.symptomatic.1995"] <- params.manual["or.symptomatic.2000"]*.85 #1.101602 
# # params.manual["or.symptomatic.2010"] <- params.manual["or.symptomatic.2000"]*1.1
# # params.manual["or.symptomatic.2020"] <- params.manual["or.symptomatic.2020"] #0.6944672 
# sim.mac.engine <- engine$run(params.manual)

# PLOT -----
simset=simset0
head(simset$get.mcmc.mixing.statistic())
cbind(simset$get.params(".el"))
simplot(
    # sim.first0,
    # sim.first1,
    sim.last0, #w=1/8
    # sim.last1, #w=1/4
    # sim.last2,#w=1/2
    # sim.last3,#w=1
    
    # sim.mac.engine,
    # simset.mac.calib$first.sim(),
    # split.by = "race", facet.by = "sex",
    # split.by = "race", facet.by = "age",
    # outcomes = c("population"),    split.by = "race", facet.by = "age",
    
    # outcomes=c("diagnosis.total"),   facet.by = "race",
    # outcomes=c("diagnosis.ps"),     facet.by = "race",
    # outcomes=c("diagnosis.el.misclassified"),   facet.by = "race",
    # outcomes=c("diagnosis.late.misclassified"),   facet.by = "race",
    
    outcomes = c("diagnosis.total","diagnosis.ps","diagnosis.el.misclassified",
                 "diagnosis.late.misclassified","hiv.testing"),
    # # outcomes = c("prevalence"),
    dimension.values = list(year = 1970:2030) 
    # style.manager = source.style.manager
)

# x+geom_hline(yintercept = 613)+geom_vline(xintercept = 2022)
# x+geom_hline(yintercept =  672) +geom_vline(xintercept = 2023)
# MCMC Diagnostics ----
simset=simset0
cbind(simset$get.params())

{
    head(simset$get.mcmc.mixing.statistic())
    simset$traceplot("transmission")
    cbind(simset$get.params("transmission"))
    cbind(sim.last$get.params("trans"))
    
    simset$traceplot("diagnoses")
    simset$traceplot("hiv")
    simset$traceplot("symptomatic")
    simset$traceplot("screening")
    simset$traceplot(".el")
    simset$traceplot("rel")
    simset$traceplot("prop.early")
    
    simset$traceplot("black.aging")
    simset$traceplot("hispanic.aging")
    simset$traceplot("mortality")
    simset$traceplot("fertility")
}

# Run Manual Simulation ----
# engine <- create.jheem.engine(VERSION, LOCATION, end.year = 2030)
# {
#     params.manual <- params.last
#     # params.manual["hiv.testing.or"] <- 1.004 
#     # params.manual["hiv.testing.slope.or"] <- 0.9995
#     # params.manual["rate.screening.ps.multiplier"] <- 0.5
#     # params.manual["rate.screening.el.multiplier"] <- 1.
#     # 
#     sim.manual <- engine$run(params.manual)
#     #
#     simplot(
#         # sim.first,
#         sim.last,
#         # sim.last2,
#         sim.manual,
#         # split.by = "sex",
#         # split.by = "age",
#         # split.by = "race",
#         # split.by = "race", facet.by = "sex", #we are matching the totals only for now
#         # split.by = "race", facet.by = "age",
#         # outcomes = c("population"),
#         # outcomes = c("diagnosis.total"),
#         outcomes = c("diagnosis.ps","diagnosis.el.misclassified","hiv.testing"),
#         # dimension.values = list(year = 1970:2030),
#         style.manager = source.style.manager
#     )
# }
# Likelihood Review ----
{
    lik=lik.inst.diag.total.no.demog$instantiate.likelihood(VERSION,LOCATION)
    # compare: 
    # Yes, if you add debug=T to either the compute.likelihood or compare.sims functions, you will enter a debug mode in the likelihood. From there, you can view the “lik.summary" (it's just a data frame object that should already be computed), which will have the actual values for each stratum (called "obs"), the sim values ("mean") and the standard deviation I think. There might also be a Z score column that standardizes how far off the sim is from the observed value, though I can't remember off the top of my head. (Note that because compare.sims takes two different sims as the arguments, you will enter the debug mode for whichever sim you have listed first).  
    lik$compare.sims(sim.last0,sim.last1, piecewise = T) #values greater than 1 mean than sim2 is better than sim1, while values less than 1 mean that sim2 is worse than sim1. 
    lik$compare.sims(sim.last1,sim.last2, piecewise = T) #values greater than 1 mean than sim2 is better than sim1, while values less than 1 mean that sim2 is worse than sim1. 
    lik$compare.sims(sim.last2,sim.last3, piecewise = T) #values greater than 1 mean than sim2 is better than sim1, while values less than 1 mean that sim2 is worse than sim1. 
    
    lik.late=late.diagnosis.total.likelihood.instructions$instantiate.likelihood(VERSION, LOCATION)
    lik.late$compute(sim.last1,debug = T)
    lik.late$compute(sim.last2,debug = T)
    
    lik.hiv=hiv.testing.total.likelihood.instructions$instantiate.likelihood(VERSION, LOCATION)
    lik.hiv$compare.sims(sim.last1,sim.last2)
    
    lik.hiv$compute(sim.last1,debug = T)
    lik.hiv$compute(sim.last2,debug = T)
    
    summary.hiv.1
    summary.hiv.2
    lik.early=early.diagnosis.total.likelihood.instructions$instantiate.likelihood(VERSION, LOCATION)
    lik.early$compare.sims(sim.last1,sim.manual, piecewise = T, log = F) #values greater than 1 mean than sim2 is better than sim1, while values less than 1 mean that sim2 is worse than sim1. 
    
    # Compute ----
    # mask = lik.summary$stratum== "15-19 years __hispanic"
    # lik.summary[mask]
    
}

# Reviewing the engine computation time -----
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


# compare priors ----
calculate.density(SHIELD.FULL.PARAMETERS.PRIOR, sim.manual$params) / calculate.density(SHIELD.FULL.PARAMETERS.PRIOR, sim.last$params) 




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
