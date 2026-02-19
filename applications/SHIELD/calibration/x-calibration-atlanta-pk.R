# detach("package:deSolve", unload = TRUE)
# detach("package:deSolve", character.only = TRUE)
# unloadNamespace("jheem2")
# unloadNamespace("deSolve")


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
MSAS.OF.INTEREST
VERSION='shield'; LOCATION="C.12060"

simsetAtlanta13 <- retrieve.simulation.set("shield",
                                             location = LOCATION,calibration.code = "calib.2.13.stage1.az",n.sim = 400)
simsetAtlanta17 <- retrieve.simulation.set("shield",
                                             location = LOCATION,calibration.code = "calib.2.17.stage1.az", n.sim = 400)
simsetAtlanta17k <- retrieve.simulation.set("shield",
                                              location = LOCATION,calibration.code = "calib.2.17.stage1k.az", n.sim = 400)

#ISSUES:#Not a good poulation fit
# PLOT -----
simplot(
    # simsetAtlanta13$last.sim(),
    # simsetAtlanta17$last.sim(),
    simsetAtlanta17k$last.sim(),
    # 
    # outcomes = c("population"),    split.by = "race", facet.by = "age"
    # outcomes = c("fertility.rate"),    split.by = "race", facet.by = "age"
    # outcomes = c("deaths")
    # outcomes = c("immigration","emigration")    #split.by = "race"
    # outcomes = c("emigration") #split.by = "race", facet.by = "age"
    # outcomes=c("diagnosis.ps")
    # outcomes=c("diagnosis.total")
    outcomes=c("diagnosis.total","diagnosis.ps",
               "diagnosis.el.misclassified","diagnosis.ll.misclassified",
               "hiv.testing","sti.screening")  
    # ,split.by = "race"
    # ,facet.by = "sex"
    
    # outcomes=c("hiv.testing")
    # ,dimension.values = list(year = 2010:2030)
    ,style.manager = source.style.manager
)

# MCMC Diagnostics ----
simset=simsetAtlanta17k
cbind(simset$get.params())
{
    # RV is ratio of variance in the first half to the second half of chain (if search is stuck, RV goes up)
    head(simset$get.mcmc.mixing.statistic())

    simset$traceplot("transmission")
    cbind(simset$get.params("transmission"))
    cbind(sim.last$get.params("trans"))

    simset$traceplot("or")
    simset$traceplot("future")
}

# MANUAL FITs----------------
engine <- create.jheem.engine(VERSION, LOCATION, end.year = 2030)
{
    params.manual <- simset$get.params()
    #
    params.manual['transmission.rate.future.change.mult']<-3
    # params.manual['transmission.rate.multiplier.heterosexual2020']<-1.01*params.manual['transmission.rate.multiplier.heterosexual2020']
    # params.manual['transmission.rate.multiplier.msm2020']<-1.01*params.manual['transmission.rate.multiplier.msm2020']
    # #
    sim.manual <- engine$run(params.manual)
}
simplot(
    # simsetBaltimore0$last.sim(),
    simset$last.sim(),
    sim.manual,
    # outcomes = c("population"),    split.by = "race", facet.by = "age"
    outcomes=c("diagnosis.total","diagnosis.ps","diagnosis.el.misclassified","diagnosis.ll.misclassified")
    # , split.by = "race"
    # ,facet.by = "sex"

    # ,dimension.values = list(year = 2010:2030)
    ,style.manager = source.style.manager
)

# COMPARE LIKELIHOODS ----
lik=lik.inst.stage1$instantiate.likelihood(VERSION,LOCATION)


lik.manual=join.likelihood.instructions(
    total.diagnosis.likelihood.instructions,
    total.diagnosis.by.strata.stage1.likelihood.instructions,
    #
    ps.diagnosis.total.likelihood.instructions,
    ps.diagnosis.by.strata.stage1.likelihood.instructions,
    #
    early.diagnosis.total.likelihood.instructions,
    early.diagnosis.by.strata.stage1.likelihood.instructions,
    #
    late.diagnosis.total.likelihood.instructions,
    late.diagnosis.by.strata.stage1.likelihood.instructions
)
lik=lik.manual$instantiate.likelihood(VERSION,LOCATION)

# compare:
# Yes, if you add debug=T to either the compute.likelihood or compare.sims functions, you will enter a debug mode in the likelihood. From there, you can view the “lik.summary" (it's just a data frame object that should already be computed), which will have the actual values for each stratum (called "obs"), the sim values ("mean") and the standard deviation I think. There might also be a Z score column that standardizes how far off the sim is from the observed value, though I can't remember off the top of my head. (Note that because compare.sims takes two different sims as the arguments, you will enter the debug mode for whichever sim you have listed first).
lik$compare.sims(simset1$last.sim(),sim.manual, piecewise = T) #values greater than 1 mean

