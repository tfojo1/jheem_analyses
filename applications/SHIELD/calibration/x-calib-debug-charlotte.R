# Cahrlotte:
# underestiamting the population, among Other
# Load Required Libraries and Commoncode----
library(plotly)
source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
# Set Plotting Styles ----
location.style.manager = create.style.manager(color.data.by = "location.type")
source.style.manager   = create.style.manager( shape.data.by = "source",color.data.by = "stratum")
stratum.style.manager  = create.style.manager(color.data.by = "stratum")
source('../jheem_analyses/applications/SHIELD/calibration/calibration_inspection_helpers.R')

# read older stage0 calib for CT
assign_simset_vars("C.16740", calibration.codes = "calib.3.10.stage0.az",sim.id = "10")
# assign_simset_vars("C.16740", calibration.codes = "calib.3.13.stage0.az",sim.id = "13")

simplot(
    lastCT10,
    # lastCT13,
    # outcomes = "population"
    # outcomes = "population",facet.by = "age"
    # outcomes = "population",split.by = "race",facet.by="sex"
    outcomes = "population",split.by = "race",facet.by = "age"
    # outcomes = "population",split.by = "sex",facet.by = "age"
    
    # outcomes = "fertility.rate",split.by = "race",facet.by = "age"
    # outcomes = c("immigration","emigration")
    # outcomes = c("immigration","emigration"),split.by = "race"
    
    # outcomes = c("immigration","emigration"),split.by = "race"#facet.by = "age"
    # outcomes="deaths"
    ,dimension.values = list(year=c(2010:2026)) 
)

#Mixing Stats:
head(simsetCT10$get.mcmc.mixing.statistic())

# Engine
engineCT=create.jheem.engine('shield','C.16740',2030)
paramCalib=lastCT10$params
simCalib=engineCT$run(paramCalib)

# Manual 
paramManual=paramCalib
paramManual['other.fertility.rate.multiplier']=1.2*paramManual['other.fertility.rate.multiplier']
paramManual['other.general.mortality.rate.multiplier']=.7*paramManual['other.general.mortality.rate.multiplier']
paramManual['age14.other.aging.rate.multiplier.1']=1.3*paramManual['age14.other.aging.rate.multiplier.1']
paramManual['age14.other.aging.rate.multiplier.2']=.8*paramManual['age14.other.aging.rate.multiplier.2']
paramManual['age19.other.aging.rate.multiplier.1']=1.2*paramManual['age19.other.aging.rate.multiplier.1']
paramManual['age19.other.aging.rate.multiplier.2']=.8*paramManual['age19.other.aging.rate.multiplier.2']
paramManual['age24.other.aging.rate.multiplier.2']=.8*paramManual['age24.other.aging.rate.multiplier.2']
paramManual['age29.other.aging.rate.multiplier.2']=.8*paramManual['age29.other.aging.rate.multiplier.2']
paramManual['age34.other.aging.rate.multiplier.2']=.8*paramManual['age34.other.aging.rate.multiplier.2']
paramManual['age39.other.aging.rate.multiplier.2']=.8*paramManual['age39.other.aging.rate.multiplier.2']
paramManual['age44.other.aging.rate.multiplier.2']=.7*paramManual['age44.other.aging.rate.multiplier.2']
paramManual['age49.other.aging.rate.multiplier.2']=.7*paramManual['age49.other.aging.rate.multiplier.2']
paramManual['age54.other.aging.rate.multiplier.2']=.6*paramManual['age54.other.aging.rate.multiplier.2']
paramManual['age64.other.aging.rate.multiplier.2']=.5*paramManual['age64.other.aging.rate.multiplier.2']
#
simManual=engineCT$run(paramManual)
#
simplot(
    simCalib,simManual,
    # outcomes = "population"
    # outcomes = "population",facet.by = "race"
    # outcomes = "population",split.by = "race",facet.by="sex"
    outcomes = "population",split.by = "race",facet.by = "age"
    # outcomes = "population",split.by = "sex",facet.by = "age"
    
    # outcomes = "fertility.rate",split.by = "race",facet.by = "age"
    # outcomes = c("immigration","emigration")
    # outcomes = c("immigration","emigration"),split.by = "race"
    
    # outcomes = c("immigration","emigration"),split.by = "race"#facet.by = "age"
    # outcomes="deaths"
)

# LIKELIHOOD
likCT=lik.inst.stage0$instantiate.likelihood('shield','C.16740')
likCT$compare.sims(sim1 = simManual,sim2 = simCalib,piecewise = F,log=T)
likCT$compare.sims(sim1 = simManual,sim2 = simCalib,piecewise = T,log=T)


simManualMac<-simManual
save.simulation.set(simManualMac)

# exp(likCT$cosimManualMac# exp(likCT$compare.sims(sim1 = simManual,sim2 = simCalib,piecewise = F,log=T))
likCT$compare.sims(sim1 = simManual,sim2 = simCalib,piecewise = F,log=F)
likCT$compare.sims(sim1 = simManual,sim2 = simCalib,piecewise = T,log=T)
# likCT$compare.sims(sim1 = simManual,sim2 = simCalib,piecewise = T,log=T)
# population         deaths fertility.rate    immigration     emigration 
# 6.28227833    11.04803665    36.65902168     0.15715760    -0.08006248 
# diagnosis.ps 
# 0.81226152 
# Compute:
likCT$compute(simCalib,log=T)
likCT$compute(simCalib,log=F)
log(likCT$compute(simCalib,log=F))
# piecewise
likCT$compute.piecewise(simCalib,log = F)
likCT$compute.piecewise(simCalib,log = T)

# prior.density* lik at point A/B determines if MCMC can choose A over B
calculate.density(SHIELD.FULL.PARAMETERS.PRIOR,paramManual)*lik$compute(simManual,log = F)/
    calculate.density(SHIELD.FULL.PARAMETERS.PRIOR,paramCalib)*lik$compute(simCalib,log=F)

#because prior.lik is >1 in manual/calib: it sugest that a truely better altenative exists and we just didnt find it
# > options increase the likelihood of weight?
# changing seed?
# increase the weight
