# Atlanta:
# underestiamting the population, all races/sexes
# accross all age groups except 65+

stg0simplot(
    lastA,
    # outcomes = "population"
    # outcomes = "population",facet.by = "sex"
    # outcomes = "population",split.by = "race",facet.by="sex"
    # outcomes = "population",split.by = "race",facet.by = "age"
    # outcomes = "population",split.by = "sex",facet.by = "age"
    
    outcomes = "fertility.rate",split.by = "race",facet.by = "age"
    # outcomes = c("immigration","emigration")
    # outcomes = c("immigration","emigration"),split.by = "race"
    
    # outcomes = c("immigration","emigration"),split.by = "race"#facet.by = "age"
    # outcomes="deaths"
)
#Mixing Stats:
head(simsetA$get.mcmc.mixing.statistic())

# Engine
engine=create.jheem.engine('shield','C.12060',2030)
paramCalib=lastA$params
simCalib=engine$run(paramCalib)

# Manual 
{
    paramManual=paramCalib
    # paramManual['age35.39.fertility.rate.multiplier']=1.2*paramManual['age35.39.fertility.rate.multiplier']
    # paramManual['age40.44.fertility.rate.multiplier']=1.2*paramManual['age40.44.fertility.rate.multiplier']
    
    paramManual['other.general.mortality.rate.multiplier']=.3*paramManual['other.general.mortality.rate.multiplier']
    paramManual['black.general.mortality.rate.multiplier']=.3*paramManual['black.general.mortality.rate.multiplier']
    paramManual['hispanic.general.mortality.rate.multiplier']=.3*paramManual['hispanic.general.mortality.rate.multiplier']
    
    # paramManual['age14.other.aging.rate.multiplier.1']=1.3*paramManual['age14.other.aging.rate.multiplier.1']
    paramManual['age14.other.aging.rate.multiplier.2']=.8*paramManual['age14.other.aging.rate.multiplier.2']
    paramManual['age14.black.aging.rate.multiplier.2']=.8*paramManual['age14.black.aging.rate.multiplier.2']
    
    # paramManual['age19.other.aging.rate.multiplier.1']=1.2*paramManual['age19.other.aging.rate.multiplier.1']
    paramManual['age19.other.aging.rate.multiplier.2']=.8*paramManual['age19.other.aging.rate.multiplier.2']
    paramManual['age19.black.aging.rate.multiplier.2']=.7*paramManual['age19.black.aging.rate.multiplier.2']
    
    paramManual['age24.other.aging.rate.multiplier.2']=.6*paramManual['age24.other.aging.rate.multiplier.2']
    paramManual['age24.black.aging.rate.multiplier.2']=.8*paramManual['age24.black.aging.rate.multiplier.2']
    
    paramManual['age29.other.aging.rate.multiplier.2']=.7*paramManual['age29.other.aging.rate.multiplier.2']
    paramManual['age29.black.aging.rate.multiplier.2']=.8*paramManual['age29.black.aging.rate.multiplier.2']
    
    paramManual['age34.other.aging.rate.multiplier.2']=.6*paramManual['age34.other.aging.rate.multiplier.2']
    paramManual['age34.black.aging.rate.multiplier.2']=.6*paramManual['age34.black.aging.rate.multiplier.2']
    
    paramManual['age39.other.aging.rate.multiplier.2']=.5*paramManual['age39.other.aging.rate.multiplier.2']
    paramManual['age39.black.aging.rate.multiplier.2']=.5*paramManual['age39.black.aging.rate.multiplier.2']
    
    paramManual['age44.other.aging.rate.multiplier.2']=.5*paramManual['age44.other.aging.rate.multiplier.2']
    paramManual['age44.black.aging.rate.multiplier.2']=.5*paramManual['age44.black.aging.rate.multiplier.2']
    
    paramManual['age49.other.aging.rate.multiplier.2']=.5*paramManual['age49.other.aging.rate.multiplier.2']
    paramManual['age49.black.aging.rate.multiplier.2']=.5*paramManual['age49.black.aging.rate.multiplier.2']
    
    paramManual['age54.other.aging.rate.multiplier.2']=.6*paramManual['age54.other.aging.rate.multiplier.2']
    paramManual['age54.black.aging.rate.multiplier.2']=.6*paramManual['age54.black.aging.rate.multiplier.2']
    
    paramManual['age64.other.aging.rate.multiplier.2']=.5*paramManual['age64.other.aging.rate.multiplier.2']
    paramManual['age64.black.aging.rate.multiplier.2']=.5*paramManual['age64.black.aging.rate.multiplier.2']
    # #
    simManual=engine$run(paramManual)
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
}
# # LIKELIHOOD
lik=lik.inst.stage0$instantiate.likelihood('shield','C.12060')
# 
lik$compare.sims(sim1 = simManual,sim2 = simCalib,piecewise = F,log=F)
# exp(lik$compare.sims(sim1 = simManual,sim2 = simCalib,piecewise = F,log=T))
# lik$compare.sims(sim1 = simManual,sim2 = simCalib,piecewise = F,log=F)
# 
# # Compute:
# lik$compute(simCalib,log=T)
# lik$compute(simCalib,log=F)
# log(lik$compute(simCalib,log=F))
# # piecewise
# lik$compute.piecewise(simCalib,log = F)
# lik$compute.piecewise(simCalib,log = T)
# 
# # prior.density* lik at point A/B determines if MCMC can choose A over B
calculate.density(SHIELD.FULL.PARAMETERS.PRIOR,paramManual)*lik$compute(simManual,log = F)/
    calculate.density(SHIELD.FULL.PARAMETERS.PRIOR,paramCalib)*lik$compute(simCalib,log=F)
# 
# #because prior.lik is >1 in manual/calib: it sugest that a truely better altenative exists and we just didnt find it
# # > options increase the likelihood of weight?
# # changing seed?
# # increase the weight
