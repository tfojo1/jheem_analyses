source("../jheem_analyses/applications/SHIELD/shield_specification.R")
# correct values that we are looking for:

specification.metadata=get.specification.metadata('shield',"C.12580")
hiv_testing_prior <- get.cached.object.for.version(name = "hiv.testing.prior",
                                                   version = specification.metadata$version)
logodd=(hiv_testing_prior$intercepts+log(.5))[["15-19 years",'black','msm']]
expit = function(x){return(1/(1+exp(-x)))}
rate.from.prob= function(p){return (-log(1 - p))}

p=expit(logodd); 
r=rate.from.prob(p)
print(paste0("prob ",p, "; rate is ",r))

#### now we test with both models

VERSION='shield'
LOCATION="C.12580"
if (1==2) {
    engine1 <- create.jheem.engine(VERSION, LOCATION, end.year = 2030)
    param.manual1=get.means(SHIELD.FULL.PARAMETERS.PRIOR)
    param.manual1["or.sti.screening.msm"]<- 1
    param.manual1["or.sti.screening.heterosexual_male"]<-1
    param.manual1["or.sti.screening.female"]<-1
    param.manual1["or.sti.screening.black"]<-1
    param.manual1["or.sti.screening.white"]<-1
    param.manual1["or.sti.screening.hispanic"]<-1
    param.manual1["or.slope.sti.screening.msm"]<-1
    param.manual1["or.slope.sti.screening.heterosexual"]<-1
    
    sim.manual1 <- engine1$run(param.manual1)
    q1=engine1$extract.quantity.values() #returns the input values to the model
    q1$rate.sti.screening.over.14.without.covid$"2010"["15-19 years",'black','msm']
    
    simplot(sim.manual1,
            c("diagnosis.ps","hiv.testing"))
}
# 
if (1==1){
    engine2 <- create.jheem.engine(VERSION, LOCATION, end.year = 2030)
    param.manual2=get.means(SHIELD.FULL.PARAMETERS.PRIOR)
    param.manual2["screening.rate.multiplier.msm.2010"]<-1 
    param.manual2["screening.rate.multiplier.het.2010"]<-1
    param.manual2["screening.rate.multiplier.msm.2020"]<-1 
    param.manual2["screening.rate.multiplier.het.2020"]<-1
    
    param.manual2["screening.rate.multiplier.black.2010"]<-1 
    param.manual2["screening.rate.multiplier.hispanic.2010"]<-1
    param.manual2["screening.rate.multiplier.other.2010"]<-1
    param.manual2["screening.rate.multiplier.black.2020"]<-1 
    param.manual2["screening.rate.multiplier.hispanic.2020"]<-1
    param.manual2["screening.rate.multiplier.other.2020"]<-1
    
    param.manual2["screening.rate.future.change.mult"] <-1
    # 
    
    sim.manual2 <- engine2$run(param.manual2)
    
    q2=engine2$extract.quantity.values() #returns the input values to the model
    q2$rate.sti.screening.over.14.without.covid$"2010"["15-19 years",'black','msm']
    
    simplot(sim.manual2,
            c("diagnosis.ps","hiv.testing"))
    
}

simplot(sim.manual1,sim.manual2,
c("diagnosis.ps","hiv.testing","sti.screening"))

q1=engine1$extract.quantity.values() #returns the input values to the model
q1$rate.sti.screening.over.14.without.covid$"2010"["15-19 years",'black','msm']


q2=engine2$extract.quantity.values() #returns the input values to the model
q2$rate.sti.screening.over.14.without.covid$"2010"["15-19 years",'black','msm']


