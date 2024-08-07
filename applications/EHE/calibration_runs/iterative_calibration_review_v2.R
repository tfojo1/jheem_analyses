source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
library(ggplot2)


load("../jheem_analyses/prelim_results/pop_trans_mort_2024-04-18_C.12580.Rdata")
sim.pop.trans.mort = sim

load("../jheem_analyses/prelim_results/pop_trans_mort_idu_2024-04-18_C.12580.Rdata")
sim.pop.trans.mort.idu = sim

load("../jheem_analyses/prelim_results/pop_trans_mort_non_idu_2024-04-18_C.12580.Rdata")
sim.pop.trans.mort.non.idu = sim

load("../jheem_analyses/prelim_results/base_plus_prep_2024-04-18_C.12580.Rdata")
sim.prep = sim

# params.manual = base.params
# sim.manual = engine$run(parameters = params.manual) 

#full.minus.supp.lik = FULL.likelihood.instructions.minus.supp$instantiate.likelihood('ehe','C.12580')

round(exp(full.minus.supp.lik$compute.piecewise(sim.pop.trans.mort.idu) - 
            full.minus.supp.lik$compute.piecewise(sim.pop.trans.mort)))
# population likelihood says mort.idu is better??
simplot(#sim.pop.trans.mort,
        #sim.pop.trans.mort.idu,
        #sim.pop.trans.mort.non.idu,
        sim.prep,
        facet.by = "age", split.by = "race", 
        outcomes = c("population"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim.pop.trans.mort,
        sim.pop.trans.mort.idu,
        #sim.pop.trans.mort.non.idu,
        #sim.prep,
        facet.by = "risk", #split.by = "race", 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim.pop.trans.mort,
        sim.pop.trans.mort.idu,
        #sim.pop.trans.mort.non.idu,
        #sim.prep,
        facet.by = "age", split.by = "sex", 
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim.pop.trans.mort,
        sim.pop.trans.mort.idu,
        facet.by = "age",
        outcomes = c("proportion.using.heroin","proportion.using.cocaine"),
        dimension.values = list(year = 2000:2030)) 

## BASE + PREP MCMC ## 
round(exp(full.minus.supp.lik$compute.piecewise(sim.prep) - 
            full.minus.supp.lik$compute.piecewise(sim.pop.trans.mort)))
# prep likelihoods say sim.prep is infinitely better...
simplot(sim.pop.trans.mort,
        sim.prep,
        facet.by = "age",
        outcomes = c("prep.uptake"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim.pop.trans.mort,
        sim.prep,
        sim,
        facet.by = "sex",
        outcomes = c("prep.indications"),
        dimension.values = list(year = 2000:2030)) 

cbind(round(base.params,3),
      #round(base.idu.params,3),
      #round(base.non.idu.params,3),
      round(base.prep.params,3))



if(1==2){
  full.params.names = dimnames(sim.pop.trans.mort$parameters)[[1]]
  old.params.names = dimnames(sim.prep$parameters)[[1]]
  new.params.names = setdiff(full.params.names,old.params.names)
  
  base.params = c(sim.pop.trans.mort$parameters)
  base.idu.params = c(sim.pop.trans.mort.idu$parameters)
  names(base.params) = names(base.idu.params) = full.params.names
  
  base.non.idu.params = c(sim.pop.trans.mort.non.idu$parameters)
  base.prep.params = c(sim.prep$parameters)
  names(base.non.idu.params) = names(base.prep.params) = old.params.names
  
  base.non.idu.params[new.params.names] = NA
  base.prep.params[new.params.names] = NA
  
  base.non.idu.params = base.non.idu.params[full.params.names]
  base.prep.params = base.prep.params[full.params.names]  
}



