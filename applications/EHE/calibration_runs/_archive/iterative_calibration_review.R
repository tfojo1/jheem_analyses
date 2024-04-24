source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
library(ggplot2)

# Have to rerun sims with new engine because of changes to simplot 
# SIM POP + TRANS + MORT 
load("../jheem_analyses/prelim_results/pop_trans_mort_2024-04-13_C.12580.Rdata")
sim.pop.trans.mort = sim
params.pop.trans.mort = c(sim.pop.trans.mort$parameters)
names(params.pop.trans.mort) = dimnames(sim.pop.trans.mort$parameters)[[1]]
engine = create.jheem.engine('ehe',sim.pop.trans.mort$location,end.year = 2030) 
sim.pop.trans.mort = engine$run(parameters = params.pop.trans.mort) 

# SIM POP + TRANS + MORT + IDU
load("../jheem_analyses/prelim_results/pop_trans_mort_idu_2024-04-13_C.12580.Rdata")
sim.pop.trans.mort.idu = sim
params.pop.trans.mort.idu = c(sim.pop.trans.mort.idu$parameters)
names(params.pop.trans.mort.idu) = dimnames(sim.pop.trans.mort.idu$parameters)[[1]]
sim.pop.trans.mort.idu = engine$run(parameters = params.pop.trans.mort.idu) 

#pop.trans.mort.idu.lik = pop.trans.mortality.idu.likelihood.instructions$instantiate.likelihood('ehe','C.12580')

# SIM MANUAL: PARAMS SET TO POP/TRANS/MORT, then change parameters one at a time to see what causes the scoop
# parameters to try: 
    # 1. incident idu parameters 
    # 3. 'other' parameters

cbind(round(sim.pop.trans.mort$parameters,3),
      round(sim.manual$parameters,3),
      round(sim.pop.trans.mort.idu$parameters,3))

params.manual = params.pop.trans.mort
## incident IDU parameters IMPROVE prop using cocaine/heroin plots WITHOUT messing up population plots - GOOD
# incident.idu.params = names(params.manual[grepl("incident.idu",names(params.manual))])
# params.manual[incident.idu.params] = params.pop.trans.mort.idu[incident.idu.params]
# sim.manual = engine$run(parameters = params.manual) 

# THESE CAUSE THE SCOOP - narrow down to a subset 
other.params = names(params.manual[grepl("other",names(params.manual))])
#params.manual[other.params] = params.pop.trans.mort.idu[other.params]

cbind(round(sim.pop.trans.mort$parameters[other.params,],3),
      round(sim.manual$parameters[other.params,],3),
      round(sim.pop.trans.mort.idu$parameters[other.params,],3))

#### OTHER AGING MULTIPLIERS BY AGE CAUSE THE SCOOP ####
# AND these seem to make cocaine and heroin plots better - that's why it's choosing these values 
params.manual["other.age1.aging.multiplier.1"] = params.pop.trans.mort.idu["other.age1.aging.multiplier.1"]
params.manual["other.age1.aging.multiplier.2"] = params.pop.trans.mort.idu["other.age1.aging.multiplier.2"]
params.manual["other.age2.aging.multiplier.1"] = params.pop.trans.mort.idu["other.age2.aging.multiplier.1"]
params.manual["other.age2.aging.multiplier.2"] = params.pop.trans.mort.idu["other.age2.aging.multiplier.2"]
params.manual["other.age3.aging.multiplier"] = params.pop.trans.mort.idu["other.age3.aging.multiplier"]

sim.manual = engine$run(parameters = params.manual) 

round(exp(pop.trans.mort.idu.lik$compute.piecewise(sim.manual) - 
            pop.trans.mort.idu.lik$compute.piecewise(sim.pop.trans.mort)),4)

# population 
simplot(sim.pop.trans.mort,
        sim.manual,
        sim.pop.trans.mort.idu, 
        facet.by = "age", split.by = "race", 
        outcomes = c("population"),
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))

# heroin/cocaine
simplot(sim.pop.trans.mort,
        sim.manual,
        sim.pop.trans.mort.idu,
        facet.by = "age", 
        outcomes = c("proportion.using.heroin","proportion.using.cocaine"),
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))


# prevalence
simplot(sim.pop.trans.mort,
        sim.manual,
        facet.by = "risk", split.by = "race",
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))

simplot(sim.pop.trans.mort,
        sim.pop.trans.mort.idu,
        facet.by = "race", 
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))

# new
simplot(sim.pop.trans.mort,
        sim.pop.trans.mort.idu,
        facet.by = "risk", 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))

simplot(sim.pop.trans.mort,
        sim.pop.trans.mort.idu,
        facet.by = "race",
        outcomes = c("new"),
        dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))

## Old code 
if(1==2){
  load("../jheem_analyses/prelim_results/pop_trans_mort_non_idu_2024-04-14_C.12580.Rdata")
  sim.pop.trans.mort.non.idu = sim

  load("../jheem_analyses/prelim_results/init.full.minus.supp_2024-04-14_C.12580.Rdata")
  sim.full.minus.supp = sim

  cbind(round(sim.pop.trans.mort$parameters,3),
        round(sim.pop.trans.mort.idu$parameters,3),
        round(sim.pop.trans.mort.non.idu$parameters,3),
        round(sim.full.minus.supp$parameters,3))
  
  pop.trans.mort.lik = pop.trans.mortality.likelihood.instructions$instantiate.likelihood('ehe','C.12580')
  pop.trans.mort.idu.lik = pop.trans.mortality.idu.likelihood.instructions$instantiate.likelihood('ehe','C.12580')
  pop.trans.mort.non.idu.lik = pop.trans.mortality.non.idu.likelihood.instructions$instantiate.likelihood('ehe','C.12580')
  full.minus.supp.lik = FULL.likelihood.instructions.minus.supp$instantiate.likelihood('ehe','C.12580')

  # IDU VS BASE: likelihoods that are better: immigration, emigration, new, hiv.mortality, aids.deaths, heroin, cocaine 
  round(exp(pop.trans.mort.idu.lik$compute.piecewise(sim.pop.trans.mort.idu) - 
              pop.trans.mort.idu.lik$compute.piecewise(sim.pop.trans.mort)),4)
  
  # NON-IDU VS BASE: likelihoods that are better: emigration, aids.deaths, prop.tested, test.positivity, awareness, 
                    # prep uptake, prep indications 
  round(exp(pop.trans.mort.non.idu.lik$compute.piecewise(sim.pop.trans.mort.non.idu) - 
              pop.trans.mort.non.idu.lik$compute.piecewise(sim.pop.trans.mort)),4)
  
  # all of the ones after pop.trans.mort are problematic
  simplot(sim.pop.trans.mort,
          sim.pop.trans.mort.idu, # scoopy thing
          #sim.pop.trans.mort.non.idu, # drops off (but then 13-44 takes off after 2020)
          #sim.full.minus.supp, # drops off (but then all ages take off after 2020)
          facet.by = "age", split.by = "race", 
          outcomes = c("population"),
          dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))
  
  
  # Two-way plots; prevalence, new (race/risk); population (age/race)
  simplot(sim.pop.trans.mort,
          sim.pop.trans.mort.idu,
          facet.by = "risk", split.by = "race", 
          outcomes = c("diagnosed.prevalence"),
          dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))
  
  simplot(sim.pop.trans.mort,
          sim.pop.trans.mort.idu,
          facet.by = "risk", split.by = "sex", 
          outcomes = c("diagnosed.prevalence"),
          dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))
  
  simplot(sim.pop.trans.mort,
          sim.pop.trans.mort.idu,
          facet.by = "risk", split.by = "race", 
          outcomes = c("new"),
          dimension.values = list(year = 2000:2030)) + ggtitle(locations::get.location.name(sim$location))
  
}