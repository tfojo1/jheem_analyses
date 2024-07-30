source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

full.lik = FULL.likelihood.instructions.with.covid$instantiate.likelihood('ehe','C.12580')
full.lik.downweight = FULL.likelihood.instructions.downweighted$instantiate.likelihood('ehe','C.12580')
engine = create.jheem.engine('ehe','C.12580',end.year = 2030)

## Full simset - better 55+ diagnoses but worse awareness
load('../jheem_analyses/prelim_results/full.with.covid_simset_2024-07-11_C.12580.Rdata')
simset.full = simset
sim.full.last = simset.full$last.sim()
params.full.last = sim.full.last$params
sim.full.last = engine$run(parameters = params.full.last) 

## Trans simset
load("../jheem_analyses/prelim_results/init.transmission.ehe_simset_2024-07-10_C.12580.Rdata")
simset.trans = simset
sim.trans.last = simset.trans$last.sim()
params.trans.last = sim.trans.last$params
sim.trans.last = engine$run(parameters = params.trans.last) 

# Take the TRANS calibration, then try to bring 55+ diagnoses back down without sacrificing awareness
params.manual = params.trans.last # params.full.last
sim.manual = engine$run(parameters = params.manual)

msm.trates = names(params.manual[grepl("msm.trate",names(params.manual))])
age.susceptibility = names(params.manual[grepl("age..susceptibility",names(params.manual))])
msm.age.susceptibility = names(params.manual[grepl("msm.susceptibility",names(params.manual))])

params.manual["age5.msm.susceptibility.rr.mult.12"] = 1 # 1
params.manual["age5.susceptibility.rr.mult"] = 0.75 # 1
params.manual["age1.black.msm.trate.2"] = 85 # 52.02131
params.manual["age1.other.msm.trate.2"] = 8 # 0.8320418
params.manual["age1.hispanic.msm.trate.2"] = 10 # 1.054786


sim.manual = engine$run(parameters = params.manual)

exp(full.lik.downweight$compute.piecewise(sim.manual) - full.lik.downweight$compute.piecewise(sim.trans.last))

## these two plots are trading off 
simplot(sim.trans.last,
        sim.manual,
        #sim.full.last,
        facet.by = "age", split.by = "sex", 
        outcomes = c("new"), 
        dimension.values = list(year = 2010:2030)) 

simplot(sim.trans.last,
        sim.manual,
        #sim.full.last,
        facet.by = "risk", split.by = "race", 
        outcomes = c("new"), 
        dimension.values = list(year = 2010:2030)) 

simplot(sim.trans.last,
        sim.manual,
        #sim.full.last,
        style.manager = location.style.manager,
        outcomes = c("new"), 
        dimension.values = list(year = 2010:2030)) 

simplot(sim.trans.last,
        sim.manual,
        sim.full.last,
        facet.by = "age", split.by = "sex", 
        outcomes = c("incidence"), 
        dimension.values = list(year = 2010:2030)) 

simplot(sim.trans.last,
        sim.manual,
        #sim.full.last,
        facet.by = "age", split.by = "sex", 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2010:2030)) 

simplot(sim.trans.last,
        sim.manual,
        sim.full.last,
        outcomes = c("awareness"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

#########

simplot(sim.07.09,
        sim.manual,
        facet.by = "risk", split.by = "race", 
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.07.09,
        sim.manual,
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.07.09,
        sim.manual,
        facet.by = "age", split.by = "sex", 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.07.09,
        sim.manual,
        facet.by = "risk", split.by = "race", 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) 

# Take the full calibration, roll only the testing parameters back to the trans calibration, 
# then try to bring 55+ diagnoses back down without sacrificing awareness
if(1==2){
  #params.manual = params.full.last
  #testing.params = names(params.manual[grepl("proportion.tested",names(params.manual))])
  #params.manual[testing.params] = params.trans.last[testing.params]
  
  params.manual["age5.msm.susceptibility.rr.mult.12"] = 0.5 # 0.9349061
  params.manual["age5.susceptibility.rr.mult"] = 0.4 # 0.8335187
  params.manual["age2.susceptibility.rr.mult"] = 2 # 1.386641
  params.manual["age2.black.msm.trate.1"] = 15 #2.875082
  params.manual["age2.other.msm.trate.1"] = 15 #0.02604319 # THIS DOES ALMOST NOTHING
  #params.manual["age2.hispanic.msm.trate.1"] = #72.19564
  
  # THESE DO ALMOST NOTHING
  params.manual["age2.black.msm.trate.2"] = 25 #2.080335
  params.manual["age2.other.msm.trate.2"] = 25 #0.01285808 
  params.manual["age2.hispanic.msm.trate.2"] = 1#0.685663  
}
