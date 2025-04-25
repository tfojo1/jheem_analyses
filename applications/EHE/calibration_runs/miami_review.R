source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

load("prelim_results/full.with.covid2_simset_2025-02-23_C.33100.Rdata")
sim.mcmc = simset$last.sim()

engine = create.jheem.engine('ehe','C.33100',end.year = 2030)

params = simset$first.sim()$params

simset$last.sim()$get.params("other.msm.trate")
simset$first.sim()$get.params("other.msm.trate")

sim.new = engine$run(params)

full.lik =  FULL.likelihood.instructions.with.covid$instantiate.likelihood('ehe','C.33100')
## prevalence infinitely better?? some strata better but total worse
full.lik$compare.sims(simset$first.sim(),sim.mcmc) 

# one-way likelihoods
total.prev.lik = total.prevalence.likelihood.instructions$instantiate.likelihood('ehe','C.33100')
race.lik = race.one.way.prevalence.likelihood.instructions$instantiate.likelihood('ehe','C.33100')
risk.lik = risk.one.way.prevalence.likelihood.instructions$instantiate.likelihood('ehe','C.33100')
age.lik = age.one.way.prevalence.likelihood.instructions$instantiate.likelihood('ehe','C.33100')
sex.lik = sex.one.way.prevalence.likelihood.instructions$instantiate.likelihood('ehe','C.33100')

# two-way likelihoods
race.risk.two.way.lik = race.risk.two.way.prevalence.likelihood.instructions$instantiate.likelihood('ehe','C.33100')
age.sex.two.way.lik = age.sex.two.way.prevalence.likelihood.instructions$instantiate.likelihood('ehe','C.33100')
age.race.two.way.lik = age.race.two.way.prevalence.likelihood.instructions$instantiate.likelihood('ehe','C.33100')
age.risk.two.way.lik = age.risk.two.way.prevalence.likelihood.instructions$instantiate.likelihood('ehe','C.33100')
race.sex.two.way.lik = race.sex.two.way.prevalence.likelihood.instructions$instantiate.likelihood('ehe','C.33100')
risk.sex.two.way.lik = risk.sex.two.way.prevalence.likelihood.instructions$instantiate.likelihood('ehe','C.33100')

# one-way likelihoods
total.prev.lik$compare.sims(simset$first.sim(),sim.mcmc) # worse, correct 
race.lik$compare.sims(simset$first.sim(),sim.mcmc) # worse 
risk.lik$compare.sims(simset$first.sim(),sim.mcmc) # worse 
sex.lik$compare.sims(simset$first.sim(),sim.mcmc) # worse
age.lik$compare.sims(simset$first.sim(),sim.mcmc) # way better***

# two-way likelihoods
race.risk.two.way.lik$compare.sims(simset$first.sim(),sim.mcmc) # way better
age.sex.two.way.lik$compare.sims(simset$first.sim(),sim.mcmc) # way better
age.race.two.way.lik$compare.sims(simset$first.sim(),sim.mcmc) # way better
age.risk.two.way.lik$compare.sims(simset$first.sim(),sim.mcmc) # WAY BETTER***
race.sex.two.way.lik$compare.sims(simset$first.sim(),sim.mcmc) # worse 
risk.sex.two.way.lik$compare.sims(simset$first.sim(),sim.mcmc) # way better 

age.risk.two.way.lik$compute(sim.mcmc,debug = T)

simplot(simset$first.sim(),
        sim.mcmc,
        outcomes = c("new"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset$first.sim(),sim.mcmc,
        facet.by = "risk",# split.by = "race",
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 

simplot(simset$first.sim(),
        sim.mcmc,
        facet.by = "age",# split.by = "sex",
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 

simplot(simset$first.sim(),
        sim.mcmc,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset$first.sim(),
        sim.mcmc,
        facet.by = "risk",# split.by = "race",
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

simplot(simset$first.sim(),
        sim.mcmc,
        facet.by = "age", split.by = "sex",
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

simplot(#simset$first.sim(),
        simset$last.sim(),
        facet.by = "race", split.by = "risk",# age, sex, race, risk; 1-way 
        outcomes = c("suppression"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.mcmc,
        outcomes = c("new","aids.diagnoses"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 
