source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

engine = create.jheem.engine('ehe','C.12580',end.year = 2030)

sim.manual = engine$run(sim.trans.last.0702$params)

load('../jheem_analyses/prelim_results/full.with.covid_simset_2024-06-28_C.12580.Rdata')
simset.full.0628 = simset
sim.last.0628 = simset.full.0628$first.sim() # FLIPPED BC OLD SIMSET

load('../jheem_analyses/prelim_results/full.with.covid_simset_2024-07-02_C.12580.Rdata')
simset.full.0702 = simset
sim.full.last.0702 = simset.full.0702$first.sim() # FLIPPED BC OLD SIMSET
sim.full.first.0702 = simset.full.0702$last.sim() # FLIPPED BC OLD SIMSET

load('../jheem_analyses/prelim_results/init.transmission.ehe_simset_2024-07-01_C.12580.Rdata')
simset.trans.0702 = simset
sim.trans.last.0702 = simset.trans.0702$first.sim() # FLIPPED BC OLD SIMSET
sim.trans.first.0702 = simset.trans.0702$last.sim() # FLIPPED BC OLD SIMSET

load('../jheem_analyses/prelim_results/full.with.covid_simset_2024-07-03_C.12580.Rdata')
simset.full.0703 = simset
sim.full.last.0703 = simset.full.0703$last.sim()
sim.full.first.0703 = simset.full.0703$first.sim()

load('../jheem_analyses/prelim_results/init.transmission.ehe_simset_2024-07-02_C.12580.Rdata')
simset.trans.0703 = simset
sim.trans.last.0703 = simset.trans.0703$last.sim()
sim.trans.first.0703 = simset.trans.0703$first.sim()

full.lik = FULL.likelihood.instructions.with.covid$instantiate.likelihood('ehe','C.12580')
#trans.lik = two.way.transmission.pop.idu.aware.likelihood.instructions$instantiate.likelihood('ehe','C.12580')

tests.change.lik = number.of.tests.year.on.year.change.likelihood.instructions$instantiate.likelihood('ehe','C.12580')


exp(full.lik$compute.piecewise(sim.trans.last.0703) - full.lik$compute.piecewise(sim.trans.first.0703))

simplot(sim.full.last,
        #facet.by = "age", # age, sex; 1-way 
        outcomes = c("sexual.transmission.rates"), 
        style.manager = location.style.manager,
        #plot.year.lag.ratio = T,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.manual,
        sim.trans.last.0702,
  #simset.trans.0702,
        #simset.trans.0703,
        #simset.full.0703,
        facet.by = "age", split.by = "race", # age, sex, race; 1- and 2-way
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.trans.0703,
        #simset.full.0703,
        facet.by = "risk", split.by = "race", # race, risk, sex; 1- and 2-way 
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.trans.0703,
        #simset.full.0703,
        facet.by = "age", split.by = "sex", # race, risk, sex; 1- and 2-way 
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.trans.0703,
        #simset.full.0703,
        facet.by = "risk", split.by = "race", # race, risk, sex; 1- and 2-way 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.trans.0703,
        #simset.full.0703,
        facet.by = "age", split.by = "sex", # race, risk, sex; 1- and 2-way 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old,
        #simset.trans,
        #simset.full,
        #sim.old.last,
        #sim.trans.last,
        #sim.full.last,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.old,
        simset.trans,
        #simset.full,
        #sim.old.last,
        #sim.trans.last,
        #sim.full.last,
        facet.by = "age", # age, sex, race, risk; 1-way 
        outcomes = c("aids.diagnoses"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(#simset.old,
        simset.trans,
        #simset.full,
        #sim.old.last,
        #sim.trans.last,
        #sim.full.last,
        outcomes = c("aids.diagnoses"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(#simset.old,
        simset.trans,
        #simset.full,
        #sim.old.last,
        #sim.trans.last,
        #sim.full.last,
        #facet.by = "age", # age, sex, race, risk; 1-way 
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.old,
        simset.trans,
        #simset.full,
        #sim.old.last,
        #sim.trans.last,
        #sim.full.last,
        outcomes = c("cdc.hiv.test.positivity"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.trans.0703,
        #simset.full.0703,
        outcomes = c("awareness"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.old,
        #simset.trans,
        simset.full,
        #sim.old.last,
        #sim.trans.last,
        #sim.full.last,
        facet.by = "age", # age, sex, race, risk; 1-way 
        outcomes = c("suppression"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

