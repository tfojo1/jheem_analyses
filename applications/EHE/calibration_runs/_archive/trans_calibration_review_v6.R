source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

#engine = create.jheem.engine('ehe','C.12580',end.year = 2030)

## NEW TRANS AND FULL (MINUS COVID) SIMSETS ## 
load('../jheem_analyses/prelim_results/init.transmission.ehe_simset_2024-07-06_C.12580.Rdata')
simset.trans = simset
simset.trans = rerun.simulations(simset.trans) # can just do copy.simulation.set instead 
sim.trans.last = simset.trans$last.sim() 

load('../jheem_analyses/prelim_results/full.with.aids_simset_2024-07-08_C.12580.Rdata')
simset.full.no.covid = simset
sim.full.last = simset.full.no.covid$last.sim() 

## OLD TRANS AND FULL SIMSETS ## 
load('../jheem_analyses/prelim_results/init.transmission.ehe_simset_2024-07-01_C.12580.Rdata')
simset.trans.old = simset
simset.trans.old.2 = copy.simulation.set(simset.trans.old)
sim.trans.old.last = simset.trans.old$last.sim() # doesn't work? 

load('../jheem_analyses/prelim_results/full.with.covid_simset_2024-07-05_C.12580.Rdata')
simset.full.old = simset
sim.full.old.last = simset.full.old$last.sim() # doesn't work? 

sim.x = rerun.simulations(sim.full.last)

trans.awareness = round(apply(simset.trans$last.sim()$diagnosed.prevalence,c("year","sex"),sum)/
        apply(simset.trans$last.sim()$infected,c("year","sex"),sum)[-1,],3)

# can't do this, but I remember msm awareness being way lower 
trans.awareness.old = round(apply(simset.trans.old$last.sim()$diagnosed.prevalence,c("year","sex"),sum)/
                          apply(simset.trans.old$last.sim()$infected,c("year","sex"),sum)[-1,],3)

simplot(simset.trans,
        #simset.trans.old,
        facet.by = "age", split.by = "race", # age, sex, race; 1- and 2-way
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.trans,
        #simset.trans.old,
        #simset.full.no.covid,
        outcomes = c("new"), 
        style.manager = source.style.manager, # use when looking at totals 
        dimension.values = list(year = 1980:2030)) 

simplot(simset.trans,
        #simset.trans.old,
        facet.by = "risk", split.by = "race", # race, risk, sex; 1- and 2-way 
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.trans.old,
        #simset.trans,
        simset.full.no.covid,
        facet.by = "age", split.by = "sex", # race, risk, sex; 1- and 2-way 
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 

simplot(#simset.trans,
        #simset.trans.old,
        simset.full.no.covid,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset.trans,
        simset.trans.old,
        facet.by = "risk", split.by = "race", # race, risk, sex; 1- and 2-way 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

simplot(#simset.trans.old,
        #simset.trans,
        simset.full.no.covid,
        facet.by = "age", split.by = "sex", # race, risk, sex; 1- and 2-way 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

simplot(simset.trans,
        facet.by = "sex", # sex; 1-way 
        outcomes = c("hiv.mortality"),
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset.trans,
        outcomes = c("total.mortality"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.trans,
        outcomes = c("aids.deaths"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 1981:2001)) 

simplot(simset.trans,
        facet.by = "age", # age, sex, race, risk; 1-way 
        outcomes = c("aids.diagnoses"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset.trans,
        outcomes = c("aids.diagnoses"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(#simset.trans.old,
        #simset.trans,
        simset.full.no.covid,
        #sim.full.last,
        #sim.x,
        #facet.by = "age", # age, sex, race, risk; 1-way 
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.trans,
        outcomes = c("cdc.hiv.test.positivity"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.trans.old,
        simset.trans,
        #simset.full.no.covid,
        outcomes = c("awareness"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 


total.gonorrhea.year.on.year.change.likelihood.instructions = 
  create.time.lagged.comparison.likelihood.instructions(outcome.for.data = "gonorrhea.ratio", # zoe changing 0-14 to 13-14, throw out 'unknown'
                                                        outcome.for.sim = "sexual.transmission.rates", 
                                                        # (2020 gon diagnoses / 2019 gon diagnoses) proportional to 
                                                        # (2020 sexual transmisson/2019 sexual transmission)
                                                        levels.of.stratification = c(0), 
                                                        #dimensions = c("sex","race","age"),
                                                        from.year = 2008, 
                                                        observation.correlation.form = 'compound.symmetry', 
                                                        error.variance.term = 0.03, # pick a smarter one
                                                        error.variance.type = 'cv',
                                                        correlation.different.years = 0.5,
                                                        weights = list(1), 
                                                        equalize.weight.by.year = T 
  )

test.change.lik = number.of.tests.year.on.year.change.likelihood.instructions$instantiate.likelihood('ehe','C.12580')
gon.lik = gonorrhea.year.on.year.change.likelihood.instructions$instantiate.likelihood('ehe','C.12580')
total.gon.lik = total.gonorrhea.year.on.year.change.likelihood.instructions$instantiate.likelihood('ehe','C.12580')
syph.lik = ps.syphilis.year.on.year.change.likelihood.instructions$instantiate.likelihood('ehe','C.12580')

test.change.lik$compute(sim.trans.last)
gon.lik$compute(sim.trans.last, debug = T)
total.gon.lik$compute(sim.trans.last, debug = T)
syph.lik$compute(sim.trans.last)

simplot(sim.trans.last,
        outcomes = c("sexual.transmission.rates"),
        style.manager = source.style.manager,
        plot.year.lag.ratio = T,
        dimension.values = list(year = 2000:2030))

save(sim.trans.last,file="../jheem_analyses/prelim_results/sim.trans.last.Rdata")
