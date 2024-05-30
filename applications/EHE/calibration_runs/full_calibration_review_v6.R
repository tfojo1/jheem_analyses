source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

load("../jheem_analyses/prelim_results/init.transmission.ehe_simset_2024-05-22_C.12580.RData")
simset.trans = simset
sim.trans.last = simset.trans[simset.trans$n.sim]
#sim.first = simset[1]
#sim.burn = simset$burn(keep = 1)

load("../jheem_analyses/prelim_results/full.with.covid_simset_2024-05-30_C.12580.Rdata")
simset.covid = simset
sim.covid.last = simset.covid[simset.covid$n.sim]

tests.change.lik = number.of.tests.year.on.year.change.likelihood.instructions$instantiate.likelihood('ehe','C.12580')
gon.change.lik = gonorrhea.year.on.year.change.likelihood.instructions$instantiate.likelihood('ehe','C.12580')
syph.change.lik = ps.syphilis.year.on.year.change.likelihood.instructions$instantiate.likelihood('ehe','C.12580')

exp(tests.change.lik$compute.piecewise(sim.covid.last) - tests.change.lik$compute.piecewise(sim.trans.last))
exp(gon.change.lik$compute.piecewise(sim.covid.last) - gon.change.lik$compute.piecewise(sim.trans.last))
exp(syph.change.lik$compute.piecewise(sim.covid.last) - syph.change.lik$compute.piecewise(sim.trans.last))

{
  simplot(simset.pop,
          #simset.trans,
          #sim.trans.last,
          facet.by = "age", 
          split.by = "race", 
          summary.type = "median.and.interval", # individual.simulation
          outcomes = c("population"), style.manager = create.style.manager(color.data.by = "stratum"),
          dimension.values = list(year = 2000:2030)) 
  
  simplot(simset.trans,
          facet.by = "risk",
          split.by = "race",
          summary.type = "median.and.interval",
          outcomes = c("new"), style.manager = create.style.manager(color.data.by = "stratum"),
          dimension.values = list(year = 2000:2030)) 
  
  simplot(simset.trans,
          facet.by = "risk",
          split.by = "race",
          #summary.type = "median.and.interval",
          outcomes = c("diagnosed.prevalence"), style.manager = create.style.manager(color.data.by = "stratum"),
          dimension.values = list(year = 2000:2030)) 
  
  simplot(simset.trans,
          #facet.by = "race", # age, sex, race, risk; 1-way 
          outcomes = c("proportion.general.population.tested"),
          dimension.values = list(year = 2000:2030)) 
  
  
  simplot(simset.trans,
          facet.by = "sex", # sex; 1-way 
          outcomes = c("hiv.mortality"),
          dimension.values = list(year = 2000:2030)) 
  
  simplot(simset.trans,
          outcomes = c("total.mortality"), # totals only 
          dimension.values = list(year = 2000:2030)) 
  
  simplot(simset.trans,
          #facet.by = "race", # sex, race, risk; 1-way --> these don't plot? 
          outcomes = c("aids.deaths"), 
          dimension.values = list(year = 1981:2001)) 
  
  simplot(simset.trans,
          #facet.by = "age", # age, sex, race, risk; 1-way 
          outcomes = c("aids.diagnoses"), 
          dimension.values = list(year = 1980:2030)) 
  
  simplot(simset.trans,
          outcomes = c("cdc.hiv.test.positivity"), # totals only 
          #plot.which = "sim.only",
          dimension.values = list(year = 2000:2030)) 
  
  simplot(simset.trans,
          outcomes = c("awareness"), # totals only 
          dimension.values = list(year = 2000:2030)) 
  
  simplot(simset.trans,
          facet.by = "risk", # age, sex, race, risk; 1-way 
          outcomes = c("suppression"), 
          dimension.values = list(year = 2000:2030)) 
  
  simplot(simset.trans,
          #facet.by = "age", # age; 1-way 
          outcomes = c("proportion.using.heroin",
                       "proportion.using.cocaine"), 
          dimension.values = list(year = 2000:2030)) 
  
  simplot(simset.trans,
          facet.by = "age", # age, sex, race; 1- and 2-way 
          outcomes = c("prep.uptake"), 
          dimension.values = list(year = 2000:2030)) 
  
  simplot(simset.trans,
          facet.by = "age", # age, sex; 1-way 
          outcomes = c("prep.indications"), 
          dimension.values = list(year = 2000:2030)) 
  
}
