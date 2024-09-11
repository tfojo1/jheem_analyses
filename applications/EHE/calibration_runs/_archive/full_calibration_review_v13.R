source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum")

load('../jheem_analyses/prelim_results/full.with.covid2_simset_2024-08-08_C.12580.Rdata')
simset.full = simset
sim.full.first = simset.full$first.sim()
sim.full.last = simset.full$last.sim()

engine = create.jheem.engine('ehe','C.12580',end.year = 2030)
params.mcmc = sim.full.last$params

params.manual = params.mcmc

#cbind(params.manual[POPULATION.PARAMETERS.PRIOR@var.names])

params.manual['other.age5.migration.multiplier.time.1'] = 2.5 # 0.825711
params.manual['other.age5.migration.multiplier.time.2'] = 2 # 1.282584 
sim.manual = engine$run(params.manual)

full.lik = FULL.likelihood.instructions.with.covid$instantiate.likelihood('ehe','C.12580') 
pop.lik = population.likelihood.instructions$instantiate.likelihood('ehe','C.12580')
imm.lik = immigration.likelihood.instructions$instantiate.likelihood('ehe','C.12580')
em.lik = emigration.likelihood.instructions$instantiate.likelihood('ehe','C.12580')
new.lik = new.diagnoses.likelihood.instructions$instantiate.likelihood('ehe','C.12580')
prev.lik = prevalence.likelihood.instructions$instantiate.likelihood('ehe','C.12580')

tst.chnge.lik = number.of.tests.year.on.year.change.likelihood.instructions$instantiate.likelihood('ehe','C.12580')
gon.lik = gonorrhea.year.on.year.change.likelihood.instructions$instantiate.likelihood('ehe','C.12580')
ps.lik = ps.syphilis.year.on.year.change.likelihood.instructions$instantiate.likelihood('ehe','C.12580')
gon.non.ratio.lik = gonorrhea.non.ratio.year.on.year.change.likelihood.instructions$instantiate.likelihood('ehe','C.12580')


# second minus first
full.lik$compare.sims(sim.full.last,sim.manual)
pop.lik$compare.sims(sim.full.last,sim.manual)
imm.lik$compare.sims(sim.full.last,sim.manual)
em.lik$compare.sims(sim.full.last,sim.manual)
new.lik$compare.sims(sim.full.last,sim.manual)
prev.lik$compare.sims(sim.full.last,sim.manual)
tst.chnge.lik$compare.sims(sim.full.last,sim.manual)
gon.lik$compare.sims(sim.full.last,sim.manual)
ps.lik$compare.sims(sim.full.last,sim.manual)
gon.age.lik$compare.sims(sim.full.last,sim.manual)
gon.non.ratio.lik$compare.sims(sim.full.last,sim.manual)

exp(gon.non.ratio.lik$compute.piecewise(sim.manual) - gon.non.ratio.lik$compute.piecewise(sim.full.last,debug = T))



simplot(#simset.full,
        #sim.full.first,
        sim.full.last,
        sim.manual,
        facet.by = "age", split.by = "race", 
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.full.last,
        sim.manual,
        outcomes = c("sexual.transmission.rates"), 
        facet.by = "age",
        #plot.year.lag.ratio = T,
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.full,
        #sim.full.first,
        sim.full.last,
        sim.manual,
        #facet.by = "age", split.by = "sex", 
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.full,
        #sim.full.first,
        sim.full.last,
        sim.manual,
        #facet.by = "age", split.by = "sex", 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.full.last,
        sim.manual,
        #facet.by = "age", # age, race; 1-way 
        outcomes = c("immigration","emigration"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.full.first,
        sim.full.last,
        #sim.manual,
        facet.by = "risk", 
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(#simset,
        sim.full.last,
        outcomes = c("awareness"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.full.last,
        sim.manual,
        facet.by = "age", # age, sex, race, risk; 1-way 
        outcomes = c("suppression"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 


gonorrhea.non.ratio.year.on.year.change.likelihood.instructions = 
  create.time.lagged.comparison.likelihood.instructions(outcome.for.data = "gonorrhea", # zoe changing 0-14 to 13-14, throw out 'unknown'
                                                        outcome.for.sim = "sexual.transmission.rates", 
                                                        # (2020 gon diagnoses / 2019 gon diagnoses) proportional to 
                                                        # (2020 sexual transmisson/2019 sexual transmission)
                                                        levels.of.stratification = c(0,1), 
                                                        dimensions = c("sex","race","age"),
                                                        from.year = 2008, 
                                                        observation.correlation.form = 'compound.symmetry', 
                                                        error.variance.term = 0.03, # pick a smarter one
                                                        error.variance.type = 'cv',
                                                        correlation.different.years = 0.5,
                                                        weights = list(1), 
                                                        equalize.weight.by.year = F 
  )
