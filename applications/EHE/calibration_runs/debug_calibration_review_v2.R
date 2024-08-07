source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum")

load('../jheem_analyses/prelim_results/full.with.covid.0_simset_2024-08-06_C.12580.Rdata')
sim.last = simset$last.sim()
sim.first = simset$first.sim()

full.lik = FULL.likelihood.instructions.with.covid$instantiate.likelihood('ehe','C.12580')
exp(full.lik$compute.piecewise(sim.last) - full.lik$compute.piecewise(sim.first))

# engine = create.jheem.engine('ehe','C.12580',end.year = 2030)
# sim = engine$run(sim.last$params)


simplot(#simset,
        sim.first,
        sim.last,
        facet.by = "risk", split.by = "race", 
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(#simset,
        sim.first,
        sim.last,
        facet.by = "risk", split.by = "race", 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) 

simplot(#simset,
        sim.first,
        sim.last,
        facet.by = "risk", # age, sex, race, risk; 1-way 
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(#simset,
        sim.first,
        sim.last,
        outcomes = c("awareness"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(#simset,
        sim.first,
        sim.last,
        outcomes = c("cdc.hiv.test.positivity"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset,
        #sim.first,
        #sim.last,
        facet.by = "age", split.by = "race", # age, sex, race; 1- and 2-way
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 
