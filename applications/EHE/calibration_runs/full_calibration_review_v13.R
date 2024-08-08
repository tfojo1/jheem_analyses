source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum")

load('../jheem_analyses/prelim_results/full.with.covid2_simset_2024-08-07_C.12580.Rdata')
simset.full = simset
sim.full.first = simset$first.sim()
sim.full.last = simset$last.sim()

engine = create.jheem.engine('ehe','C.12580',end.year = 2030)
params.mcmc = sim.full.last$params

# params.mcmc["black.proportion.tested.slope.or"] = 1
# params.mcmc["hispanic.proportion.tested.slope.or"] = 1

params.manual = params.mcmc

cbind(params.manual[POPULATION.PARAMETERS.PRIOR@var.names])

params.manual['other.age5.migration.multiplier.time.1'] = 2.5 # 0.52212255
params.manual['other.age5.migration.multiplier.time.2'] = 2 # 0.92907009
sim.manual = engine$run(params.manual)

full.lik = FULL.likelihood.instructions.with.covid$instantiate.likelihood('ehe','C.12580')

# second minus first
full.lik$compute(sim.full.last,sim.manual)

exp(full.lik$compute.piecewise(sim.manual) - full.lik$compute.piecewise(sim.full.last))

simplot(#simset,
        #sim.full.first,
        sim.full.last,
        sim.manual,
        facet.by = "age", split.by = "race", 
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.full.last,
        sim.manual,
        facet.by = "risk", split.by = "race", 
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.full.last,
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

simplot(sim.full.last,
        sim.manual,
        facet.by = "age", 
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
