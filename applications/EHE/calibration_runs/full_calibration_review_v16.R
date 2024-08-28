source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum")

load('../jheem_analyses/prelim_results/full.with.aids_simset_2024-08-25_C.12580.Rdata')
simset.0825 = simset
sim.0825.last = simset.0825$last.sim()

load('../jheem_analyses/prelim_results/full.with.aids_simset_2024-08-26_C.26420.Rdata')
simset.houston = simset

load('../jheem_analyses/prelim_results/full.with.aids_simset_2024-08-26_C.35620.Rdata')
simset.nyc = simset

full.with.aids.lik = FULL.likelihood.instructions.with.aids$instantiate.likelihood('ehe','C.12580')
# full.with.aids.lik$compare.sims(sim.0820.last,sim.manual) # second minus first
# exp(full.with.aids.lik$compute(sim.manual) - full.with.aids.lik$compute(sim.0820.last))

simplot(simset.0825,
        sim.0825.last,
        facet.by = "age", split.by = "race", 
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.0825,
        sim.0825.last,
        outcomes = c("new"), 
        style.manager = source.style.manager, 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.0825,
        sim.0825.last,
        facet.by = "risk", split.by = "race", 
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.0825,
        sim.0825.last,
        facet.by = "age", split.by = "sex", 
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.0825,
        sim.0825.last,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.0825,
        sim.0825.last,
        facet.by = "risk", split.by = "race", 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.0825,
        sim.0825.last,
        facet.by = "age", split.by = "sex",
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.0825,
        sim.0825.last,
        facet.by = "age",
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.0825,
        sim.0825.last,
        outcomes = c("cdc.hiv.test.positivity"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.0825,
        sim.0825.last,
        outcomes = c("awareness"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

