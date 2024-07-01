source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

load('../jheem_analyses/prelim_results/full.with.aids_simset_2024-06-28_C.12580.Rdata')
simset.no.covid = simset

load('../jheem_analyses/prelim_results/full.with.covid_simset_2024-06-28_C.12580.Rdata')
simset.covid = simset

full.lik = FULL.likelihood.instructions.with.covid$instantiate.likelihood('ehe','C.12580')

covid.tests.change.lik = number.of.tests.year.on.year.change.likelihood.instructions$instantiate.likelihood('ehe','C.12580')

save(sim.manual,file=paste0("prelim_results/sim.manual.Rdata"))

engine = create.jheem.engine('ehe','C.12580',end.year = 2030)

params.mcmc = simset$last.sim()$params
sim.manual = engine$run(parameters = params.mcmc)

simplot(simset.covid,
        #facet.by = "race", # age, sex, race, risk; 1-way 
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.covid,
        #facet.by = "race", # age, sex, race, risk; 1-way 
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset.covid,
        facet.by = "risk", split.by = "race", # race, risk, sex; 1- and 2-way 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.covid,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 
