source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

load('../jheem_analyses/prelim_results/init.transmission.ehe_simset_2024-06-27_C.12580.Rdata')

sim.last = simset$last.sim()
sim.first = simset$first.sim()

#full.lik = FULL.likelihood.instructions.with.aids$instantiate.likelihood('ehe','C.12580')
#trans.lik = two.way.transmission.pop.idu.likelihood.instructions$instantiate.likelihood('ehe','C.12580')

exp(trans.lik$compute.piecewise(sim.first) - trans.lik$compute.piecewise(sim.last))

cbind(sim.first$params[grepl("test",names(sim.first$params))],
      sim.last$params[grepl("test",names(sim.last$params))])

simplot(simset,
        #facet.by = "race", # age, sex, race, risk; 1-way 
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.first, sim.last,
        #facet.by = "race", # age, sex, race, risk; 1-way 
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim.first, sim.last,
        facet.by = "risk", split.by = "race", # race, risk, sex; 1- and 2-way 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.first, sim.last,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 
