source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

#full.lik = FULL.likelihood.instructions.with.aids$instantiate.likelihood('ehe','C.12580')

load("../jheem_analyses/prelim_results/init.transmission.ehe_2024-05-07_C.12580.Rdata")
sim.trans.with.pop = sim

load("../jheem_analyses/prelim_results/full.with.aids_2024-05-04_C.12580.Rdata")
sim.5.4 = sim

load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-03-29_C.12580.Rdata")
sim.pop = sim

load("../jheem_analyses/prelim_results/init.transmission.ehe_2024-04-25_C.12580.Rdata")
sim.trans.without.pop = sim

pop.params = sim.pop$parameters[,1]
pop.params.names = names(pop.params)
full.params.names = names(sim.5.4$params)
new.params.names = setdiff(full.params.names,pop.params.names)
pop.params[new.params.names] = 1
engine = create.jheem.engine('ehe','C.12580',end.year = 2030)
sim.pop = engine$run(parameters = pop.params)

round(cbind(pop.params[names(sim.trans.with.pop$params)],sim.trans.with.pop$params),4)
# a couple parameters missing from par.names.pop
# transmission calibration has to bring down new diagnoses/prevalence a lot; brings down het and IDU trates; other msm
# changes aging multipliers (nothing extreme though?)
# increases incident IDU multipliers a lot 

simplot(sim.pop, # pop calibration, but have to rerun with new engine so possible this isn't what the plot originally looked like
        sim.trans.without.pop, # last time I ran transmission, WITHOUT pop parameters
        sim.trans.with.pop, # this time I ran transmission, WITH pop parameters
        #sim.5.4, # this is the full calibration from last week 
        facet.by = "age", split.by = "race", 
        outcomes = c("population"), style.manager = create.style.manager(color.data.by = "stratum"),
        dimension.values = list(year = 2000:2030)) 

simplot(#sim.pop, 
        sim.trans.without.pop, 
        sim.trans.with.pop,
        #sim.5.4,
        #facet.by = "risk", split.by = "race", 
        outcomes = c("new"), style.manager = create.style.manager(color.data.by = "stratum"),
        dimension.values = list(year = 2000:2030)) 

simplot(#sim.pop, 
        sim.trans.without.pop, 
        sim.trans.with.pop,
        #sim.5.4,
        #facet.by = "risk", split.by = "race", 
        outcomes = c("diagnosed.prevalence"), style.manager = create.style.manager(color.data.by = "stratum"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim.pop,
        sim.trans.with.pop,
        #sim.5.4,
        #facet.by = "race", # age, sex, race, risk; 1-way 
        outcomes = c("proportion.general.population.tested"),
        dimension.values = list(year = 2000:2030)) 

