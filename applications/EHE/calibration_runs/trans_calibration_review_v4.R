source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

#full.lik = FULL.likelihood.instructions.with.aids$instantiate.likelihood('ehe','C.12580')

load('../jheem_analyses/prelim_results/init.pop.ehe_simset_2024-05-10_C.12580.Rdata')
simset.pop = simset

load('../jheem_analyses/prelim_results/init.transmission.ehe_simset_2024-05-13_C.12580.Rdata')
simset.trans = simset
sim.trans = simset.trans$burn(keep=1)

load('../jheem_analyses/prelim_results/trans.no.pop_simset_2024-05-13_C.12580.Rdata')
simset.trans.no.pop = simset
sim.trans.no.pop = simset.trans.no.pop$burn(keep=1)

# trans with pop parameters is better on: new, prev, prop tested (but not that much better, visually)
# trans without pop parameters is better on: pop, imm/em
round(exp(full.lik$compute.piecewise(sim.trans) - full.lik$compute.piecewise(sim.trans.no.pop)),5)
round(exp(full.lik$compute.piecewise(sim.trans.no.pop) - full.lik$compute.piecewise(sim.trans)),5)

simplot(simset.pop,
        #sim.trans,
        sim.trans.no.pop,
        facet.by = "age", split.by = "race", # likelihoood: age, sex, race; 1- and 2-way
        outcomes = c("population"), style.manager = create.style.manager(color.data.by = "stratum"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim.trans,
        sim.trans.no.pop,
        #simset.trans,
        #simset.trans.no.pop,
        #facet.by = "risk", split.by = "race", # race, risk, sex; 1- and 2-way 
        outcomes = c("new"), style.manager = create.style.manager(color.data.by = "stratum"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim.trans,
        sim.trans.no.pop,
        #simset.trans,
        #simset.trans.no.pop,
        facet.by = "risk", split.by = "race", # race, risk, sex; 1- and 2-way 
        outcomes = c("diagnosed.prevalence"), style.manager = create.style.manager(color.data.by = "stratum"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim.trans,
        sim.trans.no.pop,
        #simset.trans.no.pop,
        #facet.by = "race", # age, sex, race, risk; 1-way 
        outcomes = c("proportion.general.population.tested"),
        dimension.values = list(year = 2000:2030)) 

simplot(simset,
        #facet.by = "age", # age, race; 1-way 
        outcomes = c("immigration","emigration"), 
        dimension.values = list(year = 2000:2030)) 
