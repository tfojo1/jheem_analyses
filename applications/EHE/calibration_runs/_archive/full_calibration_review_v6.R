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

simplot(sim.trans.last,
        sim.covid.last,
        facet.by = "age", split.by = "race", 
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.trans.last,
        sim.covid.last,
        outcomes = c("new"), 
        style.manager = source.style.manager, 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.trans.last,
        sim.covid.last,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.trans.last,
        sim.covid.last,
        facet.by = "risk", split.by = "race", 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) 
