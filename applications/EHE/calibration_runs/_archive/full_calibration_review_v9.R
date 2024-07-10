source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

full.lik = FULL.likelihood.instructions.with.covid$instantiate.likelihood('ehe','C.12580')

load('../jheem_analyses/prelim_results/full.with.aids_simset_2024-07-08_C.12580.Rdata')
simset.full.no.covid.07.08 = simset
sim.07.08 = simset.full.no.covid.07.08$last.sim()

load('../jheem_analyses/prelim_results/full.with.aids_simset_2024-07-09_C.12580.Rdata')
simset.full.no.covid.07.09 = simset
sim.07.09 = simset.full.no.covid.07.09$last.sim()

load('../jheem_analyses/prelim_results/init.transmission.ehe_simset_2024-07-06_C.12580.Rdata')
simset.trans = simset
sim.trans = simset.trans$last.sim()

exp(full.lik$compute.piecewise(sim.07.08) - full.lik$compute.piecewise(sim.07.09))

simplot(#simset.full.no.covid.07.08,
        #simset.full.no.covid.07.09,
        sim.07.08,
        sim.07.09,
        facet.by = "age", split.by = "race", # age, sex, race; 1- and 2-way
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.full.no.covid.07.08,
        #simset.full.no.covid.07.09,
        sim.07.08,
        sim.07.09,
        outcomes = c("population"), 
        style.manager = source.style.manager, # use when looking at totals 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.full.no.covid.07.08,
        simset.full.no.covid.07.09,
        outcomes = c("new"), 
        style.manager = source.style.manager, # use when looking at totals 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.full.no.covid.07.08,
        simset.full.no.covid.07.09,
        facet.by = "risk", split.by = "race", # race, risk, sex; 1- and 2-way 
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.full.no.covid.07.08,
        simset.full.no.covid.07.09,
        #sim.07.09,
        facet.by = "age", split.by = "sex", # race, risk, sex; 1- and 2-way 
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.full.no.covid.07.08,
        #simset.full.no.covid.07.09,
        sim.07.08,
        sim.07.09,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.full.no.covid.07.08,
        simset.full.no.covid.07.09,
        #sim.07.08,
        #sim.07.09,
        facet.by = "risk", split.by = "race", # race, risk, sex; 1- and 2-way 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.full.no.covid.07.08,
        simset.full.no.covid.07.09,
        facet.by = "age", split.by = "sex", # race, risk, sex; 1- and 2-way 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.full.no.covid.07.08,
        simset.full.no.covid.07.09,
        facet.by = "sex", # sex; 1-way 
        outcomes = c("hiv.mortality"),
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.full.no.covid.07.08,
        simset.full.no.covid.07.09,
        outcomes = c("total.mortality"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 


simplot(simset.full.no.covid.07.08,
        simset.full.no.covid.07.09,
        facet.by = "age", # age, sex, race, risk; 1-way 
        outcomes = c("aids.diagnoses"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset.full.no.covid.07.08,
        simset.full.no.covid.07.09,
        outcomes = c("aids.diagnoses"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset.full.no.covid.07.08,
        simset.full.no.covid.07.09,
        #facet.by = "risk", # age, sex, race, risk; 1-way 
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.full.no.covid.07.08,
        simset.full.no.covid.07.09,
        outcomes = c("cdc.hiv.test.positivity"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.full.no.covid.07.08,
        simset.full.no.covid.07.09,
        outcomes = c("awareness"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.full.no.covid.07.08,
        simset.full.no.covid.07.09,
        facet.by = "age", # age, sex, race, risk; 1-way 
        outcomes = c("suppression"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 




gon.lik = gonorrhea.year.on.year.change.likelihood.instructions$instantiate.likelihood('ehe','C.12580')
syph.lik = ps.syphilis.year.on.year.change.likelihood.instructions$instantiate.likelihood('ehe','C.12580')

gon.lik$compute(sim.trans, debug = T)
syph.lik$compute(sim.trans)
total.gon.lik$compute(sim.trans, debug = T)

