source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

#engine = create.jheem.engine('ehe','C.12580',end.year = 2030)

load('../jheem_analyses/prelim_results/init.transmission.ehe_simset_2024-07-06_C.12580.Rdata')
simset.trans = simset
simset.trans = rerun.simulations(simset.trans)
sim.trans.last = simset.trans$last.sim() 

load('../jheem_analyses/prelim_results/init.transmission.ehe_simset_2024-07-02_C.12580.Rdata')
simset.trans.old = simset
sim.trans.old.last = simset.trans.old$last.sim() # doesn't work? 

load('../jheem_analyses/prelim_results/full.with.aids_simset_2024-07-08_C.12580.Rdata')
simset.full.no.covid = simset
sim.full.last = simset.full.no.covid$last.sim() 


simplot(simset.trans,
        facet.by = "age", split.by = "race", # age, sex, race; 1- and 2-way
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.trans,
        outcomes = c("new"), 
        style.manager = source.style.manager, # use when looking at totals 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.trans,
        facet.by = "risk", split.by = "race", # race, risk, sex; 1- and 2-way 
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.trans,
        facet.by = "age", split.by = "sex", # race, risk, sex; 1- and 2-way 
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.trans,
        facet.by = "risk", split.by = "race", # race, risk, sex; 1- and 2-way 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

simplot(simset.trans,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset.trans,
        facet.by = "age", split.by = "sex", # race, risk, sex; 1- and 2-way 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.trans,
        facet.by = "sex", # sex; 1-way 
        outcomes = c("hiv.mortality"),
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset.trans,
        outcomes = c("total.mortality"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.trans,
        outcomes = c("aids.deaths"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 1981:2001)) 

simplot(simset.trans,
        facet.by = "age", # age, sex, race, risk; 1-way 
        outcomes = c("aids.diagnoses"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset.trans,
        outcomes = c("aids.diagnoses"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset.trans,
        #facet.by = "risk", # age, sex, race, risk; 1-way 
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.trans,
        outcomes = c("cdc.hiv.test.positivity"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.trans,
        outcomes = c("awareness"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 
