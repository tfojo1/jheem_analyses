source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

#full.lik = FULL.likelihood.instructions.with.aids$instantiate.likelihood('ehe','C.12580')

load("../jheem_analyses/prelim_results/full.with.aids_covid_2024-05-02_C.12580.Rdata")
sim.old = sim

load("../jheem_analyses/prelim_results/full.with.aids_2024-05-03_C.12580.Rdata")
sim.full = sim

load("../jheem_analyses/prelim_results/full.minus.testing_2024-05-03_C.12580.Rdata")
sim.full.minus.testing = sim

load("../jheem_analyses/prelim_results/full.minus.idu_2024-05-03_C.12580.Rdata")
sim.full.minus.idu = sim

engine = create.jheem.engine('ehe','c.12580',end.year = 2030)
params.old = sim.old$params
sim.old.2 = engine$run(parameters = sim.old$params)

exp(full.lik$compute.piecewise(sim.full) - full.lik$compute.piecewise(sim.old))

simplot(sim.old,
        sim.full,
        #sim.full.minus.testing,
        #sim.full.minus.idu,
        facet.by = "age", split.by = "race", 
        outcomes = c("population"), style.manager = create.style.manager(color.data.by = "stratum"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim.old,
        sim.full,
        #sim.full.minus.testing,
        #sim.full.minus.idu,
        #facet.by = "risk", split.by = "race", 
        outcomes = c("new"), style.manager = create.style.manager(color.data.by = "stratum"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim.old,
        sim.full,
        # sim.full.minus.testing,
        # sim.full.minus.idu,
        facet.by = "risk", split.by = "race", 
        outcomes = c("diagnosed.prevalence"), style.manager = create.style.manager(color.data.by = "stratum"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim.old,
        sim.full,
        #facet.by = "age", # age; 1-way 
        outcomes = c("proportion.using.heroin",
                     "proportion.using.cocaine"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.old,
        sim.full,
        #facet.by = "race", # age, sex, race, risk; 1-way 
        outcomes = c("proportion.general.population.tested"),
        dimension.values = list(year = 2000:2030)) 

simplot(#sim.old,
        sim.full,
        outcomes = c("cdc.hiv.test.positivity"),  # totals only 
        dimension.values = list(year = 2000:2030)) 

# Other plots to check: 
simplot(sim.old,
        sim.full,
        facet.by = "sex", # sex; 1-way 
        outcomes = c("hiv.mortality"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim.old,
        sim.full,
        outcomes = c("total.mortality"), # totals only 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.old,
        sim.full,
        outcomes = c("aids.deaths"), 
        dimension.values = list(year = 1981:2001)) 

simplot(sim.old,
        sim.full,
        #facet.by = "age", # age, sex, race, risk; 1-way 
        outcomes = c("aids.diagnoses"), 
        dimension.values = list(year = 1980:2030))

simplot(sim.old,
        sim.full,
        outcomes = c("awareness"), # totals only 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.old,
        sim.full,
        facet.by = "risk", # age, sex, race, risk; 1-way 
        outcomes = c("suppression"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.old,
        sim.full,
        facet.by = "age", # age, sex, race; 1- and 2-way 
        outcomes = c("prep.uptake"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.old,
        sim.full,
        facet.by = "age", # age, sex; 1-way 
        outcomes = c("prep.indications"), 
        dimension.values = list(year = 2000:2030)) 
