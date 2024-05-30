source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

# Need to have this file locally
# load("../jheem_analyses/prelim_results/full.with.aids.minus.prep_2024-04-20_C.12580.Rdata")

## --- ANDREW TO LOOK AT ---- ## 
## @Andrew: when plotting the below by age/sex, the colors are flipped for the points vs. lines 
simplot(sim,
        facet.by = "age", split.by = "sex", # race, risk, sex; 1- and 2-way 
        outcomes = c("new"), style.manager = create.style.manager(color.data.by = "stratum"),
        dimension.values = list(year = 2000:2030)) 

## --- STANDARD PLOTS ---- ## 
simplot(sim,
        facet.by = "age", split.by = "race", # likelihoood: age, sex, race; 1- and 2-way
        outcomes = c("population"), style.manager = create.style.manager(color.data.by = "stratum"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        #facet.by = "age", # age, race; 1-way 
        outcomes = c("immigration","emigration"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        facet.by = "risk", split.by = "race", # race, risk, sex; 1- and 2-way 
        outcomes = c("new"), style.manager = create.style.manager(color.data.by = "stratum"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        facet.by = "risk", split.by = "race", # race, risk, sex; 1- and 2-way 
        outcomes = c("diagnosed.prevalence"), style.manager = create.style.manager(color.data.by = "stratum"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        facet.by = "sex", # sex; 1-way 
        outcomes = c("hiv.mortality"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        outcomes = c("total.mortality"), # totals only 
        dimension.values = list(year = 2000:2030)) 

# must ask for years 1981-2001 since the data is for that whole block
simplot(sim,
        #facet.by = "race", # sex, race, risk; 1-way --> these don't plot? 
        outcomes = c("aids.deaths"), 
        dimension.values = list(year = 1981:2001)) 

# sim has a negative value which gets cut out since the y scale starts at 0
simplot(sim,
        #facet.by = "age", # age, sex, race, risk; 1-way 
        outcomes = c("aids.diagnoses"), 
        dimension.values = list(year = 1980:2030)) 

simplot(sim,
        #facet.by = "race", # age, sex, race, risk; 1-way 
        outcomes = c("proportion.general.population.tested"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        outcomes = c("cdc.hiv.test.positivity"), # totals only 
        # plot.which = "sim.only",
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        outcomes = c("awareness"), # totals only 
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        facet.by = "risk", # age, sex, race, risk; 1-way 
        outcomes = c("suppression"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        #facet.by = "age", # age; 1-way 
        outcomes = c("proportion.using.heroin",
                     "proportion.using.cocaine"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        facet.by = "age", # age, sex, race; 1- and 2-way 
        outcomes = c("prep.uptake"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        facet.by = "age", # age, sex; 1-way 
        outcomes = c("prep.indications"), 
        dimension.values = list(year = 2000:2030)) 


# simplot(sim,outcomes = "total.hiv.tests")
