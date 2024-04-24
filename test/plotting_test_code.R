source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

# Need to have this file locally
load("../jheem_analyses/prelim_results/full.with.aids.minus.prep_2024-04-20_C.12580.Rdata")

simplot(sim,
        facet.by = "age", split.by = "race", 
        outcomes = c("population"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        outcomes = c("immigration","emigration"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        facet.by = "risk", split.by = "race", 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        facet.by = "race", 
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim.full.minus.prep,
        facet.by = "sex", 
        outcomes = c("hiv.mortality"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        outcomes = c("total.mortality"),
        dimension.values = list(year = 2000:2030)) 

# not plotting because the data are cumulative estimates
# aids deaths won't plot data unless you ask for years 1981-2001 since the data is for that whole block
simplot(sim,
        outcomes = c("aids.deaths"), 
        dimension.values = list(year = 2000:2030)) 

# sim has a negative value which gets cut out since the y scale starts at 0
simplot(sim,
        outcomes = c("aids.diagnoses"), 
        dimension.values = list(year = 1980:2030)) 

simplot(sim,
        outcomes = c("proportion.general.population.tested"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        outcomes = c("hiv.test.positivity"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        outcomes = c("awareness"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        facet.by = "risk", 
        outcomes = c("suppression"), # we have age/sex/race/risk all one way
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        outcomes = c("proportion.using.heroin","proportion.using.cocaine"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        outcomes = c("prep.uptake","prep.indications"),
        dimension.values = list(year = 2000:2030)) 
