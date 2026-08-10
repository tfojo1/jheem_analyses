## TGA engine test and plotting 
source("applications/EHE/ehe_specification.R")
source("commoncode/locations_of_interest.R")
# source("applications/state_health_departments/register_oakland_tga.R") # moved code to locations of interest 

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum")

params = suppressWarnings(get.medians(EHE.PARAMETERS.PRIOR))
params['global.trate'] = 0.09

engine.oakland = create.jheem.engine('ehe', 'TGA.OAKLAND', end.year=2035, max.run.time.seconds = 10)
sim.oakland = engine.oakland$run(parameters = params)

engine.sf = create.jheem.engine('ehe', 'C.41860', end.year=2035, max.run.time.seconds = 10)
sim.sf = engine.sf$run(parameters = params)

sim = sim.oakland

simplot(sim,
        facet.by = "age", split.by = "race", # age, sex, race; 1- and 2-way
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        outcomes = c("population"),
        style.manager = source.style.manager, # use when looking at totals
        dimension.values = list(year = 2000:2030))

simplot(sim,
        #facet.by = "age", # age, race; 1-way
        outcomes = c("immigration","emigration"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030))

simplot(sim,
        outcomes = c("new"), 
        style.manager = source.style.manager, # use when looking at totals 
        dimension.values = list(year = 1980:2030)) 

simplot(sim,
        facet.by = "risk", split.by = "race", # race, risk, sex; 1- and 2-way 
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 

simplot(sim,
        facet.by = "risk", split.by = "sex", # race, risk, sex; 1- and 2-way 
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 

simplot(sim,
        facet.by = "age", split.by = "sex", # race, risk, sex; 1- and 2-way 
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 

simplot(sim,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim,
        facet.by = "risk", split.by = "race", # race, risk, sex; 1- and 2-way 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

simplot(sim,
        facet.by = "risk", split.by = "sex", # race, risk, sex; 1- and 2-way 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

simplot(sim,
        facet.by = "age", split.by = "sex", # race, risk, sex; 1- and 2-way 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

simplot(sim,
        facet.by = "sex", # sex; 1-way 
        outcomes = c("hiv.mortality"),
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim,
        outcomes = c("total.mortality"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

# must ask for years 1981-2001 
simplot(sim,
        outcomes = c("aids.deaths"),
        style.manager = location.style.manager,
        dimension.values = list(year = 1981:2001))

simplot(sim,
        outcomes = c("aids.diagnoses"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim,
        facet.by = "race", # age, sex, race, risk; 1-way 
        outcomes = c("aids.diagnoses"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim,
        facet.by = "risk", # age, sex, race, risk; 1-way 
        outcomes = c("aids.diagnoses"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim,
        facet.by = "age", # age, sex, race, risk; 1-way 
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        outcomes = c("cdc.hiv.test.positivity"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        outcomes = c("awareness"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        outcomes = c("suppression"), 
        summary.type = "median.and.interval",
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2040)) 

simplot(sim,
        facet.by = "race", # age, sex, race, risk; 1-way 
        outcomes = c("suppression"), 
        summary.type = "median.and.interval",
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2040)) 

simplot(sim,
        facet.by = "risk", # age, sex, race, risk; 1-way 
        outcomes = c("suppression"), 
        summary.type = "median.and.interval",
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2040)) 

simplot(sim,
        facet.by = "age", # age; 1-way 
        outcomes = c("proportion.using.heroin",
                     "proportion.using.cocaine"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        facet.by = "age", # age, sex, race; 1- and 2-way 
        outcomes = c("prep.uptake"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        facet.by = "age", # age, sex; 1-way 
        outcomes = c("prep.indications"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        outcomes = c("sexual.transmission.rates"), 
        style.manager = location.style.manager,
        plot.year.lag.ratio = T,
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        outcomes = c("total.hiv.tests.per.population"), 
        style.manager = location.style.manager,plot.year.lag.ratio = T,
        dimension.values = list(year = 2000:2030)) 
