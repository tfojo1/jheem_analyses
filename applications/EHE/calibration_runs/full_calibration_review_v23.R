source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum") # this is the default right now 

load("../jheem_analyses/prelim_results/full.with.covid2_simset_2025-01-14_C.35620.Rdata")
simset.new = simset

load("../jheem_analyses/prelim_results/full.with.covid2_simset_2025-01-10_C.35620.Rdata")
simset.old = simset

simset = simset.new

range(simset$get.mcmc.mixing.statistic(NULL))
cbind(head(simset$get.mcmc.mixing.statistic(NULL),20))

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "age", split.by = "race", # age, sex, race; 1- and 2-way
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        outcomes = c("population"), 
        style.manager = source.style.manager, # use when looking at totals 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        #facet.by = "age", # age, race; 1-way 
        outcomes = c("immigration","emigration"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        outcomes = c("new"), 
        style.manager = source.style.manager, # use when looking at totals 
        dimension.values = list(year = 1980:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "risk", split.by = "race", # race, risk, sex; 1- and 2-way 
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "age", split.by = "sex", # race, risk, sex; 1- and 2-way 
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "risk", split.by = "race", # race, risk, sex; 1- and 2-way 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "age", split.by = "sex", # race, risk, sex; 1- and 2-way 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "sex", # sex; 1-way 
        outcomes = c("hiv.mortality"),
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        outcomes = c("total.mortality"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

# must ask for years 1981-2001 
simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        outcomes = c("aids.deaths"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 1981:2001)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        outcomes = c("aids.diagnoses"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "race", # age, sex, race, risk; 1-way 
        outcomes = c("aids.diagnoses"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "risk", # age, sex, race, risk; 1-way 
        outcomes = c("aids.diagnoses"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "age", # age, sex, race, risk; 1-way 
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        outcomes = c("cdc.hiv.test.positivity"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        outcomes = c("awareness"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        outcomes = c("suppression"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "race", # age, sex, race, risk; 1-way 
        outcomes = c("suppression"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "risk", # age, sex, race, risk; 1-way 
        outcomes = c("suppression"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "age", # age; 1-way 
        outcomes = c("proportion.using.heroin",
                     "proportion.using.cocaine"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "age", # age, sex, race; 1- and 2-way 
        outcomes = c("prep.uptake"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "age", # age, sex; 1-way 
        outcomes = c("prep.indications"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        outcomes = c("sexual.transmission.rates"), 
        style.manager = location.style.manager,
        plot.year.lag.ratio = T,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        outcomes = c("total.hiv.tests.per.population"), 
        style.manager = location.style.manager,plot.year.lag.ratio = T,
        dimension.values = list(year = 2000:2030)) 