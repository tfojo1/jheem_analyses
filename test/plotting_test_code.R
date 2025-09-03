source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations.R')

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum") # this is the default right now 

LOCATION = 'WI'

CALIBRATION.CODE.TO.PULL = 'final.ehe.state'

# simset = assemble.simulations.from.calibration(version = 'ehe',
#                                                location = LOCATION,
#                                                allow.incomplete = T,
#                                                chains = c(1:4),
#                                                calibration.code = CALIBRATION.CODE.TO.PULL)

simset = retrieve.simulation.set('ehe', LOCATION, CALIBRATION.CODE.TO.PULL, n.sim=1000)
#save(simset,file=paste0("prelim_results/",CALIBRATION.CODE.TO.PULL,"_simset_",Sys.Date(),"_",LOCATION,".Rdata"))

# range(simset$get.mcmc.mixing.statistic(NULL))
# cbind(head(simset$get.mcmc.mixing.statistic(NULL),20))


## --- ANDREW TO LOOK AT ---- ## 

## --- STANDARD PLOTS ---- ## 
simplot(simset$last.sim(),
        simset,
        facet.by = "age", split.by = "race", # age, sex, race; 1- and 2-way
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

# simplot(simset$last.sim(),
#         simset,
#         outcomes = c("population"), 
#         style.manager = source.style.manager, # use when looking at totals 
#         dimension.values = list(year = 2000:2030)) 
# 
# simplot(simset$last.sim(),
#         simset,
#         #facet.by = "age", # age, race; 1-way 
#         outcomes = c("immigration","emigration"), 
#         style.manager = location.style.manager,
#         dimension.values = list(year = 2000:2030)) 

simplot(simset$last.sim(),
        simset,
        outcomes = c("new"), 
        style.manager = source.style.manager, # use when looking at totals 
        dimension.values = list(year = 1980:2030)) 

simplot(simset$last.sim(),
        simset,
        facet.by = "risk", split.by = "race", # race, risk, sex; 1- and 2-way 
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 

simplot(simset$last.sim(),
        simset,
        facet.by = "risk", split.by = "sex", # race, risk, sex; 1- and 2-way 
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 

simplot(simset$last.sim(),
        simset,
        facet.by = "age", split.by = "sex", # race, risk, sex; 1- and 2-way 
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 

simplot(simset$last.sim(),
        simset,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset$last.sim(),
        simset,
        facet.by = "risk", split.by = "race", # race, risk, sex; 1- and 2-way 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

simplot(simset$last.sim(),
        simset,
        facet.by = "risk", split.by = "sex", # race, risk, sex; 1- and 2-way 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

simplot(simset$last.sim(),
        simset,
        facet.by = "age", split.by = "sex", # race, risk, sex; 1- and 2-way 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

simplot(simset$last.sim(),
        simset,
        facet.by = "sex", # sex; 1-way 
        outcomes = c("hiv.mortality"),
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset$last.sim(),
        simset,
        outcomes = c("total.mortality"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

# must ask for years 1981-2001 
# simplot(simset$last.sim(),
#         simset,
#         outcomes = c("aids.deaths"), 
#         style.manager = location.style.manager,
#         dimension.values = list(year = 1981:2001)) 

simplot(simset$last.sim(),
        simset,
        outcomes = c("aids.diagnoses"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset$last.sim(),
        simset,
        facet.by = "race", # age, sex, race, risk; 1-way 
        outcomes = c("aids.diagnoses"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset$last.sim(),
        simset,
        facet.by = "risk", # age, sex, race, risk; 1-way 
        outcomes = c("aids.diagnoses"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset$last.sim(),
        simset,
        facet.by = "age", # age, sex, race, risk; 1-way 
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset$last.sim(),
        simset,
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset$last.sim(),
        simset,
        outcomes = c("cdc.hiv.test.positivity"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset$last.sim(),
        simset,
        outcomes = c("awareness"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset$last.sim(),
        simset,
        outcomes = c("suppression"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset$last.sim(),
        simset,
        facet.by = "race", # age, sex, race, risk; 1-way 
        outcomes = c("suppression"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset$last.sim(),
        simset,
        facet.by = "risk", # age, sex, race, risk; 1-way 
        outcomes = c("suppression"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset$last.sim(),
        simset,
        facet.by = "age", # age; 1-way 
        outcomes = c("proportion.using.heroin",
                     "proportion.using.cocaine"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset$last.sim(),
        simset,
        facet.by = "age", # age, sex, race; 1- and 2-way 
        outcomes = c("prep.uptake"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset$last.sim(),
        simset,
        facet.by = "age", # age, sex; 1-way 
        outcomes = c("prep.indications"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset$last.sim(),
        simset,
        outcomes = c("sexual.transmission.rates"), 
        style.manager = location.style.manager,
        plot.year.lag.ratio = T,
        dimension.values = list(year = 2000:2030)) 

simplot(simset$last.sim(),
        simset,
        outcomes = c("total.hiv.tests.per.population"), 
        style.manager = location.style.manager,plot.year.lag.ratio = T,
        dimension.values = list(year = 2000:2030)) 

