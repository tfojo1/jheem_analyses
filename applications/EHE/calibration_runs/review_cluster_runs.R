source('../jheem_analyses/applications/EHE/calibration_runs/ehe_register_calibrations.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum") # this is the default right now 

CALIBRATION.CODE = CALIBRATION.CODE.FULL.PLUS.COVID
N.SIM = 100
LOCATION = NYC.MSA

simset = retrieve.simulation.set('ehe',LOCATION,CALIBRATION.CODE,N.SIM)

cbind(head(simset$get.mcmc.mixing.statistic(NULL),20))

simplot(simset$last.sim(),
        simset,
        facet.by = "age", split.by = "race", 
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset$last.sim(),
        simset,
        outcomes = c("new"), 
        style.manager = source.style.manager, 
        dimension.values = list(year = 1980:2030)) 

simplot(simset$last.sim(),
        simset,
        facet.by = "risk", split.by = "race", 
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 

simplot(simset$last.sim(),
        simset,
        facet.by = "age", split.by = "sex", 
        outcomes = c("new"), 
        dimension.values = list(year = 1980:2030)) 

simplot(simset$last.sim(),
        simset,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset$last.sim(),
        simset,
        facet.by = "risk", split.by = "race",
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

simplot(simset$last.sim(),
        simset,
        facet.by = "age", split.by = "sex", 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 1980:2030)) 

simplot(simset$last.sim(),
        simset,
        outcomes = c("aids.diagnoses"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset$last.sim(),
        simset,
        facet.by = "race", 
        outcomes = c("aids.diagnoses"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset$last.sim(),
        simset,
        facet.by = "risk", 
        outcomes = c("aids.diagnoses"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset$last.sim(),
        simset,
        facet.by = "age", 
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
        outcomes = c("awareness"),
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
        facet.by = "risk", 
        outcomes = c("suppression"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 
