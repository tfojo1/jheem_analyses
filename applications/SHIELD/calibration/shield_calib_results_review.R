source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
# source('../jheem_analyses/commoncode/locations_of_interest.R')

location.style.manager = create.style.manager(color.data.by = "location.type")
source.style.manager = create.style.manager(color.data.by = "source")
stratum.style.manager = create.style.manager(color.data.by = "stratum")

LOCATION='C.12580' #BALTIMORE.MSA
CALIBRATION.CODE.TO.RUN='init.pop.shield'
DATE=Sys.Date()
DATE="2024-11-04"
# Reading from file:
load(paste0("../jheem_analyses/prelim_results/",CALIBRATION.CODE.TO.RUN,"_simset_",DATE,"_",LOCATION,".Rdata"))
simset=simset;simset
# reading from ongoing calibration: doesnt require a date
simset = assemble.simulations.from.calibration(version = 'shield',
                                               location = LOCATION,
                                               calibration.code = CALIBRATION.CODE.TO.RUN,
                                               allow.incomplete = T)
#   
# 
simplot(simset$first.sim(),simset$last.sim(),
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        outcomes = c("births.from"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim,
        outcomes = c("total.mortality"), 
        dimension.values = list(year = 2010:2030)) 

rowSums(sim$total.mortality)/rowSums(sim$population)


simplot(simset$last.sim(),
        facet.by = "age", split.by = "race", 
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

simplot(
  # simset,
  simset$last.sim(),
  simset$first.sim(),
  outcomes = c("population"), 
  style.manager = source.style.manager,
  dimension.values = list(year = 2000:2030)) 

simplot( simset,
  outcomes = c("population"), 
  facet.by = "age", split.by = "race", 
  dimension.values = list(year = 2000:2030)) 

simplot( simset,
         outcomes = c("population"), 
         facet.by = "sex", split.by = "race", 
         dimension.values = list(year = 2000:2030)) 

####
simplot( simset,
         outcomes = c("births"), 
         dimension.values = list(year = 2000:2030)) 
