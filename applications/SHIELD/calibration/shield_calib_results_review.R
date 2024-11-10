source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
# source('../jheem_analyses/commoncode/locations_of_interest.R')

location.style.manager = create.style.manager(color.data.by = "location.type")
source.style.manager = create.style.manager(color.data.by = "source")
stratum.style.manager = create.style.manager(color.data.by = "stratum")

LOCATION='C.12580' #BALTIMORE.MSA
CALIBRATION.CODE.TO.RUN='pop.demog.shield.wAging'
DATE=Sys.Date()
DATE="2024-11-07"

# completed mcmc? 
# Reading from file:
load(paste0("../jheem_analyses/prelim_results/",CALIBRATION.CODE.TO.RUN,"_simset_",DATE,"_",LOCATION,".Rdata"))
simset=simset;simset

# incomplete chain:
# # reading from ongoing calibration: doesnt require a date
# simset1 = assemble.simulations.from.calibration(version = 'shield',
#                                                 location = LOCATION,
#                                                 calibration.code = 'pop.demog.shield.wAging',
#                                                 allow.incomplete = T)

# 
# simset=simset1
simplot(simset$first.sim(),simset$last.sim(),
        outcomes = c("population"), 
        dimension.values = list(year = 1940:2030)) 
simplot(simset$last.sim(),
        outcomes = c("population"), 
        dimension.values = list(year = 1940:2030)) 
simplot(simset,
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 
#mortality
simplot(simset$last.sim(),
        outcomes = c("total.mortality"), 
        dimension.values = list(year = 1940:2030)) 

sim=simset$last.sim()
sim$total.mortality
rowSums(sim$total.mortality)/rowSums(sim$population)

# births
simplot( simset,
         outcomes = c("births.from"), 
         dimension.values = list(year = 2000:2030)) 
simplot( simset,
         outcomes = c("fertility.rate"), 
         dimension.values = list(year = 2000:2030)) 

## population by strata
simplot(simset$last.sim(),
        facet.by = "age", split.by = "race", 
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 
simplot( simset$last.sim(),
         outcomes = c("population"), 
         facet.by = "sex", split.by = "race", 
         dimension.values = list(year = 2000:2030)) 

