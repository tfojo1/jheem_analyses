source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
# source('../jheem_analyses/commoncode/locations_of_interest.R')

location.style.manager = create.style.manager(color.data.by = "location.type")
source.style.manager = create.style.manager(color.data.by = "source")
stratum.style.manager = create.style.manager(color.data.by = "stratum")

LOCATION='C.12580' #BALTIMORE.MSA
CALIBRATION.CODE.TO.RUN='pop.demog.shield'
# DATE=Sys.Date()
DATE="2025-04-11"
#I removed aging out of 64year group

# completed mcmc? 
# Reading from file:
# load(paste0("../jheem_analyses/prelim_results/",CALIBRATION.CODE.TO.RUN,"_simset_",DATE,"_",LOCATION,".Rdata"))
# simset=simset;simset
# 
# # # incomplete chain:
# # # # reading from ongoing calibration: doesnt require a date
# get.calibration.progress('shield',LOCATION,CALIBRATION.CODE.TO.RUN) # shows %done of the ongoing run
# reading the incomplete run:
simset = assemble.simulations.from.calibration(version = 'shield',
                                                location = LOCATION,
                                                calibration.code = CALIBRATION.CODE.TO.RUN,
                                                allow.incomplete = T);
simset$n.sim

simplot(simset$first.sim(),simset$last.sim(),
        outcomes = c("incidence"), 
        dimension.values = list(year = 1940:2030))
        # dimension.values = list(year = 2000:2030))


simplot(simset$first.sim(),simset$last.sim(),
        outcomes = c("population"), 
        # dimension.values = list(year = 1940:2030)) 
        dimension.values = list(year = 2000:2030))

simplot(simset$first.sim(),simset$last.sim(),
        split.by = "race", 
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset$first.sim(),simset$last.sim(),
        split.by = "race", facet.by = 'age',
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset$first.sim(),simset$last.sim(),
        outcomes = c("population"), 
        facet.by = "sex", split.by = "race", 
        dimension.values = list(year = 2000:2030)) 

#mortality
simplot(simset$first.sim(),simset$last.sim(),
        outcomes = c("deaths"), 
        dimension.values = list(year = 1940:2030)) 
simplot(simset$first.sim(),simset$last.sim(),
        split.by = "race", facet.by = "age",
        outcomes = c("deaths"), 
        dimension.values = list(year = 1940:2030)) 

# births
simplot( simset$first.sim(),simset$last.sim(),
         outcomes = c("births.from"), 
         dimension.values = list(year = 2000:2030)) 
simplot( simset$first.sim(),simset$last.sim(),
         outcomes = c("births.from"), 
         split.by = "race", facet.by = "age",
         dimension.values = list(year = 2000:2030)) 

simplot( simset$first.sim(),simset$last.sim(),
         outcomes = c("fertility.rate"), 
         split.by='race',
         dimension.values = list(year = 2000:2030)) 
simplot( simset$first.sim(),simset$last.sim(),
         split.by = "race", facet.by = "age",
         outcomes = c("fertility.rate"), 
         dimension.values = list(year = 2000:2030)) 
# SURVEILLANCE.MANAGER$data$fertility.rate$estimate$cdc.wonder.natality$cdc.fertility$year__location__age__race__ethnicity[,'US',,,]

## population by strata
simplot(simset$first.sim(),simset$last.sim(),
         split.by = "race", 
        facet.by = 'age',
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

#is the chain mixing well?  Rhat: ratio of the parameter variance in all chains/average within chain variance
#theoritically in the steady state, it should be close to 1
simset$get.mcmc.mixing.statistic()
simset$traceplot("black.aging")
simset$traceplot("hispanic.aging")
simset$traceplot("mortality")
simset$traceplot("fertility")

# if it's not mixing it's either a likelhood or model problem
# params= simset$last.sim()$params
# params['']try access the params mnaually and improve 
# comaparing likelihhoods