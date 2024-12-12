source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
# source('../jheem_analyses/commoncode/locations_of_interest.R')

location.style.manager = create.style.manager(color.data.by = "location.type")
source.style.manager = create.style.manager(color.data.by = "source")
stratum.style.manager = create.style.manager(color.data.by = "stratum")

# LOCATION='C.12580' #BALTIMORE.MSA
LOCATION='US'
CALIBRATION.CODE.TO.RUN='pop.demog.shield.us'
# DATE=Sys.Date()
DATE="2024-12-11"

# completed mcmc? 
# Reading from file:
load(paste0("../jheem_analyses/prelim_results/",CALIBRATION.CODE.TO.RUN,"_simset_",DATE,"_",LOCATION,".Rdata"))
simset=simset;simset

# # incomplete chain:
# # # reading from ongoing calibration: doesnt require a date
# simset1 = assemble.simulations.from.calibration(version = 'shield',
#                                                 location = LOCATION,
#                                                 calibration.code = CALIBRATION.CODE.TO.RUN,
#                                                 allow.incomplete = T);
# simset=simset1



# strip the fertility likelihoods
# local vs nationa issue?


simplot(simset$first.sim(),simset$last.sim(),
        outcomes = c("population"), 
        # dimension.values = list(year = 1940:2030)) 
        dimension.values = list(year = 2000:2030))

simplot(simset$first.sim(),simset$last.sim(),
        split.by = "race", 
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset$first.sim(),simset$last.sim(),
        split.by = "race", 
        facet.by = 'age',
        outcomes = c("population"), 
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
         outcomes = c("fertility.rate"), 
         dimension.values = list(year = 2000:2030)) 
simplot( simset$first.sim(),simset$last.sim(),
         split.by = "race", facet.by = "age",
         outcomes = c("fertility.rate"), 
         dimension.values = list(year = 2000:2030)) 
# SURVEILLANCE.MANAGER$data$fertility.rate$estimate$cdc.wonder.natality$cdc.fertility$year__location__age__race__ethnicity[,'US',,,]

## population by strata
simplot(simset$first.sim(),simset$last.sim(),
         split.by = "race", 
        # facet.by = 'age',
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 
simplot(simset$first.sim(),simset$last.sim(),
         outcomes = c("population"), 
         facet.by = "sex", split.by = "race", 
         dimension.values = list(year = 2000:2030)) 

