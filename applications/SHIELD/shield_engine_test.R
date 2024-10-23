#CASHED FOLDER:
# https://livejohnshopkins-my.sharepoint.com/personal/tfojo1_jh_edu/_layouts/15/onedrive.aspx?e=5%3A940bf48ba6e0498495fea5596e3dc8e7&sharingv2=true&fromShare=true&at=9&CID=425e54af%2De78b%2D4d53%2D8df4%2D6abb10af6339&id=%2Fpersonal%2Ftfojo1%5Fjh%5Fedu%2FDocuments%2FJHEEM2&FolderCTID=0x012000E74D427C3A55BC45A1C18C850CDA2DB4&view=0
# Excel Sheet:
# https://livejohnshopkins-my.sharepoint.com/:x:/g/personal/zdansky1_jh_edu/EVrQ-OpGqlVIpBi_KE0P6v4B2rTpIvYcyUtLz9e1NH_oig?e=kd8bjH&wdLOR=c06087FCD-0041-804E-BB9F-F582185054BC
# https://jheem.shinyapps.io/EndingHIV/

##################
# create the SHIELD.SPECIFICATION
# create an engine object
# engine.run()
# setwd('../../')
# JHEEM.DIR="~/OneDrive - Johns Hopkins/JHEEM/Simulation/code/jheem_analyses/"
# SHIELD.DIR="~/OneDrive - Johns Hopkins/JHEEM/Simulation/code/jheem_analyses/applications/SHIELD"
# setwd(JHEEM.DIR)
# setwd(SHIELD.DIR)
# setwd('../../')
source('applications/SHIELD/shield_specification.R')


# grep('US',dimnames(CENSUS.MANAGER$data$deaths$estimate$cdc_wonder$census.cdc.wonder.births.deaths$year__location__age__race__ethnicity__sex)[2]) # TRUE


engine = create.jheem.engine('shield', 'C.12580', 2025) #fails at reading mortality

#@Todd: the national model still fails
# engine = create.jheem.engine('shield', 'US', 2025) #fails at reading population size

params=get.medians(SHIELD.FULL.PARAMETERS.PRIOR)
# params['black.fertility.rate.multiplier']=.5
sim1 = engine$run(params)
sim1$births
sim1$total.mortality
simplot(sim1,
        outcomes = c("births"), 
        facet.by = "age", split.by = "race", 
        dimension.values = list(year = 2000:2030)) 
simplot(sim1,
        outcomes = c("total.mortality"), 
        facet.by = "age", split.by = "race", 
        dimension.values = list(year = 2000:2030)) 
simplot(sim1,
        outcomes = c("population"), 
        facet.by = "age", split.by = "race", 
        dimension.values = list(year = 2000:2030)) 

#plotting:
#population data for Baltimore MSA
# SURVEILLANCE.MANAGER$data$population$estimate$census.aggregated.population$census$year__location[, "C.12580"]
simplot(c(sim,sim1),"population",data.manager = SURVEILLANCE.MANAGER,facet.by = c('age'),split.by = c('race'),dimension.values = list('age'='0-14 years',year = 2000:2030) ) #still doesnt plot the data

simplot(sim,"population",data.manager = SURVEILLANCE.MANAGER,facet.by = c('sex') )
simplot(sim,"population",data.manager = SURVEILLANCE.MANAGER,facet.by = c('age') )
simplot(sim,"population",data.manager = SURVEILLANCE.MANAGER,facet.by = c('age'),split.by = c('race') )