#CASHED FOLDER:
# https://livejohnshopkins-my.sharepoint.com/personal/tfojo1_jh_edu/_layouts/15/onedrive.aspx?e=5%3A940bf48ba6e0498495fea5596e3dc8e7&sharingv2=true&fromShare=true&at=9&CID=425e54af%2De78b%2D4d53%2D8df4%2D6abb10af6339&id=%2Fpersonal%2Ftfojo1%5Fjh%5Fedu%2FDocuments%2FJHEEM2&FolderCTID=0x012000E74D427C3A55BC45A1C18C850CDA2DB4&view=0


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

#US level data for fertility rate:
# SURVEILLANCE.MANAGER$data$fertility.rate$estimate$cdc.wonder.natality$cdc.fertility$year__location__age__race__ethnicity[,'US',,,]


# Baltimore MSA : C.12580
engine = create.jheem.engine('shield', 'C.12580', 2025)

# engine = create.jheem.engine('shield', 'US', 2025)

params=get.medians(SHIELD.FULL.PARAMETERS.PRIOR)

# params['global.trate']=1
# params['msm.trate.multiplier1']=1000

sim = engine$run(params)

#plotting:
#population data for Baltimore MSA
# SURVEILLANCE.MANAGER$data$population$estimate$census.aggregated.population$census$year__location[, "C.12580"]
simplot(sim,"population",data.manager = SURVEILLANCE.MANAGER ) #still doesnt plot the data
sim$location





# before 2020 the census gave single age brackets after they only give age-groups
# census.data: age-groups for 2020-2023
# stratified.census: single ages before 2020

#population data for Baltimore MSA
SURVEILLANCE.MANAGER$data$population$estimate$census.aggregated.population$census.data$year__location[, "C.12580"]
simplot(sim,"population",data.manager = SURVEILLANCE.MANAGER ) #still doesnt plot the data
sim$location

# SURVEILLANCE.MANAGER$data$ps.syphilis$estimate$cdc.aggregated.county$cdc.sti$year__location[, "C.12580"]
# simplot(sim,"diag.ps",data.manager = SURVEILLANCE.MANAGER)
# SURVEILLANCE.MANAGER$outcomes
# 
# SURVEILLANCE.MANAGER$data$ps.syphilis$estimate$cdc.aggregated.county$cdc.sti$year__location[, "C.12580"]
# 
# SURVEILLANCE.MANAGER$source.info
# 

# simplot(sim, 'population')
# sim$population
# sim$parameters
#
# params2=c(global.trate=2)
# sim2 = engine$run(params2)

# =generate.random.samples(SHIELD.PARAMETERS.PRIOR,10)




# simplot(sim, 'incidence')
# # ,split.by = 'age',facet.by = 'race')
# # ,dimension.values =  list(age='0-14 years'))
#         # dimension.values = list(year = 2000:2020))
# simplot(sim, 'trt.initiation')
#
# sim$params



# we ned to setup the parameter
