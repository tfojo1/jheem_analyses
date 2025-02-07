#CASHED FOLDER:
# https://livejohnshopkins-my.sharepoint.com/personal/tfojo1_jh_edu/_layouts/15/onedrive.aspx?e=5%3A940bf48ba6e0498495fea5596e3dc8e7&sharingv2=true&fromShare=true&at=9&CID=425e54af%2De78b%2D4d53%2D8df4%2D6abb10af6339&id=%2Fpersonal%2Ftfojo1%5Fjh%5Fedu%2FDocuments%2FJHEEM2&FolderCTID=0x012000E74D427C3A55BC45A1C18C850CDA2DB4&view=0
# Excel Sheet:
# https://livejohnshopkins-my.sharepoint.com/:x:/g/personal/zdansky1_jh_edu/EVrQ-OpGqlVIpBi_KE0P6v4B2rTpIvYcyUtLz9e1NH_oig?e=kd8bjH&wdLOR=c06087FCD-0041-804E-BB9F-F582185054BC
# https://jheem.shinyapps.io/EndingHIV/

##################
source('applications/SHIELD/shield_specification.R')

location= "C.12580" #Baltimore MSA
# location= "US" #National

engine = create.jheem.engine(version = 'shield', location = location, end.year = 2030)
specification.metadata=get.specification.metadata('shield',location)
params=get.medians(SHIELD.FULL.PARAMETERS.PRIOR)
sim = engine$run(params)

#Outcomes
simplot(sim,"population" )
simplot(sim,"hiv.testing" ) 
# <<<<<<< HEAD
simplot(sim,"diagnosis.ps")  #'@Andrew: no data points are shown on the plot
# SURVEILLANCE.MANAGER$data$ps.syphilis$estimate$cdc.aggregated.county$cdc.sti$year__location[,'C.12580']

simplot(sim,"diagnosis.el")

# =======
# simplot(sim,"diagnosis.primary.secondary")  #@Andrew: no data points are shown on the plot
# # SURVEILLANCE.MANAGER$data$ps.syphilis$estimate$cdc.aggregated.county$cdc.sti$year__location[,'C.12580']
# 
# >>>>>>> 0f232e8b2865756b50bbf952045f952d3565994d
simplot(sim,"trt.initiation")
simplot(sim,"births.from" )
simplot(sim,"deaths" )
simplot(sim,"immigration" ) #@Andrew: no data points are shown on the plot
simplot(sim,"emigration" )#@Andrew: no data points are shown on the plot
simplot(sim,"diagnosis.congenital")

#By 1 factor
# simplot(sim,"population", facet.by = "sex", dimension.values = list(year = 2000:2030))
# simplot(sim,"population", facet.by = "age", dimension.values = list(year = 2000:2030))
# simplot(sim,"population", facet.by = "race", dimension.values = list(year = 2000:2030))
# By 2 factors
simplot(sim,"population",
        facet.by = "age", split.by ="race", dimension.values = list(year = 2009:2030))
simplot(sim,"population", 
        facet.by = "age", split.by ="sex", dimension.values = list(year = 2000:2030))

simplot(sim,"fertility.rate")
simplot(sim,"fertility.rate",
        facet.by = "age", split.by = "race")

simplot(sim,"deaths")
simplot(sim,"deaths",
        dimension.values = list(year = 2000:2030))

# we dont have data: 
simplot(sim,
        outcomes = c("births.from"), 
        facet.by = "age", split.by = "race", 
        dimension.values = list(year = 2000:2030)) 


SURVEILLANCE.MANAGER$data$population$estimate$census.population$stratified.census$year__location__race__ethnicity['2010','US',,]
apply(SURVEILLANCE.MANAGER$data$population$estimate$census.population$stratified.census$year__location__race__ethnicity['2010','US',,],c('ethnicity'),sum)
apply(SURVEILLANCE.MANAGER$data$population$estimate$census.population$stratified.census$year__location__age__race__ethnicity__sex['2010','US',,,,],
      c('ethnicity'),sum)
