#CASHED FOLDER:
# https://livejohnshopkins-my.sharepoint.com/personal/tfojo1_jh_edu/_layouts/15/onedrive.aspx?e=5%3A940bf48ba6e0498495fea5596e3dc8e7&sharingv2=true&fromShare=true&at=9&CID=425e54af%2De78b%2D4d53%2D8df4%2D6abb10af6339&id=%2Fpersonal%2Ftfojo1%5Fjh%5Fedu%2FDocuments%2FJHEEM2&FolderCTID=0x012000E74D427C3A55BC45A1C18C850CDA2DB4&view=0
# Excel Sheet:
# https://livejohnshopkins-my.sharepoint.com/:x:/g/personal/zdansky1_jh_edu/EVrQ-OpGqlVIpBi_KE0P6v4B2rTpIvYcyUtLz9e1NH_oig?e=kd8bjH&wdLOR=c06087FCD-0041-804E-BB9F-F582185054BC
# https://jheem.shinyapps.io/EndingHIV/

##################
source('applications/SHIELD/shield_specification.R')
version= 'shield'
location= "C.12580" #Baltimore MSA


# TEST the likelihoods ----
source('applications/SHIELD/shield_likelihoods.R')
likelihood.all<- likelihood.instructions.all$instantiate.likelihood('shield',location,verbose = T)

#'@Zoe:
#1- female.population.denominator.for.fertility.rate <some MSA's are missing>: aggregate data for all MSAs regardeless of missing counties
# double check the calculation for fertility rate
#2- for immigration and emigration : to restratify the data for age to comply with SHIELD agegroups 
#
#3- prenatal care proportions: what is triggering all the NA's?

#4-proportion of births with congenital syphilis: collecting ddata from HDR for congenital syphilis 

#5- estimating error for syphilis diagnosis using HDR (differences in CDC estiamtes and health department reports)


# get.ontology.error.debug.info()
so=get.ontology.error.debug.info()[[2]]
do=get.ontology.error.debug.info()[[1]]
examine.get.mappings.to.align.ontologies(so,do)

get.mappings.to.align.ontologies(ontology(sex=so$sex),ontology(sex=do$sex))
get.mappings.to.align.ontologies(ontology(age=so$age),ontology(age=do$age))
get.mappings.to.align.ontologies(ontology(race=so$race),ontology(race=do$race))
get.mappings.to.align.ontologies(ontology(ethnicity=so$ethnicity),ontology(ethnicity=do$ethnicity))
get.mappings.to.align.ontologies(ontology(year=so$year),ontology(year=do$year))
get.mappings.to.align.ontologies(ontology(location=so$location),ontology(location=do$location))

# #prenatal care: there are missign values in the MSA aggregated data
# SURVEILLANCE.MANAGER$data$no.prenatal.care$estimate$cdc.wonder.aggregated.population$cdc.fertility$year__location__age[,location,]
# SURVEILLANCE.MANAGER$data$prenatal.care.initiation.first.trimester$estimate$cdc.wonder.aggregated.population$cdc.fertility$year__location__age[,location,]
# SURVEILLANCE.MANAGER$data$prenatal.care.initiation.second.trimester$estimate$cdc.wonder.aggregated.population$cdc.fertility$year__location__age[,location,]
# SURVEILLANCE.MANAGER$data$prenatal.care.initiation.third.trimester$estimate$cdc.wonder.aggregated.population$cdc.fertility$year__location__age[,location,]


# SURVEILLANCE.MANAGER$data$ps.syphilis.diagnoses$estimate$cdc.aggregated.county$cdc.sti$year__location[,location]
# $mapping.from.1
# [1] "A basic ontology mapping ('shield.to.census.sex') over dimension 'sex'"
# 
# $mapping.from.2
# [1] "An identity ('no-change') ontology mapping"
# > get.mappings.to.align.ontologies(ontology(age=a$age),ontology(age=b$age))
# NULL

# examine.recent.failed.ontology.mapping()


# TEST the likelihoods ----
likelihood.all<- likelihood.instructions.all$instantiate.likelihood('shield',location,verbose = T)


# get.ontology.error.debug.info()
so=get.ontology.error.debug.info()[[2]]
do=get.ontology.error.debug.info()[[1]]
get.mappings.to.align.ontologies(so,do)
get.mappings.to.align.ontologies(ontology(sex=so$sex),ontology(sex=do$sex))
get.mappings.to.align.ontologies(ontology(age=so$age),ontology(age=do$age))
get.mappings.to.align.ontologies(ontology(race=so$race),ontology(race=do$race))
get.mappings.to.align.ontologies(ontology(ethnicity=so$ethnicity),ontology(ethnicity=do$ethnicity))
get.mappings.to.align.ontologies(ontology(year=so$year),ontology(year=do$year))
get.mappings.to.align.ontologies(ontology(location=so$location),ontology(location=do$location))

# SURVEILLANCE.MANAGER$data$ps.syphilis.diagnoses$estimate$cdc.aggregated.county$cdc.sti$year__location[,location]
# $mapping.from.1
# [1] "A basic ontology mapping ('shield.to.census.sex') over dimension 'sex'"
# 
# $mapping.from.2
# [1] "An identity ('no-change') ontology mapping"
# > get.mappings.to.align.ontologies(ontology(age=a$age),ontology(age=b$age))
# NULL

# examine.recent.failed.ontology.mapping()


engine = create.jheem.engine(version = 'shield', location = location, end.year = 2030)
specification.metadata=get.specification.metadata('shield',location)
params=get.medians(SHIELD.FULL.PARAMETERS.PRIOR)
# params['global.transmission.rate']=2
sim = engine$run(params)

# sim2 = engine$run(params)
# engine1 = create.jheem.engine(version = 'shield', location = location, end.year = 2025)
# sim3 = engine1$run(params)

#POPUATION ----
#fitting to age-race-sex-specific estimates starting in 2010, and assuming fix strata before then 
simplot(sim,"population" )
simplot(sim,"population" ,split.by = 'sex')
simplot(sim,"population" ,split.by = 'race')
#'@Todd: we are over-estimating population size in years prior to 2010. how can we adjust the fix strata assumption to decline pre-2010? 
#'@Zoe: can we find any stratified data on population sizes before 2010?

# DEATHS ----
#we only fit to total deaths starting in 2010
simplot(sim, "deaths") 


# FERTILITY RATE ----
# Calibrating to age-race specific data over time
simplot(sim, 'fertility.rate')
simplot(sim, 'fertility.rate',facet.by = 'age',split.by = 'race')
#'@Zoe: can you please add the denominator data at the MSA level? <female.population.denominator.for.fertility.rate>
# dimnames(SURVEILLANCE.MANAGER$data$fertility.rate$estimate$cdc.wonder.natality$cdc.fertility$year__location__age__race__ethnicity[,'C.12580',,,])

#MIGRATION ----
# calibarted to data by year, by age, by race, by sex 
simplot(sim, 'immigration')
simplot(sim, 'immigration',split.by = 'sex')
simplot(sim, 'immigration',split.by = 'race')

#'@Andrew: why cant we see the data on the plot? 
# SURVEILLANCE.MANAGER$data$immigration$estimate$census.population$census.immigration$year__location[,'C.12580']

simplot(sim, 'emigration')
simplot(sim, 'emigration',split.by = 'sex')
simplot(sim, 'emigration',split.by = 'race')
#'@Andrew: why cant we see the data on the plot? 
# SURVEILLANCE.MANAGER$data$emigration$estimate$census.population$census.immigration$year__location[,'C.12580']

# SYPHILIS DIAGNOSES ----
simplot(sim, 'diagnosis.total')
#'@Andrew: why cant we see the data on the plot? 
# SURVEILLANCE.MANAGER$data$total.syphilis.diagnoses$estimate$cdc.sti.surveillance.reports$cdc.pdf.report$year__location[,'C.12580']

simplot(sim, 'diagnosis.ps')
#'@Andrew: why cant we see the data on the plot? 
# SURVEILLANCE.MANAGER$data$ps.syphilis.diagnoses$estimate$cdc.aggregated.county$cdc.sti$year__location[,'C.12580']

simplot(sim, 'diagnosis.el.misclassified')
simplot(sim, 'diagnosis.late.misclassified')
#'@Andrew: why cant we see the data on the plot? 

# PRENATAL CARE COVERAGE
# calibrated to data by age, race, age-race over time
simplot(sim, 'prp.prenatal.care.first.trimester')
simplot(sim, 'prp.prenatal.care.first.trimester',split.by = 'age')
simplot(sim, 'prp.prenatal.care.first.trimester',split.by = 'race')
simplot(sim, 'prp.prenatal.care.first.trimester',facet.by = 'age',split.by = 'race')
#'@Andrew: why cant we see the data on the plots? 
#SURVEILLANCE.MANAGER$data$prenatal.care.initiation.first.trimester$estimate$cdc.wonder.aggregated.population$cdc.fertility$year__location__race__ethnicity[,"C.12580",,]
# SURVEILLANCE.MANAGER$data$prenatal.care.initiation.first.trimester$estimate$cdc.wonder.aggregated.population$cdc.fertility$year__location__age[,'C.12580',]

simplot(sim, 'prp.prenatal.care.second.trimester')
simplot(sim, 'prp.prenatal.care.second.trimester',split.by = 'age')
simplot(sim, 'prp.prenatal.care.second.trimester',split.by = 'race')
simplot(sim, 'prp.prenatal.care.second.trimester',facet.by = 'age',split.by = 'race')
#'@Andrew: why cant we see the RACE data on the plot? 
#'# SURVEILLANCE.MANAGER$data$prenatal.care.initiation.first.trimester$estimate$cdc.wonder.aggregated.population$cdc.fertility$year__location__race__ethnicity[,"C.12580",,]

simplot(sim, 'prp.prenatal.care.third.trimester')
#'@Andrew: why cant we see the RACE data on the plot? 
#'# SURVEILLANCE.MANAGER$data$prenatal.care.initiation.first.trimester$estimate$cdc.wonder.aggregated.population$cdc.fertility$year__location__race__ethnicity[,"C.12580",,]

simplot(sim, 'prp.no.prenatal.care')
#'@Andrew: why cant we see the RACE data on the plot? 
#'# SURVEILLANCE.MANAGER$data$prenatal.care.initiation.first.trimester$estimate$cdc.wonder.aggregated.population$cdc.fertility$year__location__race__ethnicity[,"C.12580",,]

simplot(sim,"hiv.testing" ) 
simplot(sim,"diagnosis.ps")  #'@Andrew: no data points are shown on the plot
# SURVEILLANCE.MANAGER$data$ps.syphilis$estimate$cdc.aggregated.county$cdc.sti$year__location[,'C.12580']

simplot(sim,"diagnosis.el")
 
# #By 1 factor
# # simplot(sim,"population", facet.by = "sex", dimension.values = list(year = 2000:2030))
# # simplot(sim,"population", facet.by = "age", dimension.values = list(year = 2000:2030))
# # simplot(sim,"population", facet.by = "race", dimension.values = list(year = 2000:2030))
# # By 2 factors
# simplot(sim,"population",
#         facet.by = "age", split.by ="race", dimension.values = list(year = 2009:2030))
# simplot(sim,"population", 
#         facet.by = "age", split.by ="sex", dimension.values = list(year = 2000:2030))
# 
# simplot(sim,"fertility.rate")
# simplot(sim,"fertility.rate",
#         facet.by = "age", split.by = "race")
# 
# simplot(sim,"deaths")
# simplot(sim,"deaths",
#         dimension.values = list(year = 2000:2030))
# 
# # we dont have data: 
# simplot(sim,
#         outcomes = c("births.from"), 
#         facet.by = "age", split.by = "race", 
#         dimension.values = list(year = 2000:2030)) 
# 
# 
# SURVEILLANCE.MANAGER$data$population$estimate$census.population$stratified.census$year__location__race__ethnicity['2010','US',,]
# apply(SURVEILLANCE.MANAGER$data$population$estimate$census.population$stratified.census$year__location__race__ethnicity['2010','US',,],c('ethnicity'),sum)
# apply(SURVEILLANCE.MANAGER$data$population$estimate$census.population$stratified.census$year__location__age__race__ethnicity__sex['2010','US',,,,],
#       c('ethnicity'),sum)
