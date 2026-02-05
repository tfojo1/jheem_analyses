#CASHED FOLDER:
# https://livejohnshopkins-my.sharepoint.com/personal/tfojo1_jh_edu/_layouts/15/onedrive.aspx?e=5%3A940bf48ba6e0498495fea5596e3dc8e7&sharingv2=true&fromShare=true&at=9&CID=425e54af%2De78b%2D4d53%2D8df4%2D6abb10af6339&id=%2Fpersonal%2Ftfojo1%5Fjh%5Fedu%2FDocuments%2FJHEEM2&FolderCTID=0x012000E74D427C3A55BC45A1C18C850CDA2DB4&view=0
# Excel Sheet:
# https://livejohnshopkins-my.sharepoint.com/:x:/g/personal/zdansky1_jh_edu/EVrQ-OpGqlVIpBi_KE0P6v4B2rTpIvYcyUtLz9e1NH_oig?e=kd8bjH&wdLOR=c06087FCD-0041-804E-BB9F-F582185054BC
# https://jheem.shinyapps.io/EndingHIV/

##################
source('applications/SHIELD/shield_specification.R')

VERSION='shield'
LOCATION='C.12580' #Baltimore MSA
# LOCATION='C.35620'#NYC 


# make a run:
engine = create.jheem.engine( VERSION,  LOCATION, end.year = 2030)
specification.metadata=get.specification.metadata(VERSION,LOCATION)
params=get.medians(SHIELD.FULL.PARAMETERS.PRIOR)
sim = engine$run(params)



if (1==2) {

# INSTANTIATE LIKELIHOODS
source('applications/SHIELD/shield_likelihoods.R')
# likelihood.all<- likelihood.instructions.all$instantiate.likelihood(VERSION,LOCATION,verbose = T)
likelihood.all<- likelihood.instructions.syphilis.diagnoses$instantiate.likelihood(VERSION,LOCATION,verbose = T)
# COMPUTE LIKELIHOODS   
likelihood.all$compute.piecewise(sim)
# simplot(sim, 'diagnosis.total')



#POPUATION ----
#fitting to age-race-sex-specific estimates starting in 2010, and assuming fix strata before then 
simplot(sim,"population" )
simplot(sim,"population" ,split.by = 'sex')

# SYPHILIS DIAGNOSES ----
simplot(sim, 'diagnosis.total')
simplot(sim, 'diagnosis.ps')
simplot.data.only('diagnosis.ps',LOCATION)
lik=ps.diagnosis.likelihood.instructions$instantiate.likelihood(VERSION,LOCATION )
lik$compute(sim,debug = T)
# sd= coef.variation * mean

 
simplot(sim, 'diagnosis.el.misclassified')
simplot(sim, 'diagnosis.late.misclassified')

simplot(sim,"hiv.testing" ) 
simplot(sim,"sti.screening" ) 

}

if (1==2){
    get.ontology.error.debug.info()
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
    # SURVEILLANCE.MANAGER$data$no.prenatal.care$estimate$cdc.wonder.aggregated.population$cdc.fertility$year__location__age[,LOCATION,]
    # SURVEILLANCE.MANAGER$data$prenatal.care.initiation.first.trimester$estimate$cdc.wonder.aggregated.population$cdc.fertility$year__location__age[,LOCATION,]
    # SURVEILLANCE.MANAGER$data$prenatal.care.initiation.second.trimester$estimate$cdc.wonder.aggregated.population$cdc.fertility$year__location__age[,LOCATION,]
    # SURVEILLANCE.MANAGER$data$prenatal.care.initiation.third.trimester$estimate$cdc.wonder.aggregated.population$cdc.fertility$year__location__age[,LOCATION,]
    
    
    # SURVEILLANCE.MANAGER$data$ps.syphilis.diagnoses$estimate$cdc.aggregated.county$cdc.sti$year__location[,LOCATION]
    # $mapping.from.1
    # [1] "A basic ontology mapping ('shield.to.census.sex') over dimension 'sex'"
    # 
    # $mapping.from.2
    # [1] "An identity ('no-change') ontology mapping"
    # > get.mappings.to.align.ontologies(ontology(age=a$age),ontology(age=b$age))
    # NULL
    
    dimnames(SURVEILLANCE.MANAGER$data$fertility.rate$estimate$cdc.wonder.natality$cdc.fertility$year__location__age__race__ethnicity)
    dimnames(SURVEILLANCE.MANAGER$data$female.population.denominator.for.fertility.rate$estimate$cdc.wonder.natality$cdc.fertility$year__location__age__race__ethnicity)
    examine.recent.failed.ontology.mapping()
}
# "Instantiating sub 'jheem.basic.likelihood.instructions' for 'diagnosis.el.misclassified'..."
# x=get.ontology.error.debug.info()
# examine.get.mappings.to.align.ontologies(x$onts.i,x$uni)
# examine.recent.failed.ontology.mapping()
