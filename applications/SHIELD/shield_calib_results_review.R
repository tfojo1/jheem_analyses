source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
location.style.manager = create.style.manager(color.data.by = "location.type")
source.style.manager = create.style.manager(color.data.by = "source")
stratum.style.manager = create.style.manager(color.data.by = "stratum")

VERSION='shield'
LOCATION='C.12580' #BALTIMORE.MSA
CALIBRATION.CODE.TO.RUN='pop.demog.shield.pk1'
# DATE=Sys.Date()
DATE="2025-04-23"


# COMPLETE MCMC:Reading from file:
#Q drive:
if(1==1){
        load(paste0("/Volumes/jheem$/results/Shield/",CALIBRATION.CODE.TO.RUN,"_simset_",DATE,"_",LOCATION,".Rdata"))
        
}
#Local Mac:
if(1==2){
        load(paste0("../jheem_analyses/prelim_results/",CALIBRATION.CODE.TO.RUN,"_simset_",DATE,"_",LOCATION,".Rdata"))
}

# INCOMPLETE CHAIN:reading from ongoing calibration: doesnt require a date
if(1==2){
        get.calibration.progress('shield',LOCATION,CALIBRATION.CODE.TO.RUN) # shows %done of the ongoing run
        simset = assemble.simulations.from.calibration(version = 'shield',
                                                       location = LOCATION,
                                                       calibration.code = CALIBRATION.CODE.TO.RUN,
                                                       allow.incomplete = T);
      
}

# PLOTS ----
simset=simset;simset
# filename=paste0("prelim_results/",CALIBRATION.CODE.TO.RUN,"_simset_",Sys.Date(),"_",LOCATION,".Rdata")
# filename=paste0("/Volumes/jheem$/shield/pop.demog.1.Rdata")
# save(simset,file=filename)

# REVIEW-----
simset$n.sim
#population (calibrate to c(0,1,2))
simplot(
        # simset,
        simset$first.sim(),simset$last.sim(),
        outcomes = c("population"), 
        dimension.values = list(year = 1940:2030)
        # dimension.values = list(year = 2000:2030)
)

simplot(
        # simset$first.sim(),
        simset$last.sim(),
        # facet.by = "age",
        # facet.by = "sex",
        # facet.by = "race",
        split.by = "race", facet.by = 'age',
        # facet.by = "sex", split.by = "race",
        # facet.by = "sex", split.by = "age", 
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

#Deaths (calibrated to totals from 2010)
simplot(
        simset$first.sim(),
        simset$last.sim(),
        outcomes = c("deaths"), 
        dimension.values = list(year = 2000:2030)) 

#Fertility (calibrated to data by age/race 2007-2023  )
simplot( simset$last.sim(),
         # facet.by = "age",
         # facet.by = "sex",
         # facet.by = "race",
         split.by = "race", facet.by = 'age',
         # facet.by = "sex", split.by = "race",
         # facet.by = "sex", split.by = "age",
         outcomes = c("fertility.rate"), 
         dimension.values = list(year = 2000:2030)) 


#immigration
simplot( simset$last.sim(),
         # split.by = "race",
         outcomes = c("immigration"), 
         dimension.values = list(year = 2000:2030)) 
simplot( simset$last.sim(),
         # split.by = "race",
         outcomes = c("emigration"), 
         dimension.values = list(year = 2000:2030)) 

# PARAMETER MIXING ----
#is the chain mixing well?  Rhat: ratio of the parameter variance in all chains/average within chain variance
#theoretically in the steady state, it should be close to 1
# if it's not mixing it's either a likelihood or model problem
simset$get.mcmc.mixing.statistic()
simset$traceplot("black.aging")
simset$traceplot("other.aging")
simset$traceplot("hispanic.aging")
simset$traceplot("mortality")
simset$traceplot("fertility")

# MANUAL SIM TEST ----
# params=simset$last.sim()$params
# # engine=create.jheem.engine('shield','C.12580',end.year = 2030)
# sim1=engine$run(params)
# 
# params['other.fertility.rate.multiplier']=.8
# sim2=engine$run(params)

#instantiate added likelihoods:
lik=likelihood.instructions.demographics$instantiate.likelihood('shield','C.12580')
lik.sex.race=population.likelihood.instructions.2way.sex.race$instantiate.likelihood('shield','C.12580')
lik.sex.age=population.likelihood.instructions.2way.sex.age$instantiate.likelihood('shield','C.12580')
lik.age.race=population.likelihood.instructions.2way.age.race$instantiate.likelihood('shield','C.12580')
#compute and compare:
lik.age.race$compare.sims(sim1, sim2,piecewise = T)
lik.sex.age$compare.sims(sim1, sim2,piecewise = T)
lik.sex.race$compare.sims(sim1, sim2,piecewise = T)

simplot(
  # simset$first.sim(),
  sim1,
  # sim2,
  # facet.by = "age",
  # facet.by = "sex",
  facet.by = "age",split.by="race",
  # facet.by = "age",split.by="sex",
  # facet.by = "race",
  outcomes = c("population"), 
  dimension.values = list(year = 2000:2030)) 

#there is a jump in 2020: the stratified.census reports agegroups to 2019. then in 2020, we switch to census to use single year ages and there is a big jump
#single year ages
rowSums(SURVEILLANCE.MANAGER$data$population$estimate$census.aggregated.population$census$year__location__age[,'C.12580',])

#5-year agegroup
rowSums(SURVEILLANCE.MANAGER$data$population$estimate$census.aggregated.population$stratified.census$year__location__age[,'C.12580',])
rowSums(SURVEILLANCE.MANAGER$data$population$estimate$census.aggregated.population$stratified.census$year__location__sex[,'C.12580',])
rowSums(SURVEILLANCE.MANAGER$data$population$estimate$census.aggregated.population$stratified.census$year__location__race__ethnicity[,'C.12580',,])
rowSums(SURVEILLANCE.MANAGER$data$population$estimate$census.aggregated.population$stratified.census$year__location__race__ethnicity[,'C.12580',,'hispanic'])
rowSums(SURVEILLANCE.MANAGER$data$population$estimate$census.aggregated.population$stratified.census$year__location__race__ethnicity[,'C.12580',,'not hispanic'])


rowSums(SURVEILLANCE.MANAGER$data$population$estimate$census.aggregated.population$stratified.census$year__location__age__race__ethnicity[,'C.12580',,,])
rowSums(SURVEILLANCE.MANAGER$data$population$estimate$census.aggregated.population$stratified.census$year__location__race__ethnicity[,'C.12580',,])
rowSums(SURVEILLANCE.MANAGER$data$population$estimate$census.aggregated.population$stratified.census$year__location__race__ethnicity__sex[,'C.12580',,,])

rowSums(SURVEILLANCE.MANAGER$data$population$estimate$census.aggregated.population$stratified.census$year__location__age[,'C.12580',])
rowSums(SURVEILLANCE.MANAGER$data$population$estimate$census.aggregated.population$stratified.census$year__location__age__race__ethnicity[,'C.12580',,,])

