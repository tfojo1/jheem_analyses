source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
location.style.manager = create.style.manager(color.data.by = "location.type")
source.style.manager = create.style.manager(color.data.by = "source")
stratum.style.manager = create.style.manager(color.data.by = "stratum")

LOCATION='C.12580' #BALTIMORE.MSA
CALIBRATION.CODE.TO.RUN='pop.demog.shield'
# DATE=Sys.Date()
DATE="2025-04-18"

#sumamry: 
#04.14: PK: I removed initially infected popualtion to speed up the sims #check base_params prp.of.initial.population.infected.syphilis=0
#04.18: Ryan added immigration parameters

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
simset=simset;simset

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
        # split.by = "race", facet.by = 'age',
        facet.by = "sex", split.by = "race",
        # facet.by = "sex", split.by = "age", 
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

simplot(
        simset$first.sim(),
        simset$last.sim(),
        # facet.by = "age",
        # facet.by = "sex",
        facet.by = "race",
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
         split.by = "race", facet.by = "age",
         outcomes = c("fertility.rate"), 
         dimension.values = list(year = 2000:2030)) 
simplot( simset$first.sim(),simset$last.sim(),
         outcomes = c("fertility.rate"), 
         split.by='race',
         dimension.values = list(year = 2000:2030)) 


#immigration
simplot( simset$last.sim(),
         split.by = "race",
         outcomes = c("immigration"), 
         dimension.values = list(year = 2000:2030)) 
simplot( simset$last.sim(),
         split.by = "race",
         outcomes = c("emigration"), 
         dimension.values = list(year = 2000:2030)) 

#is the chain mixing well?  Rhat: ratio of the parameter variance in all chains/average within chain variance
#theoretically in the steady state, it should be close to 1
simset$get.mcmc.mixing.statistic()
simset$traceplot("black.aging")
simset$traceplot("other.aging")
simset$traceplot("hispanic.aging")
simset$traceplot("mortality")
simset$traceplot("fertility")


lik= likelihood.instructions.demographics$instantiate.likelihood('shield','C.12580')
lik$compare.sims(simset$subset(2), simset$last.sim(), log = T) #ratio of last one to first one 



#save.simulation.set(simset$last.sim(), 
#                    "~/Downloads/last.sim.for.Ryan.Rdata")


#'@Ryan: to manually twick the parameters to find a better trajectory
#'

# if it's not mixing it's either a likelihood or model problem
# params= simset$last.sim()$params
# params['']try access the params manually and improve 
# comparing likelihhoods

# Issues needing correction: 1) Fertility rate too high for hispanic population.
# 2) Mortality rate is ~50% higher than needs be.
# 3) Population growth is flat (most important)

# I will start with the fertility rates 

# Extract the last simulation
sim = simset$last.sim()

# Modify the parameters
params = sim$params
#params['hispanic.fertility.rate.multiplier'] = 0.9

params['other.fertility.rate.multiplier'] = 1.1

#params["black.general.mortality.rate.multiplier"]   = 0.85
#params["hispanic.general.mortality.rate.multiplier"] = 0.85
#params["other.general.mortality.rate.multiplier"]   = 0.85
#params["male.general.mortality.rate.multiplier"]     = 0.85
#params["female.general.mortality.rate.multiplier"]   = 0.85


# Assign the params to modified simulation
engine = create.jheem.engine(version = 'shield', location = 'C.12580', end.year = 2030)

sim.new = engine$run(params)

#plot for fertility rate
simplot( simset$last.sim(),
         sim.new,
         split.by = "race",
         facet.by = "age",
         outcomes = c("population"), 
         dimension.values = list(year = 2000:2030)) 

#plot for mortality
simplot(
        sim.new$last.sim(),
        outcomes = c("deaths"), 
        dimension.values = list(year = 1940:2030)) 


#plot for population growth
simplot(simset$last.sim(),
        outcomes = c("population"), 
        # dimension.values = list(year = 1940:2030))
        dimension.values = list(year = 2000:2030))


SURVEILLANCE.MANAGER$data$population$estimate$census.aggregated.population$census$year__location[,"C.12580"]


rowSums(SURVEILLANCE.MANAGER$data$population$estimate$census.aggregated.population$stratified.census$year__location__race__ethnicity[,"C.12580",,])

rowSums(SURVEILLANCE.MANAGER$data$population$estimate$census.aggregated.population$stratified.census$year__location__age__race__ethnicity__sex[,"C.12580",,,,])


lik = likelihood.instructions.demographics$instantiate.likelihood("shield", "C.12580")
lik$compare.sims(simset$last.sim(), sim.new, piecewise = F)


