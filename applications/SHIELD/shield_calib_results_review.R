source('../jheem_analyses/applications/SHIELD/shield_specification.R')
source('../jheem_analyses/applications/SHIELD/shield_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
location.style.manager = create.style.manager(color.data.by = "location.type")
source.style.manager = create.style.manager(color.data.by = "source")
stratum.style.manager = create.style.manager(color.data.by = "stratum")

LOCATION='C.12580' #BALTIMORE.MSA
CALIBRATION.CODE.TO.RUN='pop.demog.shield'
# DATE=Sys.Date()
DATE="2025-04-16"

# COMPLETE MCMC:Reading from file:
load(paste0("../jheem_analyses/prelim_results/",CALIBRATION.CODE.TO.RUN,"_simset_",DATE,"_",LOCATION,".Rdata"))
simset=simset;simset
#04.14: completed chain 
#I removed initially infected popualtion to speed up the sims
#check base_params prp.of.initial.population.infected.syphilis=0


# INCOMPLETE CHAIN:reading from ongoing calibration: doesnt require a date
# get.calibration.progress('shield',LOCATION,CALIBRATION.CODE.TO.RUN) # shows %done of the ongoing run
# reading the incomplete run:
# simset = assemble.simulations.from.calibration(version = 'shield',
#                                                 location = LOCATION,
#                                                 calibration.code = CALIBRATION.CODE.TO.RUN,
#                                                 allow.incomplete = T);


# sim=engine$run(simset$first.sim()$params)
# simplot(sim, simset$first.sim(),outcomes = c("population"), 
#         dimension.values = list(year = 1940:2030)) 
# save.simulation.set(sim,"~/Downloads/errored.sim.Rdata")

simset$n.sim
simset$last.sim()$params[['global.transmission.rate']]
c(simset$last.sim()$params)-c(simset$first.sim()$params)
#'@Todd: how can I use this for informing the next calibration try?

simplot(simset$first.sim(),simset$last.sim(),
        outcomes = c("incidence"), 
        dimension.values = list(year = 1940:2030))
        # dimension.values = list(year = 2000:2030))

simplot(simset$first.sim(),simset$last.sim(),
        outcomes = c("population"), 
        # dimension.values = list(year = 1940:2030))
        dimension.values = list(year = 2000:2030))

simplot(simset$subset(2:300),
        outcomes = c("population"), 
        # dimension.values = list(year = 1940:2030))
        dimension.values = list(year = 2000:2030))

simplot(
        # simset$first.sim(),
        simset$last.sim(),
        split.by = "race", facet.by = 'age',
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

simplot(
    # simset$first.sim(),
    simset$last.sim(),
    facet.by = "age",
    outcomes = c("population"), 
    dimension.values = list(year = 2000:2030)) 

simplot(
        # simset$first.sim(),
        simset,
        outcomes = c("population"), 
        facet.by = "sex", split.by = "race", 
        dimension.values = list(year = 2000:2030)) 

simplot(
    simset$first.sim(),
    outcomes = c("population"), 
    facet.by = "sex", split.by = "race", 
    dimension.values = list(year = 2000:2030)) 

simplot(
        # simset$first.sim(),
        simset$last.sim(),
        split.by = "sex", facet.by = 'age',
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

#mortality
simplot(
        simset$first.sim(),
        simset$last.sim(),
        outcomes = c("deaths"), 
        dimension.values = list(year = 1940:2030)) 

# Deaths raw data is not showing stratified outcomes
simplot(
        # simset$first.sim(),
        simset$last.sim(),
        split.by = "race", facet.by = "age",
        outcomes = c("deaths"), 
        dimension.values = list(year = 1940:2030)) 

simplot.data.only(outcomes = c("deaths"), locations = 'C.12580')

#Fertility
simplot( simset$last.sim(),
         split.by = "race", facet.by = "age",
         outcomes = c("fertility.rate"), 
         dimension.values = list(year = 2000:2030)) 
simplot( simset$first.sim(),simset$last.sim(),
         outcomes = c("fertility.rate"), 
         split.by='race',
         dimension.values = list(year = 2000:2030)) 

simplot( simset$first.sim(),simset$last.sim(),
         outcomes = c("births.from"), 
         dimension.values = list(year = 2000:2030)) 
simplot( simset$first.sim(),simset$last.sim(),
         outcomes = c("births.from"), 
         split.by = "race", facet.by = "age",
         dimension.values = list(year = 2000:2030)) 


#immigration

simplot( simset$last.sim(),
         split.by = "race",
         outcomes = c("immigration"), 
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


