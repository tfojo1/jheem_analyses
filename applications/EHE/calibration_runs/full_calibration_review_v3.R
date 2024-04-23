source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
library(ggplot2)

load("../jheem_analyses/prelim_results/pop_trans_mort_2024-04-18_C.12580.Rdata")
sim.pop.trans.mort = sim

load("../jheem_analyses/prelim_results/base.plus.prep_2024-04-20_C.12580.Rdata")
sim.prep = sim

load("../jheem_analyses/prelim_results/full.with.aids.minus.prep_2024-04-20_C.12580.Rdata")
sim.full.minus.prep = sim

load("../jheem_analyses/prelim_results/full.with.aids_2024-04-20_C.12580.Rdata")
sim.full = sim

# params.manual = base.params
# sim.manual = engine$run(parameters = params.manual) 

#full.lik = FULL.likelihood.instructions.with.aids$instantiate.likelihood('ehe','C.12580')

exp(full.lik$compute.piecewise(sim.prep) -
       full.lik$compute.piecewise(sim.pop.trans.mort))

simplot(sim.pop.trans.mort,
        #sim.prep,
        sim.full.minus.prep,
        sim.full,
        facet.by = "age", split.by = "race", 
        outcomes = c("population"),
        dimension.values = list(year = 2000:2020)) 

simplot(sim.pop.trans.mort,
        sim.full.minus.prep,
        #facet.by = "age", #split.by = "race", 
        outcomes = c("immigration","emigration"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim.pop.trans.mort,
        sim.full.minus.prep,
        facet.by = "race", #split.by = "race", 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim.pop.trans.mort,
        sim.full.minus.prep,
        facet.by = "race", #split.by = "race", 
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim.full.minus.prep,
        #facet.by = "sex", #split.by = "race", 
        outcomes = c("hiv.mortality"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim.pop.trans.mort,
        sim.full.minus.prep,
        #facet.by = "sex", #split.by = "race", 
        outcomes = c("total.mortality"),
        dimension.values = list(year = 2000:2030)) 

# this isn't plotting? probably an issue because the data are cumulative estimates; can take sums from sim
# undershooting so maybe we need to add aids.diagnoses back in 
simplot(sim.pop.trans.mort,
        sim.full.minus.prep,
        #facet.by = "sex", #split.by = "race", 
        outcomes = c("aids.deaths"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.pop.trans.mort,
        sim.full.minus.prep,
        #facet.by = "risk", #split.by = "race", 
        outcomes = c("aids.diagnoses"), 
        dimension.values = list(year = 1980:2030)) 

simplot(sim,
        sim.pop.trans.mort,
        sim.full.minus.prep,
        facet.by = "risk", #split.by = "race", 
        outcomes = c("proportion.general.population.tested"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim.pop.trans.mort,
        sim.full.minus.prep,
        #facet.by = "sex", #split.by = "race", 
        outcomes = c("hiv.test.positivity"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim.pop.trans.mort,
        sim.full.minus.prep,
        #facet.by = "sex", #split.by = "race", 
        outcomes = c("awareness"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim.pop.trans.mort,
        sim.full.minus.prep,
        #facet.by = "sex", #split.by = "race", 
        outcomes = c("suppression"), # we have age/sex/race/risk all one way
        dimension.values = list(year = 2000:2030)) 

simplot(sim.pop.trans.mort,
        sim.full.minus.prep,
        #facet.by = "sex", #split.by = "race", 
        outcomes = c("proportion.using.heroin","proportion.using.cocaine"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim.pop.trans.mort,
        sim.full.minus.prep,
        sim.prep,
        #facet.by = "sex", #split.by = "race", 
        outcomes = c("prep.uptake","prep.indications"),
        dimension.values = list(year = 2000:2030)) 


# only one extreme parameter value (msm.incident.idu.multiplier.2) - figure out what likelihood is driving this 
# by setting value back to 1 and seeing which likelihood is worse 
cbind(round(sim.full.minus.prep$parameters,3),
      round(sim.prep$parameters,3))
