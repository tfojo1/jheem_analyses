source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')
library(ggplot2)

load("../jheem_analyses/prelim_results/full.with.aids.minus.prep_2024-04-20_C.12580.Rdata")
sim.mcmc = sim

params.mcmc = sim.mcmc$params
params.manual = params.mcmc

sim.mcmc = engine$run(parameters = params.mcmc)
sim.manual = engine$run(parameters = params.manual)

cbind(round(sim.mcmc$params,3),
      round(sim.manual$params,3))

exp(full.lik$compute.piecewise(sim.manual) - full.lik$compute.piecewise(sim.mcmc)) 

undiagnosed.testing.rrs = names(params.manual[grepl("undiagnosed",names(params.manual))])
proportion.tested.params = names(params.manual[grepl("proportion.tested",names(params.manual))])
params.manual[proportion.tested.params]

params.manual[undiagnosed.testing.rrs] = .05 # going from 50 to 0.05 seems to have the slightest effect
sim.manual = engine$run(parameters = params.manual)

simplot(sim.mcmc,
        sim.manual,
        #facet.by = "risk", #split.by = "race", 
        outcomes = c("proportion.general.population.tested"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim.mcmc,
        sim.manual,
        #facet.by = "risk", #split.by = "race", 
        outcomes = c("hiv.test.positivity"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim.mcmc,
        sim.manual,
        facet.by = "age", split.by = "race", 
        outcomes = c("population"),
        dimension.values = list(year = 2000:2020)) 

simplot(sim.mcmc,
        sim.manual,
        facet.by = "race", #split.by = "race", 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim.mcmc,
        sim.manual,
        facet.by = "race", #split.by = "race", 
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2030)) 

round(cbind(sim.mcmc$get("aids.deaths")[,,1],sim.manual$get("aids.deaths")[,,1]))
SURVEILLANCE.MANAGER$data$aids.deaths$estimate$cdc.aids$cdc.aids.deaths$year__location[,"C.12580"]
# simplot(sim.mcmc,
#         sim.manual,
#         facet.by = "sex", #split.by = "race", 
#         outcomes = c("aids.deaths"), 
#         dimension.values = list(year = 2000:2030)) 

# aids diagnoses 
simplot(sim.mcmc,
        sim.manual,
        #facet.by = "risk", #split.by = "race", 
        outcomes = c("aids.diagnoses"), 
        dimension.values = list(year = 1980:2030)) 
