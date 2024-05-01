source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

#full.lik = FULL.likelihood.instructions.with.aids$instantiate.likelihood('ehe','C.12580')

load("../jheem_analyses/prelim_results/full.with.aids_2024-04-27_C.12580.Rdata")
sim.mcmc = sim

params.mcmc = sim.mcmc$params
params.manual = params.mcmc

full.params.names = EHE.PARAMETERS.PRIOR@var.names
old.params.names = names(sim.mcmc$params)
new.params.names = setdiff(full.params.names,old.params.names)
params.manual[new.params.names] = 1
params.mcmc[new.params.names] = 1

engine = create.jheem.engine('ehe',sim.mcmc$location,end.year = 2030) 
sim.manual = engine$run(parameters = params.manual)
sim.mcmc = engine$run(parameters = params.mcmc)

simplot(sim.mcmc,
        sim.manual,
        #facet.by = "age", 
        outcomes = c("proportion.using.heroin",
                     "proportion.using.cocaine"), 
        dimension.values = list(year = 2000:2030)) 

round(cbind(params.mcmc[old.params.names],params.manual[old.params.names]),5)
round(cbind(params.mcmc[incident.idu.params],params.manual[incident.idu.params]),5)

incident.idu.params = names(params.manual[grepl("incident.idu",names(params.manual))])
incident.idu.params.0 = names(params.manual[grepl("incident.idu.multiplier.0",names(params.manual))])
incident.idu.params.2 = names(params.manual[grepl("incident.idu.multiplier.2",names(params.manual))])
# params.manual[incident.idu.params.0] = 1
params.manual["black.incident.idu.multiplier.0"] = 2 # 1.17579
params.manual["hispanic.incident.idu.multiplier.0"] = 1 # 19.74369
params.manual["other.incident.idu.multiplier.0"] = 1 # 0.72062
params.manual["msm.incident.idu.multiplier.0"] = 2 # 6.71323

params.manual["age1.incident.idu.multiplier"] = 1 # 0.80460
params.manual["age2.incident.idu.multiplier"] = 1 # 1.49047
params.manual["age3.incident.idu.multiplier"] = 1 # 4.31603
params.manual["age3.incident.idu.multiplier"] = 1 # 1.04566
params.manual["age5.incident.idu.multiplier"] = 1 # 1.15931

params.manual[incident.idu.params.2] = 0.005
# params.manual["black.incident.idu.multiplier.2"] = .005 # 0.69099
sim.manual = engine$run(parameters = params.manual)

# which likelihoods are WORSE now that I've made IDU better? new, diagnosed.prevalence, aids.diagnoses, awareness
exp(full.lik$compute.piecewise(sim.manual) - full.lik$compute.piecewise(sim.mcmc))

simplot(sim.mcmc,
        sim.manual,
        facet.by = "risk", split.by = "race", 
        outcomes = c("new"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim.mcmc,
        sim.manual,
        #facet.by = "risk", split.by = "race", 
        outcomes = c("diagnosed.prevalence"),
        dimension.values = list(year = 2000:2030)) 

simplot(sim.mcmc,
        sim.manual,
        #facet.by = "age", 
        outcomes = c("aids.diagnoses"), 
        dimension.values = list(year = 1980:2030)) 

simplot(sim.mcmc,
        sim.manual,
        outcomes = c("awareness"), 
        dimension.values = list(year = 2000:2030)) 

