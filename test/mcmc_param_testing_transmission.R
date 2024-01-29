source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

load("../jheem_analyses/prelim_results/init.pop.migration.sim_2024-01-25_C.12580.Rdata") # Baltimore

LOCATION = sim$location

sim.mcmc = sim 
params = sim$parameters

params.manual.1 = c(params)
names(params.manual.1) = dimnames(params)[[1]]


engine = create.jheem.engine('ehe', LOCATION, start.year=1970, end.year=2025, max.run.time.seconds = 10)

# First, get a manual run that has an overall trate in the ballpark
params.manual.1["global.trate"] = 0.07
sim.manual.1 = engine$run(parameters = params.manual.1)

# Next, start a new manual run to vary parameters and compare to the first manual 
params.manual.2 = c(params.manual.1)

# These are the transmission parameters we will likely end up varying 
# transmission.parameters =
#   EHE.PARAMETERS.PRIOR@var.names[grepl("trate",EHE.PARAMETERS.PRIOR@var.names) |
#                                    grepl("susceptibility",EHE.PARAMETERS.PRIOR@var.names)]
# params.manual.2[transmission.parameters]

params.manual.2["black.msm.trate.0"] = 2.1 
params.manual.2["black.msm.trate.1"] = 3.5 
params.manual.2["black.msm.trate.2"] = 6 

params.manual.2["other.msm.trate.0"] = 0.1 # changing this to 0.01 seems to affect black msm and idu too much 
params.manual.2["other.msm.trate.1"] = 0.01 
params.manual.2["other.msm.trate.2"] = 0.01 

params.manual.2["black.heterosexual.trate.0"] = 0.4 
params.manual.2["black.heterosexual.trate.1"] = 0.15 
params.manual.2["black.heterosexual.trate.2"] = 0.005 

params.manual.2["other.heterosexual.trate.0"] = 0.00000000001 
params.manual.2["other.heterosexual.trate.1"] = 0.001
params.manual.2["other.heterosexual.trate.2"] = 0.001 

params.manual.2["black.idu.trate.0"] = 7 
params.manual.2["black.idu.trate.1"] = 105 # avoid doing this somehow? 
params.manual.2["black.idu.trate.2"] = 75 

params.manual.2["other.idu.trate.0"] = 0.01 
params.manual.2["other.idu.trate.1"] = 0.01 
params.manual.2["other.idu.trate.2"] = 0.01 

params.manual.2["hispanic.msm.trate.0"] = 1.5 
params.manual.2["hispanic.msm.trate.1"] = 1.5 
params.manual.2["hispanic.msm.trate.2"] = 5 
sim.manual.2 = engine$run(parameters = params.manual.2)

# TRYING FROM THE BEGINNING WITH A LOWER GLOBAL TRATE
params.manual.3 = c(params.manual.1)
params.manual.3["global.trate"] = 0.065

params.manual.3["black.heterosexual.trate.0"] = 1 
params.manual.3["black.heterosexual.trate.1"] = 1 
params.manual.3["black.heterosexual.trate.2"] = 1 

params.manual.3["other.heterosexual.trate.0"] = 0.5 
params.manual.3["other.heterosexual.trate.1"] = 0.5
params.manual.3["other.heterosexual.trate.2"] = 0.5

params.manual.3["black.msm.trate.0"] = 1.5 # this affects heterosexual way more than it does msm 
params.manual.3["black.msm.trate.1"] = 5 
params.manual.3["black.msm.trate.2"] = 5 
sim.manual.3 = engine$run(parameters = params.manual.3)

# Plotting comparisons
# 1 vs 2 
simplot(sim.manual.1, sim.manual.2,
        outcomes = c("new","diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020))
simplot(sim.manual.1, sim.manual.2,
        outcomes = c("diagnosed.prevalence"),
        facet.by = "risk", split.by = "race",
        dimension.values = list(year = 2000:2020))
simplot(sim.manual.1, sim.manual.2,
        outcomes = c("new"),
        facet.by = "risk", split.by = "race",
        dimension.values = list(year = 2000:2020))

# 2 vs 3 
simplot(sim.manual.2, sim.manual.3,
        outcomes = c("new","diagnosed.prevalence"),
        dimension.values = list(year = 2000:2020))
simplot(sim.manual.2, sim.manual.3,
        outcomes = c("new"),
        facet.by = "risk", split.by = "race",
        dimension.values = list(year = 2000:2020))
simplot(sim.manual.2, sim.manual.3,
        outcomes = c("diagnosed.prevalence"),
        facet.by = "risk", split.by = "race",
        dimension.values = list(year = 2000:2020))

# other plots
simplot(sim.manual.1, sim.manual.2,
        outcomes = c("new","diagnosed.prevalence"),
        facet.by = "risk", split.by = "race",
        dimension.values = list(year = 2000:2020))
simplot(sim.manual.1, sim.manual.2,
        outcomes = c("new","diagnosed.prevalence"),
        facet.by = "risk", 
        dimension.values = list(year = 2000:2020))
simplot(sim.manual.1, sim.manual.2,
        outcomes = c("new","diagnosed.prevalence"),
        facet.by = "race", 
        dimension.values = list(year = 2000:2020))


# Checking likelihoods 
full.lik = full.likelihood.instructions$instantiate.likelihood('ehe', LOCATION)

exp(full.lik$compute.piecewise(sim.manual.3,check.consistency=F) - full.lik$compute.piecewise(sim.manual.2,check.consistency=F)) 
exp(full.lik$compute.piecewise(sim.manual.3,check.consistency=F) - full.lik$compute.piecewise(sim.manual.1,check.consistency=F)) 

exp(full.lik$compute.piecewise(sim.manual.2,check.consistency=F) - full.lik$compute.piecewise(sim.manual.1,check.consistency=F)) 

exp(full.lik$compute(sim.manual,check.consistency=F) - full.lik$compute(sim.mcmc,check.consistency=F)) 
exp(full.lik$compute.piecewise(sim.manual,check.consistency=F) - full.lik$compute.piecewise(sim.mcmc,check.consistency=F)) 
full.lik$compute(sim.manual,check.consistency=F)
full.lik$compute.piecewise(sim.manual,check.consistency=F)

# pop.lik = population.likelihood.instructions$instantiate.likelihood('ehe', LOCATION)
# imm.lik = immigration.likelihood.instructions$instantiate.likelihood('ehe', LOCATION)
# em.lik = emigration.likelihood.instructions$instantiate.likelihood('ehe', LOCATION)
# prev.lik = prevalence.likelihood.instructions$instantiate.likelihood('ehe', LOCATION)
# new.lik = new.diagnoses.likelihood.instructions$instantiate.likelihood('ehe', LOCATION)
# mort.lik = mortality.likelihood.instructions$instantiate.likelihood('ehe', LOCATION) # this one is the problem 


