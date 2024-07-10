source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

full.lik = FULL.likelihood.instructions.with.covid$instantiate.likelihood('ehe','C.12580')
engine = create.jheem.engine('ehe','C.12580',end.year = 2030)

load('../jheem_analyses/prelim_results/full.with.aids_simset_2024-07-09_C.12580.Rdata')
simset.full.no.covid.07.09 = simset
sim.07.09 = simset.full.no.covid.07.09$last.sim()
params.07.09 = sim.07.09$params
sim.07.09 = engine$run(parameters = params.07.09) # have to rerun with params to get likelihoods to match 
params.manual = params.07.09

# identify blocks of parameters we might be interested in
msm.trates = names(params.manual[grepl("msm.trate",names(params.manual))])
age.susceptibility = names(params.manual[grepl("age..susceptibility",names(params.manual))])
msm.age.susceptibility = names(params.manual[grepl("msm.susceptibility",names(params.manual))])
hiv.aging = names(params.manual[grepl("hiv.aging",names(params.manual))])

cbind(params.manual[msm.age.susceptibility])

params.manual["age1.msm.susceptibility.rr.mult.0"] = 5 # 1.0901636
#params.manual["age1.msm.susceptibility.rr.mult.1"] = 2 # 0.9352303
# params.manual["age1.msm.susceptibility.rr.mult.2"] = 2 # 1.552993

params.manual["black.msm.trate.0"] = 3 # 4.037667

params.manual["other.msm.trate.0"] = .95 # 1.091424
#params.manual["other.msm.trate.1"] = 60 # 63.83674602

sim.manual = engine$run(parameters = params.manual)

simplot(sim.07.09,
        sim.manual,
        facet.by = "age", split.by = "sex", 
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.07.09,
        sim.manual,
        facet.by = "risk", split.by = "race", 
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.07.09,
        sim.manual,
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.07.09,
        sim.manual,
        facet.by = "age", split.by = "sex", 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.07.09,
        sim.manual,
        facet.by = "risk", split.by = "race", 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) 

exp(full.lik$compute.piecewise(sim.manual) - full.lik$compute.piecewise(sim.07.09))
