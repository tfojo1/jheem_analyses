source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum")

load("../jheem_analyses/prelim_results/init.pop.ehe_simset_2024-09-30_C.12060.Rdata")
simset.pop = simset

# full.with.aids has no significant differences in population - going to work with population calibration instead 
# load("../jheem_analyses/prelim_results/full.with.aids_simset_2024-10-01_C.12060.Rdata")
# sim.full = simset
 
atl.pop.lik = joint.pop.migration.total.trans.likelihood.instructions$instantiate.likelihood('ehe','C.12060')
 
engine = create.jheem.engine('ehe','C.12060',end.year = 2025)
params.mcmc = simset.pop$last.sim()$params
sim.mcmc = engine$run(params.mcmc)
 
# cbind(sim.mcmc$get.params("birth"))
# cbind(sim.mcmc$get.params("mortality"))
# cbind(sim.mcmc$get.params("migration"))
cbind(sim.mcmc$get.params("other.age2"))
# 

params.manual = params.mcmc
sim.manual = engine$run(params.manual)
params.manual["other.birth.rate.multiplier"] = 1.07 # 0.8792656
# params.manual["other.age1.migration.multiplier.time.1"] = 1 # 1.060586
params.manual["other.age1.migration.multiplier.time.2"] = .75 # 1.021673
# params.manual["other.age1.aging.multiplier.1"] = 1 # 1.296845
params.manual["other.age1.aging.multiplier.2"] = 1.3 # 1.000605
params.manual["other.age2.aging.multiplier.1"] = 1 # 0.9872116
params.manual["other.age2.aging.multiplier.2"] = 1.2 # 0.9825717
params.manual["other.age2.migration.multiplier.time.1"] = 1.5 # 0.9866867
params.manual["other.age2.migration.multiplier.time.2"] = .8 # 1.0198221
params.manual["other.age4.migration.multiplier.time.2"] = .8 # 1.0343240

sim.manual = engine$run(params.manual)
# 
atl.pop.lik$compare.sims(sim.mcmc,sim.manual)
#exp(atl.pop.lik$compute(sim.manual) - atl.pop.lik$compute(sim.mcmc))

# qplot(1:simset.pop$n.sim, simset.pop$parameters["other.immigration.multiplier.time.1",]) 

simplot(sim.mcmc,
        sim.manual,
        facet.by = "age", split.by = "race",
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.mcmc,
        sim.manual,
        outcomes = c("new"), 
        style.manager = source.style.manager, 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.mcmc,
        sim.manual,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager, 
        dimension.values = list(year = 2000:2030)) 


sim.engine.test = engine$run(parameters = params)