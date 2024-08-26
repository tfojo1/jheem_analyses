source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum")

load('../jheem_analyses/prelim_results/full.with.covid2_simset_2024-08-08_C.12580.Rdata')
simset.0808 = simset
sim.0808.last = simset.0808$last.sim()

load('../jheem_analyses/prelim_results/full.with.aids_simset_2024-08-09_C.12580.Rdata')
simset.0809 = simset
sim.0809.last = simset.0809$last.sim()

load('../jheem_analyses/prelim_results/full.with.aids_simset_2024-08-20_C.12580.Rdata')
simset.0820 = simset
sim.0820.last = simset.0820$last.sim()


# full.lik = FULL.likelihood.instructions.with.covid$instantiate.likelihood('ehe','C.12580') 
# full.lik$compare.sims(sim.0808.last,sim.0809.last) # second minus first
# exp(full.lik$compute(sim.0809.last) - full.lik$compute(sim.0808.last))
# exp(full.lik$compute.piecewise(sim.0809.last) - full.lik$compute.piecewise(sim.0808.last))
# full.lik$compare.sims(sim.0809.last,sim.0820.last) # second minus first
# exp(full.lik$compute(sim.0820.last) - full.lik$compute(sim.0809.last))

full.with.aids.lik = FULL.likelihood.instructions.with.aids$instantiate.likelihood('ehe','C.12580')
full.with.aids.lik$compare.sims(sim.0820.last,sim.manual) # second minus first
exp(full.with.aids.lik$compute(sim.manual) - full.with.aids.lik$compute(sim.0820.last))

cbind(simset.0820$get.params("susceptibility.rr"))

params.0820 = sim.0820.last$params
params.manual = params.0820

engine = create.jheem.engine('ehe','C.12580',end.year = 2025)
params.manual["max.covid.effect.prep.uptake.reduction"] = 1 
#sim.manual = engine$run(params.manual)

params.manual["age5.proportion.tested.or"] = 1 # 0.1545957
params.manual["age5.msm.susceptibility.rr"] = .25 # 0.9194297
params.manual["age1.black.msm.susceptibility.rr.01"] = 1 # 0.7012944


# params.manual["age1.black.msm.susceptibility.rr.2"] # 0.8078143
# params.manual["age2.black.msm.susceptibility.rr.01"] # 1.0515466
# params.manual["age2.black.msm.susceptibility.rr.2"] # 2.0190888
# params.manual["age1.hispanic.msm.susceptibility.rr.01"] # 0.7741866
# params.manual["age1.hispanic.msm.susceptibility.rr.2"] # 0.8587093
# params.manual["age2.hispanic.msm.susceptibility.rr.01"] # 1.1093842
# params.manual["age2.hispanic.msm.susceptibility.rr.2"] # 3.9916705
# params.manual["age1.other.msm.susceptibility.rr.01"] # 0.5029266
# params.manual["age1.other.msm.susceptibility.rr.2"] # 0.9330043
# params.manual["age2.other.msm.susceptibility.rr.01"] # 1.6784524
# params.manual["age2.other.msm.susceptibility.rr.2"] # 1.5656743


#params.manual["age5.heterosexual.susceptibility.rr"] # 0.2483298
#params.manual["age5.idu.susceptibility.rr"] # 0.7538771
#params.manual["age5.susceptibility.rr.mult.0"] # 0.2919801
sim.manual = engine$run(params.manual)

simplot(#sim.0808.last,
        sim.0820.last,
        sim.manual,
        facet.by = "age", split.by = "race", 
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

simplot(#sim.0808.last,
        sim.0820.last,
        sim.manual,
        outcomes = c("new"), 
        style.manager = source.style.manager, 
        dimension.values = list(year = 2000:2030)) 

simplot(#sim.0808.last,
        sim.0820.last,
        sim.manual,
        facet.by = "risk", split.by = "race", 
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(#sim.0808.last,
        sim.0820.last,
        sim.manual,
        facet.by = "age", split.by = "sex", 
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(#sim.0808.last,
        sim.0820.last,
        sim.manual,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(#sim.0808.last,
        sim.0820.last,
        sim.manual,
        facet.by = "risk", split.by = "race", 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) 

simplot(#sim.0808.last,
        #sim.0809.last,
        sim.0820.last,
        sim.manual,
        facet.by = "age", split.by = "sex",
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) 

simplot(#sim.0808.last,
        sim.0820.last,
        sim.manual,
        facet.by = "age",
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(#sim.0808.last,
        sim.0820.last,
        sim.manual,
        outcomes = c("cdc.hiv.test.positivity"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(#sim.0808.last,
        sim.0820.last,
        sim.manual,
        outcomes = c("awareness"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

