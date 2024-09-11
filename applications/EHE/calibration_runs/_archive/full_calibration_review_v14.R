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


full.lik = FULL.likelihood.instructions.with.covid$instantiate.likelihood('ehe','C.12580') 
full.lik$compare.sims(sim.0808.last,sim.0809.last) # second minus first
exp(full.lik$compute(sim.0809.last) - full.lik$compute(sim.0808.last))
exp(full.lik$compute.piecewise(sim.0809.last) - full.lik$compute.piecewise(sim.0808.last))

simplot(sim.0808.last,
        sim.0809.last,
        facet.by = "age", split.by = "race", 
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.0808.last,
        sim.0809.last,
        outcomes = c("new"), 
        style.manager = source.style.manager, 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.0808.last,
        sim.0809.last,
        facet.by = "risk", split.by = "race", 
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.0808.last,
        sim.0809.last,
        facet.by = "age", split.by = "sex", 
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.0808.last,
        sim.0809.last,
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.0808.last,
        sim.0809.last,
        facet.by = "risk", split.by = "race", 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.0808.last,
        sim.0809.last,
        facet.by = "age", split.by = "sex",
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.0808.last,
        sim.0809.last,
        outcomes = c("aids.diagnoses"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(sim.0808.last,
        sim.0809.last,
        facet.by = "race",
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.0808.last,
        sim.0809.last,
        outcomes = c("cdc.hiv.test.positivity"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.0808.last,
        sim.0809.last,
        outcomes = c("awareness"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.0808.last,
        sim.0809.last,
        #facet.by = "age", # age; 1-way 
        outcomes = c("proportion.using.heroin",
                     "proportion.using.cocaine"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 


params.0809 = sim.0809.last$params
params.0808 = sim.0808.last$params
msm.trate.params = names(params.0809[grepl("msm.trate",names(params.0809))])
idu.trate.params = names(params.0809[grepl("idu.trate",names(params.0809))])
testing.params = names(params.0809[grepl("test",names(params.0809))])

round(cbind(params.0808[c(msm.trate.params,idu.trate.params)],
            params.0809[c(msm.trate.params,idu.trate.params)]),3)

round(cbind(params.0808[c(testing.params)],
            params.0809[c(testing.params)]),3)







