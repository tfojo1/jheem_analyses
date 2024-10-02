source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum")

LOCATION = NYC.MSA

if(LOCATION==BALTIMORE.MSA){
  load(paste0("../jheem_analyses/prelim_results/full.with.covid2_simset_2024-09-27_",LOCATION,".Rdata"))
  simset.old = simset
  
  load(paste0("../jheem_analyses/prelim_results/full.with.covid2_simset_2024-10-02_",LOCATION,".Rdata"))
  simset.new = simset
  
} else {
  load(paste0("../jheem_analyses/prelim_results/full.with.aids_simset_2024-09-27_",LOCATION,".Rdata"))
  simset.old = simset
  
  load(paste0("../jheem_analyses/prelim_results/full.with.aids_simset_2024-10-01_",LOCATION,".Rdata"))
  simset.new = simset  
}

balt.lik = FULL.likelihood.instructions.with.covid$instantiate.likelihood('ehe','C.12580')
nyc.lik = FULL.likelihood.instructions.with.aids$instantiate.likelihood('ehe','C.35620')
houston.lik = FULL.likelihood.instructions.with.aids$instantiate.likelihood('ehe','C.26420')
chicago.lik = FULL.likelihood.instructions.with.aids$instantiate.likelihood('ehe','C.16980')
atl.lik = FULL.likelihood.instructions.with.aids$instantiate.likelihood('ehe','C.12060')

#atl.pop.lik$compare.sims(simset.old$last.sim(),simset.new$last.sim()) 
#exp(atl.pop.lik$compute(simset.new$last.sim()) - atl.pop.lik$compute(simset.old$last.sim()))

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "age", split.by = "race", # age, sex, race; 1- and 2-way
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.old$last.sim(),
        simset.new,
        simset.new$last.sim(),
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "risk", split.by = "race",
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "age", split.by = "sex", 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        outcomes = c("new"), 
        style.manager = source.style.manager, 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "risk", split.by = "race", 
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "age", split.by = "sex", 
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "age", 
        outcomes = c("testing"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        outcomes = c("awareness"),
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "age", # age, sex, race, risk; 1-way 
        outcomes = c("aids.diagnoses"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        outcomes = c("cdc.hiv.test.positivity"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        outcomes = c("aids.diagnoses"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 1980:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "sex", # sex; 1-way 
        outcomes = c("hiv.mortality"),
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        outcomes = c("total.mortality"), # totals only 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 
