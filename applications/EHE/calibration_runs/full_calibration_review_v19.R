source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum")

LOCATION = NYC.MSA

if(LOCATION==BALTIMORE.MSA){
  load(paste0("../jheem_analyses/prelim_results/full.with.covid2_simset_2024-10-03_",LOCATION,".Rdata"))
  simset.older = simset
  
  load(paste0("../jheem_analyses/prelim_results/full.with.covid2_simset_2024-10-07_",LOCATION,".Rdata"))
  simset.old = simset
  
  load(paste0("../jheem_analyses/prelim_results/full.with.covid2_simset_2024-10-11_",LOCATION,".Rdata"))
  simset.new = simset
  
} else {
  load(paste0("../jheem_analyses/prelim_results/full.with.aids_simset_2024-10-03_",LOCATION,".Rdata"))
  simset.older = simset
  
  load(paste0("../jheem_analyses/prelim_results/full.with.aids_simset_2024-10-05_",LOCATION,".Rdata"))
  simset.old = simset  
  
  load(paste0("../jheem_analyses/prelim_results/full.with.aids_simset_2024-10-11_",LOCATION,".Rdata"))
  simset.new = simset  
}

# balt.lik = FULL.likelihood.instructions.with.covid$instantiate.likelihood('ehe','C.12580')
# nyc.lik = FULL.likelihood.instructions.with.aids$instantiate.likelihood('ehe','C.35620')
# houston.lik = FULL.likelihood.instructions.with.aids$instantiate.likelihood('ehe','C.26420')
# chicago.lik = FULL.likelihood.instructions.with.aids$instantiate.likelihood('ehe','C.16980')
# atl.lik = FULL.likelihood.instructions.with.aids$instantiate.likelihood('ehe','C.12060')

# COMPARING TO 10/7 MCMC

## CHICAGO ## 
# New simset is worse overall, including prevalence, but suppression improved 
chicago.lik$compare.sims(simset.old$last.sim(),simset.new$last.sim()) 
exp(chicago.lik$compute(simset.new$last.sim()) - chicago.lik$compute(simset.old$last.sim()))

## NYC ##
# New simset is worse overall, including prevalence
# Suppression looks better but scored worse (probably because of trend of line)
nyc.lik$compare.sims(simset.old$last.sim(),simset.new$last.sim()) 
exp(nyc.lik$compute(simset.new$last.sim()) - nyc.lik$compute(simset.old$last.sim()))

## HOUSTON ## 
# New simset is better overall, including suppression
houston.lik$compare.sims(simset.old$last.sim(),simset.new$last.sim()) 
exp(houston.lik$compute(simset.new$last.sim()) - houston.lik$compute(simset.old$last.sim()))

## ATLANTA ## 
# New simset is better overall, including suppression and prevalence (new creeping up a bit)
atl.lik$compare.sims(simset.old$last.sim(),simset.new$last.sim()) 
exp(atl.lik$compute(simset.new$last.sim()) - atl.lik$compute(simset.old$last.sim()))

## BALTIMORE ## 
# New simset is better overall, but new diagnoses taking off and awareness trending down
balt.lik$compare.sims(simset.old$last.sim(),simset.new$last.sim()) 
exp(balt.lik$compute(simset.new$last.sim()) - balt.lik$compute(simset.old$last.sim()))

plot(1:simset.new$n.sim,simset.new$parameters["black.msm.trate.0",])
plot(1:simset.new$n.sim,simset.new$parameters["age1.black.msm.susceptibility.rr.01",])

simplot(simset.old$last.sim(),
        #simset.new,
        simset.new$last.sim(),
        facet.by = "age", split.by = "race", # age, sex, race; 1- and 2-way
        outcomes = c("population"), 
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.old$first.sim(),
        simset.old$last.sim(),
        #simset.new$first.sim(),
        simset.new$last.sim(),
        outcomes = c("diagnosed.prevalence"), 
        style.manager = source.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "risk", split.by = "race",
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "age", split.by = "sex", 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.old$last.sim(),
        simset.new,      
        simset.new$last.sim(),
        outcomes = c("new"), 
        style.manager = source.style.manager, 
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "risk", split.by = "race", 
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(#simset.old$last.sim(),
        simset.new$last.sim(),
        facet.by = "age", split.by = "sex", 
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
       # facet.by = "age", 
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
        outcomes = c("cdc.hiv.test.positivity"), # totals only 
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

simplot(simset.old$last.sim(),
        simset.new$last.sim(),
        #facet.by = "age", # age, sex, race, risk; 1-way 
        outcomes = c("suppression"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 
