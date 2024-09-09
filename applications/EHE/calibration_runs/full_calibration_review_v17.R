source('../jheem_analyses/applications/EHE/ehe_specification.R')
source('../jheem_analyses/applications/EHE/ehe_likelihoods.R')
source('../jheem_analyses/commoncode/locations_of_interest.R')

location.style.manager = create.style.manager(color.data.by = "location.type") 
source.style.manager = create.style.manager(color.data.by = "source") 
stratum.style.manager = create.style.manager(color.data.by = "stratum")

load('../jheem_analyses/prelim_results/full.with.covid2_simset_2024-08-29_C.12580.Rdata')
simset.balt.08.29 = simset
sim.balt.08.29.last = simset.balt.08.29$last.sim()

load('../jheem_analyses/prelim_results/full.with.covid2_simset_2024-08-29_C.12580.Rdata')
simset.balt.09.06 = simset
sim.balt.09.06.last = simset.balt.09.06$last.sim()

full.with.covid.lik = FULL.likelihood.instructions.with.covid$instantiate.likelihood('ehe','C.12580')

engine = create.jheem.engine('ehe','C.12580',end.year = 2025)
params.mcmc = sim.balt.09.06.last$params
params.manual = params.mcmc

cbind(params.manual[grepl("suppress",names(params.manual))])

params.manual["hispanic.suppressed.or"] = 2 # 89.0999283
sim.manual = engine$run(params.manual)

# suppression improved but new diagnoses/prevalence are both worse now
full.with.covid.lik$compare.sims(sim.balt.09.06.last,sim.manual)

simplot(#simset.balt.09.06,
        sim.balt.09.06.last,
        sim.manual,
        #facet.by = "race", # age, sex, race, risk; 1-way 
        outcomes = c("suppression"), 
        style.manager = location.style.manager,
        dimension.values = list(year = 2000:2030)) 

simplot(sim.balt.09.06.last,
        sim.manual,
        facet.by = "risk", split.by = "race", 
        outcomes = c("new"), 
        dimension.values = list(year = 2000:2030)) 

simplot(sim.balt.09.06.last,
        sim.manual,
        facet.by = "risk", split.by = "race", 
        outcomes = c("diagnosed.prevalence"), 
        dimension.values = list(year = 2000:2030)) 

# changing p.bias looked like it didn't do anything? 
simplot(#simset.balt.08.29,
  sim.balt.08.29.last,
  #simset.balt.09.06,
  sim.balt.09.06.last,
  outcomes = c("awareness"), 
  style.manager = location.style.manager,
  dimension.values = list(year = 2000:2030)) 

## OTHER PLOTS ####

simplot(#simset.balt,
  #sim.balt.last,
  # simset.houston,
  # sim.houston.last,
  simset.nyc,
  sim.nyc.last,
  facet.by = "race", #split.by = "race", 
  outcomes = c("population"), 
  dimension.values = list(year = 2000:2030)) 

simplot(#simset.balt,
  #sim.balt.last,
  # simset.houston,
  # sim.houston.last,
  simset.nyc,
  sim.nyc.last,
  outcomes = c("new"), 
  style.manager = source.style.manager, 
  dimension.values = list(year = 2000:2030)) 



simplot(#simset.balt,
  #sim.balt.last,
  # simset.houston,
  # sim.houston.last,
  simset.nyc,
  sim.nyc.last,
  facet.by = "age", split.by = "sex", 
  outcomes = c("new"), 
  dimension.values = list(year = 2000:2030)) 

simplot(#simset.balt,
  #sim.balt.last,
  # simset.houston,
  # sim.houston.last,
  simset.nyc,
  sim.nyc.last,
  outcomes = c("diagnosed.prevalence"), 
  style.manager = source.style.manager,
  dimension.values = list(year = 2000:2030)) 

simplot(#simset.balt,
  #sim.balt.last,
  # simset.houston,
  # sim.houston.last,
  simset.nyc,
  sim.nyc.last,
  facet.by = "risk", split.by = "race", 
  outcomes = c("diagnosed.prevalence"), 
  dimension.values = list(year = 2000:2030)) 

simplot(#simset.balt,
  #sim.balt.last,
  # simset.houston,
  # sim.houston.last,
  simset.nyc,
  sim.nyc.last,
  facet.by = "age", split.by = "sex",
  outcomes = c("diagnosed.prevalence"), 
  dimension.values = list(year = 2000:2030)) 

simplot(#simset.balt,
  #sim.balt.last,
  # simset.houston,
  # sim.houston.last,
  simset.nyc,
  sim.nyc.last,
  facet.by = "age",
  outcomes = c("testing"),
  style.manager = location.style.manager,
  dimension.values = list(year = 2000:2030)) 

simplot(#simset.balt,
  #sim.balt.last,
  # simset.houston,
  # sim.houston.last,
  simset.nyc,
  sim.nyc.last,
  outcomes = c("cdc.hiv.test.positivity"), 
  style.manager = location.style.manager,
  dimension.values = list(year = 2000:2030)) 


simplot(#simset.balt,
  #sim.balt.last,
  # simset.houston,
  # sim.houston.last,
  simset.nyc,
  sim.nyc.last,
  facet.by = "sex", # sex; 1-way 
  outcomes = c("hiv.mortality"),
  style.manager = source.style.manager,
  dimension.values = list(year = 2000:2030)) 

simplot(#simset.balt,
  #sim.balt.last,
  # simset.houston,
  # sim.houston.last,
  simset.nyc,
  sim.nyc.last,
  outcomes = c("total.mortality"), # totals only 
  style.manager = location.style.manager,
  dimension.values = list(year = 2000:2030)) 

simplot(#simset.balt,
  #sim.balt.last,
  # simset.houston,
  # sim.houston.last,
  simset.nyc,
  sim.nyc.last,
  outcomes = c("aids.deaths"), 
  style.manager = location.style.manager,
  dimension.values = list(year = 1981:2001)) 

simplot(#simset.balt,
  #sim.balt.last,
  # simset.houston,
  # sim.houston.last,
  simset.nyc,
  sim.nyc.last,
  facet.by = "age", # age, sex, race, risk; 1-way 
  outcomes = c("aids.diagnoses"), 
  style.manager = location.style.manager,
  dimension.values = list(year = 1980:2030)) 

simplot(#simset.balt,
  #sim.balt.last,
  # simset.houston,
  # sim.houston.last,
  simset.nyc,
  sim.nyc.last,
  outcomes = c("aids.diagnoses"), 
  style.manager = source.style.manager,
  dimension.values = list(year = 1980:2030)) 

simplot(#simset.balt,
  #sim.balt.last,
  # simset.houston,
  # sim.houston.last,
  simset.nyc,
  sim.nyc.last,
  facet.by = "age", # age; 1-way 
  outcomes = c("proportion.using.heroin",
               "proportion.using.cocaine"), 
  style.manager = location.style.manager,
  dimension.values = list(year = 2000:2030)) 

simplot(#simset.balt,
  #sim.balt.last,
  # simset.houston,
  # sim.houston.last,
  simset.nyc,
  sim.nyc.last,
  facet.by = "age", # age, sex, race; 1- and 2-way 
  outcomes = c("prep.uptake"), 
  style.manager = source.style.manager,
  dimension.values = list(year = 2000:2030)) 

simplot(#simset.balt,
  #sim.balt.last,
  # simset.houston,
  # sim.houston.last,
  simset.nyc,
  sim.nyc.last,
  facet.by = "age", # age, sex; 1-way 
  outcomes = c("prep.indications"), 
  style.manager = location.style.manager,
  dimension.values = list(year = 2000:2030)) 

simplot(#simset.balt,
  #sim.balt.last,
  # simset.houston,
  # sim.houston.last,
  simset.nyc,
  sim.nyc.last,
  outcomes = c("sexual.transmission.rates"), 
  style.manager = location.style.manager,
  plot.year.lag.ratio = T,
  dimension.values = list(year = 2000:2030)) 


simplot(simset.houston.trans,sim.houston.trans,
        outcomes = c("sexual.transmission.rates"), 
        style.manager = location.style.manager,
        plot.year.lag.ratio = T,
        dimension.values = list(year = 2000:2030)) 
